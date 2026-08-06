library(shiny)
library(bslib)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(plotly)
library(sf)
library(googledrive)

# --- 1. LOAD DATA FROM SHINY BUNDLE ---
drive_auth(path = ("google/ru4m-dashboard-60f0e985d2fe.json"))
target_folder_id <- "17quURw4Jfk28B6xiC6nOyV5zvR151Dr6"

load_drive_data <- function() {
  temp_file <- tempfile(fileext = ".rds")
  folder_ref <- drive_get(as_id(target_folder_id))
  
  drive_download(
    file = drive_ls(folder_ref, pattern = "^shiny_app_data\\.rds$"),
    path = temp_file,
    overwrite = TRUE
  )
  
  data <- readRDS(temp_file)
  unlink(temp_file)
  return(data)
}

shiny_data_bundle <- load_drive_data()

# Unpack bundle elements into local global environment
geo_info              <- shiny_data_bundle$geo_info
mi_counties_sf        <- shiny_data_bundle$mi_counties_sf
mi_regions_sf         <- shiny_data_bundle$mi_regions_sf
all_forecasts         <- shiny_data_bundle$all_forecasts
current_fcst          <- shiny_data_bundle$all_forecasts
weather_data          <- shiny_data_bundle$weather_data
minet_data            <- shiny_data_bundle$minet_data
matched_comparison_df <- shiny_data_bundle$matched_comparison_df

# --- FILTER OUT SITES 3250 AND 2233 GLOBALLY ---
excluded_sites <- c("3250", "2233")

if ("id" %in% names(geo_info)) geo_info <- geo_info %>% filter(!as.character(id) %in% excluded_sites)
if ("id" %in% names(minet_data)) minet_data <- minet_data %>% filter(!as.character(id) %in% excluded_sites)
if ("id" %in% names(all_forecasts)) all_forecasts <- all_forecasts %>% filter(!as.character(id) %in% excluded_sites)
if ("id" %in% names(current_fcst)) current_fcst <- current_fcst %>% filter(!as.character(id) %in% excluded_sites)
if ("id" %in% names(matched_comparison_df)) matched_comparison_df <- matched_comparison_df %>% filter(!as.character(id) %in% excluded_sites)
if ("id" %in% names(weather_data)) weather_data <- weather_data %>% filter(!as.character(id) %in% excluded_sites)

# Standardize Lat/Lon column names in geo_info robustly
geo_names <- names(geo_info)
lat_col  <- geo_names[tolower(geo_names) %in% c("latitude", "lat")][1]
lon_col  <- geo_names[tolower(geo_names) %in% c("longitude", "long", "lon")][1]

if (!is.na(lat_col) && !is.na(lon_col)) {
  geo_info <- geo_info %>%
    rename(Latitude = !!sym(lat_col), Longitude = !!sym(lon_col))
}

# Extract Config Dates
index_date    <- as.Date(shiny_data_bundle$config$index_date)
tab2_min_date <- as.Date(shiny_data_bundle$config$tab2_min_date)
tab2_max_date <- as.Date(shiny_data_bundle$config$tab2_max_date)

mi_counties_map <- suppressWarnings(st_drop_geometry(mi_counties_sf))

# Calculate Maximum Raw Limits for Sliders
max_ecoli_val <- if (nrow(minet_data) > 0) ceiling(max(exp(minet_data$ecoli_log) - 0.001, na.rm = TRUE)) else 1000
max_bacti_val <- if (nrow(minet_data) > 0) ceiling(max(exp(minet_data$bactiquick_log) - 0.001, na.rm = TRUE)) else 1000

# --- GLOBAL LIMITS ---
global_ecoli_limits <- c(min(current_fcst$Forecasted_Ecoli_Level, na.rm = TRUE), max(current_fcst$Forecasted_Ecoli_Level, na.rm = TRUE))
global_tmean_limits <- c(min(weather_data$tmean_10km_avg, na.rm = TRUE), max(weather_data$tmean_10km_avg, na.rm = TRUE))
global_ppt_limits   <- c(min(weather_data$ppt_10km_avg, na.rm = TRUE), max(weather_data$ppt_10km_avg, na.rm = TRUE))

minet_1yr <- minet_data %>% filter(as.Date(SampleDate) >= tab2_min_date & as.Date(SampleDate) <= tab2_max_date)

valid_hist_dates <- sort(unique(as.Date(minet_1yr$SampleDate)), decreasing = FALSE) 
valid_hist_dates_char <- as.character(valid_hist_dates)

if(length(valid_hist_dates_char) == 1) {
  valid_hist_dates_char <- c(valid_hist_dates_char, paste0(valid_hist_dates_char, " (Only Date)"))
} else if(length(valid_hist_dates_char) == 0) {
  valid_hist_dates_char <- c("No Data", "Available")
}

if(nrow(minet_1yr) > 0) {
  global_hist_ecoli_lims <- c(min(minet_1yr$ecoli_log, na.rm=TRUE), max(minet_1yr$ecoli_log, na.rm=TRUE))
  global_hist_bacti_lims <- c(min(minet_1yr$bactiquick_log, na.rm=TRUE), max(minet_1yr$bactiquick_log, na.rm=TRUE))
} else {
  global_hist_ecoli_lims <- c(0, 10)
  global_hist_bacti_lims <- c(0, 10)
}

# Helpers for min/max that gracefully handle all-NA groups (avoid Inf/-Inf)
safe_min <- function(x) { v <- suppressWarnings(min(x, na.rm = TRUE)); if (is.infinite(v)) NA_real_ else v }
safe_max <- function(x) { v <- suppressWarnings(max(x, na.rm = TRUE)); if (is.infinite(v)) NA_real_ else v }

# UI Choices
site_choices_vec <- unname(as.character(geo_info$id))
site_names_vec   <- as.character(geo_info$BeachName)
site_choices     <- c("All Sites (Statewide / Regional)" = "All", setNames(site_choices_vec, site_names_vec))

# --- 2. SHINY UI ---
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly", primary = "#2c3e50"),
  titlePanel("Michigan Emergency Preparedness Regions: E. coli Forecast & Monitoring"),
  
  tabsetPanel(
    # --- TAB 1: Performance & Comparison ---
    tabPanel("Performance & Comparison",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput("waterbody_filter", "Waterbody Type:",
                             choices = c("Any", "Inland Lake", "Great Lake", "River"),
                             selected = "Any"),
                 
                 selectInput("exceedance_filter", "Filter Sites by Exceedance:",
                             choices = c("All Sites" = "all",
                                         "At least 1 E. coli exceedance" = "ecoli",
                                         "At least 1 Bactiquick exceedance" = "bacti",
                                         "At least 1 exceedance in either" = "either"),
                             selected = "all"),
                 
                 selectInput("corr_filter", "Filter Sites by Correlation (\u03c1):",
                             choices = c("Any Correlations" = "all",
                                         "Strong Positive (\u03c1 \u2265 0.8)" = "strong_pos",
                                         "Moderate Positive (\u03c1 \u2265 0.5)" = "mod_pos",
                                         "Weak/Negative (\u03c1 < 0.5)" = "weak_neg"),
                             selected = "all"),
                 
                 sliderInput("ecoli_limit", "E. coli Level Range (MPN):",
                             min = 0, max = max_ecoli_val, value = c(0, max_ecoli_val)),
                 
                 sliderInput("bacti_limit", "Bactiquick Level Range (ERU):",
                             min = 0, max = max_bacti_val, value = c(0, max_bacti_val)),
                 
                 selectizeInput("comp_site", "Select Specific Site:", choices = NULL),
                 
                 sliderInput("bacti_thresh_val", "Bactiquick Exceedance Threshold (ERU):",
                             min = 10, max = 150, value = 100, step = 10),
                 hr(),
                 radioButtons("color_mode", "Relationship Color Mode:",
                              choices = c("Exceedance Disagreement" = "disagree",
                                          "Percentage Discordance" = "discordant")),
                 conditionalPanel(
                   condition = "input.color_mode == 'discordant'",
                   sliderInput("discordance_pct", "Discordance Threshold (%):",
                               min = 5, max = 100, value = 50, step = 5)
                 ),
                 helpText("This view compares lab testing data from different testing methods and against forecasted values.")
               ),
               mainPanel(
                 width = 9,
                 tabsetPanel(
                   tabPanel("Log Scale Results",
                            h4("Forecasts and Assay Results (Log Scale)"),
                            # Adjusts to 40% of the user's screen height
                            plotlyOutput("compare_plot", width = "100%", height = "40vh"), 
                            hr(),
                            h4("E. Coli Colilert 18 (MPN) and Bactiquick (ERU) Relationship (Log Scale)"),
                            div(
                              style = "width: 100%; max-width: 60vw; aspect-ratio: 1.25 / 1; height: auto;",
                              plotlyOutput("bact_scatter", width = "100%", height = "100%")
                            )
                   ),
                   tabPanel("Raw Assay Results",
                            h4("Forecasts and Actual Tests (Raw)"),
                            # Adjusts to 40% of the user's screen height
                            plotlyOutput("compare_plot_raw", width = "100%", height = "40vh"),
                            hr(),
                            h4("E. Coli Colilert 18 (MPN) and Bactiquick (ERU) Relationship (Raw)"),
                            div(
                              style = "width: 100%; max-width: 60vw; aspect-ratio: 1.25 / 1; height: auto;",
                              plotlyOutput("bact_scatter_raw", width = "100%", height = "100%")
                            )
                   )
                 )
               )
             )
    ),
    
    # --- TAB 2: Historic Maps ---
    tabPanel("Historic E. coli & Bactiquick Maps",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 sliderTextInput(
                   inputId = "tab2_date", 
                   label = "Select Historic Date (Past Year Sampling):",
                   choices = valid_hist_dates_char, 
                   selected = tail(valid_hist_dates_char, 1), 
                   animate = animationOptions(interval = 2500, loop = TRUE)
                 ),
                 helpText("Select a date to view historical site test levels across the state.")
               ),
               mainPanel(
                 width = 9,
                 h4("Daily Historic Sampling"),
                 fluidRow(
                   column(6, h5("Daily E. coli (MPN)", align = "center"), plotlyOutput("hist_ecoli_map", width = "100%", height = "350px")),
                   column(6, h5("Daily Bactiquick (ERU)", align = "center"), plotlyOutput("hist_bacti_map", width = "100%", height = "350px"))
                 ),
                 hr(),
                 h4("Past 7-Day Average (Leading up to selected date)"),
                 fluidRow(
                   column(6, h5("7-Day Avg E. coli (MPN)", align = "center"), plotlyOutput("hist_ecoli_7d_map", width = "100%", height = "350px")),
                   column(6, h5("7-Day Avg Bactiquick (ERU)", align = "center"), plotlyOutput("hist_bacti_7d_map", width = "100%", height = "350px"))
                 )
               )
             )
    ),
    
    # --- TAB 3: Forecast Dashboard & Trends ---
    tabPanel("Forecast Dashboard & Trends",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 sliderInput("map_date", "Select Date for Map:", 
                             min = min(as.Date(current_fcst$SampleDate), na.rm = TRUE), 
                             max = max(as.Date(current_fcst$SampleDate), na.rm = TRUE),
                             value = index_date, timeFormat = "%Y-%m-%d", 
                             animate = animationOptions(interval = 2500, loop = TRUE)),
                 selectizeInput("site", "Search Site (Trend Chart):", choices = NULL),
                 radioButtons("trend_metric", "Select Metric (Applies to Maps & Plot):", 
                              choices = c("Forecasted E. coli Level" = "Forecasted_Ecoli_Level", 
                                          "Probability of Exceedance" = "Probability_of_Exceedance"),
                              selected = "Forecasted_Ecoli_Level"),
                 helpText("Click ANY region on the polygon maps to filter the trend plot below.")
               ),
               mainPanel(
                 width = 9,
                 h4(textOutput("map_title")),
                 fluidRow(
                   column(6, h5("Daily Region Forecast", align = "center"), plotOutput("map_daily", height = "300px", click = "map_click_1")),
                   column(6, h5("Daily Site Forecast", align = "center"), plotlyOutput("site_dots_map", width = "100%", height = "300px"))
                 ),
                 fluidRow(
                   column(6, h5("7-Day Region Forecast", align = "center"), plotOutput("map_click_2_output", height = "300px", click = "map_click_2")),
                   column(6, h5(textOutput("trend_title"), align = "center"), plotOutput("timeseries_plot", height = "300px"))
                 )
               )
             )
    ),
    
    # --- TAB 4: Weather View ---
    tabPanel("Weather View (Historic & Forecast)",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 sliderInput("weather_date", "Select Timeline Date:",
                             min = min(as.Date(weather_data$SampleDate), na.rm = TRUE), 
                             max = max(as.Date(weather_data$SampleDate), na.rm = TRUE),
                             value = index_date, timeFormat = "%Y-%m-%d", animate = animationOptions(interval = 1200, loop = TRUE)),
                 radioButtons("weather_var", "Select Weather Variable:",
                              choices = c("Mean Temperature (°F)" = "tmean_10km_avg", "Precipitation (mm)" = "ppt_10km_avg")),
                 selectizeInput("weather_site", "Search Site Location:", choices = NULL) 
               ),
               mainPanel(
                 width = 9,
                 h4(textOutput("weather_title")),
                 plotlyOutput("weather_map", width = "100%", height = "400px"),
                 hr(),
                 h4(textOutput("weather_trend_title")),
                 plotOutput("weather_trend_plot", height = "250px")
               )
             )
    )
  )
)

# --- 3. SHINY SERVER ---
server <- function(input, output, session) {
  
  updateSelectizeInput(session, "site", choices = site_choices, server = TRUE, selected = "All")
  updateSelectizeInput(session, "weather_site", choices = site_choices, server = TRUE, selected = "All")
  
  filtered_site_ids <- reactive({
    req(input$exceedance_filter, input$corr_filter, input$bacti_thresh_val, input$ecoli_limit, input$bacti_limit)
    
    df <- minet_data %>% 
      filter(!is.na(ecoli_log) & !is.na(bactiquick_log)) %>%
      mutate(
        ecoli_raw = exp(ecoli_log) - 0.001,
        bacti_raw = exp(bactiquick_log) - 0.001
      ) %>%
      filter(
        ecoli_raw >= input$ecoli_limit[1] & ecoli_raw <= input$ecoli_limit[2],
        bacti_raw >= input$bacti_limit[1] & bacti_raw <= input$bacti_limit[2]
      )
    
    bacti_log_thresh <- log(input$bacti_thresh_val + 0.001)
    ecoli_log_thresh <- log(300 + 0.001)
    
    site_exceed <- df %>%
      group_by(id) %>%
      summarise(
        has_ecoli_exc = any(ecoli_log >= ecoli_log_thresh, na.rm = TRUE),
        has_bacti_exc = any(bactiquick_log >= bacti_log_thresh, na.rm = TRUE),
        .groups = "drop"
      )
    
    site_corrs <- df %>%
      group_by(id) %>%
      filter(n() >= 3) %>%
      summarise(
        rho = suppressWarnings(cor(ecoli_log, bactiquick_log, method = "spearman", use = "complete.obs")),
        .groups = "drop"
      )
    
    if (input$exceedance_filter == "ecoli") {
      site_exceed <- site_exceed %>% filter(has_ecoli_exc)
    } else if (input$exceedance_filter == "bacti") {
      site_exceed <- site_exceed %>% filter(has_bacti_exc)
    } else if (input$exceedance_filter == "either") {
      site_exceed <- site_exceed %>% filter(has_ecoli_exc | has_bacti_exc)
    }
    
    valid_ids <- site_exceed$id
    
    if (input$corr_filter != "all") {
      if (input$corr_filter == "strong_pos") {
        corr_ids <- site_corrs %>% filter(!is.na(rho) & rho >= 0.8) %>% pull(id)
      } else if (input$corr_filter == "mod_pos") {
        corr_ids <- site_corrs %>% filter(!is.na(rho) & rho >= 0.5) %>% pull(id)
      } else if (input$corr_filter == "weak_neg") {
        corr_ids <- site_corrs %>% filter(is.na(rho) | rho < 0.5) %>% pull(id)
      }
      valid_ids <- intersect(valid_ids, corr_ids)
    }
    
    return(valid_ids)
  })
  
  observe({
    req(input$waterbody_filter)
    valid_sites <- filtered_site_ids()
    
    if (exists("matched_comparison_df") && nrow(matched_comparison_df) > 0) {
      temp_df <- matched_comparison_df %>% filter(id %in% valid_sites)
      
      if (input$waterbody_filter != "Any") {
        temp_df <- temp_df %>% filter(waterbody_type == input$waterbody_filter)
      }
      
      available_ids <- unique(as.character(temp_df$id))
      if (length(available_ids) > 0) {
        avail_names <- as.character(geo_info$BeachName[match(available_ids, geo_info$id)])
        new_choices <- c("Overall Average (All Available Sites)" = "All", setNames(available_ids, avail_names))
      } else {
        new_choices <- c("No Sites Match Filter" = "None")
      }
      updateSelectizeInput(session, "comp_site", choices = new_choices, server = TRUE)
    } else {
      updateSelectizeInput(session, "comp_site", choices = c("No Match Between Forecast and Minet Dates" = "None"), server = TRUE)
    }
  })
  
  selected_region <- reactiveVal("All")
  
  process_map_click <- function(click_data) {
    req(click_data)
    distances <- (mi_counties_map$long - click_data$x)^2 + (mi_counties_map$lat - click_data$y)^2
    closest_idx <- which.min(distances)
    clicked_reg <- mi_counties_map$Region[closest_idx]
    
    if (selected_region() == clicked_reg) {
      selected_region("All")
    } else {
      selected_region(clicked_reg)
      updateSelectizeInput(session, "site", choices = site_choices, selected = "All", server = TRUE)
    }
  }
  
  observeEvent(input$map_click_1, process_map_click(input$map_click_1))
  observeEvent(input$map_click_2, process_map_click(input$map_click_2))
  
  # --- HISTORIC MAPS LOGIC (TAB 2) ---
  output$hist_ecoli_map <- renderPlotly({
    req(input$tab2_date)
    if(input$tab2_date == "No Data Available") return(plot_ly() %>% layout(title = "No Data Available"))
    selected_date <- as.Date(input$tab2_date)
    
    plot_data <- minet_data %>% 
      filter(as.Date(SampleDate) == selected_date) %>%
      inner_join(geo_info %>% select(id, Latitude, Longitude, BeachName), by = "id") %>%
      filter(!is.na(Latitude) & !is.na(ecoli_log))
    
    if(nrow(plot_data) == 0) return(plot_ly() %>% layout(title = "No E. coli data for this date"))
    
    p <- suppressWarnings(
      ggplot() +
        geom_sf(data = mi_counties_sf, fill = "grey90", color = "grey60", linewidth = 0.2) +
        geom_sf(data = mi_regions_sf, fill = NA, color = "black", linewidth = 0.8) +
        geom_point(data = plot_data, aes(x = Longitude, y = Latitude, color = ecoli_log,
                                         text = paste("Site:", BeachName, "<br>ID:", id, "<br>Log E.coli:", round(ecoli_log, 2))),
                   size = 4, alpha = 0.9) +
        scale_color_viridis_c(option = "rocket", direction = -1, limits = global_hist_ecoli_lims, name = "Log(E.coli)") +
        theme_void() + theme(legend.position = "right")
    )
    
    ggplotly(p, tooltip = "text") %>% 
      style(hoverinfo = "none", traces = c(1, 2)) %>%
      layout(autosize = TRUE, margin = list(l=0, r=0, b=0, t=0)) %>%
      config(responsive = TRUE)
  })
  
  output$hist_bacti_map <- renderPlotly({
    req(input$tab2_date)
    if(input$tab2_date == "No Data Available") return(plot_ly() %>% layout(title = "No Data Available"))
    selected_date <- as.Date(input$tab2_date)
    
    plot_data <- minet_data %>% 
      filter(as.Date(SampleDate) == selected_date) %>%
      inner_join(geo_info %>% select(id, Latitude, Longitude, BeachName), by = "id") %>%
      filter(!is.na(Latitude) & !is.na(bactiquick_log))
    
    if(nrow(plot_data) == 0) return(plot_ly() %>% layout(title = "No Bactiquick data for this date"))
    
    p <- suppressWarnings(
      ggplot() +
        geom_sf(data = mi_counties_sf, fill = "grey90", color = "grey60", linewidth = 0.2) +
        geom_sf(data = mi_regions_sf, fill = NA, color = "black", linewidth = 0.8) +
        geom_point(data = plot_data, aes(x = Longitude, y = Latitude, color = bactiquick_log,
                                         text = paste("Site:", BeachName, "<br>ID:", id, "<br>Log Bacti:", round(bactiquick_log, 2))),
                   size = 4, alpha = 0.9) +
        scale_color_viridis_c(option = "mako", direction = -1, limits = global_hist_bacti_lims, name = "Log(Bacti)") +
        theme_void() + theme(legend.position = "right")
    )
    
    ggplotly(p, tooltip = "text") %>% 
      style(hoverinfo = "none", traces = c(1, 2)) %>%
      layout(autosize = TRUE, margin = list(l=0, r=0, b=0, t=0)) %>%
      config(responsive = TRUE)
  })
  
  output$hist_ecoli_7d_map <- renderPlotly({
    req(input$tab2_date)
    if(input$tab2_date == "No Data Available") return(plot_ly() %>% layout(title = "No Data Available"))
    selected_date <- as.Date(input$tab2_date)
    start_date <- selected_date - 6
    
    plot_data <- minet_data %>%
      filter(as.Date(SampleDate) >= start_date & as.Date(SampleDate) <= selected_date) %>%
      group_by(id) %>%
      summarise(ecoli_log = mean(ecoli_log, na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(ecoli_log) & !is.nan(ecoli_log)) %>%
      inner_join(geo_info %>% select(id, Latitude, Longitude, BeachName), by = "id") %>%
      filter(!is.na(Latitude))
    
    if(nrow(plot_data) == 0) return(plot_ly() %>% layout(title = "No E. coli data in past 7 days"))
    
    p <- suppressWarnings(
      ggplot() +
        geom_sf(data = mi_counties_sf, fill = "grey90", color = "grey60", linewidth = 0.2) +
        geom_sf(data = mi_regions_sf, fill = NA, color = "black", linewidth = 0.8) +
        geom_point(data = plot_data, aes(x = Longitude, y = Latitude, color = ecoli_log,
                                         text = paste("Site:", BeachName, "<br>ID:", id, "<br>7-Day Log E.coli:", round(ecoli_log, 2))),
                   size = 4, alpha = 0.9) +
        scale_color_viridis_c(option = "rocket", direction = -1, limits = global_hist_ecoli_lims, name = "Avg Log(E.coli)") +
        theme_void() + theme(legend.position = "right")
    )
    
    ggplotly(p, tooltip = "text") %>% 
      style(hoverinfo = "none", traces = c(1, 2)) %>%
      layout(autosize = TRUE, margin = list(l=0, r=0, b=0, t=0)) %>%
      config(responsive = TRUE)
  })
  
  output$hist_bacti_7d_map <- renderPlotly({
    req(input$tab2_date)
    if(input$tab2_date == "No Data Available") return(plot_ly() %>% layout(title = "No Data Available"))
    selected_date <- as.Date(input$tab2_date)
    start_date <- selected_date - 6
    
    plot_data <- minet_data %>%
      filter(as.Date(SampleDate) >= start_date & as.Date(SampleDate) <= selected_date) %>%
      group_by(id) %>%
      summarise(bactiquick_log = mean(bactiquick_log, na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(bactiquick_log) & !is.nan(bactiquick_log)) %>%
      inner_join(geo_info %>% select(id, Latitude, Longitude, BeachName), by = "id") %>%
      filter(!is.na(Latitude))
    
    if(nrow(plot_data) == 0) return(plot_ly() %>% layout(title = "No Bactiquick data in past 7 days"))
    
    p <- suppressWarnings(
      ggplot() +
        geom_sf(data = mi_counties_sf, fill = "grey90", color = "grey60", linewidth = 0.2) +
        geom_sf(data = mi_regions_sf, fill = NA, color = "black", linewidth = 0.8) +
        geom_point(data = plot_data, aes(x = Longitude, y = Latitude, color = bactiquick_log,
                                         text = paste("Site:", BeachName, "<br>ID:", id, "<br>7-Day Log Bacti:", round(bactiquick_log, 2))),
                   size = 4, alpha = 0.9) +
        scale_color_viridis_c(option = "mako", direction = -1, limits = global_hist_bacti_lims, name = "Avg Log(Bacti)") +
        theme_void() + theme(legend.position = "right")
    )
    
    ggplotly(p, tooltip = "text") %>% 
      style(hoverinfo = "none", traces = c(1, 2)) %>%
      layout(autosize = TRUE, margin = list(l=0, r=0, b=0, t=0)) %>%
      config(responsive = TRUE)
  })
  
  # --- PERFORMANCE & COMPARISON LOGIC ---
  comp_plot_data <- reactive({
    req(input$comp_site, input$waterbody_filter, input$ecoli_limit, input$bacti_limit)
    if(!exists("matched_comparison_df") || nrow(matched_comparison_df) == 0 || input$comp_site == "None") {
      return(NULL)
    }
    
    valid_sites <- filtered_site_ids()
    plot_df <- matched_comparison_df %>% filter(id %in% valid_sites)
    
    if(input$waterbody_filter != "Any") plot_df <- plot_df %>% filter(waterbody_type == input$waterbody_filter)
    if(input$comp_site != "All") plot_df <- plot_df %>% filter(id == input$comp_site)
    
    if(nrow(plot_df) == 0) return(NULL)
    
    plot_df %>% 
      mutate(
        SampleDate = as.Date(SampleDate),
        ecoli_actual_raw = exp(minet_ecoli_log) - 0.001,
        bact_actual_raw  = exp(minet_bact_log) - 0.001
      ) %>%
      filter(
        ecoli_actual_raw >= input$ecoli_limit[1] & ecoli_actual_raw <= input$ecoli_limit[2],
        bact_actual_raw >= input$bacti_limit[1] & bact_actual_raw <= input$bacti_limit[2]
      ) %>%
      group_by(SampleDate) %>% 
      summarise(
        ecoli_actual     = mean(minet_ecoli_log, na.rm = TRUE),
        ecoli_actual_min = safe_min(minet_ecoli_log),
        ecoli_actual_max = safe_max(minet_ecoli_log),
        ecoli_actual_n   = sum(!is.na(minet_ecoli_log)),
        bact_actual      = mean(minet_bact_log, na.rm = TRUE),
        bact_actual_min  = safe_min(minet_bact_log),
        bact_actual_max  = safe_max(minet_bact_log),
        bact_actual_n    = sum(!is.na(minet_bact_log)),
        ecoli_fcst       = mean(fcst_ecoli_log, na.rm = TRUE),
        ecoli_fcst_min   = safe_min(fcst_ecoli_log),
        ecoli_fcst_max   = safe_max(fcst_ecoli_log),
        ecoli_fcst_n     = sum(!is.na(fcst_ecoli_log)),
        ecoli_actual_raw = mean(ecoli_actual_raw, na.rm = TRUE),
        bact_actual_raw  = mean(bact_actual_raw, na.rm = TRUE),
        ecoli_fcst_raw   = mean(exp(fcst_ecoli_log) - 0.001, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  # INTERACTIVE LOG SCALE COMPARISON PLOT
  output$compare_plot <- renderPlotly({
    plot_df <- comp_plot_data()
    if(is.null(plot_df) || nrow(plot_df) == 0) {
      return(plot_ly() %>% layout(title = "No Data Available for Filters"))
    }
    
    color_mapping <- c("E. coli (MPN)" = "black", "Bactiquick" = "blue", "Forecasted E. coli" = "#e74c3c")
    thresh_val <- log(300 + 0.001)
    
    p <- ggplot(plot_df, aes(x = SampleDate)) +
      geom_hline(yintercept = thresh_val, linetype = "dashed", color = "red", linewidth = 0.8, alpha = 0.7) +
      geom_ribbon(aes(ymin = ecoli_actual_min, ymax = ecoli_actual_max), fill = "black", alpha = 0.12) +
      geom_ribbon(aes(ymin = bact_actual_min, ymax = bact_actual_max), fill = "blue", alpha = 0.12) +
      geom_ribbon(aes(ymin = ecoli_fcst_min, ymax = ecoli_fcst_max), fill = "#e74c3c", alpha = 0.12) +
      geom_line(aes(y = ecoli_actual, color = "E. coli (MPN)"), linewidth = 1.2, linetype = "dashed", alpha = 0.8) +
      geom_point(aes(y = ecoli_actual, color = "E. coli (MPN)",
                     text = paste0("<b>Date:</b> ", format(SampleDate, "%Y-%m-%d"),
                                   "<br><b>Metric:</b> E. coli (Colilert 18)",
                                   "<br><b>Log Value (Mean):</b> ", round(ecoli_actual, 3),
                                   "<br><b>Min:</b> ", round(ecoli_actual_min, 3),
                                   "<br><b>Max:</b> ", round(ecoli_actual_max, 3),
                                   "<br><b>N:</b> ", ecoli_actual_n)), size = 3, shape = 18, alpha = 0.8) +
      geom_line(aes(y = bact_actual, color = "Bactiquick"), linewidth = 1.2, linetype = "dotted", alpha = 0.8) +
      geom_point(aes(y = bact_actual, color = "Bactiquick",
                     text = paste0("<b>Date:</b> ", format(SampleDate, "%Y-%m-%d"),
                                   "<br><b>Metric:</b> Bactiquick",
                                   "<br><b>Log Value (Mean):</b> ", round(bact_actual, 3),
                                   "<br><b>Min:</b> ", round(bact_actual_min, 3),
                                   "<br><b>Max:</b> ", round(bact_actual_max, 3),
                                   "<br><b>N:</b> ", bact_actual_n)), size = 3, shape = 17, alpha = 0.8) +
      geom_line(aes(y = ecoli_fcst, color = "Forecasted E. coli"), linewidth = 1.2, alpha = 0.8) +
      geom_point(aes(y = ecoli_fcst, color = "Forecasted E. coli",
                     text = paste0("<b>Date:</b> ", format(SampleDate, "%Y-%m-%d"),
                                   "<br><b>Metric:</b> Forecasted E. coli",
                                   "<br><b>Log Value (Mean):</b> ", round(ecoli_fcst, 3),
                                   "<br><b>Min:</b> ", round(ecoli_fcst_min, 3),
                                   "<br><b>Max:</b> ", round(ecoli_fcst_max, 3),
                                   "<br><b>N:</b> ", ecoli_fcst_n)), size = 3, alpha = 0.8) +
      scale_color_manual(values = color_mapping, name = "Data Source") + 
      theme_minimal() + 
      labs(x = "Date", y = "Log Level") + 
      theme(
        text = element_text(size = 12),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 0)
      )
    
    ggplotly(p, tooltip = "text") %>%
      style(hoverinfo = "none", traces = c(1, 2, 3, 4)) %>%
      layout(
        autosize = TRUE,
        margin = list(l = 45, r = 120, t = 20, b = 40),
        legend = list(orientation = "v", x = 1.02, xanchor = "left", y = 1, yanchor = "top")
      ) %>%
      config(responsive = TRUE)
  })
  
  # INTERACTIVE RAW ASSAY COMPARISON PLOT
  output$compare_plot_raw <- renderPlotly({
    plot_df <- comp_plot_data()
    if(is.null(plot_df) || nrow(plot_df) == 0) {
      return(plot_ly() %>% layout(title = "No Data Available for Filters"))
    }
    
    color_mapping <- c("E. coli (MPN)" = "black", "Bactiquick" = "blue", "Forecasted E. coli" = "#e74c3c")
    
    p <- ggplot(plot_df, aes(x = SampleDate)) +
      geom_hline(yintercept = 300, linetype = "dashed", color = "red", linewidth = 0.8, alpha = 0.7) +
      geom_line(aes(y = ecoli_actual_raw, color = "E. coli (MPN)"), linewidth = 1.2, linetype = "dashed", alpha = 0.8) +
      geom_point(aes(y = ecoli_actual_raw, color = "E. coli (MPN)",
                     text = paste0("<b>Date:</b> ", format(SampleDate, "%Y-%m-%d"),
                                   "<br><b>Metric:</b> E. coli (Colilert 18)",
                                   "<br><b>Raw Value:</b> ", round(ecoli_actual_raw, 1), " MPN")), size = 3, shape = 18, alpha = 0.8) +
      geom_line(aes(y = bact_actual_raw, color = "Bactiquick"), linewidth = 1.2, linetype = "dotted", alpha = 0.8) +
      geom_point(aes(y = bact_actual_raw, color = "Bactiquick",
                     text = paste0("<b>Date:</b> ", format(SampleDate, "%Y-%m-%d"),
                                   "<br><b>Metric:</b> Bactiquick",
                                   "<br><b>Raw Value:</b> ", round(bact_actual_raw, 1), " ERU")), size = 3, shape = 17, alpha = 0.8) +
      geom_line(aes(y = ecoli_fcst_raw, color = "Forecasted E. coli"), linewidth = 1.2, alpha = 0.8) +
      geom_point(aes(y = ecoli_fcst_raw, color = "Forecasted E. coli",
                     text = paste0("<b>Date:</b> ", format(SampleDate, "%Y-%m-%d"),
                                   "<br><b>Metric:</b> Forecasted E. coli",
                                   "<br><b>Raw Value:</b> ", round(ecoli_fcst_raw, 1), " MPN")), size = 3, alpha = 0.8) +
      scale_color_manual(values = color_mapping, name = "Data Source") + 
      theme_minimal() + 
      labs(x = "Date", y = "Raw Assay Result (MPN / ERU)") + 
      theme(
        text = element_text(size = 12),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 0)
      )
    
    ggplotly(p, tooltip = "text") %>%
      style(hoverinfo = "none", traces = 1) %>%
      layout(
        autosize = TRUE,
        margin = list(l = 55, r = 120, t = 20, b = 40),
        legend = list(orientation = "v", x = 1.02, xanchor = "left", y = 1, yanchor = "top")
      ) %>%
      config(responsive = TRUE)
  })
  
  get_scatter_data <- reactive({
    req(input$comp_site, input$color_mode, input$waterbody_filter, input$bacti_thresh_val, input$ecoli_limit, input$bacti_limit)
    if(nrow(minet_data) == 0 || input$comp_site == "None") return(NULL)
    
    valid_sites <- filtered_site_ids()
    target_data <- minet_data %>% filter(id %in% valid_sites)
    
    if(input$comp_site != "All") target_data <- target_data %>% filter(id == input$comp_site)
    
    target_data <- target_data %>% 
      inner_join(geo_info %>% select(id, BeachName), by = "id") %>% 
      filter(!is.na(ecoli_log) & !is.na(bactiquick_log)) %>%
      mutate(
        ecoli_raw = exp(ecoli_log) - 0.001,
        bacti_raw = exp(bactiquick_log) - 0.001
      ) %>%
      filter(
        ecoli_raw >= input$ecoli_limit[1] & ecoli_raw <= input$ecoli_limit[2],
        bacti_raw >= input$bacti_limit[1] & bacti_raw <= input$bacti_limit[2]
      )
    
    if(input$waterbody_filter != "Any" && exists("matched_comparison_df")) {
      v_ids <- unique(matched_comparison_df$id[matched_comparison_df$waterbody_type == input$waterbody_filter])
      target_data <- target_data %>% filter(id %in% v_ids)
    }
    
    return(target_data)
  })
  
  output$bact_scatter <- renderPlotly({
    target_data <- get_scatter_data()
    if(is.null(target_data) || nrow(target_data) == 0) return(plot_ly() %>% layout(title = "No Data for Selected View"))
    
    ct <- cor.test(target_data$ecoli_log, target_data$bactiquick_log, method = "spearman", exact = FALSE)
    rho <- round(ct$estimate, 3)
    pval <- format.pval(ct$p.value, digits = 3)
    n_pts <- nrow(target_data)
    
    bacti_thresh <- log(input$bacti_thresh_val + 0.001)
    ecoli_thresh <- log(300 + 0.001)
    
    if (input$color_mode == "disagree") {
      target_data <- target_data %>% 
        mutate(ecoli_exc = ecoli_log >= ecoli_thresh, 
               bacti_exc = bactiquick_log >= bacti_thresh, 
               ColorStatus = ifelse(ecoli_exc == bacti_exc, "Agree", "Disagree"))
      
      colors_map <- c("Agree" = "#16a085", "Disagree" = "#e74c3c")
      disagree_pct <- round(mean(target_data$ColorStatus == "Disagree", na.rm = TRUE) * 100, 1)
      stats_text <- paste0("N: ", n_pts, "<br>Spearman \u03c1: ", rho, "<br>p-value: ", pval, "<br>Disagreement: ", disagree_pct, "%")
      
    } else {
      req(input$discordance_pct)
      pct_thresh <- input$discordance_pct / 100
      
      target_data <- target_data %>%
        mutate(
          ColorStatus = ifelse(
            bactiquick_log >= (1 + pct_thresh) * ecoli_log | 
              bactiquick_log <= (1 - pct_thresh) * ecoli_log, 
            "Discordant", 
            "Concordant"
          )
        )
      
      colors_map <- c("Concordant" = "#3498db", "Discordant" = "#e67e22")
      discord_pct <- round(mean(target_data$ColorStatus == "Discordant", na.rm = TRUE) * 100, 1)
      stats_text <- paste0("N: ", n_pts, "<br>Spearman \u03c1: ", rho, "<br>p-value: ", pval, "<br>Discordant (>+/-", input$discordance_pct, "%): ", discord_pct, "%")
    }
    
    p <- suppressWarnings(
      ggplot(target_data, aes(x = ecoli_log, y = bactiquick_log, color = ColorStatus)) +
        geom_vline(xintercept = ecoli_thresh, linetype = "dashed", color = "red", alpha = 0.5) +
        geom_hline(yintercept = bacti_thresh, linetype = "dashed", color = "blue", alpha = 0.5) +
        geom_point(aes(text = paste("Site ID:", id, "<br>Name:", BeachName, "<br>Date:", SampleDate, "<br>Log E.coli:", round(ecoli_log, 2), "<br>Log Bacti:", round(bactiquick_log, 2), "<br>Status:", ColorStatus)), size = 3, alpha = 0.7) +
        geom_smooth(aes(group = 1), method = "lm", formula = y ~ x, color = "#2c3e50", linetype = "dashed", linewidth = 1) +
        scale_color_manual(values = colors_map) +
        theme_minimal() +
        labs(x = "Log(E. coli MPN + 0.001) [Colilert 18]", y = "Log(Bactiquick ERU + 0.001)", color = "Status") +
        theme(text = element_text(size = 14), legend.position = "right")
    )
    
    suppressWarnings(
      ggplotly(p, tooltip = "text") %>%
        layout(
          autosize = TRUE,
          annotations = list(x = 1, y = 0, text = stats_text, showarrow = FALSE, xref = 'paper', yref = 'paper', xanchor = 'right', yanchor = 'bottom', align = 'right', font = list(size = 12, color = "black"), bgcolor = "rgba(255, 255, 255, 0.9)", bordercolor = "black", borderwidth = 1, borderpad = 4)
        ) %>%
        config(responsive = TRUE)
    )
  })
  
  output$bact_scatter_raw <- renderPlotly({
    target_data <- get_scatter_data()
    if(is.null(target_data) || nrow(target_data) == 0) return(plot_ly() %>% layout(title = "No Data for Selected View"))
    
    ct <- cor.test(target_data$ecoli_raw, target_data$bacti_raw, method = "spearman", exact = FALSE)
    rho <- round(ct$estimate, 3)
    pval <- format.pval(ct$p.value, digits = 3)
    n_pts <- nrow(target_data)
    
    bacti_thresh <- input$bacti_thresh_val
    ecoli_thresh <- 300
    
    if (input$color_mode == "disagree") {
      target_data <- target_data %>% 
        mutate(ecoli_exc = ecoli_raw >= ecoli_thresh, 
               bacti_exc = bacti_raw >= bacti_thresh, 
               ColorStatus = ifelse(ecoli_exc == bacti_exc, "Agree", "Disagree"))
      
      colors_map <- c("Agree" = "#16a085", "Disagree" = "#e74c3c")
      disagree_pct <- round(mean(target_data$ColorStatus == "Disagree", na.rm = TRUE) * 100, 1)
      stats_text <- paste0("N: ", n_pts, "<br>Spearman \u03c1: ", rho, "<br>p-value: ", pval, "<br>Disagreement: ", disagree_pct, "%")
      
    } else {
      req(input$discordance_pct)
      pct_thresh <- input$discordance_pct / 100
      
      target_data <- target_data %>%
        mutate(
          ColorStatus = ifelse(
            bacti_raw >= (1 + pct_thresh) * ecoli_raw | 
              bacti_raw <= (1 - pct_thresh) * ecoli_raw, 
            "Discordant", 
            "Concordant"
          )
        )
      
      colors_map <- c("Concordant" = "#3498db", "Discordant" = "#e67e22")
      discord_pct <- round(mean(target_data$ColorStatus == "Discordant", na.rm = TRUE) * 100, 1)
      stats_text <- paste0("N: ", n_pts, "<br>Spearman \u03c1: ", rho, "<br>p-value: ", pval, "<br>Discordant (>+/-", input$discordance_pct, "%): ", discord_pct, "%")
    }
    
    p <- suppressWarnings(
      ggplot(target_data, aes(x = ecoli_raw, y = bacti_raw, color = ColorStatus)) +
        geom_vline(xintercept = 300, linetype = "dashed", color = "red", alpha = 0.5) +
        geom_hline(yintercept = bacti_thresh, linetype = "dashed", color = "blue", alpha = 0.5) +
        geom_point(aes(text = paste("Site ID:", id, "<br>Name:", BeachName, "<br>Date:", SampleDate, "<br>E.coli:", round(ecoli_raw, 1), "MPN<br>Bacti:", round(bacti_raw, 1), "ERU<br>Status:", ColorStatus)), size = 3, alpha = 0.7) +
        geom_smooth(aes(group = 1), method = "lm", formula = y ~ x, color = "#2c3e50", linetype = "dashed", linewidth = 1) +
        scale_color_manual(values = colors_map) +
        theme_minimal() +
        labs(x = "E. coli (MPN/100ml) [Colilert 18]", y = "Bactiquick (ERU)", color = "Status") +
        theme(text = element_text(size = 14), legend.position = "right")
    )
    
    suppressWarnings(
      ggplotly(p, tooltip = "text") %>%
        layout(
          autosize = TRUE,
          annotations = list(x = 1, y = 0, text = stats_text, showarrow = FALSE, xref = 'paper', yref = 'paper', xanchor = 'right', yanchor = 'bottom', align = 'right', font = list(size = 12, color = "black"), bgcolor = "rgba(255, 255, 255, 0.9)", bordercolor = "black", borderwidth = 1, borderpad = 4)
        ) %>%
        config(responsive = TRUE)
    )
  })
  
  # --- FORECAST LOGIC ---
  output$map_title <- renderText({
    req(input$map_date)
    paste("Data Snapshot for:", format(as.Date(input$map_date), "%B %d, %Y"))
  })
  
  output$trend_title <- renderText({
    req(input$site, input$trend_metric)
    metric_label <- ifelse(input$trend_metric == "Forecasted_Ecoli_Level", "E. coli Forecast", "Probability Forecast")
    
    if (input$site != "All") {
      site_display_name <- names(site_choices)[site_choices == input$site]
      paste("Trend:", metric_label, "-", site_display_name)
    } else if (selected_region() != "All") {
      paste("Trend:", metric_label, "- Region", selected_region())
    } else {
      paste("Trend:", metric_label, "- Statewide Avg")
    }
  })
  
  render_quadrant_map <- function(timeframe_type) {
    renderPlot({
      req(input$map_date, input$trend_metric)
      metric_name <- input$trend_metric
      
      geo_subset <- geo_info %>% select(id, Region)
      sel_date <- as.Date(input$map_date)
      
      if(timeframe_type == "single") {
        target_data <- current_fcst %>% 
          filter(as.Date(SampleDate) == sel_date) %>% 
          inner_join(geo_subset, by = "id")
      } else {
        target_data <- current_fcst %>% 
          filter(as.Date(SampleDate) >= sel_date & as.Date(SampleDate) <= (sel_date + 6)) %>% 
          inner_join(geo_subset, by = "id")
      }
      
      region_means <- target_data %>%
        group_by(Region) %>%
        summarise(metric_val = mean(as.numeric(as.character(.data[[metric_name]])), na.rm = TRUE), .groups = "drop")
      
      map_data_sf <- mi_counties_sf %>%
        left_join(region_means, by = "Region") %>%
        mutate(
          is_selected = (selected_region() == "All") | (Region == selected_region()),
          alpha_val = ifelse(is_selected, 1, 0.25)
        )
      
      if (metric_name == "Forecasted_Ecoli_Level") {
        fill_scale <- scale_fill_viridis_c(option = "rocket", direction = -1, limits = global_ecoli_limits, na.value = "grey90", name = "Mean E. coli")
      } else {
        fill_scale <- scale_fill_viridis_c(option = "mako", direction = -1, limits = c(0,1), na.value = "grey90", name = "Exc. Prob.")
      }
      
      ggplot() +
        geom_sf(data = map_data_sf, aes(fill = metric_val, alpha = alpha_val), color = "grey60", linewidth = 0.2) +
        geom_sf(data = mi_regions_sf, fill = NA, color = "black", linewidth = 0.8) +
        geom_sf_label(data = mi_regions_sf, aes(label = Region), fontface = "bold", size = 4, fill = "white", color = "black", alpha = 0.8) +
        scale_alpha_identity() + fill_scale + theme_void() +
        theme(legend.position = "right", legend.title = element_text(face = "bold", size = 11), legend.text = element_text(size = 9))
    })
  }
  
  output$map_daily <- render_quadrant_map("single")
  output$map_click_2_output  <- render_quadrant_map("7day")
  
  output$site_dots_map <- renderPlotly({
    req(input$trend_metric, input$map_date)
    sel_date <- as.Date(input$map_date)
    
    fcst_sub <- current_fcst %>% 
      filter(as.Date(SampleDate) == sel_date)
    
    if ("Longitude" %in% names(fcst_sub) && "Latitude" %in% names(fcst_sub)) {
      target_data <- fcst_sub %>%
        left_join(geo_info %>% select(id, Region, BeachName), by = "id")
    } else {
      target_data <- fcst_sub %>%
        left_join(geo_info %>% select(id, Region, BeachName, Latitude, Longitude), by = "id")
    }
    
    target_data <- target_data %>%
      filter(!is.na(Longitude) & !is.na(Latitude)) %>%
      mutate(metric_val = as.numeric(as.character(.data[[input$trend_metric]])))
    
    if(nrow(target_data) == 0) return(plot_ly() %>% layout(title = "No Coordinate Data Available for this Date/Site"))
    
    if (input$trend_metric == "Forecasted_Ecoli_Level") {
      color_scale <- scale_color_viridis_c(option = "rocket", direction = -1, limits = global_ecoli_limits, name = "E. coli Level")
    } else {
      color_scale <- scale_color_viridis_c(option = "mako", direction = -1, limits = c(0,1), name = "Exceed Prob.")
    }
    
    p <- suppressWarnings(
      ggplot() +
        geom_sf(data = mi_counties_sf, fill = "grey85", color = "grey60", linewidth = 0.2) +
        geom_sf(data = mi_regions_sf, fill = NA, color = "black", linewidth = 0.8) +
        geom_point(data = target_data, aes(x = Longitude, y = Latitude, color = metric_val,
                                           text = paste("Site ID:", id, "<br>Name:", BeachName, "<br>Date:", SampleDate)), size = 3, alpha = 0.8) +
        color_scale + theme_void() + theme(legend.position = "right", legend.title = element_text(face = "bold", size = 10), legend.text = element_text(size = 8))
    )
    
    ggplotly(p, tooltip = "text") %>% 
      style(hoverinfo = "none", traces = c(1, 2)) %>%
      layout(autosize = TRUE, margin = list(l = 0, r = 0, b = 0, t = 0)) %>%
      config(responsive = TRUE)
  })
  
  output$timeseries_plot <- renderPlot({
    req(input$trend_metric, input$site, input$map_date)
    sel_date <- as.Date(input$map_date)
    
    if (input$site != "All") {
      trend_df <- current_fcst %>% filter(as.character(id) == as.character(input$site)) %>% mutate(metric_val = as.numeric(as.character(.data[[input$trend_metric]])))
    } else if (selected_region() != "All") {
      trend_df <- current_fcst %>% 
        inner_join(geo_info %>% select(id, Region), by = "id") %>% 
        filter(Region == selected_region()) %>%
        group_by(SampleDate, Type) %>% 
        summarise(metric_val = mean(as.numeric(as.character(.data[[input$trend_metric]])), na.rm = TRUE), .groups = "drop")
    } else {
      trend_df <- current_fcst %>% 
        group_by(SampleDate, Type) %>% 
        summarise(metric_val = mean(as.numeric(as.character(.data[[input$trend_metric]])), na.rm = TRUE), .groups = "drop")
    }
    
    if (input$trend_metric == "Forecasted_Ecoli_Level") {
      trend_df <- trend_df %>% mutate(metric_val = log(metric_val + 0.001))
      y_label <- "Log(E. coli Level + 0.001)"
    } else {
      y_label <- "Probability of Exceedance"
    }
    
    trend_df <- trend_df %>% mutate(SampleDate = as.Date(SampleDate))
    
    ggplot(trend_df, aes(x = SampleDate, y = metric_val, group = 1)) +
      geom_line(color = "darkgray", linewidth = 1.2) +
      geom_point(color = "#2c3e50", size = 3) +
      geom_vline(xintercept = sel_date, color = "#3498db", linetype = "dashed", linewidth = 1.2) +
      theme_minimal() + labs(x = "Date", y = y_label) + theme(text = element_text(size = 12))
  })
  
  # --- WEATHER LOGIC ---
  output$weather_title <- renderText({
    var_label <- ifelse(input$weather_var == "tmean_10km_avg", "Mean Temperature", "Precipitation")
    sel_date  <- as.Date(input$weather_date)
    phase     <- ifelse(sel_date >= index_date, "(Forecast)", "(Historical)")
    paste(var_label, phase, "- Date:", format(sel_date, "%B %d, %Y"))
  })
  
  output$weather_map <- renderPlotly({
    req(input$weather_date, input$weather_var)
    sel_date <- as.Date(input$weather_date)
    
    target_weather <- weather_data %>% 
      filter(as.Date(SampleDate) == sel_date) %>% 
      mutate(metric_val = as.numeric(as.character(get(input$weather_var))))
    
    if(nrow(target_weather) == 0) return(plot_ly() %>% layout(title = "No Data"))
    
    color_scale <- if(input$weather_var == "tmean_10km_avg") {
      scale_color_viridis_c(option = "inferno", limits = global_tmean_limits, name = "Temp (°F)")
    } else {
      scale_color_viridis_c(option = "cividis", direction = -1, limits = global_ppt_limits, name = "Precip (mm)")
    }
    
    p <- suppressWarnings(
      ggplot() +
        geom_sf(data = mi_counties_sf, fill = "grey90", color = "grey60", linewidth = 0.2) +
        geom_sf(data = mi_regions_sf, fill = NA, color = "black", linewidth = 0.8) +
        geom_point(data = target_weather, aes(x = Longitude, y = Latitude, color = metric_val,
                                              text = paste("Site ID:", id, "<br>Name:", BeachName, "<br>Date:", SampleDate)), size = 3, alpha = 0.8) +
        color_scale + theme_void() + theme(legend.position = "right", legend.title = element_text(face = "bold", size = 12), legend.text = element_text(size = 10))
    )
    
    ggplotly(p, tooltip = "text") %>% 
      style(hoverinfo = "none", traces = c(1, 2)) %>%
      layout(autosize = TRUE, margin = list(l = 0, r = 0, b = 0, t = 0)) %>%
      config(responsive = TRUE)
  })
  
  output$weather_trend_title <- renderText({
    metric_label <- ifelse(input$weather_var == "tmean_10km_avg", "Mean Temperature (°F)", "Precipitation (mm)")
    loc_label <- if(input$weather_site == "All") "Statewide Overall Mean" else names(site_choices)[site_choices == input$weather_site]
    paste(loc_label, "Trend:", metric_label)
  })
  
  output$weather_trend_plot <- renderPlot({
    req(input$weather_var, input$weather_date, input$weather_site)
    sel_date <- as.Date(input$weather_date)
    
    if (input$weather_site != "All") {
      trend_df <- weather_data %>% 
        filter(as.character(id) == as.character(input$weather_site)) %>%
        mutate(metric_val = as.numeric(as.character(.data[[input$weather_var]])))
    } else {
      trend_df <- weather_data %>%
        group_by(SampleDate) %>%
        summarise(metric_val = mean(as.numeric(as.character(.data[[input$weather_var]])), na.rm = TRUE), .groups = "drop")
    }
    
    trend_df <- trend_df %>% 
      mutate(
        SampleDate = as.Date(SampleDate),
        Timeframe = ifelse(SampleDate < index_date, "Historic", "Forecast")
      )
    y_label  <- ifelse(input$weather_var == "tmean_10km_avg", "Mean Temp (°F)", "Mean Precip (mm)")
    
    ggplot(trend_df, aes(x = SampleDate, y = metric_val)) +
      geom_vline(xintercept = index_date - 0.5, color = "black", linetype = "dotted", linewidth = 1) +
      geom_line(aes(color = Timeframe, group = 1), linewidth = 1.2) +
      geom_point(aes(color = Timeframe), size = 3) +
      geom_vline(xintercept = sel_date, color = "#3498db", linetype = "dashed", linewidth = 1.2) +
      scale_color_manual(values = c("Historic" = "black", "Forecast" = "#e74c3c")) +
      theme_minimal() +
      labs(x = "Date", y = y_label, color = "Data Type") +
      theme(text = element_text(size = 14), legend.position = "bottom")
  })
}

shinyApp(ui = ui, server = server)