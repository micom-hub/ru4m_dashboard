library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(plotly)
library(sf)
library(googledrive)
library(bslib)
library(pROC)

# Disable S2 spherical geometry to prevent polygon winding order bugs
sf::sf_use_s2(FALSE)

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

# --- FIX INVALID GEOMETRIES & REPROJECT TO WGS 84 (EPSG:4326) ---
if (inherits(mi_counties_sf, "sf")) {
  mi_counties_sf <- sf::st_make_valid(mi_counties_sf) %>% sf::st_transform(4326)
}

if (inherits(mi_regions_sf, "sf")) {
  mi_regions_sf <- sf::st_make_valid(mi_regions_sf) %>% sf::st_transform(4326)
}

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

# Populate long/lat coordinates for map click distance calculation
mi_counties_map <- suppressWarnings(st_drop_geometry(mi_counties_sf))
if (inherits(mi_counties_sf, "sf")) {
  mi_coords <- suppressWarnings(st_coordinates(st_centroid(mi_counties_sf)))
  mi_counties_map$long <- mi_coords[, 1]
  mi_counties_map$lat  <- mi_coords[, 2]
}

# Calculate Maximum Raw Limits for Sliders (Using log10)
max_ecoli_val <- if (nrow(minet_data) > 0) ceiling(max(10^(minet_data$ecoli_log) - 0.001, na.rm = TRUE)) else 1000
max_bacti_val <- if (nrow(minet_data) > 0) ceiling(max(10^(minet_data$bactiquick_log) - 0.001, na.rm = TRUE)) else 1000

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

# --- 2. SHINY UI ----
ui <- navbarPage(
  title = "Michigan Recreational Water Fecal Contamination",
  theme = bs_theme(version = 5, bootswatch = "sandstone"),
  collapsible = TRUE,
  
  header = tags$head(
    tags$style(HTML("
      .navbar-nav > li > a {
        font-size: 18px !important;
        font-weight: 600;
      }
      .navbar-nav > li {
        margin-right: 25px; /* Adds space between tab choices */
      }
      .navbar-brand {
        font-size: 20px !important;
        font-weight: bold;
      }
    "))
  ),
  
  # --- TAB 1: Performance & Comparison ---
  tabPanel("Comparison",
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
                                       "No Bactiquick exceedance" = "no_bacti",
                                       "No E. coli exceedance" = "no_ecoli",
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
               
               selectizeInput("comp_site", "Select Specific Site(s):", choices = NULL, multiple = TRUE),
               actionLink("clear_sites", "Clear All Selected Sites", style = "color: #e74c3c;"),
               br(), br(),
               
               numericInput("ecoli_thresh_val", "E. coli Threshold (MPN):",
                            value = 300, step = 1),
               
               numericInput("bacti_thresh_val", "Bactiquick Threshold (ERU):",
                            value = 100, step = 1),
               
               checkboxInput("show_qpcr_ddpcr", "Show qPCR and ddPCR on Comparison", value = FALSE),
               
               hr(),
               radioButtons("color_mode", "Relationship Color Mode:",
                            choices = c("Exceedance Disagreement" = "disagree",
                                        "Percentage Discordance" = "discordant")),
               conditionalPanel(
                 condition = "input.color_mode == 'discordant'",
                 sliderInput("discordance_pct", "Discordance Threshold (%):",
                             min = 5, max = 100, value = 50, step = 5)
               ),
               helpText("This view compares E. coli colilert18 and Bactiquick testing results. Site selection allows multiple choice and exceedance thresholds are dynamic by user definition.")
             ),
             mainPanel(
               width = 9,
               tabsetPanel(
                 tabPanel("Log10 Scale Results",
                          h4("E. Coli Colilert 18 (MPN) and Bactiquick (ERU) Relationship (Log10 Scale)"),
                          fluidRow(
                            column(8,
                                   div(
                                     style = "width: 100%; aspect-ratio: 1.25 / 1; height: auto;",
                                     plotlyOutput("bact_scatter", width = "100%", height = "100%")
                                   )
                            ),
                            column(4,
                                   tableOutput("bact_scatter_stats")
                            )
                          ),
                          hr(),
                          h4("Assay Result Comparison Overtime"),
                          plotlyOutput("compare_plot", width = "100%", height = "40vh")
                 ),
                 tabPanel("Raw Assay Results",
                          h4("E. Coli Colilert 18 (MPN) and Bactiquick (ERU) Relationship (Raw)"),
                          fluidRow(
                            column(8,
                                   div(
                                     style = "width: 100%; aspect-ratio: 1.25 / 1; height: auto;",
                                     plotlyOutput("bact_scatter_raw", width = "100%", height = "100%")
                                   )
                            ),
                            column(4,
                                   tableOutput("bact_scatter_raw_stats")
                            )
                          ),
                          hr(),
                          h4("Forecasts and Actual Tests (Raw)"),
                          plotlyOutput("compare_plot_raw", width = "100%", height = "40vh")
                 )
               )
             )
           )
  ),
  
  # --- TAB 2: Site Sample Pair & Agreement Map ---
  tabPanel("Site agreement map",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               numericInput("tab2_map_ecoli_thresh", "E. coli Threshold (MPN):",
                            value = 300, step = 1),
               numericInput("tab2_map_bacti_thresh", "Bactiquick Threshold (ERU):",
                            value = 100, step = 1),
               hr(),
               helpText("Dot size represents paired samples count for a given site and dot color indicates percentage agreement on exceedance.")
             ),
             mainPanel(
               width = 9,
               fluidRow(
                 column(6,
                        h4("Site Data Availability", align = "center"),
                        plotlyOutput("all_sites_availability_map", width = "100%", height = "600px")
                 ),
                 column(6,
                        h4("Exceedance Agreement Map", align = "center"),
                        plotlyOutput("pairs_agreement_map", width = "100%", height = "600px")
                 )
               )
             )
           )
  ),
  
  # --- TAB 3: Historic Maps ---
  tabPanel("Testing result map",
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
               helpText("Select a date to view historical site test results. Only sites with available data on selected date are displayed.")
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
  
  # --- TAB 4: Forecast Performance ---
  tabPanel("Forecast Performance",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               selectInput("perf_waterbody_filter", "Waterbody Type:",
                           choices = c("Any", "Inland Lake", "Great Lake", "River"),
                           selected = "Any"),
               
               selectizeInput("perf_comp_site", "Select Specific Site(s):", choices = NULL, multiple = TRUE),
               actionLink("perf_clear_sites", "Clear All Selected Sites", style = "color: #e74c3c;"),
               br(), br(),
               
               numericInput("perf_ecoli_thresh", "E. coli Exceedance Threshold (MPN):",
                            value = 300, min = 1, max = 10000, step = 1),
               
               helpText("Evaluate forecast accuracy against observed E. coli levels. Set a customizable exceedance threshold (MPN) to compute AUROC, confidence intervals, and classification performance.")
             ),
             mainPanel(
               width = 9,
               h4("Forecast and Observed E. coli Over Time (Log10 Scale)"),
               plotlyOutput("perf_compare_plot", width = "100%", height = "40vh"),
               hr(),
               h4("Exceedance Detection ROC & AUROC Analysis"),
               fluidRow(
                 column(7,
                        div(
                          style = "width: 100%; aspect-ratio: 1.25 / 1; height: auto;",
                          plotlyOutput("perf_auc_plot", width = "100%", height = "100%")
                        )
                 ),
                 column(5,
                        h5("AUROC & Classification Statistics"),
                        tableOutput("perf_auc_stats")
                 )
               )
             )
           )
  ),
  
  # --- TAB 5: Forecast Dashboard & Trends ---
  tabPanel("Regional forecast",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               sliderInput("map_date", "Select Date for Map:", 
                           min = min(as.Date(current_fcst$SampleDate), na.rm = TRUE), 
                           max = max(as.Date(current_fcst$SampleDate), na.rm = TRUE),
                           value = index_date, timeFormat = "%Y-%m-%d", 
                           animate = animationOptions(interval = 2500, loop = TRUE)),
               selectizeInput("region_select", "Select Region(s):", choices = NULL, multiple = TRUE),
               selectizeInput("site", "Search Site (Trend Chart):", choices = NULL),
               actionLink("regional_clear_sites", "Clear All Selected Sites", style = "color: #e74c3c;"),
               br(), br(),
               radioButtons("trend_metric", "Select Metric (Applies to Maps & Plot):", 
                            choices = c("Forecasted E. coli Level" = "Forecasted_Ecoli_Level", 
                                        "Probability of Exceedance" = "Probability_of_Exceedance"),
                            selected = "Forecasted_Ecoli_Level"),
               helpText("This tab displays regional mean forecasting E. coli levels and exceedance probabilities for Michigan Emergency Preparedness Regions. Select one or multiple regions using the dropdown or by clicking directly on the map.")
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
  
  # --- TAB 6: Weather View ---
  tabPanel("Weather trend",
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
  ),
  
  ## --- TAB 7: About ----
  tabPanel("About",
           fluidRow(
             column(10, offset = 1,
                    br(),
                    div(style = "display: flex; justify-content: space-around; align-items: center; flex-wrap: wrap; gap: 20px; margin: 30px 0;",
                        img(src = "MichiganTech_Vertical_TwoColor.png", style = "height: 100px; max-width: 200px; object-fit: contain;"),
                        img(src = "msu_logo.png", style = "height: 100px; max-width: 200px; object-fit: contain;"),
                        img(src = "U-M_Logo-Hex.png", style = "height: 100px; max-width: 200px; object-fit: contain;"),
                        img(src = "wsu_logo.png", style = "height: 100px; max-width: 200px; object-fit: contain;"),
                        img(src = "egle.svg", style = "height: 100px; max-width: 200px; object-fit: contain;")
                    ),
                    hr(),
                    h3(tags$b("Project Overview")),
                    p("Recreational water quality monitoring and public health responses rely on rapid fecal contamination detection. Current fecal indicator monitoring methods take 18-24 hours for readings and leave a delay between contamination events and preventative advisories. This project aims to four R1 institutions in Michigan and the Michigan Environmental, Great Lakes, and Energy (EGLE) department to develop and validate a rapid testing method enabling a near real-time monitoring system with forecasting capabilities."),
                    br(),
                    h3(tags$b("Objectives")),
                    tags$ul(
                      tags$li(tags$b("Forecasting: "), "Model past spatiotemporal fecal indicator trends to predict beach exceedance locations using artificial intelligence/machine learning approaches."),
                      tags$li(tags$b("Rapid testing: "), "Evaluate and validate endotoxin testing performance against traditional culture-based and molecular methods for determining water quality."),
                      tags$li(tags$b("Integration: "), "Develop a data-driven framework for utilizing the rapid endotoxin testing system to support local partners.")
                    ),
                    br(),
                    h3(tags$b("Participating Laboratories and Health Departments")),
                    tags$ul(
                      tags$li("Bay County Health Department"),
                      tags$li("District Health Department No. 2"),
                      tags$li("Michigan Environmental, Great Lakes, and Energy"),
                      tags$li("Ferris State University"),
                      tags$li("Grand Valley State University"),
                      tags$li("Great Lakes Environmental Center"),
                      tags$li("Kalamazoo County Health and Community Services"),
                      tags$li("Kent County Health Department"),
                      tags$li("Oakland County Health Department"),
                      tags$li("Public Health - Muskegon County"),
                      tags$li("Robert B. Annis Water Resources Institute"),
                      tags$li("Saginaw Valley State University"),
                      tags$li("the Watershed Center"),
                      tags$li("Washtenaw County Health Department"),
                      tags$li("Western Upper Peninsula Health Department")
                    ),
                    br(),
                    h3(tags$b("Methods")),
                    tags$ul(
                      tags$li("Laboratory",
                              tags$ul(
                                tags$li("Bactiquick is a rapid environmental endotoxin test that has been developed to measure beach contamination. It is reported as Endotoxin Risk Units (ERU). 
                                        Endotoxins are present in Gram Negative bacteria (which contains most human pathogens). 
                                        E. coli is regulatory standard for beach closures in measured as Most Probable Number (MPN)."),
                                tags$li("Droplet Digitial PCR (ddPCR) and Quantitative PCR (qPCR) are used for E. coli quantification in some collected samples.")
                              )),
                      tags$li("Analysis",
                              tags$ul(
                                tags$li("The correlation between E. coli colilert and Bactiquick assay results are analyzed to determine an applicable exceedance threshold for Bactiquick tests."),
                                tags$li("Machine learning prediction models using ensembles of elastic net, random forest, additive, gradient boosting, and multi-layer perceptron models are trained 
                                        on historic BeachGuard data on E. coli Colilert 18 levels from 2021 to 2025 calendar years with engineered weather features based on weather pattern rasters from ",
                                        tags$a(href = "https://prism.oregonstate.edu/", "PRISM"),". E. coli levels are forecasting using site geographical location and weather features based on weather 
                                        forecasts from ",tags$a(href="https://open-meteo.com/","Open Meteo"),".")
                              ))
                    ),
                    br(),
                    h3(tags$b("Contact")),
                    p(
                      "Please contact us at ",
                      tags$a(
                        href = "mailto:beachdata@umich.edu", 
                        "beachdata@umich.edu"
                      ),
                      " for any questions."
                    ),
                    br(),
                    hr(),
                    wellPanel(
                      p(em("Disclaimer: This project is conducted in collaboration with EGLE staff memebers. EGLE does not provide funding support for this project. Predictive models can create errors. Always refer to local advisory for beach closures (BeachGuard)."))
                    )
             )
           )
  ),
  nav_spacer(),
  nav_item(
    input_dark_mode(id = "dark_mode", mode="light")
  )
)


# --- 3. SHINY SERVER ----
server <- function(input, output, session) {
  
  updateSelectizeInput(session, "site", choices = site_choices, server = TRUE, selected = "All")
  updateSelectizeInput(session, "weather_site", choices = site_choices, server = TRUE, selected = "All")
  
  # Initialize Region Choices for Dropdown in Tab 5
  region_choices_vec <- sort(unique(as.character(mi_regions_sf$Region)))
  updateSelectizeInput(session, "region_select", 
                       choices = c("All Regions" = "All", setNames(region_choices_vec, paste("Region", region_choices_vec))), 
                       selected = "All", 
                       server = TRUE)
  
  observeEvent(input$clear_sites, {
    updateSelectizeInput(session, "comp_site", selected = character(0))
  })
  
  observeEvent(input$regional_clear_sites, {
    updateSelectizeInput(session, "site", selected = "All")
    updateSelectizeInput(session, "region_select", selected = "All")
  })
  
  filtered_site_ids <- reactive({
    req(input$exceedance_filter, input$corr_filter, input$bacti_thresh_val, input$ecoli_thresh_val, input$ecoli_limit, input$bacti_limit)
    
    df <- minet_data %>% 
      filter(!is.na(ecoli_log) & !is.na(bactiquick_log)) %>%
      mutate(
        ecoli_raw = 10^(ecoli_log) - 0.001,
        bacti_raw = 10^(bactiquick_log) - 0.001
      ) %>%
      filter(
        ecoli_raw >= input$ecoli_limit[1] & ecoli_raw <= input$ecoli_limit[2],
        bacti_raw >= input$bacti_limit[1] & bacti_raw <= input$bacti_limit[2]
      )
    
    bacti_log_thresh <- log10(input$bacti_thresh_val + 0.001)
    ecoli_log_thresh <- log10(input$ecoli_thresh_val + 0.001)
    
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
    } else if (input$exceedance_filter == "no_bacti") {
      site_exceed <- site_exceed %>% filter(!has_bacti_exc)
    } else if (input$exceedance_filter == "no_ecoli") {
      site_exceed <- site_exceed %>% filter(!has_ecoli_exc)
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
      updateSelectizeInput(session, "comp_site", choices = new_choices, selected = "All", server = TRUE)
    } else {
      updateSelectizeInput(session, "comp_site", choices = c("No Match Between Forecast and Minet Dates" = "None"), server = TRUE)
    }
  })
  
  # --- FORECAST PERFORMANCE SITE SELECTOR OBSERVER ---
  observeEvent(input$perf_clear_sites, {
    updateSelectizeInput(session, "perf_comp_site", selected = character(0))
  })
  
  observe({
    req(input$perf_waterbody_filter)
    valid_sites <- filtered_site_ids()
    
    if (exists("matched_comparison_df") && nrow(matched_comparison_df) > 0) {
      temp_df <- matched_comparison_df %>% filter(id %in% valid_sites)
      
      if (input$perf_waterbody_filter != "Any") {
        temp_df <- temp_df %>% filter(waterbody_type == input$perf_waterbody_filter)
      }
      
      available_ids <- unique(as.character(temp_df$id))
      if (length(available_ids) > 0) {
        avail_names <- as.character(geo_info$BeachName[match(available_ids, geo_info$id)])
        new_choices <- c("Overall Average (All Available Sites)" = "All", setNames(available_ids, avail_names))
      } else {
        new_choices <- c("No Sites Match Filter" = "None")
      }
      updateSelectizeInput(session, "perf_comp_site", choices = new_choices, selected = "All", server = TRUE)
    } else {
      updateSelectizeInput(session, "perf_comp_site", choices = c("No Match Between Forecast and Minet Dates" = "None"), server = TRUE)
    }
  })
  
  
  # --- TAB 2 LOGIC 1: STATIC/ALL SITES DATA AVAILABILITY MAP (LEFT) ---
  output$all_sites_availability_map <- renderPlotly({
    site_avail <- minet_data %>%
      group_by(id) %>%
      summarise(
        has_ecoli = any(!is.na(ecoli_log)),
        has_bacti = any(!is.na(bactiquick_log)),
        n_ecoli   = sum(!is.na(ecoli_log)),
        n_bacti   = sum(!is.na(bactiquick_log)),
        .groups   = "drop"
      )
    
    all_sites_df <- geo_info %>%
      filter(!is.na(Latitude) & !is.na(Longitude)) %>%
      left_join(site_avail, by = "id") %>%
      mutate(
        has_ecoli = ifelse(is.na(has_ecoli), FALSE, has_ecoli),
        has_bacti = ifelse(is.na(has_bacti), FALSE, has_bacti),
        n_ecoli   = ifelse(is.na(n_ecoli), 0, n_ecoli),
        n_bacti   = ifelse(is.na(n_bacti), 0, n_bacti),
        data_avail = case_when(
          has_ecoli & has_bacti ~ "Both E. coli & Bactiquick Available",
          has_ecoli             ~ "Only E. coli Available",
          has_bacti             ~ "Bactiquick Only Available",
          TRUE                  ~ "No Assay Data Available"
        )
      ) %>%
      filter(data_avail != "No Assay Data Available")
    
    if (nrow(all_sites_df) == 0) return(plot_ly() %>% layout(title = "No Site Data Available"))
    
    p <- suppressWarnings(
      ggplot() +
        geom_sf(data = mi_counties_sf, fill = "grey92", color = "grey70", linewidth = 0.2) +
        geom_sf(data = mi_regions_sf, fill = NA, color = "black", linewidth = 0.8) +
        geom_point(data = all_sites_df, aes(
          x = Longitude, y = Latitude,
          color = data_avail,
          text = paste0("<b>Site:</b> ", BeachName,
                        "<br><b>ID:</b> ", id,
                        "<br><b>Data Status:</b> ", data_avail,
                        "<br><b>E. coli Samples:</b> ", n_ecoli,
                        "<br><b>Bactiquick Samples:</b> ", n_bacti)
        ), size = 3, alpha = 0.85) +
        scale_color_manual(
          name = "Data Status",
          values = c(
            "Both E. coli & Bactiquick Available" = "#00274C",
            "Only E. coli Available"              = "#FFCB05",
            "Bactiquick Only Available"           = "#9b59b6"
          )
        ) +
        theme_void()
    )
    
    ggplotly(p, tooltip = "text") %>%
      style(hoverinfo = "none", traces = c(1, 2)) %>%
      layout(
        autosize = TRUE,
        showlegend = TRUE,
        margin = list(l = 10, r = 10, b = 120, t = 10),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.1,
          yanchor = "top"
        )
      ) %>%
      config(responsive = TRUE)
  })
  
  # --- TAB 2 LOGIC 2: PAIRWISE SITES AGREEMENT MAP (RIGHT) ---
  output$pairs_agreement_map <- renderPlotly({
    req(input$tab2_map_ecoli_thresh, input$tab2_map_bacti_thresh)
    
    ecoli_log_thresh <- log10(input$tab2_map_ecoli_thresh + 0.001)
    bacti_log_thresh <- log10(input$tab2_map_bacti_thresh + 0.001)
    
    paired_df <- minet_data %>%
      filter(!is.na(ecoli_log) & !is.na(bactiquick_log)) %>%
      group_by(id) %>%
      summarise(
        n_pairs = n(),
        pct_agree = mean((ecoli_log >= ecoli_log_thresh) == (bactiquick_log >= bacti_log_thresh), na.rm = TRUE) * 100,
        .groups = "drop"
      ) %>%
      inner_join(geo_info %>% select(id, Latitude, Longitude, BeachName), by = "id") %>%
      filter(!is.na(Latitude) & !is.na(Longitude) & n_pairs > 0)
    
    if(nrow(paired_df) == 0) return(plot_ly() %>% layout(title = "No Paired Assay Data Available"))
    
    p <- suppressWarnings(
      ggplot() +
        geom_sf(data = mi_counties_sf, fill = "grey92", color = "grey70", linewidth = 0.2) +
        geom_sf(data = mi_regions_sf, fill = NA, color = "black", linewidth = 0.8) +
        geom_point(data = paired_df, aes(
          x = Longitude, y = Latitude,
          size = n_pairs, color = pct_agree,
          text = paste0("<b>Site:</b> ", BeachName,
                        "<br><b>ID:</b> ", id,
                        "<br><b>Paired Samples:</b> ", n_pairs,
                        "<br><b>Agreement:</b> ", round(pct_agree, 1), "%")
        ), alpha = 0.85) +
        scale_size_continuous(range = c(2.5, 9), name = "Pairs Count") +
        scale_color_viridis_c(option = "viridis", limits = c(0, 100), name = "% Agreement") +
        theme_void()
    )
    
    p_plotly <- ggplotly(p, tooltip = "text") %>%
      style(hoverinfo = "none", traces = c(1, 2))
    
    for (i in seq_along(p_plotly$x$data)) {
      if (!is.null(p_plotly$x$data[[i]]$marker$colorbar)) {
        p_plotly$x$data[[i]]$marker$colorbar$orientation <- "h"
        p_plotly$x$data[[i]]$marker$colorbar$x <- 0.5
        p_plotly$x$data[[i]]$marker$colorbar$xanchor <- "center"
        p_plotly$x$data[[i]]$marker$colorbar$y <- -0.05
        p_plotly$x$data[[i]]$marker$colorbar$yanchor <- "top"
        p_plotly$x$data[[i]]$marker$colorbar$len <- 0.65
        p_plotly$x$data[[i]]$marker$colorbar$title <- list(text = "% Agreement", side = "top")
      }
    }
    
    p_plotly %>%
      layout(
        autosize = TRUE,
        showlegend = TRUE,
        margin = list(l = 10, r = 10, b = 120, t = 10),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.22,
          yanchor = "top"
        )
      ) %>%
      config(responsive = TRUE)
  })
  
  # Reactive evaluation for active selected region(s)
  active_regions <- reactive({
    sel <- input$region_select
    if (is.null(sel) || "All" %in% sel || length(sel) == 0) {
      return("All")
    }
    return(as.character(sel))
  })
  
  process_map_click <- function(click_data) {
    req(click_data, click_data$x, click_data$y)
    
    click_pt <- sf::st_sfc(sf::st_point(c(click_data$x, click_data$y)), crs = 4326)
    hits <- sf::st_intersects(click_pt, mi_counties_sf)
    
    if (length(hits[[1]]) > 0) {
      clicked_reg <- as.character(mi_counties_sf$Region[hits[[1]][1]])
    } else {
      distances <- sqrt((mi_counties_map$long - click_data$x)^2 + (mi_counties_map$lat - click_data$y)^2)
      closest_idx <- which.min(distances)
      clicked_reg <- as.character(mi_counties_map$Region[closest_idx])
    }
    
    if (length(clicked_reg) > 0 && !is.na(clicked_reg)) {
      current_sel <- input$region_select
      
      if (is.null(current_sel) || "All" %in% current_sel) {
        new_sel <- clicked_reg
      } else if (clicked_reg %in% current_sel) {
        new_sel <- setdiff(current_sel, clicked_reg)
        if (length(new_sel) == 0) new_sel <- "All"
      } else {
        new_sel <- c(current_sel, clicked_reg)
      }
      
      updateSelectizeInput(session, "region_select", selected = new_sel)
      updateSelectizeInput(session, "site", choices = site_choices, selected = "All", server = TRUE)
    }
  }
  
  observeEvent(input$map_click_1, process_map_click(input$map_click_1))
  observeEvent(input$map_click_2, process_map_click(input$map_click_2))
  
  # --- HISTORIC MAPS LOGIC (TAB 3) ---
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
                                         text = paste("Site:", BeachName, "<br>ID:", id, "<br>Log10 E.coli:", round(ecoli_log, 2))),
                   size = 4, alpha = 0.9) +
        scale_color_viridis_c(option = "rocket", direction = -1, limits = global_hist_ecoli_lims, name = "Log10(E.coli)") +
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
                                         text = paste("Site:", BeachName, "<br>ID:", id, "<br>Log10 Bacti:", round(bactiquick_log, 2))),
                   size = 4, alpha = 0.9) +
        scale_color_viridis_c(option = "mako", direction = -1, limits = global_hist_bacti_lims, name = "Log10(Bacti)") +
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
                                         text = paste("Site:", BeachName, "<br>ID:", id, "<br>7-Day Log10 E.coli:", round(ecoli_log, 2))),
                   size = 4, alpha = 0.9) +
        scale_color_viridis_c(option = "rocket", direction = -1, limits = global_hist_ecoli_lims, name = "Avg Log10(E.coli)") +
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
                                         text = paste("Site:", BeachName, "<br>ID:", id, "<br>7-Day Log10 Bacti:", round(bactiquick_log, 2))),
                   size = 4, alpha = 0.9) +
        scale_color_viridis_c(option = "mako", direction = -1, limits = global_hist_bacti_lims, name = "Avg Log10(Bacti)") +
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
    
    if(!exists("matched_comparison_df") || nrow(matched_comparison_df) == 0 || 
       (length(input$comp_site) > 0 && "None" %in% input$comp_site)) {
      return(NULL)
    }
    
    valid_sites <- filtered_site_ids()
    plot_df <- matched_comparison_df %>% filter(id %in% valid_sites)
    
    if(input$waterbody_filter != "Any") plot_df <- plot_df %>% filter(waterbody_type == input$waterbody_filter)
    
    if(!is.null(input$comp_site) && 
       !any(c("All", "Overall Average") %in% input$comp_site) && 
       length(input$comp_site) > 0) {
      plot_df <- plot_df %>% filter(id %in% input$comp_site)
    }
    
    if(nrow(plot_df) == 0) return(NULL)
    
    find_pcr_col <- function(df, patterns) {
      cols <- names(df)
      for (p in patterns) {
        m <- grep(paste0("^", p, "$"), cols, ignore.case = TRUE, value = TRUE)
        if (length(m) > 0) return(m[1])
      }
      for (p in patterns) {
        m <- grep(p, cols, ignore.case = TRUE, value = TRUE)
        if (length(m) > 0) return(m[1])
      }
      return(NA_character_)
    }
    
    pcr_patterns_q  <- c("qpcr_log", "minet_qpcr_log", "qpcr_log10", "qpcr", "qpcr_val", "qpcr_results", "qpcr_copies")
    pcr_patterns_dd <- c("ddpcr_log", "minet_ddpcr_log", "ddpcr_log10", "ddpcr", "ddpcr_val", "ddpcr_results", "ddpcr_copies")
    
    qpcr_col  <- find_pcr_col(plot_df, pcr_patterns_q)
    ddpcr_col <- find_pcr_col(plot_df, pcr_patterns_dd)
    
    if ((is.na(qpcr_col) || is.na(ddpcr_col)) && exists("minet_data") && nrow(minet_data) > 0) {
      mq <- find_pcr_col(minet_data, pcr_patterns_q)
      mdd <- find_pcr_col(minet_data, pcr_patterns_dd)
      
      needed_cols <- c(mq, mdd)[!is.na(c(mq, mdd))]
      if (length(needed_cols) > 0) {
        sub_minet <- minet_data %>% 
          select(id, SampleDate, any_of(needed_cols)) %>%
          mutate(SampleDate = as.Date(SampleDate))
        
        plot_df <- plot_df %>% 
          mutate(SampleDate = as.Date(SampleDate)) %>%
          left_join(sub_minet, by = c("id", "SampleDate"))
        
        if (is.na(qpcr_col))  qpcr_col  <- find_pcr_col(plot_df, pcr_patterns_q)
        if (is.na(ddpcr_col)) ddpcr_col <- find_pcr_col(plot_df, pcr_patterns_dd)
      }
    }
    
    get_clean_pcr <- function(df, col_name) {
      if (is.na(col_name) || !col_name %in% names(df)) {
        return(list(log = rep(NA_real_, nrow(df)), raw = rep(NA_real_, nrow(df))))
      }
      vals <- as.numeric(df[[col_name]])
      max_v <- suppressWarnings(max(vals, na.rm = TRUE))
      if (!is.infinite(max_v) && max_v > 25) {
        raw_v <- vals
        log_v <- log10(pmax(vals, 0) + 0.001)
      } else {
        log_v <- vals
        raw_v <- 10^(vals) - 0.001
      }
      list(log = log_v, raw = raw_v)
    }
    
    q_parsed  <- get_clean_pcr(plot_df, qpcr_col)
    dd_parsed <- get_clean_pcr(plot_df, ddpcr_col)
    
    plot_df$qpcr_in_log  <- q_parsed$log
    plot_df$qpcr_in_raw  <- q_parsed$raw
    plot_df$ddpcr_in_log <- dd_parsed$log
    plot_df$ddpcr_in_raw <- dd_parsed$raw
    
    plot_df %>% 
      mutate(
        SampleDate = as.Date(SampleDate),
        ecoli_actual_raw = 10^(minet_ecoli_log) - 0.001,
        bact_actual_raw  = 10^(minet_bact_log) - 0.001
      ) %>%
      filter(
        is.na(ecoli_actual_raw) | (ecoli_actual_raw >= input$ecoli_limit[1] & ecoli_actual_raw <= input$ecoli_limit[2]),
        is.na(bact_actual_raw)  | (bact_actual_raw >= input$bacti_limit[1] & bact_actual_raw <= input$bacti_limit[2])
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
        
        ecoli_raw_min    = safe_min(ecoli_actual_raw),
        ecoli_raw_max    = safe_max(ecoli_actual_raw),
        ecoli_actual_raw = mean(ecoli_actual_raw, na.rm = TRUE),
        
        bact_raw_min     = safe_min(bact_actual_raw),
        bact_raw_max     = safe_max(bact_actual_raw),
        bact_actual_raw  = mean(bact_actual_raw, na.rm = TRUE),
        
        ecoli_fcst_raw   = mean(10^(fcst_ecoli_log) - 0.001, na.rm = TRUE),
        
        qpcr_actual      = { v <- mean(qpcr_in_log, na.rm = TRUE); if(is.nan(v) || is.infinite(v)) NA_real_ else v },
        qpcr_actual_min  = safe_min(qpcr_in_log),
        qpcr_actual_max  = safe_max(qpcr_in_log),
        qpcr_actual_n    = sum(!is.na(qpcr_in_log)),
        qpcr_actual_raw  = { v <- mean(qpcr_in_raw, na.rm = TRUE); if(is.nan(v) || is.infinite(v)) NA_real_ else v },
        qpcr_raw_min     = safe_min(qpcr_in_raw),
        qpcr_raw_max     = safe_max(qpcr_in_raw),
        
        ddpcr_actual     = { v <- mean(ddpcr_in_log, na.rm = TRUE); if(is.nan(v) || is.infinite(v)) NA_real_ else v },
        ddpcr_actual_min = safe_min(ddpcr_in_log),
        ddpcr_actual_max = safe_max(ddpcr_in_log),
        ddpcr_actual_n   = sum(!is.na(ddpcr_in_log)),
        ddpcr_actual_raw = { v <- mean(ddpcr_in_raw, na.rm = TRUE); if(is.nan(v) || is.infinite(v)) NA_real_ else v },
        ddpcr_raw_min    = safe_min(ddpcr_in_raw),
        ddpcr_raw_max    = safe_max(ddpcr_in_raw),
        
        .groups = "drop"
      )
  })
  
  # INTERACTIVE LOG SCALE COMPARISON PLOT
  output$compare_plot <- renderPlotly({
    plot_df <- comp_plot_data()
    if(is.null(plot_df) || nrow(plot_df) == 0) {
      return(plot_ly() %>% layout(title = "No Data Available for Filters"))
    }
    
    color_mapping <- c("E. coli (MPN)" = "black", "Bactiquick" = "blue", "qPCR" = "#27ae60", "ddPCR" = "#8e44ad")
    thresh_val <- log10(input$ecoli_thresh_val + 0.001)
    
    ecoli_df <- plot_df %>% filter(!is.na(ecoli_actual))
    bact_df  <- plot_df %>% filter(!is.na(bact_actual))
    qpcr_df  <- plot_df %>% filter(!is.na(qpcr_actual))
    ddpcr_df <- plot_df %>% filter(!is.na(ddpcr_actual))
    
    p <- ggplot(plot_df, aes(x = SampleDate)) +
      geom_hline(yintercept = thresh_val, linetype = "dashed", color = "red", linewidth = 0.8, alpha = 0.7) +
      geom_ribbon(data = ecoli_df, aes(ymin = ecoli_actual_min, ymax = ecoli_actual_max, fill = "E. coli (MPN)"), alpha = 0.12) +
      geom_ribbon(data = bact_df, aes(ymin = bact_actual_min, ymax = bact_actual_max, fill = "Bactiquick"), alpha = 0.12) +
      geom_line(data = ecoli_df, aes(y = ecoli_actual, color = "E. coli (MPN)"), linewidth = 1.2, linetype = "dashed", alpha = 0.8) +
      geom_point(data = ecoli_df, aes(y = ecoli_actual, color = "E. coli (MPN)",
                                      text = paste0("<b>Date:</b> ", format(SampleDate, "%Y-%m-%d"),
                                                    "<br><b>Metric:</b> E. coli (Colilert 18)",
                                                    "<br><b>Log10 Value (Mean):</b> ", round(ecoli_actual, 3),
                                                    "<br><b>Min:</b> ", round(ecoli_actual_min, 3),
                                                    "<br><b>Max:</b> ", round(ecoli_actual_max, 3),
                                                    "<br><b>N:</b> ", ecoli_actual_n)), size = 3, shape = 18, alpha = 0.8) +
      geom_line(data = bact_df, aes(y = bact_actual, color = "Bactiquick"), linewidth = 1.2, linetype = "dotted", alpha = 0.8) +
      geom_point(data = bact_df, aes(y = bact_actual, color = "Bactiquick",
                                     text = paste0("<b>Date:</b> ", format(SampleDate, "%Y-%m-%d"),
                                                   "<br><b>Metric:</b> Bactiquick",
                                                   "<br><b>Log10 Value (Mean):</b> ", round(bact_actual, 3),
                                                   "<br><b>Min:</b> ", round(bact_actual_min, 3),
                                                   "<br><b>Max:</b> ", round(bact_actual_max, 3),
                                                   "<br><b>N:</b> ", bact_actual_n)), size = 3, shape = 17, alpha = 0.8)
    
    if(input$show_qpcr_ddpcr) {
      if(nrow(qpcr_df) > 0) {
        p <- p + geom_ribbon(data = qpcr_df, aes(ymin = qpcr_actual_min, ymax = qpcr_actual_max, fill = "qPCR"), alpha = 0.12) +
          geom_line(data = qpcr_df, aes(y = qpcr_actual, color = "qPCR"), linewidth = 1, linetype = "twodash", alpha = 0.8) +
          geom_point(data = qpcr_df, aes(y = qpcr_actual, color = "qPCR",
                                         text = paste0("<b>Date:</b> ", format(SampleDate, "%Y-%m-%d"),
                                                       "<br><b>Metric:</b> qPCR",
                                                       "<br><b>Log10 Value (Mean):</b> ", round(qpcr_actual, 3),
                                                       "<br><b>Min:</b> ", round(qpcr_actual_min, 3),
                                                       "<br><b>Max:</b> ", round(qpcr_actual_max, 3),
                                                       "<br><b>N:</b> ", qpcr_actual_n)), size = 3, shape = 15, alpha = 0.8)
      }
      if(nrow(ddpcr_df) > 0) {
        p <- p + geom_ribbon(data = ddpcr_df, aes(ymin = ddpcr_actual_min, ymax = ddpcr_actual_max, fill = "ddPCR"), alpha = 0.12) +
          geom_line(data = ddpcr_df, aes(y = ddpcr_actual, color = "ddPCR"), linewidth = 1, linetype = "longdash", alpha = 0.8) +
          geom_point(data = ddpcr_df, aes(y = ddpcr_actual, color = "ddPCR",
                                          text = paste0("<b>Date:</b> ", format(SampleDate, "%Y-%m-%d"),
                                                        "<br><b>Metric:</b> ddPCR",
                                                        "<br><b>Log10 Value (Mean):</b> ", round(ddpcr_actual, 3),
                                                        "<br><b>Min:</b> ", round(ddpcr_actual_min, 3),
                                                        "<br><b>Max:</b> ", round(ddpcr_actual_max, 3),
                                                        "<br><b>N:</b> ", ddpcr_actual_n)), size = 3, shape = 16, alpha = 0.8)
      }
    }
    
    p <- p + 
      scale_color_manual(values = color_mapping, name = "Data Source") + 
      scale_fill_manual(values = color_mapping, name = "Data Source") + 
      guides(fill = "none") + 
      theme_minimal() + 
      labs(x = "Date", y = "Log10 Level") + 
      theme(
        text = element_text(size = 12),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 0)
      )
    
    p_plotly <- ggplotly(p, tooltip = "text")
    
    # Clean up Plotly trace names and deduplicate legend items
    seen_names <- c()
    for (i in seq_along(p_plotly$x$data)) {
      if (!is.null(p_plotly$x$data[[i]]$name)) {
        clean_name <- gsub("^\\((.*),\\d+\\)$", "\\1", p_plotly$x$data[[i]]$name)
        p_plotly$x$data[[i]]$name <- clean_name
        
        if (clean_name %in% seen_names) {
          p_plotly$x$data[[i]]$showlegend <- FALSE
        } else {
          seen_names <- c(seen_names, clean_name)
        }
      }
    }
    
    p_plotly %>%
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
    
    color_mapping <- c("E. coli (MPN)" = "black", "Bactiquick" = "blue", "qPCR" = "#27ae60", "ddPCR" = "#8e44ad")
    
    ecoli_df <- plot_df %>% filter(!is.na(ecoli_actual_raw))
    bact_df  <- plot_df %>% filter(!is.na(bact_actual_raw))
    qpcr_df  <- plot_df %>% filter(!is.na(qpcr_actual_raw))
    ddpcr_df <- plot_df %>% filter(!is.na(ddpcr_actual_raw))
    
    p <- ggplot(plot_df, aes(x = SampleDate)) +
      geom_hline(yintercept = input$ecoli_thresh_val, linetype = "dashed", color = "red", linewidth = 0.8, alpha = 0.7)
    
    if(nrow(ecoli_df) > 0) {
      p <- p + geom_ribbon(data = ecoli_df, aes(ymin = ecoli_raw_min, ymax = ecoli_raw_max, fill = "E. coli (MPN)"), alpha = 0.15) + 
        geom_line(data = ecoli_df, aes(y = ecoli_actual_raw, color = "E. coli (MPN)"), linewidth = 1.2, linetype = "dashed", alpha = 0.8) +
        geom_point(data = ecoli_df, aes(y = ecoli_actual_raw, color = "E. coli (MPN)",
                                        text = paste0("<b>Date:</b> ", format(SampleDate, "%Y-%m-%d"),
                                                      "<br><b>Metric:</b> E. coli (Colilert 18)",
                                                      "<br><b>Raw Value (Mean):</b> ", round(ecoli_actual_raw, 1), " MPN",
                                                      "<br><b>Min:</b> ", round(ecoli_raw_min, 1),
                                                      "<br><b>Max:</b> ", round(ecoli_raw_max, 1),
                                                      "<br><b>N:</b> ", ecoli_actual_n)), size = 3, shape = 18, alpha = 0.8)
    }
    
    if(nrow(bact_df) > 0) {
      p <- p + geom_ribbon(data = bact_df, aes(ymin = bact_raw_min, ymax = bact_raw_max, fill = "Bactiquick"), alpha = 0.15) + 
        geom_line(data = bact_df, aes(y = bact_actual_raw, color = "Bactiquick"), linewidth = 1.2, linetype = "dotted", alpha = 0.8) +
        geom_point(data = bact_df, aes(y = bact_actual_raw, color = "Bactiquick",
                                       text = paste0("<b>Date:</b> ", format(SampleDate, "%Y-%m-%d"),
                                                     "<br><b>Metric:</b> Bactiquick",
                                                     "<br><b>Raw Value (Mean):</b> ", round(bact_actual_raw, 1), " ERU",
                                                     "<br><b>Min:</b> ", round(bact_raw_min, 1),
                                                     "<br><b>Max:</b> ", round(bact_raw_max, 1),
                                                     "<br><b>N:</b> ", bact_actual_n)), size = 3, shape = 17, alpha = 0.8)
    }
    
    if(input$show_qpcr_ddpcr) {
      if(nrow(qpcr_df) > 0) {
        p <- p + geom_ribbon(data = qpcr_df, aes(ymin = qpcr_raw_min, ymax = qpcr_raw_max, fill = "qPCR"), alpha = 0.15) + 
          geom_line(data = qpcr_df, aes(y = qpcr_actual_raw, color = "qPCR"), linewidth = 1, linetype = "twodash", alpha = 0.8) +
          geom_point(data = qpcr_df, aes(y = qpcr_actual_raw, color = "qPCR",
                                         text = paste0("<b>Date:</b> ", format(SampleDate, "%Y-%m-%d"),
                                                       "<br><b>Metric:</b> qPCR",
                                                       "<br><b>Raw Value (Mean):</b> ", round(qpcr_actual_raw, 1),
                                                       "<br><b>Min:</b> ", round(qpcr_raw_min, 1),
                                                       "<br><b>Max:</b> ", round(qpcr_raw_max, 1),
                                                       "<br><b>N:</b> ", qpcr_actual_n)), size = 3, shape = 15, alpha = 0.8)
      }
      if(nrow(ddpcr_df) > 0) {
        p <- p + geom_ribbon(data = ddpcr_df, aes(ymin = ddpcr_raw_min, ymax = ddpcr_raw_max, fill = "ddPCR"), alpha = 0.15) + 
          geom_line(data = ddpcr_df, aes(y = ddpcr_actual_raw, color = "ddPCR"), linewidth = 1, linetype = "longdash", alpha = 0.8) +
          geom_point(data = ddpcr_df, aes(y = ddpcr_actual_raw, color = "ddPCR",
                                          text = paste0("<b>Date:</b> ", format(SampleDate, "%Y-%m-%d"),
                                                        "<br><b>Metric:</b> ddPCR",
                                                        "<br><b>Raw Value (Mean):</b> ", round(ddpcr_actual_raw, 1),
                                                        "<br><b>Min:</b> ", round(ddpcr_raw_min, 1),
                                                        "<br><b>Max:</b> ", round(ddpcr_raw_max, 1),
                                                        "<br><b>N:</b> ", ddpcr_actual_n)), size = 3, shape = 16, alpha = 0.8)
      }
    }
    
    p <- p + 
      scale_color_manual(values = color_mapping, name = "Data Source") + 
      scale_fill_manual(values = color_mapping, name = "Data Source") + 
      guides(fill = "none") + 
      theme_minimal() + 
      labs(x = "Date", y = "Raw Assay Result (MPN / ERU)") + 
      theme(
        text = element_text(size = 12),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 0)
      )
    
    p_plotly <- ggplotly(p, tooltip = "text")
    
    # Clean up Plotly trace names and deduplicate legend items
    seen_names <- c()
    for (i in seq_along(p_plotly$x$data)) {
      if (!is.null(p_plotly$x$data[[i]]$name)) {
        clean_name <- gsub("^\\((.*),\\d+\\)$", "\\1", p_plotly$x$data[[i]]$name)
        p_plotly$x$data[[i]]$name <- clean_name
        
        if (clean_name %in% seen_names) {
          p_plotly$x$data[[i]]$showlegend <- FALSE
        } else {
          seen_names <- c(seen_names, clean_name)
        }
      }
    }
    
    p_plotly %>%
      layout(
        autosize = TRUE,
        margin = list(l = 55, r = 120, t = 20, b = 40),
        legend = list(orientation = "v", x = 1.02, xanchor = "left", y = 1, yanchor = "top")
      ) %>%
      config(responsive = TRUE)
  })
  
  get_scatter_data <- reactive({
    req(input$color_mode, input$waterbody_filter, input$bacti_thresh_val, input$ecoli_thresh_val, input$ecoli_limit, input$bacti_limit)
    if(nrow(minet_data) == 0 || (!is.null(input$comp_site) && "None" %in% input$comp_site)) return(NULL)
    
    valid_sites <- filtered_site_ids()
    target_data <- minet_data %>% filter(id %in% valid_sites)
    
    if(!is.null(input$comp_site) && !("All" %in% input$comp_site) && length(input$comp_site) > 0) {
      target_data <- target_data %>% filter(id %in% input$comp_site)
    }
    
    target_data <- target_data %>% 
      inner_join(geo_info %>% select(id, BeachName), by = "id") %>% 
      filter(!is.na(ecoli_log) & !is.na(bactiquick_log)) %>%
      mutate(
        ecoli_raw = 10^(ecoli_log) - 0.001,
        bacti_raw = 10^(bactiquick_log) - 0.001
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
  
  # --- LOG10 SCATTER PLOT ---
  output$bact_scatter <- renderPlotly({
    target_data <- get_scatter_data()
    if(is.null(target_data) || nrow(target_data) == 0) return(plot_ly() %>% layout(title = "No Data for Selected View"))
    
    bacti_thresh <- log10(input$bacti_thresh_val + 0.001)
    ecoli_thresh <- log10(input$ecoli_thresh_val + 0.001)
    
    if (input$color_mode == "disagree") {
      target_data <- target_data %>% 
        mutate(ecoli_exc = ecoli_log >= ecoli_thresh, 
               bacti_exc = bactiquick_log >= bacti_thresh, 
               ColorStatus = ifelse(ecoli_exc == bacti_exc, "Agree", "Disagree"))
      colors_map <- c("Agree" = "#16a085", "Disagree" = "#e74c3c")
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
    }
    
    p <- suppressWarnings(
      ggplot(target_data, aes(x = ecoli_log, y = bactiquick_log, color = ColorStatus)) +
        geom_vline(xintercept = ecoli_thresh, linetype = "dashed", color = "red", alpha = 0.5) +
        geom_hline(yintercept = bacti_thresh, linetype = "dashed", color = "blue", alpha = 0.5) +
        geom_point(aes(text = paste("Site ID:", id, "<br>Name:", BeachName, "<br>Date:", SampleDate, "<br>Log10 E.coli:", round(ecoli_log, 2), "<br>Log10 Bacti:", round(bactiquick_log, 2), "<br>Status:", ColorStatus)), size = 3, alpha = 0.7) +
        geom_smooth(aes(group = 1), method = "lm", formula = y ~ x, color = "#2c3e50", linetype = "dashed", linewidth = 1) +
        scale_color_manual(values = colors_map) +
        theme_minimal() +
        labs(x = "Log10(E. coli MPN + 0.001) [Colilert 18]", y = "Log10(Bactiquick ERU + 0.001)", color = "Status") +
        theme(text = element_text(size = 14), legend.position = "right")
    )
    
    suppressWarnings(
      ggplotly(p, tooltip = "text") %>%
        layout(autosize = TRUE) %>%
        config(responsive = TRUE)
    )
  })
  
  # --- LOG10 SUMMARY TABLE ---
  output$bact_scatter_stats <- renderTable({
    target_data <- get_scatter_data()
    if(is.null(target_data) || nrow(target_data) == 0) return(NULL)
    
    ct <- cor.test(target_data$ecoli_log, target_data$bactiquick_log, method = "spearman", exact = FALSE)
    rho <- round(ct$estimate, 3)
    pval <- format.pval(ct$p.value, digits = 3)
    n_pts <- nrow(target_data)
    
    bacti_thresh <- log10(input$bacti_thresh_val + 0.001)
    ecoli_thresh <- log10(input$ecoli_thresh_val + 0.001)
    
    q1_both    <- sum(target_data$ecoli_log >= ecoli_thresh & target_data$bactiquick_log >= bacti_thresh, na.rm = TRUE)
    q2_bacti   <- sum(target_data$ecoli_log < ecoli_thresh & target_data$bactiquick_log >= bacti_thresh, na.rm = TRUE)
    q3_neither <- sum(target_data$ecoli_log < ecoli_thresh & target_data$bactiquick_log < bacti_thresh, na.rm = TRUE)
    q4_ecoli   <- sum(target_data$ecoli_log >= ecoli_thresh & target_data$bactiquick_log < bacti_thresh, na.rm = TRUE)
    
    if (input$color_mode == "disagree") {
      disagree_pct <- round(mean((target_data$ecoli_log >= ecoli_thresh) != (target_data$bactiquick_log >= bacti_thresh), na.rm = TRUE) * 100, 1)
      rel_label <- "Disagreement Percentage"
      rel_val   <- paste0(disagree_pct, "%")
    } else {
      req(input$discordance_pct)
      pct_thresh  <- input$discordance_pct / 100
      discord_pct <- round(mean(target_data$bactiquick_log >= (1 + pct_thresh) * target_data$ecoli_log | 
                                  target_data$bactiquick_log <= (1 - pct_thresh) * target_data$ecoli_log, na.rm = TRUE) * 100, 1)
      rel_label <- paste0("Discordance (>±", input$discordance_pct, "%)")
      rel_val   <- paste0(discord_pct, "%")
    }
    
    data.frame(
      Statistic = c(
        "Spearman's rank correlation (\u03c1)",
        "p-value",
        rel_label,
        "Total paired observations (n)",
        "Exceedances detected by both methods (Top-Right quadrant)",
        "Non-exceedances agreed by both methods (Bottom-Left quadrant)",
        "Exceedances detected by Colilert only (Bottom-Right quadrant)",
        "Exceedances detected by Bactiquick only (Top-Left quadrant)"
      ),
      Value = c(
        as.character(rho),
        as.character(pval),
        rel_val,
        as.character(n_pts),
        sprintf("%d (%.1f%%)", q1_both, (q1_both / n_pts) * 100),
        sprintf("%d (%.1f%%)", q3_neither, (q3_neither / n_pts) * 100),
        sprintf("%d (%.1f%%)", q4_ecoli, (q4_ecoli / n_pts) * 100),
        sprintf("%d (%.1f%%)", q2_bacti, (q2_bacti / n_pts) * 100)
      ),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, width = "100%", colnames = TRUE)
  
  # --- RAW SCATTER PLOT ---
  output$bact_scatter_raw <- renderPlotly({
    target_data <- get_scatter_data()
    if(is.null(target_data) || nrow(target_data) == 0) return(plot_ly() %>% layout(title = "No Data for Selected View"))
    
    bacti_thresh <- input$bacti_thresh_val
    ecoli_thresh <- input$ecoli_thresh_val
    
    if (input$color_mode == "disagree") {
      target_data <- target_data %>% 
        mutate(ecoli_exc = ecoli_raw >= ecoli_thresh, 
               bacti_exc = bacti_raw >= bacti_thresh, 
               ColorStatus = ifelse(ecoli_exc == bacti_exc, "Agree", "Disagree"))
      colors_map <- c("Agree" = "#16a085", "Disagree" = "#e74c3c")
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
    }
    
    p <- suppressWarnings(
      ggplot(target_data, aes(x = ecoli_raw, y = bacti_raw, color = ColorStatus)) +
        geom_vline(xintercept = ecoli_thresh, linetype = "dashed", color = "red", alpha = 0.5) +
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
        layout(autosize = TRUE) %>%
        config(responsive = TRUE)
    )
  })
  
  # --- RAW SUMMARY TABLE ---
  output$bact_scatter_raw_stats <- renderTable({
    target_data <- get_scatter_data()
    if(is.null(target_data) || nrow(target_data) == 0) return(NULL)
    
    ct <- cor.test(target_data$ecoli_raw, target_data$bacti_raw, method = "spearman", exact = FALSE)
    rho <- round(ct$estimate, 3)
    pval <- format.pval(ct$p.value, digits = 3)
    n_pts <- nrow(target_data)
    
    bacti_thresh <- input$bacti_thresh_val
    ecoli_thresh <- input$ecoli_thresh_val
    
    q1_both    <- sum(target_data$ecoli_raw >= ecoli_thresh & target_data$bacti_raw >= bacti_thresh, na.rm = TRUE)
    q2_bacti   <- sum(target_data$ecoli_raw < ecoli_thresh & target_data$bacti_raw >= bacti_thresh, na.rm = TRUE)
    q3_neither <- sum(target_data$ecoli_raw < ecoli_thresh & target_data$bacti_raw < bacti_thresh, na.rm = TRUE)
    q4_ecoli   <- sum(target_data$ecoli_raw >= ecoli_thresh & target_data$bacti_raw < bacti_thresh, na.rm = TRUE)
    
    if (input$color_mode == "disagree") {
      disagree_pct <- round(mean((target_data$ecoli_raw >= ecoli_thresh) != (target_data$bacti_raw >= bacti_thresh), na.rm = TRUE) * 100, 1)
      rel_label <- "Disagreement Percentage"
      rel_val   <- paste0(disagree_pct, "%")
    } else {
      req(input$discordance_pct)
      pct_thresh  <- input$discordance_pct / 100
      discord_pct <- round(mean(target_data$bacti_raw >= (1 + pct_thresh) * target_data$ecoli_raw | 
                                  target_data$bacti_raw <= (1 - pct_thresh) * target_data$ecoli_raw, na.rm = TRUE) * 100, 1)
      rel_label <- paste0("Discordance (>±", input$discordance_pct, "%)")
      rel_val   <- paste0(discord_pct, "%")
    }
    
    data.frame(
      Statistic = c(
        "Spearman's rank correlation (\u03c1)",
        "p-value",
        rel_label,
        "Total paired observations (n)",
        "Exceedances detected by both methods (Top-Right quadrant)",
        "Non-exceedances agreed by both methods (Bottom-Left quadrant)",
        "Exceedances detected by Colilert only (Bottom-Right quadrant)",
        "Exceedances detected by Bactiquick only (Top-Left quadrant)"
      ),
      Value = c(
        as.character(rho),
        as.character(pval),
        rel_val,
        as.character(n_pts),
        sprintf("%d (%.1f%%)", q1_both, (q1_both / n_pts) * 100),
        sprintf("%d (%.1f%%)", q3_neither, (q3_neither / n_pts) * 100),
        sprintf("%d (%.1f%%)", q4_ecoli, (q4_ecoli / n_pts) * 100),
        sprintf("%d (%.1f%%)", q2_bacti, (q2_bacti / n_pts) * 100)
      ),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, width = "100%", colnames = TRUE)
  
  # --- FORECAST LOGIC ---
  output$map_title <- renderText({
    req(input$map_date)
    paste("Data Snapshot for:", format(as.Date(input$map_date), "%B %d, %Y"))
  })
  
  output$trend_title <- renderText({
    req(input$site, input$trend_metric)
    metric_label <- ifelse(input$trend_metric == "Forecasted_Ecoli_Level", "E. coli Forecast", "Probability Forecast")
    sel_regs <- active_regions()
    
    if (input$site != "All") {
      site_display_name <- names(site_choices)[site_choices == input$site]
      paste("Trend:", metric_label, "-", site_display_name)
    } else if (!("All" %in% sel_regs)) {
      paste("Trend:", metric_label, "- Region(s):", paste(sel_regs, collapse = ", "))
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
      sel_regs <- active_regions()
      
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
          is_selected = ("All" %in% sel_regs) | (as.character(Region) %in% sel_regs),
          alpha_val = ifelse(is_selected, 1, 0.25)
        )
      
      if (metric_name == "Forecasted_Ecoli_Level") {
        fill_scale <- scale_fill_viridis_c(option = "rocket", direction = -1, limits = global_ecoli_limits, na.value = "grey90", name = "Mean E. coli")
      } else {
        fill_scale <- scale_fill_viridis_c(option = "mako", direction = -1, limits = c(0,1), na.value = "grey90", name = "Exc. Prob.")
      }
      
      suppressWarnings(
        ggplot() +
          geom_sf(data = map_data_sf, aes(fill = metric_val, alpha = alpha_val), color = "grey60", linewidth = 0.2) +
          geom_sf(data = mi_regions_sf, fill = NA, color = "black", linewidth = 0.8) +
          geom_sf_label(data = mi_regions_sf, aes(label = Region), fontface = "bold", size = 4, fill = "white", color = "black", alpha = 0.8) +
          scale_alpha_identity() + fill_scale + theme_void() +
          theme(legend.position = "right", legend.title = element_text(face = "bold", size = 11), legend.text = element_text(size = 9))
      )
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
    sel_regs <- active_regions()
    
    if (input$site != "All") {
      trend_df <- current_fcst %>% filter(as.character(id) == as.character(input$site)) %>% mutate(metric_val = as.numeric(as.character(.data[[input$trend_metric]])))
    } else if (!("All" %in% sel_regs)) {
      trend_df <- current_fcst %>% 
        inner_join(geo_info %>% select(id, Region), by = "id") %>% 
        filter(as.character(Region) %in% sel_regs) %>%
        group_by(SampleDate, Type) %>% 
        summarise(metric_val = mean(as.numeric(as.character(.data[[input$trend_metric]])), na.rm = TRUE), .groups = "drop")
    } else {
      trend_df <- current_fcst %>% 
        group_by(SampleDate, Type) %>% 
        summarise(metric_val = mean(as.numeric(as.character(.data[[input$trend_metric]])), na.rm = TRUE), .groups = "drop")
    }
    
    if (input$trend_metric == "Forecasted_Ecoli_Level") {
      trend_df <- trend_df %>% mutate(metric_val = log10(metric_val + 0.001))
      y_label <- "Log10(E. coli Level + 0.001)"
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
  
  
  # --- FORECAST PERFORMANCE LOGIC ---
  perf_plot_data <- reactive({
    req(input$perf_comp_site, input$perf_waterbody_filter, input$ecoli_limit)
    
    if(!exists("matched_comparison_df") || nrow(matched_comparison_df) == 0 || 
       (length(input$perf_comp_site) > 0 && "None" %in% input$perf_comp_site)) {
      return(NULL)
    }
    
    valid_sites <- filtered_site_ids()
    plot_df <- matched_comparison_df %>% filter(id %in% valid_sites)
    
    if(input$perf_waterbody_filter != "Any") plot_df <- plot_df %>% filter(waterbody_type == input$perf_waterbody_filter)
    
    if(!is.null(input$perf_comp_site) && 
       !any(c("All", "Overall Average") %in% input$perf_comp_site) && 
       length(input$perf_comp_site) > 0) {
      plot_df <- plot_df %>% filter(id %in% input$perf_comp_site)
    }
    
    if(nrow(plot_df) == 0) return(NULL)
    
    plot_df %>% 
      mutate(
        SampleDate = as.Date(SampleDate),
        ecoli_actual_raw = 10^(minet_ecoli_log) - 0.001
      ) %>%
      filter(
        is.na(ecoli_actual_raw) | (ecoli_actual_raw >= input$ecoli_limit[1] & ecoli_actual_raw <= input$ecoli_limit[2])
      ) %>%
      group_by(SampleDate) %>% 
      summarise(
        ecoli_actual     = mean(minet_ecoli_log, na.rm = TRUE),
        ecoli_actual_min = safe_min(minet_ecoli_log),
        ecoli_actual_max = safe_max(minet_ecoli_log),
        ecoli_actual_n   = sum(!is.na(minet_ecoli_log)),
        
        ecoli_fcst       = mean(fcst_ecoli_log, na.rm = TRUE),
        ecoli_fcst_min   = safe_min(fcst_ecoli_log),
        ecoli_fcst_max   = safe_max(fcst_ecoli_log),
        ecoli_fcst_n     = sum(!is.na(fcst_ecoli_log)),
        
        .groups = "drop"
      )
  })
  
  output$perf_compare_plot <- renderPlotly({
    plot_df <- perf_plot_data()
    if(is.null(plot_df) || nrow(plot_df) == 0) {
      return(plot_ly() %>% layout(title = "No Data Available for Selected Filters"))
    }
    
    color_mapping <- c("Observed E. coli" = "black", "Forecasted E. coli" = "#e74c3c")
    thresh_val <- log10(input$perf_ecoli_thresh + 0.001)
    
    ecoli_df <- plot_df %>% filter(!is.na(ecoli_actual))
    fcst_df  <- plot_df %>% filter(!is.na(ecoli_fcst))
    
    p <- ggplot(plot_df, aes(x = SampleDate)) +
      geom_hline(yintercept = thresh_val, linetype = "dashed", color = "red", linewidth = 0.8, alpha = 0.7) +
      geom_ribbon(data = ecoli_df, aes(ymin = ecoli_actual_min, ymax = ecoli_actual_max), fill = "black", alpha = 0.12) +
      geom_ribbon(data = fcst_df, aes(ymin = ecoli_fcst_min, ymax = ecoli_fcst_max), fill = "#e74c3c", alpha = 0.12) +
      geom_line(data = ecoli_df, aes(y = ecoli_actual, color = "Observed E. coli"), linewidth = 1.2, linetype = "dashed", alpha = 0.8) +
      geom_point(data = ecoli_df, aes(y = ecoli_actual, color = "Observed E. coli",
                                      text = paste0("<b>Date:</b> ", format(SampleDate, "%Y-%m-%d"),
                                                    "<br><b>Metric:</b> Observed E. coli",
                                                    "<br><b>Log10 Value (Mean):</b> ", round(ecoli_actual, 3),
                                                    "<br><b>Min:</b> ", round(ecoli_actual_min, 3),
                                                    "<br><b>Max:</b> ", round(ecoli_actual_max, 3),
                                                    "<br><b>N:</b> ", ecoli_actual_n)), size = 3, shape = 18, alpha = 0.8) +
      geom_line(data = fcst_df, aes(y = ecoli_fcst, color = "Forecasted E. coli"), linewidth = 1.2, alpha = 0.8) +
      geom_point(data = fcst_df, aes(y = ecoli_fcst, color = "Forecasted E. coli",
                                     text = paste0("<b>Date:</b> ", format(SampleDate, "%Y-%m-%d"),
                                                   "<br><b>Metric:</b> Forecasted E. coli",
                                                   "<br><b>Log10 Value (Mean):</b> ", round(ecoli_fcst, 3),
                                                   "<br><b>Min:</b> ", round(ecoli_fcst_min, 3),
                                                   "<br><b>Max:</b> ", round(ecoli_fcst_max, 3),
                                                   "<br><b>N:</b> ", ecoli_fcst_n)), size = 3, shape = 16, alpha = 0.8) +
      scale_color_manual(values = color_mapping, name = "Data Source") + 
      theme_minimal() + 
      labs(x = "Date", y = "Log10 Level") + 
      theme(
        text = element_text(size = 12),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 0)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        autosize = TRUE,
        margin = list(l = 45, r = 120, t = 20, b = 40),
        legend = list(orientation = "v", x = 1.02, xanchor = "left", y = 1, yanchor = "top")
      ) %>%
      config(responsive = TRUE)
  })
  
  perf_roc_data <- reactive({
    req(input$perf_comp_site, input$perf_waterbody_filter, input$perf_ecoli_thresh, input$ecoli_limit)
    
    if(!exists("matched_comparison_df") || nrow(matched_comparison_df) == 0 || 
       (length(input$perf_comp_site) > 0 && "None" %in% input$perf_comp_site)) {
      return(NULL)
    }
    
    valid_sites <- filtered_site_ids()
    df <- matched_comparison_df %>% filter(id %in% valid_sites)
    
    if(input$perf_waterbody_filter != "Any") df <- df %>% filter(waterbody_type == input$perf_waterbody_filter)
    
    if(!is.null(input$perf_comp_site) && 
       !any(c("All", "Overall Average") %in% input$perf_comp_site) && 
       length(input$perf_comp_site) > 0) {
      df <- df %>% filter(id %in% input$perf_comp_site)
    }
    
    df <- df %>%
      filter(!is.na(minet_ecoli_log) & !is.na(fcst_ecoli_log)) %>%
      mutate(
        obs_ecoli_raw = 10^(minet_ecoli_log) - 0.001,
        fcst_ecoli_raw = 10^(fcst_ecoli_log) - 0.001
      ) %>%
      filter(obs_ecoli_raw >= input$ecoli_limit[1] & obs_ecoli_raw <= input$ecoli_limit[2])
    
    if(nrow(df) == 0) return(NULL)
    
    ecoli_thresh <- input$perf_ecoli_thresh
    df <- df %>%
      mutate(
        obs_exceed = ifelse(obs_ecoli_raw >= ecoli_thresh, 1, 0),
        fcst_score = fcst_ecoli_log
      )
    
    return(df)
  })
  
  output$perf_auc_plot <- renderPlotly({
    df <- perf_roc_data()
    if(is.null(df) || nrow(df) < 3) {
      return(plot_ly() %>% layout(title = "Insufficient Data for ROC Analysis"))
    }
    
    n_pos <- sum(df$obs_exceed == 1)
    n_neg <- sum(df$obs_exceed == 0)
    
    if(n_pos == 0 || n_neg == 0) {
      return(plot_ly() %>% layout(title = paste0("ROC curve requires both exceedance and non-exceedance cases.\n(Current selection has ", n_pos, " exceedances, ", n_neg, " non-exceedances)")))
    }
    
    roc_obj <- tryCatch({
      pROC::roc(response = df$obs_exceed, predictor = df$fcst_score, quiet = TRUE)
    }, error = function(e) NULL)
    
    if(is.null(roc_obj)) {
      return(plot_ly() %>% layout(title = "Could not calculate ROC curve"))
    }
    
    roc_df <- data.frame(
      FPR = 1 - roc_obj$specificities,
      TPR = roc_obj$sensitivities,
      Threshold = roc_obj$thresholds
    )
    roc_df <- roc_df %>% arrange(FPR, TPR)
    
    p <- ggplot(roc_df, aes(x = FPR, y = TPR)) +
      geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "grey50") +
      geom_line(color = "#e74c3c", linewidth = 1.2) +
      geom_point(aes(text = paste0("<b>Sensitivity (TPR):</b> ", round(TPR, 3),
                                   "<br><b>1 - Specificity (FPR):</b> ", round(FPR, 3),
                                   "<br><b>Forecast Cutoff (Log10):</b> ", round(Threshold, 3))),
                 color = "#e74c3c", size = 2, alpha = 0.6) +
      labs(x = "1 - Specificity (False Positive Rate)",
           y = "Sensitivity (True Positive Rate)",
           title = paste0("ROC Curve for Exceedance Detection (\u2265 ", input$perf_ecoli_thresh, " MPN)")) +
      theme_minimal() +
      theme(text = element_text(size = 13))
    
    ggplotly(p, tooltip = "text") %>%
      layout(autosize = TRUE) %>%
      config(responsive = TRUE)
  })
  
  output$perf_auc_stats <- renderTable({
    df <- perf_roc_data()
    if(is.null(df) || nrow(df) < 3) return(NULL)
    
    n_pos <- sum(df$obs_exceed == 1)
    n_neg <- sum(df$obs_exceed == 0)
    n_total <- nrow(df)
    
    if(n_pos == 0 || n_neg == 0) {
      return(data.frame(
        Metric = c("Total Observations", "Observed Exceedances", "Observed Non-exceedances", "Status"),
        Value = c(as.character(n_total), as.character(n_pos), as.character(n_neg), "Requires both exceedance & non-exceedance cases for ROC"),
        stringsAsFactors = FALSE
      ))
    }
    
    roc_obj <- tryCatch({
      pROC::roc(response = df$obs_exceed, predictor = df$fcst_score, quiet = TRUE, ci = TRUE)
    }, error = function(e) NULL)
    
    if(is.null(roc_obj)) return(NULL)
    
    auc_val <- as.numeric(pROC::auc(roc_obj))
    ci_vals <- pROC::ci.auc(roc_obj)
    
    wt <- suppressWarnings(wilcox.test(fcst_score ~ obs_exceed, data = df))
    p_val_str <- format.pval(wt$p.value, digits = 3)
    
    thresh_log <- log10(input$perf_ecoli_thresh + 0.001)
    fcst_exceed <- ifelse(df$fcst_score >= thresh_log, 1, 0)
    
    tp <- sum(df$obs_exceed == 1 & fcst_exceed == 1)
    fp <- sum(df$obs_exceed == 0 & fcst_exceed == 1)
    tn <- sum(df$obs_exceed == 0 & fcst_exceed == 0)
    fn <- sum(df$obs_exceed == 1 & fcst_exceed == 0)
    
    sens <- ifelse((tp + fn) > 0, round(tp / (tp + fn) * 100, 1), NA)
    spec <- ifelse((tn + fp) > 0, round(tn / (tn + fp) * 100, 1), NA)
    acc  <- round((tp + tn) / n_total * 100, 1)
    
    data.frame(
      Metric = c(
        "Exceedance threshold",
        "AUROC (95% CI)",
        "p-value (AUC = 0.5)",
        "Sample size (n)",
        "Exceedances (n, %)",
        "Non-exceedances (n, %)",
        "Sensitivity (%)",
        "Specificity (%)",
        "Accuracy (%)"
      ),
      Value = c(
        paste0("\u2265 ", input$perf_ecoli_thresh, " MPN/100 mL"),
        paste0(round(auc_val, 3), " (", round(ci_vals[1], 3), "\u2013", round(ci_vals[3], 3), ")"),
        p_val_str,
        as.character(n_total),
        paste0(n_pos, " (", round(n_pos / n_total * 100, 1), "%)"),
        paste0(n_neg, " (", round(n_neg / n_total * 100, 1), "%)"),
        ifelse(is.na(sens), "N/A", as.character(sens)),
        ifelse(is.na(spec), "N/A", as.character(spec)),
        as.character(acc)
      ),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, width = "100%", colnames = TRUE)
  
}

shinyApp(ui = ui, server = server)