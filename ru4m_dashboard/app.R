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

# --- FILTER CURRENT FORECAST TO STRICTLY 7 DAYS FROM INDEX DATE (EXCLUDE HISTORIC FORECASTS) ---
current_fcst <- current_fcst %>%
  filter(as.Date(SampleDate) >= index_date & as.Date(SampleDate) <= (index_date + 6))

# Populate long/lat coordinates for map calculations
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
# ui <- navbarPage(
#   title = "Michigan Recreational Water Fecal Contamination",
#   theme = bs_theme(version = 5, bootswatch = "sandstone"),
#   collapsible = TRUE,
#   
#   header = tags$head(
#     tags$style(HTML("
#       .navbar-nav > li > a {
#         font-size: 18px !important;
#         font-weight: 600;
#       }
#       .navbar-nav > li {
#         margin-right: 25px; /* Adds space between tab choices */
#       }
#       .navbar-brand {
#         font-size: 20px !important;
#         font-weight: bold;
#       }
#     "))
#   ),
#   

ui <- navbarPage(
  title = "Michigan Recreational Water Fecal Contamination",
  theme = bs_theme(version = 5, bootswatch = "sandstone"),
  collapsible = TRUE,
  
  header = tags$head(
    # 1. Force Bootstrap 5 dark theme context on the navbar element via JS
    tags$script(HTML("
      $(document).ready(function() {
        $('.navbar').attr('data-bs-theme', 'dark');
      });
    ")),
    
    tags$style(HTML("
      /* Dark navbar container background */
      .navbar {
        background-color: #212529 !important;
      }
      
      /* Style links and brand text */
      .navbar-brand, 
      .navbar-nav .nav-link {
        color: rgba(255, 255, 255, 0.85) !important;
      }
      .navbar-brand:hover, 
      .navbar-nav .nav-link:hover,
      .navbar-nav .nav-link.active {
        color: #ffffff !important;
      }
      .navbar-nav > li > a {
        font-size: 18px !important;
        font-weight: 600;
      }
      .navbar-nav > li {
        margin-right: 25px;
      }
      .navbar-brand {
        font-size: 20px !important;
        font-weight: bold;
      }
      
      /* Match 3-dash toggler icon & border color directly to link text */
      .navbar-toggler-icon {
        filter: invert(1) grayscale(100%) brightness(200%) !important;
      }
      .navbar-toggler {
        border-color: rgba(255, 255, 255, 0.85) !important;
        color: rgba(255, 255, 255, 0.85) !important;
      }
      
      /* Match Light/Dark Mode switch/symbol color */
      .bslib-theme-switch,
      .bslib-theme-switch *,
      .theme-switch-toggle,
      .navbar .theme-switch {
        color: rgba(255, 255, 255, 0.85) !important;
        fill: rgba(255, 255, 255, 0.85) !important;
      }
    "))
  ),
  
  
  # --- TAB 2: Site Sample Pair & Agreement Map ---
  tabPanel("Site agreement map",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               numericInput("tab2_map_ecoli_thresh", "Colilert Threshold (MPN):",
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
               ),
               hr(),
               h5("Site Summary", style = "font-weight: bold; margin-top: 15px;"),
               tableOutput("site_agreement_summary_table")
             )
           )
  ),
  
  # --- TAB 3: Historic Maps ---
  # --- TAB 3: Historic Maps & Summary ---
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
               h4("Daily Sampling"),
               fluidRow(
                 column(6, h5("Daily E. coli (MPN)", align = "center"), plotlyOutput("hist_ecoli_map", width = "100%", height = "350px")),
                 column(6, h5("Daily Bactiquick (ERU)", align = "center"), plotlyOutput("hist_bacti_map", width = "100%", height = "350px"))
               ),
               hr(),
               h4("Available Sites Summary"),
               div(style = "overflow-x: auto;", tableOutput("tab3_site_summary_table"))
             )
           )
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
               
               sliderInput("ecoli_limit", "Colilert Level Range (MPN):",
                           min = 0, max = max_ecoli_val, value = c(0, max_ecoli_val)),
               
               sliderInput("bacti_limit", "Bactiquick Level Range (ERU):",
                           min = 0, max = max_bacti_val, value = c(0, max_bacti_val)),
               
               selectizeInput("comp_site", "Select Specific Site(s):", 
                              choices = NULL, multiple = TRUE,options = list(plugins = list("remove_button"))),
               actionLink("clear_sites", "Clear All Selected Sites", style = "color: #e74c3c;"),
               br(), br(),
               
               numericInput("ecoli_thresh_val", "Colilert Threshold (MPN):",
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
                          h4("E. Coli Colilert 18 and Bactiquick Relationship"),
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
                          h4("Assay Result Comparison"),
                          plotlyOutput("compare_plot", width = "100%", height = "40vh")
                 ),
                 tabPanel("Raw Assay Results",
                          h4("E. Coli Colilert 18 and Bactiquick Relationship"),
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
                          h4("Assay Result Comparison"),
                          plotlyOutput("compare_plot_raw", width = "100%", height = "40vh")
                 )
               )
             )
           )
  ),
  # --- TAB 4: Forecast Dashboard & Trends ---
  tabPanel("Forecast",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               sliderInput("map_date", "Select Date for Map:", 
                           min = index_date, 
                           max = index_date + 6,
                           value = index_date, timeFormat = "%Y-%m-%d", 
                           animate = animationOptions(interval = 2500, loop = TRUE)),
               selectizeInput("region_select", "Select Region(s):", choices = NULL, multiple = TRUE,
                              options = list(plugins = list("remove_button"))),
               selectizeInput("site", "Search Site (Trend Chart):", choices = NULL, multiple = TRUE,
                              options = list(plugins = list("remove_button"))),
               actionLink("regional_clear_sites", "Clear All Selected Sites", style = "color: #e74c3c;"),
               br(), br(),
               radioButtons("trend_metric", "Select Metric (Applies to Maps & Plot):", 
                            choices = c("Forecasted E. coli Level" = "Forecasted_Ecoli_Level", 
                                        "Probability of Exceedance" = "Probability_of_Exceedance"),
                            selected = "Forecasted_Ecoli_Level"),
               helpText("This tab displays regional mean forecasting E. coli levels and exceedance probabilities for Michigan Emergency Preparedness Regions. Select one or multiple regions using the dropdown.")
             ),
             mainPanel(
               width = 9,
               h4(textOutput("map_title")),
               fluidRow(
                 column(6, h5("Daily Region Forecast", align = "center"), plotOutput("map_daily", height = "300px")),
                 column(6, h5("Daily Site Forecast", align = "center"), plotlyOutput("site_dots_map", width = "100%", height = "300px"))
               ),
               fluidRow(
                 column(6, h5(textOutput("map_7day_title"), align = "center"), plotOutput("map_click_2_output", height = "300px")),
                 column(6, h5(textOutput("trend_title"), align = "center"), plotlyOutput("timeseries_plot", height = "300px"))
               )
             )
           )
  ),
  
  # --- TAB 5: Forecast Performance ---
  tabPanel("Forecast Performance",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               selectInput("perf_waterbody_filter", "Waterbody Type:",
                           choices = c("Any", "Inland Lake", "Great Lake", "River"),
                           selected = "Any"),
               
               selectizeInput("perf_comp_site", "Select Specific Site(s):", choices = NULL, multiple = TRUE,
                              options = list(plugins = list("remove_button"))),
               actionLink("perf_clear_sites", "Clear All Selected Sites", style = "color: #e74c3c;"),
               br(), br(),
               
               numericInput("perf_ecoli_thresh", "Colilert Threshold (MPN):",
                            value = 300, min = 1, max = 10000, step = 1),
               
               radioButtons("perf_thresh_type", "Threshold Selection Mode:",
                            choices = c("Optimal Threshold (Youden's J)" = "optimal",
                                        "Input Colilert Threshold" = "input"),
                            selected = "input"),
               
               helpText("Evaluate forecast accuracy against observed E. coli levels. Set a customizable exceedance threshold (MPN) to compute AUROC, confidence intervals, and classification performance.")
             ),
             mainPanel(
               width = 9,
               h4("Forecast and Observed E. coli Over Time"),
               plotlyOutput("perf_compare_plot", width = "100%", height = "40vh"),
               hr(),
               h4("Prediction Performance"),
               fluidRow(
                 column(7,
                        div(
                          style = "width: 100%; aspect-ratio: 1.25 / 1; height: auto;",
                          plotlyOutput("perf_auc_plot", width = "100%", height = "100%")
                        )
                 ),
                 column(5,
                        h5("Classification Statistics"),
                        tableOutput("perf_auc_stats")
                 )
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
                                        forecasts from ",tags$a(href="https://open-meteo.com/","Open Meteo"),"."),
                                div(
                                  style = "display: flex; justify-content: center; align-items: center;",
                                  img(src = "ru4m_flow.png", height = "180px", width = "auto")
                                )
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
                    ),
                    br()
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
  
  # Initialize Region Choices for Dropdown in Tab 4
  region_choices_vec <- sort(unique(as.character(mi_regions_sf$Region)))
  updateSelectizeInput(session, "region_select", 
                       choices = c("All Regions" = "All", setNames(region_choices_vec, paste("Region", region_choices_vec))), 
                       selected = "All", 
                       server = TRUE)
  
  # --- MUTUALLY EXCLUSIVE SELECTION OBSERVERS ("All" vs Specific Items) ---
  observeEvent(input$region_select, {
    sel <- input$region_select
    if (length(sel) > 1 && "All" %in% sel) {
      if (tail(sel, 1) == "All") {
        updateSelectizeInput(session, "region_select", selected = "All")
      } else {
        updateSelectizeInput(session, "region_select", selected = setdiff(sel, "All"))
      }
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$site, {
    sel <- input$site
    if (length(sel) > 1 && "All" %in% sel) {
      if (tail(sel, 1) == "All") {
        updateSelectizeInput(session, "site", selected = "All")
      } else {
        updateSelectizeInput(session, "site", selected = setdiff(sel, "All"))
      }
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$comp_site, {
    sel <- input$comp_site
    if (length(sel) > 1 && "All" %in% sel) {
      if (tail(sel, 1) == "All") {
        updateSelectizeInput(session, "comp_site", selected = "All")
      } else {
        updateSelectizeInput(session, "comp_site", selected = setdiff(sel, "All"))
      }
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$perf_comp_site, {
    sel <- input$perf_comp_site
    if (length(sel) > 1 && "All" %in% sel) {
      if (tail(sel, 1) == "All") {
        updateSelectizeInput(session, "perf_comp_site", selected = "All")
      } else {
        updateSelectizeInput(session, "perf_comp_site", selected = setdiff(sel, "All"))
      }
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
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
  
  output$site_agreement_summary_table <- renderTable({
    req(input$tab2_map_ecoli_thresh, input$tab2_map_bacti_thresh)
    
    ecoli_log_thresh <- log10(input$tab2_map_ecoli_thresh + 0.001)
    bacti_log_thresh <- log10(input$tab2_map_bacti_thresh + 0.001)
    
    site_stats <- minet_data %>%
      group_by(id) %>%
      summarise(
        has_ecoli = any(!is.na(ecoli_log)),
        has_bacti = any(!is.na(bactiquick_log)),
        has_any = has_ecoli | has_bacti,
        has_both = has_ecoli & has_bacti,
        has_ecoli_only = has_ecoli & !has_bacti,
        has_ecoli_exceedance = any(ecoli_log >= ecoli_log_thresh, na.rm = TRUE),
        has_bact_exceedance = any(bactiquick_log >= bacti_log_thresh, na.rm = TRUE),
        pct_agree = ifelse(
          sum(!is.na(ecoli_log) & !is.na(bactiquick_log)) > 0,
          mean((ecoli_log[!is.na(ecoli_log) & !is.na(bactiquick_log)] >= ecoli_log_thresh) == 
                 (bactiquick_log[!is.na(ecoli_log) & !is.na(bactiquick_log)] >= bacti_log_thresh)) * 100,
          NA_real_
        ),
        .groups = "drop"
      )
    
    # Total sites calculated dynamically from minet_data for any site with data
    total_sites <- sum(site_stats$has_any, na.rm = TRUE)
    
    n_both <- sum(site_stats$has_both, na.rm = TRUE)
    n_ecoli_exc <- sum(site_stats$has_ecoli_exceedance, na.rm = TRUE)
    n_bact_exc <- sum(site_stats$has_bact_exceedance, na.rm = TRUE)
    n_ecoli_only <- sum(site_stats$has_ecoli_only, na.rm = TRUE)
    
    both_sites <- site_stats %>% filter(has_both)
    n_0_24   <- sum(both_sites$pct_agree >= 0  & both_sites$pct_agree < 25, na.rm = TRUE)
    n_25_49  <- sum(both_sites$pct_agree >= 25 & both_sites$pct_agree < 50, na.rm = TRUE)
    n_50_74  <- sum(both_sites$pct_agree >= 50 & both_sites$pct_agree < 75, na.rm = TRUE)
    n_75_100 <- sum(both_sites$pct_agree >= 75 & both_sites$pct_agree <= 100, na.rm = TRUE)
    
    fmt_pct <- function(cnt, total) {
      if (total == 0) return(sprintf("%d (0.0%%)", cnt))
      sprintf("%d (%.1f%%)", cnt, (cnt / total) * 100)
    }
    
    data.frame(
      Category = c(
        "Total Number of Sites",
        "Sites with Exceedances (Colilert)",
        "Sites with Exceedances (Bactiquick)",
        "Sites with Only Colilert Available",
        "Sites with Both Data Available",
        "0-24% Agreement",
        "25-49% Agreement",
        "50-74% Agreement",
        "75-100% Agreement"
      ),
      Count = c(
        as.character(total_sites),
        as.character(n_ecoli_exc),
        as.character(n_bact_exc),
        as.character(n_ecoli_only),
        as.character(n_both),
        fmt_pct(n_0_24, n_both),
        fmt_pct(n_25_49, n_both),
        fmt_pct(n_50_74, n_both),
        fmt_pct(n_75_100, n_both)
      ),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, colnames = FALSE)
  
  # Reactive evaluation for active selected region(s)
  active_regions <- reactive({
    sel <- input$region_select
    if (is.null(sel) || "All" %in% sel || length(sel) == 0) {
      return("All")
    }
    return(as.character(sel))
  })
  
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
                   size = 2, alpha = 0.9) +
        # --- REPLACED COLOR SCALE HERE ---
        scale_color_gradient2(
          low = "darkgreen",       # Muted green (safe/low)
          mid = "yellow",       # Neutral yellow (midpoint threshold)
          high = "red",      # Red (high/exceedance)
          midpoint = log10(300), # Center gradient at log10(300) ~ 2.477
          limits = global_hist_ecoli_lims,
          name = "Log10(E.coli)"
        ) +
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
                   size = 2, alpha = 0.9) +
        scale_color_viridis_c(option = "mako", direction = -1, limits = global_hist_bacti_lims, name = "Log10(Bacti)") +
        theme_void() + theme(legend.position = "right")
    )
    
    ggplotly(p, tooltip = "text") %>% 
      style(hoverinfo = "none", traces = c(1, 2)) %>%
      layout(autosize = TRUE, margin = list(l=0, r=0, b=0, t=0)) %>%
      config(responsive = TRUE)
  })
  
  output$tab3_site_summary_table <- renderTable({
    req(input$tab2_date)
    if (input$tab2_date == "No Data Available") return(NULL)
    
    selected_date <- as.Date(input$tab2_date)
    start_date <- selected_date - 29  # Exactly 30 days inclusive
    
    # 1. Calculate past 30-day summary metrics per site
    monthly_summary <- minet_data %>%
      filter(as.Date(SampleDate) >= start_date & as.Date(SampleDate) <= selected_date) %>%
      group_by(id) %>%
      summarise(
        avg_colilert_month = ifelse(all(is.na(ecoli_log)), NA_real_, mean(10^(ecoli_log) - 0.001, na.rm = TRUE)),
        avg_bacti_month    = ifelse(all(is.na(bactiquick_log)), NA_real_, mean(10^(bactiquick_log) - 0.001, na.rm = TRUE)),
        colilert_avail_days_month = sum(!is.na(ecoli_log) & as.Date(SampleDate) >= start_date & as.Date(SampleDate) <= selected_date),
        bacti_avail_days_month    = sum(!is.na(bactiquick_log) & as.Date(SampleDate) >= start_date & as.Date(SampleDate) <= selected_date),
        .groups = "drop"
      )
    
    # 2. Get daily measurements for selected date & merge monthly summary
    daily_data <- minet_data %>%
      filter(as.Date(SampleDate) == selected_date) %>%
      left_join(geo_info %>% select(id, BeachName), by = "id") %>%
      left_join(monthly_summary, by = "id") %>%
      filter(!is.na(ecoli_log) | !is.na(bactiquick_log)) %>%
      mutate(
        colilert_level = ifelse(is.na(ecoli_log), NA_real_, 10^(ecoli_log) - 0.001),
        bactiquick_level = ifelse(is.na(bactiquick_log), NA_real_, 10^(bactiquick_log) - 0.001)
      ) %>%
      select(
        Site = BeachName,
        `Site ID` = id,
        `Colilert Level (MPN)` = colilert_level,
        `Bactiquick Level (ERU)` = bactiquick_level,
        `Past 30-Day Colilert Avg` = avg_colilert_month,
        `Past 30-Day Bactiquick Avg` = avg_bacti_month,
        `Colilert Avail. Days (Past 30d)` = colilert_avail_days_month,
        `Bactiquick Avail. Days (Past 30d)` = bacti_avail_days_month
      )
    
    if (nrow(daily_data) == 0) return(NULL)
    
    # Format numbers for clean presentation
    daily_data %>%
      mutate(
        `Colilert Level (MPN)` = ifelse(is.na(`Colilert Level (MPN)`), "N/A", sprintf("%.1f", `Colilert Level (MPN)`)),
        `Bactiquick Level (ERU)` = ifelse(is.na(`Bactiquick Level (ERU)`), "N/A", sprintf("%.1f", `Bactiquick Level (ERU)`)),
        `Past 30-Day Colilert Avg` = ifelse(is.na(`Past 30-Day Colilert Avg`), "N/A", sprintf("%.1f", `Past 30-Day Colilert Avg`)),
        `Past 30-Day Bactiquick Avg` = ifelse(is.na(`Past 30-Day Bactiquick Avg`), "N/A", sprintf("%.1f", `Past 30-Day Bactiquick Avg`)),
        `Colilert Avail. Days (Past 30d)` = as.character(`Colilert Avail. Days (Past 30d)`),
        `Bactiquick Avail. Days (Past 30d)` = as.character(`Bactiquick Avail. Days (Past 30d)`)
      )
  }, striped = TRUE, bordered = TRUE, hover = TRUE, align = "c")
  
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
  
  # --- FORECAST DASHBOARD & REGIONAL LOGIC ---
  output$map_title <- renderText({
    req(input$map_date)
    paste("Data Snapshot for:", format(as.Date(input$map_date), "%B %d, %Y"))
  })
  
  output$map_7day_title <- renderText({
    req(input$map_date)
    d1 <- as.Date(input$map_date)
    d2 <- d1 + 6
    paste0("7-Day Region Forecast (", as.numeric(format(d1, "%m")), "/", as.numeric(format(d1, "%d")), "/-", as.numeric(format(d2, "%m")), "/", as.numeric(format(d2, "%d")), "/", format(d2, "%Y"), ")")
  })
  
  output$trend_title <- renderText({
    req(input$trend_metric)
    metric_label <- ifelse(input$trend_metric == "Forecasted_Ecoli_Level", "E. coli Forecast", "Probability Forecast")
    sel_regs  <- active_regions()
    sel_sites <- input$site
    
    if (!is.null(sel_sites) && !("All" %in% sel_sites) && length(sel_sites) > 0) {
      if (length(sel_sites) == 1) {
        site_display_name <- names(site_choices)[site_choices == sel_sites]
        if (length(site_display_name) == 0) site_display_name <- sel_sites
        paste("Trend:", metric_label, "-", site_display_name)
      } else {
        paste("Trend:", metric_label, "-", length(sel_sites), "Selected Sites")
      }
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
        fill_scale <- scale_fill_gradient2(low = "darkgreen", mid = "yellow", high = "red", midpoint = log10(300), limits = global_ecoli_limits, na.value = "grey90", name = "Mean E. coli")
      } else {
        fill_scale <- scale_fill_gradient2(low = "darkgreen", mid = "yellow", high = "red", midpoint = 0.5, limits = c(0,1), na.value = "grey90", name = "Exc. Prob.")
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
      color_scale <- scale_color_gradient2(low = "darkgreen", mid = "yellow", high = "red", midpoint = log10(300), limits = global_ecoli_limits, name = "E. coli Level")
    } else {
      color_scale <- scale_color_gradient2(low = "darkgreen", mid = "yellow", high = "red", midpoint = 0.5, limits = c(0,1), name = "Exc. Prob.")
    }
    
    p <- suppressWarnings(
      ggplot() +
        geom_sf(data = mi_counties_sf, fill = "grey85", color = "grey60", linewidth = 0.2) +
        geom_sf(data = mi_regions_sf, fill = NA, color = "black", linewidth = 0.8) +
        geom_point(data = target_data, aes(x = Longitude, y = Latitude, color = metric_val,
                                           text = paste0("<b>Site:</b> ", BeachName,
                                                         "<br><b>ID:</b> ", id,
                                                         "<br><b>Date:</b> ", SampleDate,
                                                         "<br><b>Forecasted E. coli:</b> ", round(Forecasted_Ecoli_Level, 2), " MPN",
                                                         "<br><b>Prob. Exceedance:</b> ", round(Probability_of_Exceedance, 3))), size = 1.8, alpha = 0.85) +
        color_scale + theme_void() + theme(legend.position = "right", legend.title = element_text(face = "bold", size = 10), legend.text = element_text(size = 8))
    )
    
    ggplotly(p, tooltip = "text") %>% 
      style(hoverinfo = "none", traces = c(1, 2)) %>%
      layout(autosize = TRUE, margin = list(l = 0, r = 0, b = 0, t = 0)) %>%
      config(responsive = TRUE)
  })
  
  output$timeseries_plot <- renderPlotly({
    req(input$trend_metric, input$map_date)
    sel_date <- as.Date(input$map_date)
    sel_regs  <- active_regions()
    sel_sites <- input$site
    
    is_all_sites <- is.null(sel_sites) || ("All" %in% sel_sites) || length(sel_sites) == 0
    
    if (is_all_sites) {
      if (!("All" %in% sel_regs)) {
        base_df <- current_fcst %>% 
          inner_join(geo_info %>% select(id, Region), by = "id") %>% 
          filter(as.character(Region) %in% sel_regs)
      } else {
        base_df <- current_fcst
      }
      
      trend_df <- base_df %>% 
        mutate(
          SampleDate = as.Date(SampleDate),
          raw_metric = as.numeric(as.character(.data[[input$trend_metric]]))
        )
      
      if (input$trend_metric == "Forecasted_Ecoli_Level") {
        trend_df <- trend_df %>% mutate(metric_val = log10(raw_metric + 0.001))
        y_label <- "Log10(E. coli Level + 0.001)"
        val_unit <- " MPN"
      } else {
        trend_df <- trend_df %>% mutate(metric_val = raw_metric)
        y_label <- "Probability of Exceedance"
        val_unit <- ""
      }
      
      summary_df <- trend_df %>%
        group_by(SampleDate) %>%
        summarise(
          mean_val = mean(metric_val, na.rm = TRUE),
          min_val  = safe_min(metric_val),
          max_val  = safe_max(metric_val),
          n_sites  = sum(!is.na(metric_val)),
          .groups  = "drop"
        )
      
      p <- ggplot(summary_df, aes(x = SampleDate)) +
        geom_ribbon(aes(ymin = min_val, ymax = max_val, fill = "Min-Max Range"), alpha = 0.2) +
        geom_line(aes(y = mean_val, color = "Mean Trend"), linewidth = 1) +
        geom_point(aes(y = mean_val, color = "Mean Trend",
                       text = paste0("<b>Date:</b> ", format(SampleDate, "%Y-%m-%d"),
                                     "<br><b>Mean:</b> ", round(mean_val, 3), val_unit,
                                     "<br><b>Min:</b> ", round(min_val, 3), val_unit,
                                     "<br><b>Max:</b> ", round(max_val, 3), val_unit,
                                     "<br><b>Sites Count:</b> ", n_sites)), size = 2) +
        geom_vline(xintercept = sel_date, color = "#e74c3c", linetype = "dashed", linewidth = 0.8) +
        scale_color_manual(name = "", values = c("Mean Trend" = "#2c3e50")) +
        scale_fill_manual(name = "", values = c("Min-Max Range" = "#3498db")) +
        theme_minimal() +
        labs(x = "Date", y = y_label) +
        theme(text = element_text(size = 11))
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          autosize = TRUE,
          margin = list(l = 40, r = 20, t = 20, b = 40),
          showlegend = FALSE
        ) %>%
        config(responsive = TRUE)
      
    } else {
      base_df <- current_fcst %>% 
        filter(as.character(id) %in% sel_sites) %>%
        left_join(geo_info %>% select(id, BeachName), by = "id") %>%
        mutate(
          SampleDate = as.Date(SampleDate),
          raw_metric = as.numeric(as.character(.data[[input$trend_metric]]))
        )
      
      if (input$trend_metric == "Forecasted_Ecoli_Level") {
        base_df <- base_df %>% mutate(metric_val = log10(raw_metric + 0.001))
        y_label <- "Log10(E. coli Level + 0.001)"
        val_unit <- " MPN"
      } else {
        base_df <- base_df %>% mutate(metric_val = raw_metric)
        y_label <- "Probability of Exceedance"
        val_unit <- ""
      }
      
      p <- ggplot(base_df, aes(x = SampleDate, y = metric_val, color = BeachName, group = BeachName)) +
        geom_line(linewidth = 1) +
        geom_point(aes(text = paste0("<b>Site:</b> ", BeachName,
                                     "<br><b>ID:</b> ", id,
                                     "<br><b>Date:</b> ", format(SampleDate, "%Y-%m-%d"),
                                     "<br><b>Predicted Value:</b> ", round(metric_val, 3), val_unit)), size = 2) +
        geom_vline(xintercept = sel_date, color = "grey40", linetype = "dashed", linewidth = 0.8) +
        theme_minimal() +
        labs(x = "Date", y = y_label, color = "Site Name") +
        theme(text = element_text(size = 11))
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          autosize = TRUE,
          margin = list(l = 40, r = 20, t = 20, b = 40),
          showlegend = TRUE,
          legend = list(orientation = "v", x = 1.02, xanchor = "left", y = 1)
        ) %>%
        config(responsive = TRUE)
    }
  })
  
  # --- FORECAST PERFORMANCE LOGIC (TAB 5) ---
  perf_plot_data <- reactive({
    req(input$perf_comp_site, input$perf_waterbody_filter)
    
    if(!exists("matched_comparison_df") || nrow(matched_comparison_df) == 0 || 
       (length(input$perf_comp_site) > 0 && "None" %in% input$perf_comp_site)) {
      return(NULL)
    }
    
    valid_sites <- filtered_site_ids()
    plot_df <- matched_comparison_df %>% filter(id %in% valid_sites)
    
    if(input$perf_waterbody_filter != "Any") {
      plot_df <- plot_df %>% filter(waterbody_type == input$perf_waterbody_filter)
    }
    
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
      return(plot_ly() %>% layout(title = "No Data Available for Filters"))
    }
    
    thresh_val <- log10(input$perf_ecoli_thresh + 0.001)
    
    obs_df  <- plot_df %>% filter(!is.na(ecoli_actual))
    fcst_df <- plot_df %>% filter(!is.na(ecoli_fcst))
    
    p <- ggplot(plot_df, aes(x = SampleDate)) +
      geom_hline(yintercept = thresh_val, linetype = "dashed", color = "red", linewidth = 0.8, alpha = 0.7) +
      geom_ribbon(data = obs_df, aes(ymin = ecoli_actual_min, ymax = ecoli_actual_max, fill = "Observed E. coli"), alpha = 0.15) +
      geom_line(data = obs_df, aes(y = ecoli_actual, color = "Observed E. coli"), linewidth = 1.2, linetype = "solid") +
      geom_point(data = obs_df, aes(y = ecoli_actual, color = "Observed E. coli",
                                    text = paste0("<b>Date:</b> ", format(SampleDate, "%Y-%m-%d"),
                                                  "<br><b>Observed Log10:</b> ", round(ecoli_actual, 3),
                                                  "<br><b>Min:</b> ", round(ecoli_actual_min, 3),
                                                  "<br><b>Max:</b> ", round(ecoli_actual_max, 3),
                                                  "<br><b>N:</b> ", ecoli_actual_n)), size = 2.5) +
      geom_ribbon(data = fcst_df, aes(ymin = ecoli_fcst_min, ymax = ecoli_fcst_max, fill = "Forecasted E. coli"), alpha = 0.15) +
      geom_line(data = fcst_df, aes(y = ecoli_fcst, color = "Forecasted E. coli"), linewidth = 1.2, linetype = "dashed") +
      geom_point(data = fcst_df, aes(y = ecoli_fcst, color = "Forecasted E. coli",
                                     text = paste0("<b>Date:</b> ", format(SampleDate, "%Y-%m-%d"),
                                                   "<br><b>Forecasted Log10:</b> ", round(ecoli_fcst, 3),
                                                   "<br><b>Min:</b> ", round(ecoli_fcst_min, 3),
                                                   "<br><b>Max:</b> ", round(ecoli_fcst_max, 3),
                                                   "<br><b>N:</b> ", ecoli_fcst_n)), size = 2.5) +
      scale_color_manual(values = c("Observed E. coli" = "#2c3e50", "Forecasted E. coli" = "#e74c3c"), name = "Metric") +
      scale_fill_manual(values = c("Observed E. coli" = "#2c3e50", "Forecasted E. coli" = "#e74c3c"), name = "Metric") +
      guides(fill = "none") +
      theme_minimal() +
      labs(x = "Date", y = "Log10 E. coli (MPN)") +
      theme(text = element_text(size = 12))
    
    p_plotly <- ggplotly(p, tooltip = "text")
    
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
  
  # --- ROC & AUROC ANALYSIS LOGIC ---
  perf_roc_data <- reactive({
    req(input$perf_comp_site, input$perf_waterbody_filter, input$perf_ecoli_thresh)
    
    if(!exists("matched_comparison_df") || nrow(matched_comparison_df) == 0 || 
       (length(input$perf_comp_site) > 0 && "None" %in% input$perf_comp_site)) {
      return(NULL)
    }
    
    valid_sites <- filtered_site_ids()
    plot_df <- matched_comparison_df %>% filter(id %in% valid_sites)
    
    if(input$perf_waterbody_filter != "Any") {
      plot_df <- plot_df %>% filter(waterbody_type == input$perf_waterbody_filter)
    }
    
    if(!is.null(input$perf_comp_site) && 
       !any(c("All", "Overall Average") %in% input$perf_comp_site) && 
       length(input$perf_comp_site) > 0) {
      plot_df <- plot_df %>% filter(id %in% input$perf_comp_site)
    }
    
    plot_df <- plot_df %>% filter(!is.na(minet_ecoli_log) & !is.na(fcst_ecoli_log))
    
    if(nrow(plot_df) < 5) return(NULL)
    
    thresh_log <- log10(input$perf_ecoli_thresh + 0.001)
    
    plot_df <- plot_df %>%
      mutate(actual_exceed = as.numeric(minet_ecoli_log >= thresh_log))
    
    if(length(unique(plot_df$actual_exceed)) < 2) return(NULL)
    
    roc_obj <- tryCatch({
      pROC::roc(response = plot_df$actual_exceed, 
                predictor = plot_df$fcst_ecoli_log, 
                quiet = TRUE, 
                ci = TRUE)
    }, error = function(e) NULL)
    
    if(is.null(roc_obj)) return(NULL)
    
    list(
      roc_obj = roc_obj,
      df = plot_df,
      thresh_log = thresh_log
    )
  })
  
  output$perf_auc_plot <- renderPlotly({
    roc_res <- perf_roc_data()
    if(is.null(roc_res)) {
      return(plot_ly() %>% layout(
        title = "Insufficient data or no exceedances detected for ROC analysis",
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE)
      ))
    }
    
    roc_obj <- roc_res$roc_obj
    auc_val <- as.numeric(pROC::auc(roc_obj))
    ci_val  <- pROC::ci.auc(roc_obj)
    
    roc_df <- data.frame(
      FPR = 1 - roc_obj$specificities,
      TPR = roc_obj$sensitivities,
      Threshold = roc_obj$thresholds
    ) %>% arrange(FPR, TPR)
    
    selected_thresh <- if (input$perf_thresh_type == "optimal") {
      coords_best <- pROC::coords(roc_obj, "best", ret = "threshold")
      if (is.matrix(coords_best) || is.data.frame(coords_best)) coords_best <- coords_best[1, 1]
      as.numeric(coords_best)
    } else {
      roc_res$thresh_log
    }
    
    opt_coord <- pROC::coords(roc_obj, x = selected_thresh, input = "threshold", ret = c("threshold", "sensitivity", "specificity"))
    if (is.matrix(opt_coord) || is.data.frame(opt_coord)) opt_coord <- opt_coord[1, ]
    opt_fpr <- 1 - as.numeric(opt_coord["specificity"])
    opt_tpr <- as.numeric(opt_coord["sensitivity"])
    
    p <- ggplot(roc_df, aes(x = FPR, y = TPR)) +
      geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "grey50") +
      geom_line(color = "#00274C", linewidth = 1.2) +
      geom_point(data = data.frame(FPR = opt_fpr, TPR = opt_tpr),
                 aes(x = FPR, y = TPR,
                     text = paste0("<b>Selected Threshold Point</b>",
                                   "<br><b>FPR (1-Spec):</b> ", round(opt_fpr, 3),
                                   "<br><b>TPR (Sens):</b> ", round(opt_tpr, 3),
                                   "<br><b>Threshold (Log10):</b> ", round(selected_thresh, 3))),
                 color = "#e74c3c", size = 3.5) +
      theme_minimal() +
      labs(x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
      theme(text = element_text(size = 12))
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        autosize = TRUE,
        margin = list(l = 45, r = 20, t = 20, b = 40)
      ) %>%
      config(responsive = TRUE)
  })
  
  output$perf_auc_stats <- renderTable({
    roc_res <- perf_roc_data()
    if(is.null(roc_res)) return(NULL)
    
    roc_obj <- roc_res$roc_obj
    auc_val <- as.numeric(pROC::auc(roc_obj))
    ci_val  <- pROC::ci.auc(roc_obj)
    
    selected_thresh <- if (input$perf_thresh_type == "optimal") {
      coords_best <- pROC::coords(roc_obj, "best", ret = "threshold")
      if (is.matrix(coords_best) || is.data.frame(coords_best)) coords_best <- coords_best[1, 1]
      as.numeric(coords_best)
    } else {
      roc_res$thresh_log
    }
    
    coords_val <- pROC::coords(roc_obj, x = selected_thresh, input = "threshold",
                               ret = c("sensitivity", "specificity", "accuracy", "ppv", "npv"))
    if (is.matrix(coords_val) || is.data.frame(coords_val)) coords_val <- coords_val[1, ]
    
    sens  <- as.numeric(coords_val["sensitivity"])
    spec  <- as.numeric(coords_val["specificity"])
    acc   <- as.numeric(coords_val["accuracy"])
    ppv   <- as.numeric(coords_val["ppv"])
    npv   <- as.numeric(coords_val["npv"])
    
    data.frame(
      Metric = c("AUROC (95% CI)", "Selected Cutoff (Log10)", "Sensitivity (TPR)", "Specificity (1 - FPR)", "Accuracy", "Positive Predictive Value", "Negative Predictive Value"),
      Value  = c(
        sprintf("%.3f (%.3f - %.3f)", auc_val, ci_val[1], ci_val[3]),
        sprintf("%.3f", selected_thresh),
        sprintf("%.1f%%", sens * 100),
        sprintf("%.1f%%", spec * 100),
        sprintf("%.1f%%", acc * 100),
        sprintf("%.1f%%", ppv * 100),
        sprintf("%.1f%%", npv * 100)
      ),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, width = "100%", colnames = TRUE)
  
  # --- WEATHER VIEW LOGIC (TAB 6) ---
  output$weather_title <- renderText({
    req(input$weather_date, input$weather_var)
    var_label <- ifelse(input$weather_var == "tmean_10km_avg", "Mean Temperature (°F)", "Precipitation (mm)")
    paste("Weather Map:", var_label, "on", format(as.Date(input$weather_date), "%B %d, %Y"))
  })
  
  output$weather_trend_title <- renderText({
    req(input$weather_var, input$weather_site)
    var_label <- ifelse(input$weather_var == "tmean_10km_avg", "Mean Temperature (°F)", "Precipitation (mm)")
    sel_site <- input$weather_site
    
    site_label <- if (is.null(sel_site) || "All" %in% sel_site || length(sel_site) == 0) {
      "All Sites (Statewide Avg)"
    } else if (length(sel_site) == 1) {
      s_name <- names(site_choices)[site_choices == sel_site]
      if (length(s_name) == 0) sel_site else s_name
    } else {
      paste(length(sel_site), "Selected Sites")
    }
    paste("Weather Trend:", var_label, "-", site_label)
  })
  
  output$weather_map <- renderPlotly({
    req(input$weather_date, input$weather_var)
    sel_date <- as.Date(input$weather_date)
    var_name <- input$weather_var
    
    target_data <- weather_data %>%
      mutate(id = as.character(id), SampleDate = as.Date(SampleDate)) %>%
      filter(SampleDate == sel_date) %>%
      filter(!is.na(Latitude) & !is.na(Longitude)) %>%
      mutate(var_val = as.numeric(as.character(.data[[var_name]])))
    
    if (nrow(target_data) == 0) return(plot_ly() %>% layout(title = "No Weather Data Available for Selected Date"))
    
    lims <- if (var_name == "tmean_10km_avg") global_tmean_limits else global_ppt_limits
    lims <- unname(lims)
    
    var_label <- ifelse(var_name == "tmean_10km_avg", "Mean Temp (°F)", "Precipitation (mm)")
    
    p <- suppressWarnings(
      ggplot() +
        geom_sf(data = mi_counties_sf, fill = "grey90", color = "grey60", linewidth = 0.2) +
        geom_sf(data = mi_regions_sf, fill = NA, color = "black", linewidth = 0.8) +
        geom_point(data = target_data, aes(
          x = Longitude, y = Latitude, color = var_val,
          text = paste0("<b>Site:</b> ", BeachName,
                        "<br><b>ID:</b> ", id,
                        "<br><b>Date:</b> ", SampleDate,
                        "<br><b>", var_label, ":</b> ", round(var_val, 2))
        ), size = 1, alpha = 0.85) +
        scale_color_viridis_c(option = ifelse(var_name == "tmean_10km_avg", "inferno", "mako"), 
                              limits = lims, name = var_label) +
        theme_void() +
        theme(legend.position = "right")
    )
    
    ggplotly(p, tooltip = "text") %>%
      style(hoverinfo = "none", traces = c(1, 2)) %>%
      layout(autosize = TRUE, margin = list(l = 0, r = 0, b = 0, t = 0)) %>%
      config(responsive = TRUE)
  })
  
  output$weather_trend_plot <- renderPlot({
    req(input$weather_var, input$weather_site)
    var_name <- input$weather_var
    sel_site <- input$weather_site
    
    df <- weather_data %>%
      mutate(
        id = as.character(id),
        SampleDate = as.Date(SampleDate),
        var_val = as.numeric(as.character(.data[[var_name]]))
      )
    
    if (!is.null(sel_site) && !("All" %in% sel_site) && length(sel_site) > 0) {
      df <- df %>% filter(id %in% sel_site)
    }
    
    plot_df <- df %>%
      group_by(SampleDate) %>%
      summarise(var_val = mean(var_val, na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(SampleDate) & !is.na(var_val)) %>%
      mutate(Data_Type = ifelse(SampleDate < index_date, "Historic", "Forecast"))
    
    if (nrow(plot_df) == 0) return(NULL)
    
    var_label <- ifelse(var_name == "tmean_10km_avg", "Mean Temp (°F)", "Precipitation (mm)")
    
    hist_last <- plot_df %>% filter(Data_Type == "Historic") %>% filter(SampleDate == max(SampleDate))
    if (nrow(hist_last) > 0) {
      fcst_part <- plot_df %>% filter(Data_Type == "Forecast")
      if (nrow(fcst_part) > 0) {
        hist_last_as_fcst <- hist_last %>% mutate(Data_Type = "Forecast")
        plot_df_lines <- bind_rows(plot_df, hist_last_as_fcst) %>% arrange(SampleDate)
      } else {
        plot_df_lines <- plot_df
      }
    } else {
      plot_df_lines <- plot_df
    }
    
    ggplot() +
      geom_line(data = plot_df_lines, aes(x = SampleDate, y = var_val, color = Data_Type, group = Data_Type), linewidth = 1) +
      geom_point(data = plot_df, aes(x = SampleDate, y = var_val, color = Data_Type), size = 2.5) +
      geom_vline(xintercept = index_date, linetype = "dashed", color = "grey40", linewidth = 0.8) +
      scale_color_manual(
        values = c("Historic" = "black", "Forecast" = "red"),
        name = "Data Type"
      ) +
      labs(x = "Date", y = var_label) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "top",
        panel.grid.minor = element_blank()
      )
  })
}

shinyApp(ui = ui, server = server)