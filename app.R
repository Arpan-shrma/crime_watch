# Libraries
library(shiny)
library(shinydashboard)
library(data.table)
library(plotly)
library(leaflet)
library(DT)
library(scales)
library(viridis)
library(leaflet.extras)
library(shinycssloaders)
library(sf)
library(data.table)
library(plotly)

cat("Starting application...\n")


# At the start of your app, load Canada map data
canada_provinces <- readRDS("data/canada_provinces.rds")
# Convert to a simpler format for faster rendering
canada_provinces <- st_simplify(canada_provinces, dTolerance = 0.01)


# Set options
#options(shiny.maxRequestSize = 30*1024^2)
options(shiny.autoreload = TRUE)

# Load data
tryCatch({
  # Load and prepare crime data
  crime_data <- fread("data/processed/crime_data.csv")
  setkey(crime_data, year, level, province, violation_type)
  
  # Create selections
  regions <- unique(crime_data$region) %>% sort()
  crime_types <- unique(crime_data$violation_type) %>% sort()
  levels <- unique(crime_data$level) %>% sort()
  provinces <- crime_data[level == "Province" | level == "Territory", unique(location)] %>% sort()
  
  # Create province-city mapping
  province_city_mapping <- crime_data[level == "City", .(
    province = province,
    city = location
  )][order(province, city)] %>% unique()
  
}, error = function(e) {
  stop("Error loading data: ", e$message)
})

# UI Definition
ui <- dashboardPage(
  skin = "green",
  
  dashboardHeader(
    title = span(
      tags$i(class = "fa fa-shield-alt", style = "margin-right: 10px;"),
      "Canada Crime Watch"
    ),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Geographic Analysis", tabName = "geography", icon = icon("map-marked-alt")),
      menuItem("Crime Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Trends", tabName = "trends", icon = icon("chart-bar")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Location Comparison", tabName = "location_comparison", icon = icon("balance-scale")),
      menuItem("Data Explorer", tabName = "explorer", icon = icon("table"))
    ),
    
    # In dashboardSidebar UI
    div(
      class = "sidebar-filters",
      style = "padding: 15px; margin: 10px; background-color: rgba(204,255,204,0.9); border-radius: 5px; display: flex; flex-direction: column;",
      
      # Year Range Slider
      div(
        style = "margin-bottom: 20px;",
        tags$label("Select Years:", class = "control-label"),
        sliderInput("year_range", 
                    label = NULL,
                    min = min(crime_data$year),
                    max = max(crime_data$year),
                    value = c(min(crime_data$year), max(crime_data$year)),
                    step = 1,
                    sep = "")
      ),
      
      # Geographic Level Selection
      div(
        style = "margin-bottom: 20px;",
        tags$label("Geographic Level:", class = "control-label"),
        selectInput("geographic_level", 
                    label = NULL,
                    choices = c(
                      "All" = "All",
                      "Provincial/Territorial" = "Province",
                      "City" = "City"
                    ),
                    selected = "All")
      ),
      
      # Province Selection (Conditional)
      conditionalPanel(
        condition = "input.geographic_level == 'Province' || input.geographic_level == 'City'",
        div(
          style = "margin-bottom: 20px;",
          tags$label("Province/Territory:", class = "control-label"),
          selectizeInput("province", 
                         label = NULL,
                         choices = c("All" = "All", as.list(provinces)),
                         selected = "All",
                         multiple = TRUE)
        )
      ),
      
      # City Selection (Conditional)
      conditionalPanel(
        condition = "input.geographic_level == 'City'",
        div(
          style = "margin-bottom: 20px;",
          tags$label("City:", class = "control-label"),
          selectizeInput("city", 
                         label = NULL,
                         choices = c("All" = "All"),
                         selected = "All",
                         multiple = TRUE)
        )
      ),
      
      # Crime Type Selection
      div(
        style = "margin-bottom: 20px;",
        tags$label("Crime Category:", class = "control-label"),
        selectInput("crime_type", 
                    label = NULL,
                    choices = c("All" = "All", as.list(crime_types)),
                    selected = "All",
                    multiple = TRUE)
      ),
      
      # Reset Button - properly centered
      div(
        style = "margin-top: auto; margin-bottom: 10px; width: 100%;",
        actionButton("reset_filters", 
                     "Reset Filters",
                     width = "100%",
                     class = "btn-primary",
                     style = "display: block; margin: 0 auto; background-color: #337ab7; color: white; border: none; padding: 8px 15px; border-radius: 4px;")
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Existing styles */
        .main-sidebar, .sidebar {
        background-color: #2E8B57 !important; /* Forest green */
        color: white !important; /* Adjust text color for readability */
    }
    
    .sidebar a {
        color: #ffffff !important; /* Ensure links are visible */
    }
    
    .sidebar a:hover {
        color: #004d33 !important; /* Lighter green on hover */
        
    }
     /* Sidebar menu item active (selected) */
    .sidebar-menu > li.active > a {
        background-color: #004d33 !important; /* Darkest green for selected item */
        color: white !important; /* Ensure text is readable */
        font-weight: bold !important; /* Highlight the selected item */
        border-left: 5px solid #ffffff !important; /* Optional: white border to indicate selection */
        
    }
        
        /* Updated sidebar filter styles */
        .sidebar-filters {
            background-color: rgba(204,255,204,0.9) !important;
            padding: 15px !important;
            margin: 10px !important;
            border-radius: 5px !important;
        }
       /* Customize the tooltip for the slider */
input.slider + .irs .irs-single {
            background-color: #2E8B57 !important;
            color: white !important;
            font-weight: bold !important;
            border-radius: 5px !important;
            padding: 2px 5px !important;
            font-size: 14px !important;
            border: 2px solid #004d33 !important;
            z-index: 9999 !important;
        }

/* Customize the slider handles */
.irs-slider {
    background-color: #28a745 !important; /* Bright green handle */
    border: 2px solid #004d33 !important; /* Dark green border */
    border-radius: 50% !important; /* Round handles */
    width: 20px !important;
    height: 20px !important;
}

/* Customize the slider bar */
.irs-bar {
    background-color: #28a745 !important; /* Bright green bar */
    border: none !important;
}

/* Customize the grid ticks and labels */
.irs-grid-text {
    color: #2E8B57 !important; /* Dark green for grid text */
    font-weight: bold !important;
}
            .content-wrapper { 
          background: linear-gradient(120deg, #f5f5dc 0%, #2E8B57 100%); 
          padding: 20px; 
      }
        .box {
          border-radius: 15px; 
          box-shadow: 0 8px 16px rgba(0,0,0,0.1); 
          border: none; 
      }
        
        .control-label {
            color: #2c3e50;
            font-weight: 600;
            margin-bottom: 8px;
            display: block;
        }
        
        /* Slider specific styles */
        .irs-line {
            height: 8px !important;
        }
        
        .irs-grid-text {
            color: #218838 !important;
        }
        
        .irs-grid-pol {
            background: #218838 !important;
        }
        
        .irs-bar {
            background: #28a745 !important;
        }
        
        .irs-handle {
            border-color: #218838 !important;
        }
        
        /* Button styles */
        .btn-primary {
            background-color: #28a745 !important;
            border: none !important;
            width: 100% !important;
            padding: 8px 15px !important;
            border-radius: 50px !important;
            margin-top: 10px !important;
            position: relative !important;
            bottom: 0 !important;
        }
        
        .btn-primary:hover {
            background-color: #218838 !important;
        }
        
        /* Select input styles */
        .selectize-input {
            border-radius: 4px !important;
            border: 1px solid #ddd !important;
        }
        
        /* graph background styles */
        .box {
            border-radius: 10px !important;
            overflow: hidden !important;
            margin: 10px !important;
        }
        .box-header {
            border-radius: 10px 10px 0 0 !important;
        }
        .box.box-primary {
            border-color: #2E8B57 !important; /* green */
        }
        .box.box-primary > .box-header {
            background-color: #2E8B57 !important; /* Change header background */
        }
        .box.box-success, .box.box-primary {
        border-radius: 10px !important;
        overflow: hidden !important;
        }
    
        .box.box-success > .box-header, .box.box-primary > .box-header {
            border-radius: 10px 10px 0 0 !important;
        }
    "))
    ),
    
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("total_incidents_box", width = 3),
          valueBoxOutput("avg_rate_box", width = 3),
          valueBoxOutput("clearance_box", width = 3),
          valueBoxOutput("yoy_change_box", width = 3)
        ),
        fluidRow(
          box(
            title = "Crime Rate Trends",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("trend_plot", height = "400px") %>%
              withSpinner(type = 8, color = "#2196F3"),
            width = 8,  # Full width to center it
            HTML("<p style='text-align: center; font-size: 14px; margin-top: 10px; color: #2E8B57;'>
      This line graph illustrates the <i>trends</i> for <b>Crime rate in per 100k of population</b> 
      from <b>2014</b> to <b>2023</b>. The different colored lines <i>represent</i> distinct crime categories.
     </p>")
          ),box(
            title = "Crime Distribution",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("distribution_plot", height = "380px") %>%
              withSpinner(type = 8, color = "#2196F3"),
            width = 4,  # Full width to center it
            HTML("<p style='text-align: center; font-size: 14px; margin-top: 10px; color: #2E8B57;'>
      This pie chart highlights the <i>distribution</i> of various <b>crime types</b>, 
      showcasing their <i>proportions</i> within the <b>total reported crimes</b>.
     </p>")
          
          )
        )
      ),
      
      tabItem(
        tabName = "geography",
        fluidRow(
          box(
            title = "Crime Rate Map",
            status = "success",
            solidHeader = TRUE,
            leafletOutput("crime_map", height = "500px") %>%
              withSpinner(type = 8, color = "#2196F3"),
            width = 8,
            HTML("<p style='text-align: center; font-size: 14px; margin-top: 10px; color: #2E8B57;'>
      This geospatial graph <b>depicts</b> the <i>crime rates</i> across different locations of <b>Canada</b>. 
      If you <i>click</i> on a particular location, you can see the <b>Crime Rate per 100K</b> 
      and <b>Total Incidents</b> in that location.
     </p>"),
            HTML("<p style='text-align: center; font-size: 11px; margin-top: 8px; color: #2E8B57;'>
      Note: Tick <b>Legend check-box</b> only while selecting more than one location in the filter. Moreover, if you see the pop-ups cluttered. You can <b>zoom in</b> the map for a better view.
     </p>")
          ),
          box(
            title = "Regional Statistics",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("regional_stats", height = "520px") %>%
              withSpinner(type = 8, color = "#2196F3"),
            width = 4,
            HTML("<p style='text-align: center; font-size: 14px; margin-top: 10px; color: #2E8B57;'>
      This graph <b>provides insights</b> into <i>crime statistics</i>, such as <b>Total Incidents</b> 
      and <b>Crime Rates</b> across different regions.
     </p>")
          ),
          box(
            title = "Clearance Rate Trends",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("clearance_trend", height = "400px") %>%
              withSpinner(type = 4, color = "#2196F3"),
            width = 12,
            HTML("<p style='text-align: center; font-size: 14px; margin-top: 10px; color: #2E8B57;'>
  This line graph tracks <i>clearance rates</i> over time, showing how the percentage of solved cases changes across years. 
  
</p>"),
            HTML("<p style='text-align: center; font-size: 11px; margin-top: 8px; color: #2E8B57;'>
      Note: Selecting <b>'All'</b> in any filter shows the aggregated trend, while specific selections display individual lines.
     </p>")
          )
        )
      ),
      tabItem(
        tabName = "analysis",
        fluidRow(
          box(
            title = "Crime Category Breakdown",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("category_breakdown", height = "400px") %>%
              withSpinner(type = 8, color = "#2196F3"),
            width = 12,
            HTML("<p style='text-align: center; font-size: 14px; margin-top: 10px; color: #2E8B57;'>
      This <b>treemap</b> visualizes the <b>distribution</b> of different <b>crime categories</b> 
      based on the <b>number of occurrences</b>. The <b>size</b> of each box represents the 
      <b>relative frequency</b> of that crime category.
     </p>")
          )
        ),
        fluidRow(
          box(
            title = "Clearance Rates by Category",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("clearance_rates", height = "400px") %>%
              withSpinner(type = 8, color = "#2196F3"),
            width = 12,
            HTML("<p style='text-align: center; font-size: 14px; margin-top: 10px; color: #2E8B57;'>
      This <b>bar graph</b> shows the <b>top 10 crime types</b> with the <b>highest clearance rates</b>, expressed as a <b>percentage</b>. 
      The <b>height</b> of each bar represents the <b>clearance rate</b> for that crime type.
     </p>")
          ),
        )
      ),
      
      tabItem(
        tabName = "trends",
        fluidRow(
          box(
            title = "Year-over-Year Changes",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("yoy_trends_plot", height = "400px") %>%
              withSpinner(type = 8, color = "#2196F3"),
            width = 12,
            HTML("<p style='text-align: center; font-size: 14px; margin-top: 10px; color: #2E8B57;'>
      This <b>line graph</b> shows the <b>percentage change</b> in different <b>crime categories</b> 
      <b>year-over-year</b> from <b>2014 to 2023</b>. The <b>different colored lines</b> represent distinct <b>crime categories</b>.
     </p>")
          )
        ),
        fluidRow(
          box(
            title = "Top Increasing Crimes",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("increasing_crimes_plot", height = "400px") %>%
              withSpinner(type = 8, color = "#2196F3"),
            width = 6
          ),
          box(
            title = "Top Decreasing Crimes",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("decreasing_crimes_plot", height = "400px") %>%
              withSpinner(type = 8, color = "#2196F3"),
            width = 6
          )
        )
      ),
      
      tabItem(
        tabName = "demographics",
        fluidRow(
          box(
            title = "Youth vs Adult Comparison",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("youth_adult_plot", height = "650px") %>%
              withSpinner(type = 8, color = "#2196F3"),
            width = 12,
            HTML("<p style='text-align: center; font-size: 14px; margin-top: 10px; color: #2E8B57;'>
      This <b>bar chart</b> illustrates the <b>Youth vs Adult distribution</b> for different <b>crime types</b>. 
      The <b>x-axis</b> lists various <b>crime categories</b>, while the <b>y-axis</b> represents the <b>percentage</b> of individuals 
      within each <b>age group</b> (<b>Adult</b> and <b>Youth</b>) involved in those crimes.
     </p>")
          )
        )
      ),
      tabItem(
        tabName = "location_comparison",
        fluidRow(
          box(
            title = "Crime Rate Comparison Across diffrent locations",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("location_comparison_plot", height = "600px") %>%
              withSpinner(type = 8, color = "#2196F3"),
            width = 12,
            HTML("<p style='font-size: 14px; color: #2E8B57;'>
             <b>Note:</b> To compare two or more locations, you can select <b>Geographic Level</b> as <i>Provincial/Territorial</i>. 
             If you want to view data for cities, select <b>Cities</b> in <b>Geographic Level</b> 
             and then choose two or more cities from different provinces for comparison.
           </p>")
          )
        )
      ),
      
      tabItem(
        tabName = "explorer",
        fluidRow(
          box(
            title = "Crime Data Explorer",
            status = "success",
            solidHeader = TRUE,
            DTOutput("data_table") %>%
              withSpinner(type = 8, color = "#2196F3"),
            width = 12,
            HTML("<p style='text-align: center; font-size: 14px; margin-top: 10px; color: #2E8B57;'>
      <b>Note:</b> In the <b>Data Explorer</b>, you can download the data filtered using the available filters. 
      Use the <b>download buttons</b> to export the filtered dataset in your preferred format.
     </p>")
            
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    yearly_agg = NULL,
    violation_agg = NULL,
    total_incidents = NULL
  )
  
  filtered_data <- reactive({
    req(input$year_range)
    
    validate(
      need(input$year_range[1] <= input$year_range[2], "Invalid year range")
    )
    
    data <- copy(crime_data)
    data <- data[year >= input$year_range[1] & year <= input$year_range[2]]
    
    if(input$geographic_level == "All") {
      data <- data[aggregation_note == "National aggregate"]
    } else if(input$geographic_level == "Province") {
      # Now handling both Provincial and Territorial aggregate data
      data <- data[level %in% c("Province", "Territory")]
      
      if(!("All" %in% input$province)) {
        data <- data[province %in% input$province]
      }
    } else if(input$geographic_level == "City") {
      data <- data[aggregation_note == "City-level data"]
      
      if(!("All" %in% input$province)) {
        data <- data[province %in% input$province]
        
        if(!("All" %in% input$city)) {
          data <- data[location %in% input$city]
        }
      }
    }
    
    if(!is.null(input$crime_type) && !"All" %in% input$crime_type) {
      data <- data[violation_type %in% input$crime_type]
    }
    
    validate(
      need(nrow(data) > 0, "No data available for selected filters")
    )
    
    data
  })
  
  observe({
    req(filtered_data())
    data <- filtered_data()
    
    validate(
      need(nrow(data) > 0, "No data available for aggregation")
    )
    
    yearly_agg <- data[, list(
      total_incidents = sum(Actual_Incidents, na.rm = TRUE),
      avg_rate = mean(Rate_per_100k_People, na.rm = TRUE)
    ), by = list(year, violation_type)]
    
    rv$yearly_agg <- yearly_agg
    
    violation_agg <- data[, list(
      total = sum(Actual_Incidents, na.rm = TRUE),
      avg_rate = mean(Rate_per_100k_People, na.rm = TRUE),
      clearance_rate = mean(Clearance_Rate, na.rm = TRUE)
    ), by = list(violation_type)]
    
    rv$violation_agg <- violation_agg
  })
  
  output$total_incidents_box <- renderValueBox({
    data <- filtered_data()
    total <- data[, sum(Actual_Incidents, na.rm = TRUE)]
    rv$total_incidents <- total
    
    valueBox(
      formatC(total, format = "d", big.mark = ","),
      "Total Incidents",
      icon = icon("exclamation-triangle"),
      color = "red",
      
    )
  })
  
  output$avg_rate_box <- renderValueBox({
    data <- filtered_data()
    avg_rate <- data[, mean(Rate_per_100k_People, na.rm = TRUE)]
    
    valueBox(
      sprintf("%.1f", avg_rate),
      "Average Rate per 100,000",
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  
  output$clearance_box <- renderValueBox({
    data <- filtered_data()
    avg_clearance <- data[, mean(Clearance_Rate, na.rm = TRUE)]
    
    valueBox(
      sprintf("%.1f%%", avg_clearance),
      "Average Clearance Rate",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  observeEvent(input$reset_filters, {
    # Reset dropdowns (select inputs)
    updateSelectInput(session, "province", selected = "All")
    updateSelectInput(session, "city", selected = "All")
    updateSelectInput(session, "geographic_level", selected = "All")
    updateSelectInput(session, "crime_type", selected = "All")
    
    # Reset sliders (year range)
    updateSliderInput(session, "year_range", value = c(2014, 2023))
  })
  
  output$yoy_change_box <- renderValueBox({
    data <- filtered_data()
    
    # Calculate YoY change using data.table syntax
    changes <- data[, .(
      total = sum(Actual_Incidents, na.rm = TRUE)
    ), by = year][order(year)][
      , `:=`(
        prev_total = data.table::shift(total, type = "lag"),
        yoy_change = ((total - data.table::shift(total, type = "lag")) / 
                        data.table::shift(total, type = "lag") * 100)
      )
    ][year == max(year)]
    
    avg_change <- if (nrow(changes) > 0 && !is.na(changes$yoy_change)) changes$yoy_change else 0
    
    valueBox(
      sprintf("%.1f%%", avg_change),
      "Year-over-Year Change",
      icon = icon("percentage"),
      color = if(avg_change > 0) "red" else "green"
    )
  })
  
  output$trend_plot <- renderPlotly({
    data <- filtered_data()
    
    # Data aggregation based on filters
    trend_data <- if(input$geographic_level == "All") {
      if("All" %in% input$crime_type) {
        # Aggregate all crime types
        data[aggregation_note == "National aggregate", .(
          avg_rate = mean(Rate_per_100k_People, na.rm = TRUE),
          total_incidents = sum(Actual_Incidents, na.rm = TRUE)
        ), by = .(year)]
      } else {
        # Show selected crime types separately
        data[aggregation_note == "National aggregate" & 
               violation_type %in% input$crime_type, .(
                 avg_rate = mean(Rate_per_100k_People, na.rm = TRUE),
                 total_incidents = sum(Actual_Incidents, na.rm = TRUE)
               ), by = .(year, violation_type)]
      }
    } else if(input$geographic_level == "Province") {
      if("All" %in% input$province) {
        if("All" %in% input$crime_type) {
          # Aggregate all provinces and crimes
          data[level %in% c("Province", "Territory"), .(
            avg_rate = mean(Rate_per_100k_People, na.rm = TRUE),
            total_incidents = sum(Actual_Incidents, na.rm = TRUE)
          ), by = .(year)]
        } else {
          # Aggregate provinces but keep crimes separate
          data[level %in% c("Province", "Territory") & 
                 violation_type %in% input$crime_type, .(
                   avg_rate = mean(Rate_per_100k_People, na.rm = TRUE),
                   total_incidents = sum(Actual_Incidents, na.rm = TRUE)
                 ), by = .(year, violation_type)]
        }
      } else {
        if("All" %in% input$crime_type) {
          # Show selected provinces, aggregate crimes
          data[level %in% c("Province", "Territory") & 
                 province %in% input$province, .(
                   avg_rate = mean(Rate_per_100k_People, na.rm = TRUE),
                   total_incidents = sum(Actual_Incidents, na.rm = TRUE)
                 ), by = .(year, province)]
        } else {
          # Show selected provinces and crimes
          data[level %in% c("Province", "Territory") & 
                 province %in% input$province & 
                 violation_type %in% input$crime_type, .(
                   avg_rate = mean(Rate_per_100k_People, na.rm = TRUE),
                   total_incidents = sum(Actual_Incidents, na.rm = TRUE)
                 ), by = .(year, province, violation_type)]
        }
      }
    } else {
      if("All" %in% input$city) {
        if("All" %in% input$province) {
          if("All" %in% input$crime_type) {
            # Aggregate all cities and crimes
            data[level == "City", .(
              avg_rate = mean(Rate_per_100k_People, na.rm = TRUE),
              total_incidents = sum(Actual_Incidents, na.rm = TRUE)
            ), by = .(year)]
          } else {
            # Aggregate cities, show selected crimes
            data[level == "City" & 
                   violation_type %in% input$crime_type, .(
                     avg_rate = mean(Rate_per_100k_People, na.rm = TRUE),
                     total_incidents = sum(Actual_Incidents, na.rm = TRUE)
                   ), by = .(year, violation_type)]
          }
        } else {
          if("All" %in% input$crime_type) {
            # Show selected provinces' cities, aggregate crimes
            data[level == "City" & 
                   province %in% input$province, .(
                     avg_rate = mean(Rate_per_100k_People, na.rm = TRUE),
                     total_incidents = sum(Actual_Incidents, na.rm = TRUE)
                   ), by = .(year, province)]
          } else {
            # Show selected provinces' cities and crimes
            data[level == "City" & 
                   province %in% input$province & 
                   violation_type %in% input$crime_type, .(
                     avg_rate = mean(Rate_per_100k_People, na.rm = TRUE),
                     total_incidents = sum(Actual_Incidents, na.rm = TRUE)
                   ), by = .(year, province, violation_type)]
          }
        }
      } else {
        if("All" %in% input$crime_type) {
          # Show selected cities, aggregate crimes
          data[level == "City" & 
                 location %in% input$city, .(
                   avg_rate = mean(Rate_per_100k_People, na.rm = TRUE),
                   total_incidents = sum(Actual_Incidents, na.rm = TRUE)
                 ), by = .(year, location)]
        } else {
          # Show selected cities and crimes
          data[level == "City" & 
                 location %in% input$city & 
                 violation_type %in% input$crime_type, .(
                   avg_rate = mean(Rate_per_100k_People, na.rm = TRUE),
                   total_incidents = sum(Actual_Incidents, na.rm = TRUE)
                 ), by = .(year, location, violation_type)]
        }
      }
    }
    
    validate(
      need(nrow(trend_data) > 0, "No data available for selected filters")
    )
    
    plot_ly(trend_data) %>%
      add_trace(
        x = ~year,
        y = ~avg_rate,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = 2),
        marker = list(size = 8),
        color = if(!("All" %in% input$crime_type)) ~violation_type
        else if(input$geographic_level == "Province" && !("All" %in% input$province)) ~province 
        else if(input$geographic_level == "City" && !("All" %in% input$city)) ~location 
        else NULL,
        hovertext = ~sprintf(
          "<b>%s%s</b><br>Year: %d<br>Rate per 100k: %.1f<br>Total Incidents: %s",
          if(exists("violation_type", trend_data)) paste(violation_type, "\n") else "",
          if("All" %in% input$province && "All" %in% input$city) "Overall" else
            if(exists("province", trend_data)) province else
              if(exists("location", trend_data)) location else "National",
          year,
          avg_rate,
          formatC(total_incidents, format="d", big.mark=",")
        ),
        hoverinfo = 'text'
      ) %>%
      layout(
        title = list(
          text = sprintf("Crime Rate Trends - %s", 
                         if("All" %in% input$province && input$geographic_level != "All") "All Regions" else
                           if(input$geographic_level == "All") "National" else
                             if(input$geographic_level == "Province") "Provincial" else "City"),
          font = list(size = 16, color = "#2E8B57")
        ),
        xaxis = list(
          title = list(text = "Year", font = list(size = 14, color = "#2E8B57")),
          tickformat = "d",
          tickfont = list(size = 14, color = "#2E8B57"),
          dtick = 1,
          gridcolor = "rgba(46, 139, 87, 0.1)"
        ),
        yaxis = list(
          title = list(text = "Rate per 100,000", font = list(size = 14, color = "#2E8B57")),
          gridcolor = "rgba(46, 139, 87, 0.1)",
          tickfont = list(size = 14, color = "#2E8B57")
        ),
        showlegend = FALSE,
        hovermode = 'closest',
        margin = list(t = 50, b = 60, l = 60, r = 20),
        plot_bgcolor = "rgba(255, 255, 255, 0.9)",
        paper_bgcolor = "rgba(255, 255, 255, 0.9)"
      )
  })
  
  output$distribution_plot <- renderPlotly({
    req(rv$violation_agg)
    validate(
      need(nrow(rv$violation_agg) > 0, "No data available for the selected filters"),
      need(!all(is.na(rv$violation_agg$total)), "No incident data available for the selected filters")
    )
    
    # Prepare data
    dist_data <- copy(rv$violation_agg)[
      order(-total)
    ][
      1:min(10, .N)  # Take top 16 or all if less than 16
    ][
      , `:=`(
        percentage = (total / sum(total) * 100),
        violation_type = factor(violation_type, levels = violation_type)
      )
    ]
    
    # Create custom green color palette
    custom_greens <- c(
      "#1B4D3E", "#265C4B", "#316B58", "#3D7A65", 
      "#488972", "#54987F", "#60A78C", "#6CB699", 
      "#78C5A6", "#84D4B3", "#90E3C0", "#9CF2CD", 
      "#A8FFDA", "#B4FFE7", "#C0FFF4", "#CCFFFF"
    )
    
    # Create donut chart
    plot_ly(dist_data) %>%
      add_pie(
        labels = ~violation_type,
        values = ~total,
        type = 'pie',
        hole = 0.6,
        textinfo = 'none',
        marker = list(
          colors = custom_greens[1:nrow(dist_data)],
          line = list(color = '#FFFFFF', width = 2)
        ),
        hoverinfo = 'text',
        text = ~sprintf(
          "<b>%s</b><br>Total Incidents: %s<br>Share: %.1f%%<br>Rate per 100k: %.1f<br>Clearance Rate: %.1f%%",
          violation_type,
          formatC(total, format="d", big.mark=","),
          percentage,
          avg_rate,
          clearance_rate
        )
      ) %>%
      layout(
        title = list(
          text = "Top Crime Types Distribution",
          font = list(size = 16, color = "#2E8B57"),
          y = 0.98
        ),
        showlegend = FALSE,
        margin = list(
          t = 50,
          b = 20,
          l = 20,
          r = 20,
          pad = 0
        ),
        paper_bgcolor = "rgba(255, 255, 255, 0.9)"
      )
  })
  
  observe({
    req(input$geographic_level)
    
    if(input$geographic_level == "All") {
      updateSelectizeInput(session, "province",
                           choices = c("All" = "All"),
                           selected = "All")
      updateSelectizeInput(session, "city",
                           choices = c("All" = "All"),
                           selected = "All")
      return()
    }
    
    # Now handling both provinces and territories together
    choices <- if(input$geographic_level == "Province") {
      provinces  # This now includes both provinces and territories
    } else {
      unique(province_city_mapping$province)
    }
    
    updateSelectizeInput(session, "province",
                         choices = c("All" = "All", as.list(choices)),
                         selected = "All")
  })
  
  observeEvent(input$province, {
    # If "All" is selected along with other provinces, remove "All"
    if ("All" %in% input$province && length(input$province) > 1) {
      updateSelectInput(
        session,
        "province",
        selected = setdiff(input$province, "All")
      )
    }
  })
  
  observeEvent(input$city, {
    # If "All" is selected along with other cities, remove "All"
    if ("All" %in% input$city && length(input$city) > 1) {
      updateSelectizeInput(
        session,
        "city",
        selected = setdiff(input$city, "All")
      )
    }
  })
  
  observeEvent(input$crime_type, {
    # If "All" is selected along with other crime types, remove "All"
    if ("All" %in% input$crime_type && length(input$crime_type) > 1) {
      updateSelectInput(
        session,
        "crime_type",
        selected = setdiff(input$crime_type, "All")
      )
    }
  })
  
  observe({
    req(input$geographic_level, input$province)
    
    if(input$geographic_level != "City" || "All" %in% input$province) {
      updateSelectizeInput(session, "city",
                           choices = c("All" = "All"),
                           selected = "All")
      return()
    }
    
    cities <- province_city_mapping[
      province %in% input$province,
      unique(city)
    ]
    
    updateSelectizeInput(session, "city",
                         choices = c("All" = "All", as.list(cities)),
                         selected = "All")
  })
  
  
  
  output$crime_map <- renderLeaflet({
    req(input$geographic_level)
    data <- filtered_data()
    
    validate(
      need(data, "No data available")
    )
    
    # Base map setup
    m <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -106.3468, lat = 56.1304, zoom = 4)
    
    # Create color palette function
    create_palette <- function(data_range) {
      colorNumeric(
        palette = "viridis",
        domain = data_range,
        na.color = "#E8E8E8"
      )
    }
    
    if(input$geographic_level == "All") {
      # National level visualization
      national_data <- data[aggregation_note == "National aggregate", .(
        total_incidents = sum(Actual_Incidents, na.rm = TRUE),
        rate_per_100k = mean(Rate_per_100k_People, na.rm = TRUE)
      )]
      
      pal <- create_palette(range(national_data$rate_per_100k, na.rm = TRUE))
      
      m <- m %>%
        addPolygons(
          data = canada_provinces,
          fillColor = pal(national_data$rate_per_100k),
          fillOpacity = 0.5,
          weight = 1,
          color = "#666666",
          popup = sprintf(
            "<strong>Canada</strong><br/>Total Incidents: %s<br/>Rate per 100k: %.1f",
            formatC(national_data$total_incidents, format = "d", big.mark = ","),
            national_data$rate_per_100k
          )
        )
      
    } else if(input$geographic_level == "Province") {
      # Provincial level visualization
      province_data <- data[level %in% c("Province", "Territory"), .(
        total_incidents = sum(Actual_Incidents, na.rm = TRUE),
        rate_per_100k = mean(Rate_per_100k_People, na.rm = TRUE)
      ), by = .(province)]
      
      pal <- create_palette(range(province_data$rate_per_100k, na.rm = TRUE))
      
      for(region_name in canada_provinces$name) {
        data_name <- ifelse(region_name == "Québec", "Quebec", region_name)
        region_stats <- province_data[province == data_name]
        
        if(nrow(region_stats) > 0 && 
           (("All" %in% input$province) || (data_name %in% input$province))) {
          fill_color <- pal(region_stats$rate_per_100k)
          fill_opacity <- 0.7
          popup_content <- sprintf(
            "<strong>%s</strong><br/>Total Incidents: %s<br/>Rate per 100k: %.1f",
            region_name,
            formatC(region_stats$total_incidents, format = "d", big.mark = ","),
            region_stats$rate_per_100k
          )
        } else {
          fill_color <- "#E8E8E8"
          fill_opacity <- 0.2
          popup_content <- sprintf("<strong>%s</strong>", region_name)
        }
        
        m <- m %>%
          addPolygons(
            data = subset(canada_provinces, name == region_name),
            fillColor = fill_color,
            fillOpacity = fill_opacity,
            weight = 1,
            color = "#666666",
            popup = popup_content
          )
      }
      
    } else if(input$geographic_level == "City") {
      # City level visualization with enhanced province boundaries
      
      # Get selected provinces
      selected_provinces <- if("All" %in% input$province) {
        unique(data$province)
      } else {
        input$province
      }
      
      # First, add ALL province boundaries with different styling based on selection
      for(region_name in canada_provinces$name) {
        data_name <- ifelse(region_name == "Québec", "Quebec", region_name)
        
        # Check if this province is selected
        is_selected <- data_name %in% selected_provinces
        
        m <- m %>%
          addPolygons(
            data = subset(canada_provinces, name == region_name),
            fillColor = if(is_selected) "#4C8B74" else "#ffffff",  # Light background for selected provinces
            fillOpacity = if(is_selected) 0.2 else 0.1,
            weight = if(is_selected) 2 else 1,
            color = if(is_selected) "#8B2E2E" else "#cccccc",  # Green border for selected provinces
            popup = sprintf("<strong>%s</strong>", region_name)
          )
      }
      
      # Prepare city data
      city_data <- data[level == "City", .(
        total_incidents = sum(Actual_Incidents, na.rm = TRUE),
        rate_per_100k = mean(Rate_per_100k_People, na.rm = TRUE),
        latitude = first(latitude),
        longitude = first(longitude)
      ), by = .(location, province)]
      
      # Filter cities based on selected provinces
      city_data <- city_data[province %in% selected_provinces]
      
      # Apply city filter if specific cities are selected
      if(!"All" %in% input$city) {
        city_data <- city_data[location %in% input$city]
      }
      
      if(nrow(city_data) > 0) {
        pal <- create_palette(range(city_data$rate_per_100k, na.rm = TRUE))
        
        m <- m %>%
          addCircleMarkers(
            data = city_data,
            lng = ~longitude,
            lat = ~latitude,
            radius = ~sqrt(total_incidents/max(total_incidents)) * 15,
            fillColor = ~pal(rate_per_100k),
            color = "#ffffff",  # White border for circles
            weight = 1.5,
            opacity = 1,
            fillOpacity = 0.8,
            popup = ~sprintf(
              "<strong>%s</strong><br/>Total Incidents: %s<br/>Rate per 100k: %.1f",
              location,
              formatC(total_incidents, format = "d", big.mark = ","),
              rate_per_100k
            ),
            label = ~location,  # Add hover labels
            labelOptions = labelOptions(
              noHide = FALSE,
              direction = "auto",
              style = list(
                "font-weight" = "bold",
                padding = "3px 8px"
              )
            )
          )
      }
    }
    
    # Add legend if not at national level
    if(input$geographic_level != "All") {
      m <- m %>%
        addLegend(
          "bottomright",
          pal = pal,
          values = if(input$geographic_level == "City") 
            city_data$rate_per_100k else province_data$rate_per_100k,
          title = "Rate per 100,000",
          opacity = 0.7,
          group = "Legend"
        ) %>%
        addLayersControl(
          overlayGroups = c("Legend"),
          options = layersControlOptions(
            collapsed = FALSE,
            position = "topright"
          )
        )
    }
    
    m
  })
  
  
  # First, the clean donut chart without labels
  output$regional_stats <- renderPlotly({
    data <- filtered_data()
    
    reg_data <- data[, .(
      total_incidents = sum(Actual_Incidents, na.rm = TRUE),
      avg_rate = mean(Rate_per_100k_People, na.rm = TRUE),
      clearance_rate = mean(Clearance_Rate, na.rm = TRUE),
      youth_rate = sum(Total_youth_charged, na.rm = TRUE) / sum(Total_persons_charged, na.rm = TRUE) * 100
    ), by = region][
      total_incidents > 0
    ][
      order(-total_incidents)
    ][
      , pct_of_total := (total_incidents / sum(total_incidents)) * 100
    ]
    
    plot_ly(reg_data) %>%
      add_pie(
        labels = ~region,
        values = ~total_incidents,
        hole = 0.6,
        type = 'pie',
        textinfo = 'none',  # Removed labels
        marker = list(
          colors = colorRampPalette(c("#98FB98", "#2E8B57"))(nrow(reg_data)),
          line = list(color = '#FFFFFF', width = 2)
        ),
        hoverinfo = 'text',
        text = ~sprintf(
          "<b>%s</b><br>Total Incidents: %s<br>Share: %.1f%%<br>Rate per 100k: %.1f<br>Clearance Rate: %.1f%%<br>Youth Rate: %.1f%%",
          region,
          formatC(total_incidents, format="d", big.mark=","),
          pct_of_total,
          avg_rate,
          clearance_rate,
          youth_rate
        )
      ) %>%
      layout(
        title = list(
          text = "Regional Crime Distribution",
          font = list(size = 16, color = "#2E8B57")
        ),
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          x = 0.5,
          y = -0.1,
          xanchor = "center"
        ),
        annotations = list(
          list(
            x = 0.5,
            y = 0.5,
            text = sprintf("Total<br>%s", formatC(sum(reg_data$total_incidents), format="d", big.mark=",")),
            showarrow = FALSE,
            font = list(size = 14)
          )
        ),
        margin = list(t = 50, b = 80, l = 20, r = 20)
      )
  })
  
  # New clearance trend chart
  output$clearance_trend <- renderPlotly({
    data <- filtered_data()
    
    # Enhanced data aggregation with proper handling of "All" selections
    clearance_data <- if(input$geographic_level == "All") {
      if("All" %in% input$crime_type) {
        # Aggregate all crime types
        data[aggregation_note == "National aggregate", .(
          clearance_rate = mean(Clearance_Rate, na.rm = TRUE),
          total_incidents = sum(Actual_Incidents, na.rm = TRUE)
        ), by = .(year)]
      } else {
        # Show selected crime types separately
        data[aggregation_note == "National aggregate" & 
               violation_type %in% input$crime_type, .(
                 clearance_rate = mean(Clearance_Rate, na.rm = TRUE),
                 total_incidents = sum(Actual_Incidents, na.rm = TRUE)
               ), by = .(year, violation_type)]
      }
    } else if(input$geographic_level == "Province") {
      if("All" %in% input$province) {
        # Aggregate all provinces but keep crime types separate if specified
        if("All" %in% input$crime_type) {
          data[level %in% c("Province", "Territory"), .(
            clearance_rate = mean(Clearance_Rate, na.rm = TRUE),
            total_incidents = sum(Actual_Incidents, na.rm = TRUE)
          ), by = .(year)]
        } else {
          data[level %in% c("Province", "Territory") & 
                 violation_type %in% input$crime_type, .(
                   clearance_rate = mean(Clearance_Rate, na.rm = TRUE),
                   total_incidents = sum(Actual_Incidents, na.rm = TRUE)
                 ), by = .(year, violation_type)]
        }
      } else {
        # Show selected provinces with crime types
        if("All" %in% input$crime_type) {
          data[level %in% c("Province", "Territory") & 
                 province %in% input$province, .(
                   clearance_rate = mean(Clearance_Rate, na.rm = TRUE),
                   total_incidents = sum(Actual_Incidents, na.rm = TRUE)
                 ), by = .(year, province)]
        } else {
          data[level %in% c("Province", "Territory") & 
                 province %in% input$province & 
                 violation_type %in% input$crime_type, .(
                   clearance_rate = mean(Clearance_Rate, na.rm = TRUE),
                   total_incidents = sum(Actual_Incidents, na.rm = TRUE)
                 ), by = .(year, province, violation_type)]
        }
      }
    } else {
      # City level
      if("All" %in% input$city) {
        if("All" %in% input$province) {
          if("All" %in% input$crime_type) {
            data[level == "City", .(
              clearance_rate = mean(Clearance_Rate, na.rm = TRUE),
              total_incidents = sum(Actual_Incidents, na.rm = TRUE)
            ), by = .(year)]
          } else {
            data[level == "City" & 
                   violation_type %in% input$crime_type, .(
                     clearance_rate = mean(Clearance_Rate, na.rm = TRUE),
                     total_incidents = sum(Actual_Incidents, na.rm = TRUE)
                   ), by = .(year, violation_type)]
          }
        } else {
          if("All" %in% input$crime_type) {
            data[level == "City" & 
                   province %in% input$province, .(
                     clearance_rate = mean(Clearance_Rate, na.rm = TRUE),
                     total_incidents = sum(Actual_Incidents, na.rm = TRUE)
                   ), by = .(year, province)]
          } else {
            data[level == "City" & 
                   province %in% input$province & 
                   violation_type %in% input$crime_type, .(
                     clearance_rate = mean(Clearance_Rate, na.rm = TRUE),
                     total_incidents = sum(Actual_Incidents, na.rm = TRUE)
                   ), by = .(year, province, violation_type)]
          }
        }
      } else {
        if("All" %in% input$crime_type) {
          data[level == "City" & 
                 location %in% input$city, .(
                   clearance_rate = mean(Clearance_Rate, na.rm = TRUE),
                   total_incidents = sum(Actual_Incidents, na.rm = TRUE)
                 ), by = .(year, location)]
        } else {
          data[level == "City" & 
                 location %in% input$city & 
                 violation_type %in% input$crime_type, .(
                   clearance_rate = mean(Clearance_Rate, na.rm = TRUE),
                   total_incidents = sum(Actual_Incidents, na.rm = TRUE)
                 ), by = .(year, location, violation_type)]
        }
      }
    }
    
    # Validate we have data
    validate(
      need(nrow(clearance_data) > 0, "No clearance rate data available for selected filters")
    )
    
    # Create the plot
    p <- plot_ly(clearance_data) %>%
      add_trace(
        x = ~year,
        y = ~clearance_rate,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = 2),
        marker = list(size = 8),
        color = if(!("All" %in% input$crime_type)) ~violation_type else
          if(input$geographic_level == "Province" && !("All" %in% input$province)) ~province else
            if(input$geographic_level == "City" && !("All" %in% input$city)) ~location else NULL,
        hovertext = ~sprintf(
          "<b>%s%s</b><br>Year: %d<br>Clearance Rate: %.1f%%<br>Total Incidents: %s",
          if(exists("violation_type", clearance_data)) paste(violation_type, "\n") else "",
          if("All" %in% input$province && "All" %in% input$city) "Overall" else
            if(exists("province", clearance_data)) province else
              if(exists("location", clearance_data)) location else "National",
          year,
          clearance_rate,
          formatC(total_incidents, format="d", big.mark=",")
        ),
        hoverinfo = 'text'
      ) %>%
      layout(
        title = list(
          text = sprintf("Clearance Rate Trends - %s", 
                         if("All" %in% input$province && input$geographic_level != "All") "All Regions" else
                           if(input$geographic_level == "All") "National" else
                             if(input$geographic_level == "Province") "Provincial" else "City"),
          font = list(size = 16, color = "#2E8B57")
        ),
        xaxis = list(
          title = list(text = "Year", font = list(size = 14, color = "#2E8B57")),
          tickformat = "d",
          dtick = 1,
          gridcolor = "rgba(46, 139, 87, 0.1)",
          tickfont = list(size = 12, color = "#2E8B57")
        ),
        yaxis = list(
          title = list(text = "Clearance Rate (%)", font = list(size = 14, color = "#2E8B57")),
          ticksuffix = "%",
          range = c(0, max(clearance_data$clearance_rate) * 1.1),
          gridcolor = "rgba(46, 139, 87, 0.1)",
          tickfont = list(size = 12, color = "#2E8B57")
        ),
        showlegend = FALSE,
        hovermode = 'closest',
        margin = list(t = 50, b = 60, l = 60, r = 20),
        plot_bgcolor = "rgba(255, 255, 255, 0.9)",
        paper_bgcolor = "rgba(255, 255, 255, 0.9)"
      )
    
    p
  })
  
  output$yoy_trends_plot <- renderPlotly({
    data <- filtered_data()
    
    # Enhanced data aggregation based on filters
    base_data <- if(input$geographic_level == "All") {
      if("All" %in% input$crime_type) {
        # Aggregate all crime types
        data[aggregation_note == "National aggregate", .(
          total_incidents = sum(Actual_Incidents, na.rm = TRUE)
        ), by = list(year)]
      } else {
        # Show selected crime types separately
        data[aggregation_note == "National aggregate" & 
               violation_type %in% input$crime_type, .(
                 total_incidents = sum(Actual_Incidents, na.rm = TRUE)
               ), by = list(year, violation_type)]
      }
    } else if(input$geographic_level == "Province") {
      if("All" %in% input$province) {
        if("All" %in% input$crime_type) {
          # Aggregate all provinces and crimes
          data[level %in% c("Province", "Territory"), .(
            total_incidents = sum(Actual_Incidents, na.rm = TRUE)
          ), by = list(year)]
        } else {
          # Aggregate provinces but keep crimes separate
          data[level %in% c("Province", "Territory") & 
                 violation_type %in% input$crime_type, .(
                   total_incidents = sum(Actual_Incidents, na.rm = TRUE)
                 ), by = list(year, violation_type)]
        }
      } else {
        if("All" %in% input$crime_type) {
          # Show selected provinces, aggregate crimes
          data[level %in% c("Province", "Territory") & 
                 province %in% input$province, .(
                   total_incidents = sum(Actual_Incidents, na.rm = TRUE)
                 ), by = list(year, province)]
        } else {
          # Show selected provinces and crimes
          data[level %in% c("Province", "Territory") & 
                 province %in% input$province & 
                 violation_type %in% input$crime_type, .(
                   total_incidents = sum(Actual_Incidents, na.rm = TRUE)
                 ), by = list(year, province, violation_type)]
        }
      }
    } else {
      if("All" %in% input$city) {
        if("All" %in% input$province) {
          if("All" %in% input$crime_type) {
            # Aggregate all cities and crimes
            data[level == "City", .(
              total_incidents = sum(Actual_Incidents, na.rm = TRUE)
            ), by = list(year)]
          } else {
            # Aggregate cities, show selected crimes
            data[level == "City" & 
                   violation_type %in% input$crime_type, .(
                     total_incidents = sum(Actual_Incidents, na.rm = TRUE)
                   ), by = list(year, violation_type)]
          }
        } else {
          if("All" %in% input$crime_type) {
            # Show selected provinces' cities, aggregate crimes
            data[level == "City" & 
                   province %in% input$province, .(
                     total_incidents = sum(Actual_Incidents, na.rm = TRUE)
                   ), by = list(year, province)]
          } else {
            # Show selected provinces' cities and crimes
            data[level == "City" & 
                   province %in% input$province & 
                   violation_type %in% input$crime_type, .(
                     total_incidents = sum(Actual_Incidents, na.rm = TRUE)
                   ), by = list(year, province, violation_type)]
          }
        }
      } else {
        if("All" %in% input$crime_type) {
          # Show selected cities, aggregate crimes
          data[level == "City" & 
                 location %in% input$city, .(
                   total_incidents = sum(Actual_Incidents, na.rm = TRUE)
                 ), by = list(year, location)]
        } else {
          # Show selected cities and crimes
          data[level == "City" & 
                 location %in% input$city & 
                 violation_type %in% input$crime_type, .(
                   total_incidents = sum(Actual_Incidents, na.rm = TRUE)
                 ), by = list(year, location, violation_type)]
        }
      }
    }
    
    trend_data <- base_data[order(year)][
      , `:=`(
        prev_total = data.table::shift(total_incidents, type = "lag"),
        yoy_change = ((total_incidents - data.table::shift(total_incidents, type = "lag")) / 
                        data.table::shift(total_incidents, type = "lag") * 100)
      ),
      by = {
        if("All" %in% input$crime_type) {
          if(exists("province", base_data)) {
            list(province)
          } else if(exists("location", base_data)) {
            list(location)
          } else {
            list()
          }
        } else {
          list(violation_type)
        }
      }
    ][!is.na(yoy_change)]
    
    validate(
      need(nrow(trend_data) > 0, "No data available for selected filters")
    )
    
    # Get y-axis range
    y_max <- max(trend_data$yoy_change, na.rm = TRUE)
    y_min <- min(trend_data$yoy_change, na.rm = TRUE)
    
    # Create plot
    plot_ly(trend_data) %>%
      add_trace(
        x = ~year,
        y = ~yoy_change,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = 2),
        marker = list(size = 8, line = list(width = 1, color = 'white')),
        color = if(!("All" %in% input$crime_type)) ~violation_type else
          if(input$geographic_level == "Province" && !("All" %in% input$province)) ~province else
            if(input$geographic_level == "City" && !("All" %in% input$city)) ~location else NULL,
        hovertext = ~sprintf(
          "<b>%s%s</b><br>Year: %d<br>Change: %.1f%%<br>Total Incidents: %s<br>Previous Year: %s",
          if(exists("violation_type", trend_data)) paste(violation_type, "\n") else
            if(exists("province", trend_data)) paste(province, "\n") else
              if(exists("location", trend_data)) paste(location, "\n") else "",
          if("All" %in% input$province && "All" %in% input$city) "Overall" else "",
          year,
          yoy_change,
          formatC(total_incidents, format="d", big.mark=","),
          formatC(prev_total, format="d", big.mark=",")
        ),
        hoverinfo = 'text'
      ) %>%
      layout(
        title = list(
          text = sprintf("Year-over-Year Changes - %s", 
                         if("All" %in% input$province && input$geographic_level != "All") "All Regions" else
                           if(input$geographic_level == "All") "National" else
                             if(input$geographic_level == "Province") "Provincial" else "City"),
          font = list(size = 16, color = "#2E8B57")
        ),
        xaxis = list(
          title = "Year",
          gridcolor = 'rgba(46,139,87,0.1)',
          zeroline = FALSE,
          tickmode = 'linear'
        ),
        yaxis = list(
          title = "Percent Change",
          ticksuffix = "%",
          gridcolor = 'rgba(46,139,87,0.1)',
          zeroline = FALSE
        ),
        showlegend = FALSE,
        hovermode = 'closest',
        plot_bgcolor = 'rgba(255,255,255,0.9)',
        paper_bgcolor = 'rgba(255,255,255,0.9)',
        margin = list(t = 50, b = 60, l = 60, r = 20),
        shapes = list(
          list(
            type = 'rect',
            x0 = min(trend_data$year),
            x1 = max(trend_data$year),
            y0 = 0,
            y1 = y_max + (y_max * 0.1),
            fillcolor = 'rgba(255,0,0,0.1)',
            line = list(width = 0),
            layer = 'below'
          ),
          list(
            type = 'rect',
            x0 = min(trend_data$year),
            x1 = max(trend_data$year),
            y0 = y_min - (abs(y_min) * 0.1),
            y1 = 0,
            fillcolor = 'rgba(0,255,0,0.1)',
            line = list(width = 0),
            layer = 'below'
          ),
          list(
            type = 'line',
            x0 = min(trend_data$year),
            x1 = max(trend_data$year),
            y0 = 0,
            y1 = 0,
            line = list(
              color = 'rgba(0,0,0,0.5)',
              width = 1,
              dash = 'dot'
            )
          )
        )
      )
  })
  
  output$increasing_crimes_plot <- renderPlotly({
    data <- filtered_data()
    
    # First apply the crime type filter
    if (!is.null(input$crime_type) && !"All" %in% input$crime_type) {
      data <- data[violation_type %in% input$crime_type]
    }
    
    validate(
      need(nrow(data) > 0, "No data available for selected crime types")
    )
    
    # Calculate aggregated values with proper grouping based on geographic level
    group_cols <- c("year", "violation_type")
    if (input$geographic_level != "All") {
      if (input$geographic_level == "Province" && !("All" %in% input$province)) {
        group_cols <- c(group_cols, "province")
      } else if (input$geographic_level == "City" && !("All" %in% input$city)) {
        group_cols <- c(group_cols, "location")
      }
    }
    
    inc_data <- data[, list(
      total_incidents = sum(Actual_Incidents, na.rm = TRUE)
    ), by = group_cols] %>%
      .[, list(
        start_value = first(total_incidents),
        end_value = last(total_incidents),
        start_year = first(year),
        end_year = last(year),
        location = first(if ("location" %in% names(.)) location else "All")
      ), by = violation_type] %>%
      .[complete.cases(.)] %>%
      # Only calculate percentage for non-zero start values
      .[start_value > 0, pct_change := ((end_value - start_value) / start_value) * 100] %>%
      .[!is.na(pct_change) & pct_change > 0] %>%
      .[order(-pct_change)] %>%
      .[1:5]
    
    validate(
      need(nrow(inc_data) > 0, "No increasing trends found for the selected filters")
    )
    
    year_range <- sprintf("(%d-%d)", 
                          min(data$year, na.rm = TRUE),
                          max(data$year, na.rm = TRUE))
    
    plot_ly(inc_data,
            x = ~reorder(violation_type, pct_change),
            y = ~pct_change,
            type = 'bar',
            marker = list(
              color = "rgb(255, 65, 54)",
              line = list(color = "rgb(128, 0, 0)", width = 1.5)
            ),
            hovertext = ~sprintf(
              "%s<br>%s<br>Increase: %.1f%%<br>%d: %s incidents<br>%d: %s incidents<br>Net Change: +%s",
              violation_type,
              location,
              pct_change,
              start_year, formatC(start_value, format="d", big.mark=","),
              end_year, formatC(end_value, format="d", big.mark=","),
              formatC(end_value - start_value, format="d", big.mark=",")
            ),
            hoverinfo = 'text'
    ) %>%
      layout(
        title = list(
          text = paste("Top 5 Increasing Crime Types", year_range),
          font = list(size = 16, color = "#2E8B57")
        ),
        xaxis = list(
          title = "",
          tickangle = 45,
          tickfont = list(size = 11,color = "#2e8b57")
        ),
        yaxis = list(
          title = list(text = "Percent Increase", font = list(size = 14, color = "#2E8B57")),
          ticksuffix = "%",
          tickfont = list(size = 12, color = "#2E8B57"),
          gridcolor = 'rgba(0,0,0,0.1)',
          zeroline = TRUE,
          zerolinecolor = 'rgba(0,0,0,0.3)',
          range = c(0, max(inc_data$pct_change) * 1.1),
          tickformat = '.1f'
        ),
        showlegend = FALSE,
        hovermode = 'closest',
        margin = list(b = 100, l = 80, r = 40, t = 100),
        plot_bgcolor = 'rgba(255,255,255,0.9)'
      )
  })
  
  # the decreasing crimes plot
  output$decreasing_crimes_plot <- renderPlotly({
    data <- filtered_data()
    
    if (!is.null(input$crime_type) && !"All" %in% input$crime_type) {
      data <- data[violation_type %in% input$crime_type]
    }
    
    validate(
      need(nrow(data) > 0, "No data available for selected crime types")
    )
    
    # Use the same grouping logic as the increasing plot
    group_cols <- c("year", "violation_type")
    if (input$geographic_level != "All") {
      if (input$geographic_level == "Province" && !("All" %in% input$province)) {
        group_cols <- c(group_cols, "province")
      } else if (input$geographic_level == "City" && !("All" %in% input$city)) {
        group_cols <- c(group_cols, "location")
      }
    }
    
    dec_data <- data[, list(
      total_incidents = sum(Actual_Incidents, na.rm = TRUE)
    ), by = group_cols] %>%
      .[, list(
        start_value = first(total_incidents),
        end_value = last(total_incidents),
        start_year = first(year),
        end_year = last(year),
        location = first(if ("location" %in% names(.)) location else "All")
      ), by = violation_type] %>%
      .[complete.cases(.)] %>%
      .[start_value > 0, pct_change := ((end_value - start_value) / start_value) * 100] %>%
      .[!is.na(pct_change) & pct_change < 0] %>%
      .[order(pct_change)] %>%
      .[1:5]
    
    validate(
      need(nrow(dec_data) > 0, "No decreasing trends found for the selected filters")
    )
    
    year_range <- sprintf("(%d-%d)", 
                          min(data$year, na.rm = TRUE),
                          max(data$year, na.rm = TRUE))
    
    plot_ly(dec_data,
            x = ~reorder(violation_type, -pct_change),
            y = ~pct_change,
            type = 'bar',
            marker = list(
              color = "rgb(50, 205, 50)",
              line = list(color = "rgb(0, 100, 0)", width = 1.5)
            ),
            hovertext = ~sprintf(
              "%s<br>%s<br>Decrease: %.1f%%<br>%d: %s incidents<br>%d: %s incidents<br>Net Change: %s",
              violation_type,
              location,
              pct_change,
              start_year, formatC(start_value, format="d", big.mark=","),
              end_year, formatC(end_value, format="d", big.mark=","),
              formatC(end_value - start_value, format="d", big.mark=",")
            ),
            hoverinfo = 'text'
    ) %>%
      layout(
        title = list(
          text = paste("Top 5 Decreasing Crime Types", year_range),
          font = list(size = 16, color = "#2E8B57")
        ),
        xaxis = list(
          title = "",
          tickangle = 45,
          tickfont = list(size = 11, color = "#2e8b57")
        ),
        yaxis = list(
          title = list(text = "Percent Decrease", font = list(size = 14, color = "#2E8B57")),
          ticksuffix = "%",
          tickfont = list(size = 12, color = "#2E8B57"),
          gridcolor = 'rgba(0,0,0,0.1)',
          zeroline = TRUE,
          zerolinecolor = 'rgba(0,0,0,0.3)',
          range = c(min(dec_data$pct_change) * 1.1, 0),
          tickformat = '.1f'
        ),
        showlegend = FALSE,
        hovermode = 'closest',
        margin = list(b = 100, l = 80, r = 40, t = 100),
        plot_bgcolor = 'rgba(255,255,255,0.9)'
      )
  })
  
  
  output$category_breakdown <- renderPlotly({
    req(rv$violation_agg)
    validate(
      need(nrow(rv$violation_agg) > 0, "No data available for the selected filters")
    )
    
    # Prepare data for the treemap
    cat_data <- copy(rv$violation_agg)[
      order(-total)
    ][
      , `:=`(
        percentage = (total / sum(total)) * 100,
        violation_type = factor(violation_type, levels = violation_type)
      )
    ]
    
    # Create the treemap
    plot_ly(
      data = cat_data,
      type = 'treemap',
      labels = ~violation_type,
      parents = NA,  # Flat structure
      values = ~total,
      textinfo = "label+value+percent",
      hoverinfo = 'label+value+percent',
      marker = list(colors = viridis::viridis(nrow(cat_data)))  # Apply custom colors
    ) %>%
      layout(
        title = "Crime Category Breakdown - Treemap",
        font = list(size = 12, color = "#2E8B57")
      )
  }) %>% bindCache(input$year_range, input$geographic_level, input$province, input$city, input$crime_type)
  
  output$clearance_rates <- renderPlotly({
    req(rv$violation_agg)
    validate(
      need(nrow(rv$violation_agg) > 0, "No data available for the selected filters")
    )
    
    clearance_data <- copy(rv$violation_agg)[
      order(-clearance_rate)
    ][
      1:10
    ]
    
    plot_ly(clearance_data) %>%
      add_bars(
        x = ~reorder(violation_type, clearance_rate),
        y = ~clearance_rate,
        marker = list(color = viridis::viridis(10)),
        text = ~sprintf("%.1f%%", clearance_rate),  # Show only percentage on bars
        textposition = 'auto',
        hovertext = ~sprintf(
          "Crime Type: %s<br>Clearance Rate: %.1f%%<br>Total Incidents: %s",
          violation_type,
          clearance_rate,
          formatC(total, format="d", big.mark=",")
        ),
        hoverinfo = 'text'
      ) %>%
      layout(
        title = "Top 10 Clearance Rates by Crime Type",
        font = list(size = 12, color = "#2E8B57"),
        xaxis = list(title = "", tickangle = 45, tickfont = list(size = 12, color = "#2E8B57")),
        yaxis = list(title = "Clearance Rate (%)",tickfont = list(size = 12, color = "#2E8B57")),
        showlegend = FALSE,
        hovermode = 'closest'
      )
  }) %>% bindCache(input$year_range, input$geographic_level, input$province, input$city, input$crime_type)
  
  


  output$youth_adult_plot <- renderPlotly({
    data <- filtered_data()
    
    age_data <- data[, list(
      Youth = sum(Total_youth_charged, na.rm = TRUE),
      Adult = sum(Total_adult_charged, na.rm = TRUE)
    ), by = violation_type][
      # Calculate total and percentages for all crime types
      , `:=`(
        Total = Youth + Adult,
        Youth_Pct = (Youth / (Youth + Adult)) * 100,
        Adult_Pct = (Adult / (Youth + Adult)) * 100
      )
    ][
      Total > 0  # Only keep crime types with some charges
    ][
      order(-Total)  # Order by total charges for better visualization
    ][
      , melt(.SD, 
             id.vars = c("violation_type", "Total", "Youth", "Adult"),
             measure.vars = c("Youth_Pct", "Adult_Pct"),
             variable.name = "Age_Group",
             value.name = "Percentage"
      )
    ][
      , `:=`(
        Age_Group = gsub("_Pct", "", Age_Group),
        violation_type = factor(violation_type, levels = rev(unique(violation_type)))
      )
    ]
    
    plot_ly(
      data = age_data,
      x = ~Percentage,
      y = ~violation_type,
      color = ~Age_Group,
      colors = c("Youth" = "#fcb9aa", "Adult" = "#55cbcd"),
      type = 'bar',
      orientation = 'h',
      hovertext = ~sprintf(
        "%s<br>%s: %.1f%%<br>Total Charged: %s<br>Count: %s",
        violation_type,
        Age_Group,
        Percentage,
        formatC(Total, format="d", big.mark=","),
        formatC(ifelse(Age_Group == "Youth", Youth, Adult), format="d", big.mark=",")
      ),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = list(
          text = "Youth vs Adult Distribution by Crime Type",
          font = list(size = 16, color = "#2E8B57")
        ),
        xaxis = list(
          title = list(text = "Percentage",font = list(size = 12, color = "#2E8B57")),
          ticksuffix = "%",
          range = c(0, 100),
          gridcolor = 'rgba(0,0,0,0.1)',
          zeroline = TRUE,
          zerolinecolor = 'rgba(0,0,0,0.3)'
        ),
        yaxis = list(
          title = "",
          tickangle = 0,
          tickfont = list(size = 11,color = "#2E8B57")
        ),
        legend = list(
          title = list(text = "Age Group",font = list(size = 12, color = "#2E8B57")),
          font = list(size = 12, color = "#2E8B57"),
          x = 0.85,
          y = 1.1,
          orientation = 'h'
        ),
        barmode = 'group',
        bargap = 0.2,
        bargroupgap = 0.1,
        margin = list(l = 200, r = 40, t = 80, b = 40),
        showlegend = TRUE,
        hovermode = 'closest',
        plot_bgcolor = 'rgba(255,255,255,0.9)'
      )
  })
  
  
  output$location_comparison_plot <- renderPlotly({
    # Ensure there is data to process
    data <- filtered_data()
    validate(
      need(nrow(data) > 0, "No data available for the selected filters"),
      need(input$geographic_level != "All", "Please select a specific level (Province or City) for comparison"),
      need(
        (input$geographic_level == "Province" & input$province != "All") | 
          (input$geographic_level == "City" & input$city != "All"), 
        "Please select one or more provinces/cities for comparison"
      )
    )
    
    # Filter data for comparison
    compare_data <- data[level %in% input$geographic_level & 
                           ((input$geographic_level == "Province" & province %in% input$province) |
                              (input$geographic_level == "City" & location %in% input$city)), .(
                                Avg_Crime_Rate = mean(Rate_per_100k_People, na.rm = TRUE)
                              ), by = .(year, level, location)]
    
    # Generate the line chart
    plot_ly(
      data = compare_data,
      x = ~year,
      y = ~Avg_Crime_Rate,
      color = ~location,
      type = 'scatter',
      mode = 'lines+markers',
      text = ~paste(
        "Location:", location,
        "<br>Year:", year,
        "<br>Avg Crime Rate per 100k:", sprintf("%.2f", Avg_Crime_Rate)
      ),
      hoverinfo = "text",
      line = list(width = 2),
      marker = list(size = 8)
    ) %>%
      layout(
        title = list(
          text = paste0(
            '<b style="color: #2E8B57; font-size: 20px;">Comparative Crime Rates by Location</b>',
            '<br>',
            '<span style="color: #2E8B57; font-size: 14px;">Analysis of crime rates across selected regions over time</span>'
          ),
          font = list(family = "Arial"),
          y = 0.95
        ),
        xaxis = list(
          title = list(
            text = "Year",
            font = list(size = 14, color = "#2E8B57")),
          tickfont = list(size = 12, color = "#2E8B57")
          ),
        yaxis = list(
          title = list(
                      text = "Average Crime Rate per 100k People",
                      font = list(size = 14, color = "#2E8B57")),
          tickfont = list(size = 12, color = "#2E8B57")
          ),
        legend = list(
          title = list(
            text = "Location",
            font = list(size = 12, color = "#2E8B57")
          ),
          font = list(size = 12, color = "#2E8B57")
        ),
        hovermode = "closest",
        margin = list(t = 80)  # Increased top margin to accommodate subtitle
      )
  })
  
  
  
  output$data_table <- renderDT({
    data <- filtered_data()[, list(
      Year = year,
      Location = location,
      Region = region,
      `Crime Type` = violation_type,
      `Total Incidents` = Actual_Incidents,
      `Rate per 100k` = Rate_per_100k_People,
      `Clearance Rate` = Clearance_Rate
    )]
    
    datatable(
      data,
      options = list(
        processing = TRUE,
        pageLength = 25,
        scrollX = TRUE,
        order = list(list(4, 'desc')),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#2E8B57', 'color': '#FFFFFF'});", # Green header
          "}"
        )
      ),
      extensions = 'Buttons',
      rownames = FALSE,
      class = "display nowrap compact"
    ) %>%
      formatRound(c("Rate per 100k", "Clearance Rate"), digits = 1) %>%
      formatCurrency("Total Incidents", currency = "", digits = 0) %>%
      formatStyle(
        # Style all values to be green
        columns = c("Year", "Location", "Region", "Crime Type", "Total Incidents", "Rate per 100k", "Clearance Rate"),
        color = "rgba(46, 139, 87, 1)",  # Green text
        fontWeight = "bold"  # Optional: Make it bold
      )
  }, server = TRUE)
}

# Run the application
shinyApp(ui = ui, server = server)
