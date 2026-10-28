
# ============================================================
# WEATHER ANALYSIS AND TEMPERATURE PREDICTION
# R Shiny Mini Project with Custom CSS
# ============================================================

# Install required packages once if needed:
# install.packages(c("shiny", "ggplot2", "dplyr"))

library(shiny)
library(ggplot2)
library(dplyr)

# ------------------------------------------------------------
# 1. SAMPLE WEATHER DATA
# ------------------------------------------------------------

set.seed(123)

weather_data <- data.frame(
  Date = seq.Date(
    from = as.Date("2026-01-01"),
    by = "day",
    length.out = 120
  ),
  City = sample(
    c("Delhi", "Mumbai", "Ludhiana", "Chandigarh", "Jaipur"),
    120,
    replace = TRUE
  ),
  Humidity = sample(25:95, 120, replace = TRUE),
  WindSpeed = round(runif(120, 2, 30), 1)
)

# Generate realistic illustrative temperature values
weather_data$Temperature <- round(
  24 +
    9 * sin(2 * pi * as.numeric(format(weather_data$Date, "%j")) / 365) +
    0.08 * weather_data$Humidity -
    0.12 * weather_data$WindSpeed +
    rnorm(120, 0, 2.5),
  1
)

weather_data$Condition <- ifelse(
  weather_data$Humidity > 78,
  "Cloudy",
  ifelse(
    weather_data$Temperature > 28,
    "Sunny",
    "Clear"
  )
)

# ------------------------------------------------------------
# 2. USER INTERFACE
# ------------------------------------------------------------

ui <- fluidPage(
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
      body {
        background: #f1f5fb;
        font-family: 'Segoe UI', Arial, sans-serif;
        color: #1c2d45;
      }

      .container-fluid {
        padding: 24px;
      }

      .weather-header {
        background: linear-gradient(135deg, #102a56, #1769aa, #36b6c9);
        color: white;
        padding: 32px;
        border-radius: 20px;
        margin-bottom: 25px;
        box-shadow: 0 10px 30px rgba(23, 70, 130, 0.18);
      }

      .weather-header h1 {
        font-size: 32px;
        font-weight: 800;
        margin-top: 0;
      }

      .weather-header p {
        font-size: 15px;
        line-height: 1.7;
      }

      .weather-card {
        background: white;
        padding: 22px;
        border-radius: 16px;
        margin-bottom: 22px;
        border: 1px solid #e2eaf5;
        box-shadow: 0 5px 18px rgba(25, 50, 90, 0.06);
        transition: 0.25s ease;
        min-height: 135px;
      }

      .weather-card:hover {
        transform: translateY(-4px);
        box-shadow: 0 10px 25px rgba(25, 50, 90, 0.12);
      }

      .weather-card h4 {
        color: #62748b;
        font-size: 14px;
        font-weight: 600;
      }

      .weather-card h2 {
        color: #1769aa;
        font-size: 29px;
        font-weight: 800;
        margin: 10px 0;
      }

      .panel {
        background: white;
        border: 1px solid #e2eaf5;
        border-radius: 17px;
        padding: 22px;
        margin-bottom: 24px;
        box-shadow: 0 5px 18px rgba(25, 50, 90, 0.05);
      }

      .panel h3 {
        color: #173f70;
        font-size: 21px;
        font-weight: 750;
        margin-top: 0;
        margin-bottom: 20px;
      }

      .sidebar-panel {
        background: white;
        padding: 20px;
        border: 1px solid #e2eaf5;
        border-radius: 16px;
        margin-bottom: 20px;
      }

      .btn-primary {
        background: linear-gradient(135deg, #1769aa, #36a6c8) !important;
        border: none !important;
        border-radius: 10px !important;
        padding: 11px 18px !important;
        font-weight: 700 !important;
        color: white !important;
      }

      .btn-primary:hover {
        filter: brightness(1.08);
      }

      .form-control {
        border: 1px solid #d5e0ef;
        border-radius: 9px;
        min-height: 40px;
        box-shadow: none;
      }

      .form-control:focus {
        border-color: #319ac4;
        box-shadow: 0 0 0 3px rgba(49, 154, 196, 0.12);
      }

      label {
        color: #344c6b;
        font-weight: 600;
      }

      .footer {
        text-align: center;
        padding: 20px;
        color: #718198;
        font-size: 13px;
      }

      @media (max-width: 768px) {
        .container-fluid {
          padding: 12px;
        }

        .weather-header {
          padding: 22px;
        }

        .weather-header h1 {
          font-size: 25px;
        }

        .weather-card h2 {
          font-size: 24px;
        }
      }
    "))
  ),
  
  # Dashboard header
  div(
    class = "weather-header",
    h1("☁️ Weather Analytics Pro"),
    p("Smart Weather Analysis & Temperature Prediction"),
    p("Explore weather patterns, compare cities, and estimate future temperatures."),
    tags$span("R SHINY MINI PROJECT  |  DATA ANALYTICS")
  ),
  
  # Filters and dashboard content
  fluidRow(
    
    # Sidebar
    column(
      width = 3,
      
      div(
        class = "sidebar-panel",
        h3("⚙️ Dashboard Filters"),
        
        selectInput(
          "city",
          "Select City",
          choices = c("All Cities", sort(unique(weather_data$City))),
          selected = "All Cities"
        ),
        
        selectInput(
          "condition",
          "Weather Condition",
          choices = c("All Conditions", "Sunny", "Clear", "Cloudy"),
          selected = "All Conditions"
        ),
        
        dateRangeInput(
          "dates",
          "Select Date Range",
          start = min(weather_data$Date),
          end = max(weather_data$Date),
          min = min(weather_data$Date),
          max = max(weather_data$Date),
          format = "yyyy-mm-dd"
        ),
        
        actionButton(
          "reset",
          "Reset Filters",
          class = "btn-primary",
          width = "100%"
        )
      ),
      
      div(
        class = "sidebar-panel",
        h3("🔮 Temperature Prediction"),
        
        numericInput(
          "humidity",
          "Humidity (%)",
          value = 60,
          min = 0,
          max = 100
        ),
        
        numericInput(
          "wind",
          "Wind Speed (km/h)",
          value = 10,
          min = 0,
          max = 150
        ),
        
        actionButton(
          "predict",
          "Predict Temperature",
          class = "btn-primary",
          width = "100%"
        ),
        
        br(),
        br(),
        
        h4("Predicted Temperature"),
        uiOutput("prediction")
      )
    ),
    
    # Main dashboard
    column(
      width = 9,
      
      # Summary cards
      fluidRow(
        column(
          4,
          div(
            class = "weather-card",
            h4("🌡️ Average Temperature"),
            h2(textOutput("avg_temp", inline = TRUE)),
            p("Selected records")
          )
        ),
        
        column(
          4,
          div(
            class = "weather-card",
            h4("🔥 Maximum Temperature"),
            h2(textOutput("max_temp", inline = TRUE)),
            p("Highest recorded value")
          )
        ),
        
        column(
          4,
          div(
            class = "weather-card",
            h4("💧 Average Humidity"),
            h2(textOutput("avg_humidity", inline = TRUE)),
            p("Relative humidity")
          )
        )
      ),
      
      # Temperature trend chart
      div(
        class = "panel",
        h3("📈 Temperature Trend"),
        plotOutput("temperature_plot", height = "320px")
      ),
      
      # Condition and humidity charts
      fluidRow(
        column(
          6,
          div(
            class = "panel",
            h3("🌤️ Weather Conditions"),
            plotOutput("condition_plot", height = "280px")
          )
        ),
        
        column(
          6,
          div(
            class = "panel",
            h3("💧 Humidity Analysis"),
            plotOutput("humidity_plot", height = "280px")
          )
        )
      ),
      
      # Data table
      div(
        class = "panel",
        h3("📋 Weather Records"),
        tableOutput("weather_table")
      )
    )
  ),
  
  div(
    class = "footer",
    "Weather Analytics Pro | Built with R, Shiny, ggplot2 and CSS"
  )
)

# ------------------------------------------------------------
# 3. SERVER LOGIC
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  # Reset dashboard filters
  observeEvent(input$reset, {
    updateSelectInput(
      session, "city",
      selected = "All Cities"
    )
    
    updateSelectInput(
      session, "condition",
      selected = "All Conditions"
    )
    
    updateDateRangeInput(
      session, "dates",
      start = min(weather_data$Date),
      end = max(weather_data$Date)
    )
  })
  
  # Filter weather data
  filtered_data <- reactive({
    
    req(input$dates)
    
    df <- weather_data %>%
      filter(
        Date >= input$dates[1],
        Date <= input$dates[2]
      )
    
    if (input$city != "All Cities") {
      df <- df %>% filter(City == input$city)
    }
    
    if (input$condition != "All Conditions") {
      df <- df %>% filter(Condition == input$condition)
    }
    
    df
  })
  
  # Summary metrics
  output$avg_temp <- renderText({
    df <- filtered_data()
    req(nrow(df) > 0)
    paste0(round(mean(df$Temperature), 1), " °C")
  })
  
  output$max_temp <- renderText({
    df <- filtered_data()
    req(nrow(df) > 0)
    paste0(round(max(df$Temperature), 1), " °C")
  })
  
  output$avg_humidity <- renderText({
    df <- filtered_data()
    req(nrow(df) > 0)
    paste0(round(mean(df$Humidity), 1), "%")
  })
  
  # Temperature trend chart
  output$temperature_plot <- renderPlot({
    
    df <- filtered_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = Date, y = Temperature)) +
      geom_line(
        color = "#1687c9",
        linewidth = 1.1
      ) +
      geom_point(
        color = "#0c4778",
        size = 2,
        alpha = 0.7
      ) +
      labs(
        x = "Date",
        y = "Temperature (°C)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.background = element_rect(
          fill = "white", color = NA
        ),
        panel.grid.minor = element_blank(),
        axis.title = element_text(
          color = "#425873",
          face = "bold"
        )
      )
  })
  
  # Weather condition chart
  output$condition_plot <- renderPlot({
    
    df <- filtered_data()
    req(nrow(df) > 0)
    
    counts <- df %>%
      count(Condition)
    
    ggplot(
      counts,
      aes(
        x = reorder(Condition, -n),
        y = n,
        fill = Condition
      )
    ) +
      geom_col(width = 0.65) +
      scale_fill_manual(
        values = c(
          "Sunny" = "#f4b942",
          "Clear" = "#36a6c8",
          "Cloudy" = "#7186a8"
        )
      ) +
      labs(
        x = "Condition",
        y = "Number of Records"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "none",
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
  })
  
  # Humidity analysis chart
  output$humidity_plot <- renderPlot({
    
    df <- filtered_data()
    req(nrow(df) > 0)
    
    ggplot(
      df,
      aes(x = Humidity)
    ) +
      geom_histogram(
        bins = 12,
        fill = "#36a6c8",
        color = "white",
        linewidth = 0.6
      ) +
      labs(
        x = "Humidity (%)",
        y = "Frequency"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
  })
  
  # Weather data table
  output$weather_table <- renderTable({
    
    df <- filtered_data()
    req(nrow(df) > 0)
    
    df %>%
      arrange(desc(Date)) %>%
      mutate(Date = format(Date, "%d %b %Y")) %>%
      head(15)
  }, striped = TRUE, bordered = FALSE, hover = TRUE)
  
  # Linear regression prediction model
  # Model is trained using illustrative sample data.
  prediction_model <- lm(
    Temperature ~ Humidity + WindSpeed,
    data = weather_data
  )
  
  predicted_value <- eventReactive(input$predict, {
    
    req(
      !is.null(input$humidity),
      !is.null(input$wind)
    )
    
    validate(
      need(
        input$humidity >= 0 && input$humidity <= 100,
        "Humidity must be between 0 and 100."
      ),
      need(
        input$wind >= 0 && input$wind <= 150,
        "Wind speed must be between 0 and 150 km/h."
      )
    )
    
    new_data <- data.frame(
      Humidity = input$humidity,
      WindSpeed = input$wind
    )
    
    as.numeric(
      predict(prediction_model, newdata = new_data)
    )
  })
  
  output$prediction <- renderUI({
    
    if (input$predict == 0) {
      return(
        tags$p(
          "Enter weather values and click Predict.",
          style = "color:#718198;"
        )
      )
    }
    
    value <- tryCatch(
      predicted_value(),
      error = function(e) NA_real_
    )
    
    if (length(value) != 1 || !is.finite(value)) {
      return(
        tags$p(
          "Prediction unavailable. Check your inputs.",
          style = "color:#c0392b;"
        )
      )
    }
    
    div(
      style = paste(
        "background:#eaf6ff;",
        "padding:18px;",
        "border-radius:12px;",
        "border:1px solid #cce8fa;"
      ),
      h2(
        paste0(round(value, 1), " °C"),
        style = "color:#1769aa;font-weight:800;"
      ),
      p(
        "Estimated by a linear regression model",
        style = "color:#536b85;"
      )
    )
  })
}

# ------------------------------------------------------------
# 4. RUN APPLICATION
# ------------------------------------------------------------

shinyApp(ui = ui, server = server)
