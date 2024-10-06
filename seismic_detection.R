library(shiny)
library(ggplot2)
library(dplyr)

# Sample data generator function for real-time data
generate_data <- function(n = 200) {
  cumsum(rnorm(n))  # Cumulative sum of random values to simulate time series data
}

# Moving average function
moving_average <- function(data, window_size) {
  stats::filter(data, rep(1/window_size, window_size), sides = 1)
}

ui <- fluidPage(
  titlePanel("Real-Time STA and LTA Plot with Prediction"),
  sidebarLayout(
    sidebarPanel(
      numericInput("sta_length", "Short-Term Average Length:", 5, min = 1, max = 50),
      numericInput("lta_length", "Long-Term Average Length:", 20, min = 1, max = 100),
      actionButton("refresh", "Refresh Data")
    ),
    mainPanel(
      plotOutput("plot"),
      textOutput("difference"),
      textOutput("prediction")
    )
  )
)

server <- function(input, output, session) {
  # Reactive data that refreshes periodically for real-time simulation
  data <- reactiveVal(generate_data(200))
  
  observe({
    invalidateLater(1000, session)  # Refresh data every 1000 milliseconds (1 second)
    data(generate_data(200))        # Generate new data each second
  })
  
  output$plot <- renderPlot({
    req(data())
    
    # Calculating STA and LTA
    sta <- moving_average(data(), input$sta_length)
    lta <- moving_average(data(), input$lta_length)
    
    # Align lengths of STA and LTA with the original data length
    n <- length(data())
    sta <- c(rep(NA, input$sta_length - 1), sta)[1:n]
    lta <- c(rep(NA, input$lta_length - 1), lta)[1:n]
    
    # Create a data frame for plotting
    df <- data.frame(
      x = 1:n,
      data = data(),
      STA = sta,
      LTA = lta
    )
    
    # Plotting data, STA, and LTA
    ggplot(df, aes(x = x)) +
      geom_line(aes(y = data), color = "gray", alpha = 0.5, linetype = "dotted") +
      geom_line(aes(y = STA), color = "blue") +
      geom_line(aes(y = LTA), color = "red") +
      labs(title = "Real-Time STA and LTA Plot", y = "Value", x = "Time") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  output$difference <- renderText({
    req(data())
    
    # Calculating STA and LTA
    sta <- moving_average(data(), input$sta_length)
    lta <- moving_average(data(), input$lta_length)
    
    # Align lengths of STA and LTA with the original data length
    n <- length(data())
    sta <- c(rep(NA, input$sta_length - 1), sta)[1:n]
    lta <- c(rep(NA, input$lta_length - 1), lta)[1:n]
    
    # Difference calculation (only where both STA and LTA are defined)
    valid_range <- !is.na(sta) & !is.na(lta)
    diff <- mean(sta[valid_range] - lta[valid_range], na.rm = TRUE)
    
    paste("Average Difference between STA and LTA:", round(diff, 2))
  })
  
  output$prediction <- renderText({
    req(data())
    
    # Calculating STA and LTA
    sta <- moving_average(data(), input$sta_length)
    lta <- moving_average(data(), input$lta_length)
    
    # Align lengths of STA and LTA with the original data length
    n <- length(data())
    sta <- c(rep(NA, input$sta_length - 1), sta)[1:n]
    lta <- c(rep(NA, input$lta_length - 1), lta)[1:n]
    
    # Prediction based on STA/LTA comparison
    if (tail(sta, 1) > tail(lta, 1) * 1.2) {
      "Prediction: Event Detected (STA significantly higher than LTA)"
    } else {
      "Prediction: Normal Condition"
    }
  })
}

shinyApp(ui, server)n