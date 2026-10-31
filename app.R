library(shiny)
library(ggplot2)
library(quantmod)

# Step 1: Data Collection and Setup (Modified for Rakeshkumar)
stock_symbol <- "MSFT"
start_date <- "2023-02-01"
end_date <- "2023-08-01"

stock_data <- getSymbols(stock_symbol, src = "yahoo", 
                         from = start_date, to = end_date, 
                         auto.assign = FALSE)

# Define User Interface (UI)
ui <- fluidPage(
  titlePanel("Microsoft Portfolio Stock Visualization Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # Widget 1: Select Date Range
      dateRangeInput("date_range", "Select Date Range:", 
                     start = "2023-02-01", end = "2023-08-01"),
      
      # Widget 2: Select Time Frame
      selectInput("time_frame", "Select Time Frame:", 
                  choices = c("Daily", "Weekly", "Monthly")),
      
      # Widget 3: Technical Indicators Checkbox
      checkboxGroupInput("technical_indicators", "Select Technical Indicators:",
                         choices = c("Moving Averages", "RSI", "MACD"))
    ),
    
    mainPanel(
      # Output: Display the stock chart
      plotOutput("stock_chart", height = "500px")
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  
  output$stock_chart <- renderPlot({
    # Filter data based on user input date range
    filtered_data <- subset(stock_data, 
                            index(stock_data) >= input$date_range[1] & 
                              index(stock_data) <= input$date_range[2])
    
    # Calculate Moving Averages for MSFT
    short_ma <- SMA(Cl(filtered_data), n = 20)
    long_ma <- SMA(Cl(filtered_data), n = 50)
    
    # Implement Trading Rules safely with NA handling
    signals <- rep("Hold", nrow(filtered_data))
    valid_idx <- which(!is.na(short_ma) & !is.na(long_ma))
    if(length(valid_idx) > 0) {
      signals[valid_idx] <- ifelse(short_ma[valid_idx] > long_ma[valid_idx], "Buy", 
                                   ifelse(short_ma[valid_idx] < long_ma[valid_idx], "Sell", "Hold"))
    }
    
    # Create data frame for ggplot
    df <- data.frame(
      Date = index(filtered_data),
      Close = as.numeric(Cl(filtered_data)),
      ShortMA = as.numeric(short_ma),
      LongMA = as.numeric(long_ma),
      Signal = signals
    )
    
    # Base ggplot object with distinct color (purple)
    p <- ggplot(df, aes(x = Date, y = Close)) +
      geom_line(color = "purple", size = 1) +
      labs(title = paste("Portfolio Performance -", stock_symbol),
           x = "Trading Date", y = "Closing Price") +
      theme_minimal()
    
    # Overlay Technical Indicators & Annotate Signals safely
    if (!is.null(input$technical_indicators) && "Moving Averages" %in% input$technical_indicators) {
      p <- p + geom_line(aes(y = ShortMA), color = "darkorange", size = 0.8) +
        geom_line(aes(y = LongMA), color = "firebrick", size = 0.8)
      
      # Safe Annotation using filtered subset
      sig_df <- subset(df, Signal != "Hold")
      if(nrow(sig_df) > 0) {
        p <- p + geom_text(data = sig_df, aes(y = Close, label = Signal), 
                           vjust = -1, color = "forestgreen", size = 3)
      }
    }
    
    # Render the plot
    print(p)
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)