# creating a shiny dashboard for the banknifty data 
# we start of by creating a UX
# therefore create a function 

# for the dash board we will have the followng :

# 1) date (we want to change from date, to week, day, and time) 
# 2) we want the important covariates like opening and range HL
# 3) We want to have these fpr each of the types


#Load packages

library(tidyverse)
library(shiny)

#Create shiny aspp folder for this processess

shiny::runApp()

# UI
ui <- fluidPage(
  titlePanel("BankNifty Ordinal Logistic Regression"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload BankNifty_data - Copy.xlsx", accept = ".csv"),
      actionButton("run_model", "Run Model"),
      hr(),
      h4("Predict on new values:"),
      numericInput("open", "Open Price", value = 45000),
      numericInput("range_HL", "Range HL", value = 500),
      numericInput("weekday", "Weekday (0 = Mon, 5 = Fri)", value = 1, min = 0, max = 5),
      numericInput("time", "Time (numeric)", value = 7000),
      actionButton("predict", "Predict Market Type")
    ),
    
    mainPanel(
      verbatimTextOutput("model_summary"),
      verbatimTextOutput("prediction"),
      tableOutput("sample_data")
    )
  )
)

# Server ----
server <- function(input, output, session) {
  # Reactive value to store the dataset and model
  data_reactive <- reactiveVal(NULL)
  model_reactive <- reactiveVal(NULL)
  
  # Load and preprocess data
  observeEvent(input$file, {
    req(input$file)
    df <- read.csv(input$file$datapath)
    
    # Ensure correct types
    df$type <- factor(df$type, levels = c("Bearish", "Tie", "Bullish"), ordered = TRUE)
    
    # Standardize selected columns
    df <- df %>%
      mutate(
        open = scale(open)[, 1],
        range_HL = scale(range_HL)[, 1],
        time = scale(time)[, 1],
        weekday = scale(weekday)[, 1]
      )
    
    data_reactive(df)
    output$sample_data <- renderTable(head(df))
  })
  
  # Fit the model
  observeEvent(input$run_model, {
    req(data_reactive())
    df <- data_reactive()
    
    model <- polr(type ~ open + range_HL + weekday + time, data = df, method = "logistic")
    model_reactive(model)
    
    output$model_summary <- renderPrint(summary(model))
  })
  
  # Make prediction based on user input
  observeEvent(input$predict, {
    req(model_reactive())
    model <- model_reactive()
    
    # Standardize user input using mean and sd of training data
    df <- data_reactive()
    
    newdata <- data.frame(
      open = (input$open - mean(df$open)) / sd(df$open),
      range_HL = (input$range_HL - mean(df$range_HL)) / sd(df$range_HL),
      weekday = (input$weekday - mean(df$weekday)) / sd(df$weekday),
      time = (input$time - mean(df$time)) / sd(df$time)
    )
    
    pred <- predict(model, newdata = newdata, type = "class")
    
    output$prediction <- renderPrint({
      paste("Predicted market type:", pred)
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


