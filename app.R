# Load required packages
library(shiny)
library(tidyverse)
library(ggplot2)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("TA36 Data Analysis App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose TA36 CSV File",
                accept = c(".csv")),
      fileInput("file2", "Choose Summer Tomama CSV File",
                accept = c(".csv")),
      tags$hr(),
      checkboxInput("header1", "TA36 Header", TRUE),
      checkboxInput("header2", "Summer Tomama Header", TRUE),
      radioButtons("sep1", "TA36 Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      radioButtons("sep2", "Summer Tomama Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      actionButton("process", "Process Data"),
      tags$hr(),
      downloadButton("downloadData", "Download Processed Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data", DTOutput("contents")),
        tabPanel("Plots",
                 plotOutput("plot1"),
                 plotOutput("plot2"),
                 plotOutput("plot3"),
                 plotOutput("plot4"),
                 plotOutput("plot5")
        ),
        tabPanel("Summary",
                 verbatimTextOutput("summary1"),
                 verbatimTextOutput("summary2")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive expressions to read the CSV files
  ta36_data <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, header = input$header1, sep = input$sep1)
  })
  
  summer_tomama_data <- reactive({
    req(input$file2)
    read.csv(input$file2$datapath, header = input$header2, sep = input$sep2)
  })
  
  # Process the data when the button is clicked
  processedData <- eventReactive(input$process, {
    ta36_df <- ta36_data()
    summer_tomama_df <- summer_tomama_data()
    
    df_longer <- ta36_df %>%
      pivot_longer(c(arbascular, vesicle, hyphea), names_to = "class", values_to = "value")
    
    df_combined <- df_longer %>%
      left_join(summer_tomama_df %>% dplyr::select(enveolop, TA36), by = c("tube_num" = "TA36"))
    
    df_notrap <- df_combined %>%
      filter(!str_detect(enveolop, "TRAP"))
    
    df_notrapa <- df_notrap %>%
      mutate(percent = value / total_intersection * 100) %>%
      unite("plant_name", c(exp, treatment1, tube_num, rep))
    
    df_planty <- df_notrapa %>%
      group_by(plant_name, class, enveolop) %>%
      summarise(average = mean(percent), .groups = 'drop')
    
    list(
      df_longer = df_longer,
      df_combined = df_combined,
      df_notrap = df_notrap,
      df_notrapa = df_notrapa,
      df_planty = df_planty
    )
  })
  
  # Display the data
  output$contents <- renderDT({
    req(input$process)
    datatable(ta36_data())
  })
  
  # Generate plots
  output$plot1 <- renderPlot({
    req(input$process)
    ggplot(ta36_data(), aes(x = treatment1, y = arbascular)) + geom_boxplot() +
      ggtitle("Boxplot of Arbuscular")
  })
  
  output$plot2 <- renderPlot({
    req(input$process)
    ggplot(ta36_data(), aes(x = treatment1, y = vesicle)) + geom_boxplot() +
      ggtitle("Boxplot of Vesicle")
  })
  
  output$plot3 <- renderPlot({
    req(input$process)
    ggplot(ta36_data(), aes(x = treatment1, y = hyphea)) + geom_boxplot() +
      ggtitle("Boxplot of Hyphea")
  })
  
  output$plot4 <- renderPlot({
    req(input$process)
    processedData()$df_notrapa %>%
      ggplot(aes(x = percent, y = enveolop)) + 
      geom_boxplot(aes(color = class)) + 
      geom_jitter(aes(color = class)) + 
      facet_grid(. ~ class) + 
      theme(text = element_text(size = 18), element_line(size = 2)) +
      ggtitle("Boxplot of Percent by Enveolop")
  })
  
  output$plot5 <- renderPlot({
    req(input$process)
    processedData()$df_planty %>%
      ggplot(aes(x = average, y = enveolop)) +
      geom_boxplot(aes(color = class)) +
      geom_jitter(aes(color = class)) +
      facet_grid(. ~ class) +
      theme(text = element_text(size = 18), element_line(size = 2)) +
      ggtitle("Average Percent by Enveolop")
  })
  
  # Summary statistics
  output$summary1 <- renderPrint({
    req(input$process)
    processedData()$df_notrapa %>% count(class, enveolop)
  })
  
  output$summary2 <- renderPrint({
    req(input$process)
    processedData()$df_planty %>%
      filter(class == "arbascular") %>%
      group_by(enveolop) %>%
      count(enveolop)
  })
  
  # Download processed data
  output$downloadData <- downloadHandler(
    filename = function() { paste("processed_data", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      write.csv(processedData()$df_planty, file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
