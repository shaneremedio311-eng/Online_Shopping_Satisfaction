
library(shiny)
library(ggplot2)
library(dplyr)

online_shopping <- read.csv("online_shopping_satisfaction.csv")

ui <- fluidPage(
  titlePanel("Online Shopping Frequency and Consumer Satisfaction"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filter Data"),
      
      selectInput(
        "frequency",
        "Shopping Frequency:",
        choices = c("All", sort(unique(online_shopping$Shopping_Frequency)))
      ),
      
      selectInput(
        "platform",
        "Preferred Platform:",
        choices = c("All", sort(unique(online_shopping$Preferred_Platform)))
      ),
      
      selectInput(
        "gender",
        "Gender:",
        choices = c("All", sort(unique(online_shopping$Gender)))
      ),
      
      actionButton("reset", "Reset Filters")
    ),
    
    mainPanel(
      h3("Summary"),
      
      fluidRow(
        column(6, wellPanel(
          h4("Total Consumers"),
          textOutput("total_consumers")
        )),
        column(6, wellPanel(
          h4("Average Satisfaction"),
          textOutput("average_satisfaction")
        ))
      ),
      
      fluidRow(
        column(6, wellPanel(
          h4("Average Monthly Orders"),
          textOutput("average_orders")
        )),
        column(6, wellPanel(
          h4("Average Monthly Spending"),
          textOutput("average_spending")
        ))
      ),
      
      h3("Graphs"),
      
      plotOutput("frequency_plot"),
      plotOutput("platform_plot"),
      plotOutput("satisfaction_plot"),
      
      h3("Consumer Data"),
      tableOutput("consumer_table")
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data <- online_shopping
    
    if (input$frequency != "All") {
      data <- data[data$Shopping_Frequency == input$frequency, ]
    }
    
    if (input$platform != "All") {
      data <- data[data$Preferred_Platform == input$platform, ]
    }
    
    if (input$gender != "All") {
      data <- data[data$Gender == input$gender, ]
    }
    
    data
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "frequency", selected = "All")
    updateSelectInput(session, "platform", selected = "All")
    updateSelectInput(session, "gender", selected = "All")
  })
  
  output$total_consumers <- renderText({
    nrow(filtered_data())
  })
  
  output$average_satisfaction <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) return("No data")
    round(mean(data$Overall_Satisfaction), 2)
  })
  
  output$average_orders <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) return("No data")
    round(mean(data$Monthly_Orders), 2)
  })
  
  output$average_spending <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) return("No data")
    paste0("PHP ", format(
      round(mean(data$Monthly_Spending_PHP), 2),
      big.mark = ","
    ))
  })
  
  output$frequency_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Shopping_Frequency)) +
      geom_bar(fill = "steelblue") +
      labs(
        title = "Shopping Frequency Distribution",
        x = "Shopping Frequency",
        y = "Number of Consumers"
      ) +
      theme_minimal()
  })
  
  output$platform_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Preferred_Platform)) +
      geom_bar(fill = "darkseagreen") +
      labs(
        title = "Preferred Online Shopping Platforms",
        x = "Platform",
        y = "Number of Consumers"
      ) +
      theme_minimal()
  })
  
  output$satisfaction_plot <- renderPlot({
    ggplot(filtered_data(),
           aes(x = Shopping_Frequency,
               y = Overall_Satisfaction)) +
      stat_summary(fun = mean, geom = "col",
                   fill = "coral") +
      labs(
        title = "Average Satisfaction by Shopping Frequency",
        x = "Shopping Frequency",
        y = "Average Satisfaction (1–5)"
      ) +
      theme_minimal()
  })
  
  output$consumer_table <- renderTable({
    filtered_data()
  })
}

shinyApp(ui = ui, server = server)
