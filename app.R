#
library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Monthly Expenses Tracker"),
  sidebarLayout(
    sidebarPanel(
      h3("Enter Monthly Expenses"),
      numericInput("jan", "January", value = 0, min = 0),
      numericInput("feb", "February", value = 0, min = 0),
      numericInput("mar", "March", value = 0, min = 0),
      numericInput("apr", "April", value = 0, min = 0),
      numericInput("may", "May", value = 0, min = 0),
      numericInput("jun", "June", value = 0, min = 0),
      numericInput("jul", "July", value = 0, min = 0),
      numericInput("aug", "August", value = 0, min = 0),
      numericInput("sep", "September", value = 0, min = 0),
      numericInput("oct", "October", value = 0, min = 0),
      numericInput("nov", "November", value = 0, min = 0),
      numericInput("dec", "December", value = 0, min = 0)
    ),
    mainPanel(
      h3("Total Monthly Expenses"),
      verbatimTextOutput("total"),
      plotOutput("barplot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Calculate total expenses
  totalExpenses <- reactive({
    total <- input$jan + input$feb + input$mar + input$apr + input$may + input$jun +
      input$jul + input$aug + input$sep + input$oct + input$nov + input$dec
    return(total)
  })
  
  # Output total expenses to UI
  output$total <- renderText({
    paste0("$", totalExpenses())
  })
  
  # Create bar plot of monthly expenses
  output$barplot <- renderPlot({
    expenses <- c(input$jan, input$feb, input$mar, input$apr, input$may, input$jun,
                  input$jul, input$aug, input$sep, input$oct, input$nov, input$dec)
    months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    data <- data.frame(expenses, months)
    ggplot(data, aes(factor(months, months),expenses,fill=months)) + 
      theme(legend.position="none")+
      geom_bar(stat = "identity") +
      labs(x = "Month", y = "Total Expenses", title = "Monthly Expenses Tracker")
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
