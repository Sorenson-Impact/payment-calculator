library(shiny)
library(tidyverse)
library(scales)
# library(readr)



# Define UI for slider demo app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Monthly Co-Pay Calculator"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      
    
      # Number ===================================================
      # Change the value
      numericInput("inNumber", "Montly Income:",
                   min = 1, max = 6700, value = 2500, step = 1),
      
      # Number ===================================================
      # Change the value
      numericInput("fNumber", "Children in Household:",
                   min = 1, max = 30, value = 1, step = 1)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table summarizing the values entered ----
      textOutput("values")
      
    )
  )
)

# Define server logic for slider examples ----
server <- function(input, output) {
  
  fam_data <- read_csv("./fam_data.csv")
  
  
  test <- reactive({
    
    cost_vec <- pull(fam_data[,2])
    
    col_index <- if_else(input$fNumber <= 2, 3,
                         if_else(input$fNumber >= 8, 8, input$fNumber + 1))
    
    # browser()
    
    col <- pull(fam_data[,col_index])
    
    row_index <- sum(if_else(input$inNumber > col, TRUE, FALSE), na.rm = T) + 1
    
    return(cost_vec[row_index])
    
  }
  )
  
  # Show the values in an HTML table ----
  output$values <- renderText({
    paste0("With ", input$fNumber, " children and a monthly income of ", dollar(input$inNumber), ", your estimated monthly payment is ", dollar(test()), ".")
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)