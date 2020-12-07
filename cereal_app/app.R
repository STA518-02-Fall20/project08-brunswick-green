# Load packages ----
library(shiny)
library(tidyverse)
library(ggplot2)

# Load data ----
cereal <- read_csv("data/cereal.csv")


ui <- basicPage(
    # Give the page a title
    titlePanel("Cereal Information"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
        
        # Define the sidebar with one input
        sidebarPanel(
            selectInput("column", "Column:", 
                        choices=colnames(cereal)),
            hr(),
            helpText("Data from US Cereal Dataset.")
        ),
        
        # Create a spot for the barplot
        mainPanel(
            plotOutput("plot1", click = "plot_click"),
            verbatimTextOutput("info")
        )
        
    )
)  


server <- function(input, output) {
    output$plot1 <- renderPlot({
        barplot(cereal$fat, cereal$protein)
    })
    
    output$info <- renderPrint({
        # With base graphics, need to tell it what the x and y variables are.
        nearPoints(cereal, input$plot_click, xvar = "wt", yvar = "mpg")
        # nearPoints() also works with hover and dblclick events
    })
}

shinyApp(ui, server)