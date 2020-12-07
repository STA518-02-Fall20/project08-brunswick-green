# Load packages ----
library(shiny)
library(tidyverse)
library(ggplot2)

# Load data ----
cereal <- read_csv("data/cereal.csv")

#Renaming manufacturer names
cereal1 <- cereal %>%
    mutate(mfr = str_replace_all(mfr, c("A" = "American Home Food Products")),
           mfr = str_replace_all(mfr, c("G" = "General Mills")),
           mfr = str_replace_all(mfr, c("K" = "Kellogs")),
           mfr = str_replace_all(mfr, c("N" = "Nabisco")),
           mfr = str_replace_all(mfr, c("P" = "Post")),
           mfr = str_replace_all(mfr, c("Q" = "Quaker Oats")),
           mfr = str_replace_all(mfr, c("R" = "Ralston Purina")),
           type = str_replace_all(type, c("H" = "Hot")),
           type = str_replace_all(type, c("C" = "Cold")))



ui <- fluidPage(    
    
    # Give the page a title
    titlePanel("U.S. Cereal Data"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
        
        # Define the sidebar with one input
        sidebarPanel(
            selectInput("variable", "Nutritional Facts:",
                        c("Calories (per serving)" = "calories",
                          "Shelf" = "shelf",
                          "Type" = "type",
                          "Manufacturer" = "mfr",
                          "Protein (g)" = "protein",
                          "Fat (g)" = "fat",
                          "Sodium (mg)" = "sodium",
                          "Fiber (g)" = "fiber",
                          "Carbohydrates (g)" = "carbo",
                          "Sugars (g)" = "sugars",
                          "Potassium (mg)" = "potass",
                          "Vitamins and Minerals (%)" = "vitamins",
                          "Weight (oz per serving)" = "weight",
                          "Cups (per serving)" = "cups",
                          "Rating" = "rating")),
            
            hr(),
            helpText("Use the dropdown to view nutritional value on U.S. Cereals.")
        ,
        
        # Input: Slider for the number of bins ----
        sliderInput(inputId = "bins",
                    label = "Number of bins:",
                    min = 1,
                    max = 50,
                    value = 30)
        
        ),
        
        # Create a spot for the table
        mainPanel(
            plotOutput("distPlot"),
            tableOutput("cerealTable")  
        )
        
    )
)
server <- function(input, output) {
    myPlot = reactiveVal()
    
    myData = reactive({
        input$variable
        data = data.frame(x = 1:10, y = runif(10))
        myPlot(ggplot(data, aes(x = mfr, y = input$variable)) + geom_point())
        data
    })
    
    output$distPlot <- renderPlot({
        x    <- cereal1$calories
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        hist(x, breaks = bins, col = "#75AADB", border = "white",
             xlab = "",
             main = "Histogram of ")
        
    })
    
    output$cerealTable <- renderTable({
        cereal1[, c("name", input$variable), drop = FALSE]
    }, rownames = TRUE)
}

shinyApp(ui, server)