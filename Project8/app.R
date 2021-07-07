# Load packages 
library(shiny)
library(tidyverse)
library(rsconnect)

# Load data 
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
           type = str_replace_all(type, c("C" = "Cold"))) %>%
    rename("manufacturer" = mfr)



ui <- fluidPage(    
    
    # Giving the page a title
    titlePanel("R Shiny App"),
    #Giving the page a subtitle
    strong(div(em(p("of U.S. Cereal Data")), style = "color:maroon")),
    
    # Generate a row with a sidebar
    sidebarLayout(      
        
        # Define the sidebar with inputs
        sidebarPanel(
            #Header for table
            h3("Table:"),
            #Table header subtitle
            p("Use the dropdowns below to view information on U.S. Cereals in the table."),
            #Input for first column choice
            selectInput("table1", "Nutritional Facts:",
                        c("Calories (per serving)" = "calories",
                          "Protein (g)" = "protein",
                          "Fat (g)" = "fat",
                          "Sodium (mg)" = "sodium",
                          "Fiber (g)" = "fiber",
                          "Carbohydrates (g)" = "carbo",
                          "Sugars (g)" = "sugars",
                          "Potassium (mg)" = "potass",
                          "Vitamins and Minerals (%)" = "vitamins")),
            #Input for second column choice
            selectInput("table2", "Quality:",
                        c("Manufacturer" = "manufacturer",
                          "Type" = "type")),
            #Input for third column choice
            selectInput("table3", "Measures (per serving):",
                        c("Weight (oz)" = "weight",
                          "Cups" = "cups")),
            #Input for fourth column choice
            selectInput("table4", "Ranking:",
                        c("Rating" = "rating",
                          "Shelf Level" = "shelf")),
            #Help Text for shelf level
            helpText("Note: shelf level is display height ",
                     strong("(1, 2, 3)"), 
                     "counting from the floor. Rating goes from",
                    strong("0 - 100"),
                    "with 100 being the most healthy."),
            
            #Number of observations
            numericInput(inputId = "obs",
                         label = "Number of observations:",
                         min = 1,
                         max = 77,
                         value = 10),
            #Noting the maximum number of observations
            helpText("Note: The maximum number of observations is", 
            strong("77.")),
            
            #Header for graph
            h3("Graph:"),
            #Graph subtitle
            p("Analyze the graph to the bottom right of this webpage to make comparisons with the Table on U.S.
              Cereals."),
            br(),
            
            # Cereal Photo
            img(src = "cereal2.jpg", height = 180, width = 230),
            helpText("This photo shows that the healthier cereal (Cheerios) is displayed on shelf 1 while
                     the high-sugar cereals (Cap'n Crunch, Cocoa Puffs, Lucky Charms, etc.) are displayed
                     on shelves 2 and 3. This photo was found",
              a("here.", 
                href = "http://casualkitchen.blogspot.com/2009/04/just-say-no-to-overpriced-boxed-cereal.html")),
        ),
        
        # MAIN PANEL
        mainPanel(
            #Center Header
            h1("U.S. Cereal", align = "center"),
            h3(div(em("Nutritional Facts and Advertising Strategy")), align = "center"),
            #Creators
            strong(div(em(p("GVSU STA 518 02 ~ Team brunswick-green: Ian Fetterman and Jessica Tagudar", 
                            align = "center"))), style = "color:maroon"),
            # Kaggle Dataset Source
            p("This dataset was found on ",
              a("Kaggle.", 
                href = "https://www.kaggle.com/crawford/80-cereals?select=cereal.csv"),
            #Github source
            "For the R code, visit our ",
            a("Github repository.", 
              href = "https://github.com/STA518-02-Fall20/project08-brunswick-green")),
            # User Help Text
            strong("We all have that sugary cereal that we love. Sometimes, it ends up in our carts when it was
            never even on the grocery list. Why is this? There are many ways that U.S. cereals are specifically 
            advertised based on their nutritional value, quality, measures per serving, and ranking."),
            br(),
            # Research Questions
            h5("Questions to consider:", align = "center"),
            p("How do these factors create differences between U.S. cereals? For example,
            do the nutritional facts have an influence on the shelf display level? How does that affect their rating? 
            Do the nutritional facts (eg. sugars, calories) alter your opinion on your favorite cereal?"),
            em("Use the dropdowns on the left to explore these possibilities and 
            view different frequencies on the table. Be sure to adjust the 
            number of observations to expand your findings, as needed."),
            br(),
            br(),
            #Table 
            strong(div(p("Table on U.S. Cereals", align = "center"))),
            tableOutput("cerealTable"),  
            textOutput("textTable"),
            br(),
            # Directing audience towards an analysis
            p(strong("Try this:"),
                em("Choose Sugars (g) from the Nutritional Facts drop-down, Shelf Level from the 
                    Ranking drop-down, and change the Number of Observations to 20. What do you notice?")),
            helpText("In this set of 20 observations, there are 6 cereals on the 2nd shelf and they all have
            at least 9 grams of sugar. It appears that high sugar cereals are frequently 
            displayed on the 2nd shelf, which is eye-level for many consumers. What a great adverstising strategy!",
                     style = "color:gray"),
            br(),
            plotOutput("cerealPlot"),
            textOutput("textGraph"),
            br(),
            # Graph Comparison
            p(strong("Making comparisons:"),
              em("In the table above, we saw that many high-sugar cereals are seen on the 2nd shelf.
               This graph shows the comparison between shelf level and healthy ratings. What can you conclude?")),
            helpText("Now that we know high-sugar cereals are usually displayed at eye-level, we can also make the
                     conclusion that those 2nd shelf cereals will typically have low and unhealthy ratings. Does this
                     change your mind about your favorite cereal?"),
            br()
        )
        
    )
)
server <- function(input, output) {
    
    # Creating table
    # User can specify number of obs
    output$cerealTable <- renderTable({
        cereal_table <- cereal1 %>%
            select(name, input$table1, input$table2, input$table3, input$table4) %>%
            head(input$obs)
    }, rownames = TRUE)
    
    #Text info on table
    output$textTable <- renderText({
        paste("- This table is currently showing ", input$obs, " observations with the name, ", input$table1, ",", input$table2, 
              ",", input$table3, "per serving, and", input$table4, "of each cereal.")
    })
    
    # Creating bar plot
    # Need to fix input x value, and bins
    output$cerealPlot <- renderPlot({
        x <- cereal$shelf
        y <- cereal1$rating
        
        ggplot() +
            geom_col(mapping = aes(x, y), fill = "orange") +
                           labs(title = "U.S. Cereal Rating",
                                subtitle = "by Shelf Level",
                                x = "Shelf Level",
                                y = "Rating") +
            theme_bw()

    })

    #Text info on graph
    output$textGraph <- renderText({
        paste("- This graph shows the relationship between shelf level and rating of the cereals.")
    })
    
}

shinyApp(ui, server)