# app.R
# A base dashboard to edit for your final project. See "Using the Dashboard Template."
# Based on "Hello Shiny!" https://shiny.posit.co/r/gallery/start-simple/faithful/
  
library(shiny)
library(tidyverse)


# The Data ---------------------------------------------------------------------------------------

# Save your cleaned data in app/data/, then delete the # below.
# my_data <- read_csv("data/my_data.csv")


# The UI (what the reader sees) ------------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Commuting from Mercer County To Philadelphia"),
  p("For: Current and New Residents Commuting From Mercer County"),
  p("The decision: Which Town gives you the shortest Train Trip and is Driving Faster?"),
  
  selectInput(inputId = "pick", label = "Pick one:", choices = c("choice 1", "choice 2")),
  
  h3("Door-to-Door Train Commute to Philadelphia by Town"),
  plotOutput(outputId = "fig_1"),
  p("Shows the breakdown of the amount of time taken for each train ride in each town in Mercer County"),
  
  h3("Figure 2 title"),
  plotOutput(outputId = "fig_2"),
  p("a sentence or two saying what the figure shows."),
  
  h3("What this dashboard cannot tell you"),
  p("the limits of your data.")
  
)


# The Server (how the figures are made) ----------------------------------------------------------

server <- function(input, output) {
  
  output$fig_1 <- renderPlot({
    
    ggplot()     # replace with your code for Figure 1
    
  })
  
  output$fig_2 <- renderPlot({
    
    ggplot()     # replace with your code for Figure 2
    
  })
  
}


# Run the App ------------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)