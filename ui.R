library(shiny)
library(shinycssloaders)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("state","State",tolower(state.abb),"nj"),
       sliderInput("year",
                   "Year:",
                   min = 1987,
                   max = 2009,
                   value = 2001)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       withSpinner(leafletOutput("leafletplot"))
    )
  )
))
