library(leaflet)
library(shiny)
library(shinycssloaders)
library(tidyverse)
library(plotly)
county_cancer_chem = readRDS( "cancer_county_chem_pop.rds") %>% 
  mutate(prevalence = n/pop_est) #need a way to avoid reading this in twice.
# Define UI for application that draws a histogram
cancer_names <- county_cancer_chem %>%
  pull(cancer) %>%
  unique()
chemical_names <- county_cancer_chem %>%
  pull(chemical) %>%
  unique()

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Cancer Leaflet Plots"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput("cancer","Cancer",cancer_names, "lung"),
      selectInput("chemical","Chemical",chemical_names, "BENZENE"),
      selectInput("state","State",tolower(state.abb),"nj"),
       sliderInput("year",
                   "Year:",
                   min = 1987,
                   max = 2009,
                   value = 2001),
      actionButton("plot","create plots"),
      htmlOutput("introduction")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id = "tabs",
      tabPanel("Tox-Cancer Map",
       withSpinner(leafletOutput("leafletplot"))
       ),
      tabPanel("Cancer Incidence Over Time",
        withSpinner(plotlyOutput("state_line_plot"))
        ),
      tabPanel("Release Categories",
       withSpinner(plotlyOutput("stacked_yearly_release"))
       )
    )
    )
  )
))
