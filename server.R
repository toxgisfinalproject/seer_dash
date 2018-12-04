
library(leaflet)
library(magrittr)
library(rgdal)
library(rgeos)
library(maptools)
library(plotly)
library(tidyverse)
library(tigris)
library(shiny)
library(sf)
#input<-list(state="nj",chemical="BENZENE")
####state_chem_plot <- ggplot() +
####  geom_sf(data=state_chem, aes(fill=log((total_rel_summ + 0.001)/aland),group=namelsad, label=namelsad)) +
  #geom_text(aes(label = name, x = intptlon.y, y = intptlat.y), size = 2) + 
  # gghighlight::gghighlight(AREA > 0.20) +
  #ggsflabel::geom_sf_label(data = state_chem, aes(label = namelsad)) +
####  scale_fill_viridis_c(option = "magma") +
####  labs(fill = toupper(stringr::str_c(isolate(input$state))," ",tolower(isolate(input$chemical))," releases by county, divided by land area, log transformed")) +
####  theme(legend.position = "bottom") 
#plotly::ggplotly(state_chem_plot) #please note that turning this into a ggplotly object will work, but 
# Define server logic required to draw a histogram
#I think I'll try leaflet here. It's just not fast enough with ggplot.
shinyServer(function(session, input, output) {
   
  output$leafletplot <- renderLeaflet({
    #add layers butto.n
    county_cancer_chem = readRDS( "cancer_county_chem_pop.rds") %>% 
      mutate(prevalence = n/pop_est)
    
    options(tigris_class = "sf")
    state_sf <- tigris::counties(state = isolate(input$state)) %>% #it would  load faster if we had all of the objects in RAM, or at least in RDS files.
      #sf::st_simplify(TRUE, dTolerance = 10000) %>% #this reduces the size of the SF object drastically.
      janitor::clean_names() %>% 
      mutate(name = tolower(name))
    
    input$chemical
    input$year
    state_chem <- county_cancer_chem %>% 
      filter(chemical == isolate(input$chemical) &
               st == isolate(input$state), year == isolate(input$year)) %>% 
      select(total_rel_summ,county,year) %>% 
      ungroup() %>% 
      distinct() %>% 
      right_join(state_sf, by = c("county" = "name")) %>% 
      mutate(log_total_rel = log(total_rel_summ + 0.001))
    state_joined_chem <- state_sf %>%
      left_join(state_chem, by = c("name" = "county")) %>%
      replace_na(list(total_rel_summ = 0)) %>% 
      mutate(geometry = geometry.x)
    
    viridis_pal <- colorNumeric(palette = "viridis", domain = state_chem$log_total_rel, na.color = "grey")
    leaflet() %>% 
      addProviderTiles("OpenStreetMap.Mapnik") %>% #can change this to MapBox
      addPolygons(data = state_joined_chem, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.3,
                  fillColor = ~viridis_pal(state_chem$log_total_rel))
    
    
  })
  
})
