library(shinyLP)
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
  county_cancer_chem = readRDS( "cancer_county_chem_pop.rds") %>% 
    mutate(prevalence = n/pop_est)
  tri_df <- readRDS("tri_df.rds")  
  output$leafletplot <- renderLeaflet({
    #add layers button
    
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
    #end chemical layers (did this first as it just came most naturally).
    #begin cancer layer
#    browser() #there has got to be a way to skip all of this if there's no change in cancer data.
    input$cancer
    cancer_state_subset <- county_cancer_chem %>% 
      filter(cancer == isolate(input$cancer) &
               st == isolate(input$state)) %>% 
      select(prevalence,year,cancer) %>% 
      spread(year,prevalence) %>% 
      ungroup() %>% 
      select(-chemical,-st,-cancer) %>% 
      distinct() %>% 
      replace_na(list(prevalence = 0)) %>% 
      as.data.frame()
    #browser()  
#    ca_sf[is.na(ca_sf)]<-0 #drop_na should replace this step (although I wanted to avoid it 
    cancer_sf_joined <- state_sf %>%
      left_join(cancer_state_subset, by=c("name" = "county")) #distinct at this step causes issues.

    

    #get the lowest year
    first_year <- intersect(1987:2009,names(cancer_sf_joined)) %>% 
      min()
    #get the highest year
    last_year <- intersect(1987:2009,names(cancer_sf_joined)) %>% 
      max()
    #plug in below to convert to long format
    cancer_sf_gathered <- cancer_sf_joined %>%
      gather(key=year,value=prevalence,first_year:last_year)
    # 
    # 
    # 
    cancer_lm <- cancer_sf_gathered %>%
      replace_na(list(prevalence = 0)) %>%
      mutate(year = as.numeric(year)) %>%
      group_by(name) %>%
      mutate(prev_kurt = e1071::kurtosis(prevalence)) %>%
      nest() %>%
      mutate(county_prev_lm = map(data, ~lm(formula = prevalence ~ year, data = .x)) ) %>%
      mutate(lm_tidy = map(county_prev_lm, broom::tidy, conf.int = TRUE)) %>%
      select(-data, -county_prev_lm) %>%
      unnest() %>%
      filter(term == "year")
    cancer_sf_joined_lm <- cancer_sf_joined %>% 
      right_join(cancer_lm, by = "name")

    #browser()
    chemical_pal <- colorNumeric(palette = "viridis", domain = state_chem[["log_total_rel"]], na.color = "grey")
    cancer_slope_pal <- colorNumeric(palette = "viridis", domain = cancer_sf_joined_lm[["estimate"]]*1e5, na.color = "grey")
    browser()
  tri_df_filtered  <- tri_df %>% 
      filter(chemical == isolate(input$chemical),
             state == (isolate(input$state) %>%
                         toupper()) ) 
    leafletplot <- leaflet() %>% 
      addProviderTiles("OpenStreetMap.Mapnik") %>% #can change this to MapBox
      addPolygons(data = state_joined_chem, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.5,
                  fillColor = ~chemical_pal(state_chem[["log_total_rel"]]), group = "chemicals",
                  popup = paste("County: ", state_joined_chem[["name"]], "<br>",
                  "Chemical: ", state_joined_chem[["chemical"]], "<br>", "Amount Released: ",
                  state_joined_chem[["total_rel_summ"]])
                  ) %>% 
    addPolygons(data = cancer_sf_joined_lm, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.5,
                fillColor = ~cancer_slope_pal(cancer_sf_joined_lm$estimate), group = "cancer_slope", popup = paste("County: ", cancer_sf_joined_lm[["name"]], "<br>",
                                                                                                                   "Slope: ", cancer_sf_joined_lm[["estimate"]], "<br>",
                                                                                                                   "Cancer: ", cancer_sf_joined_lm[["cancer"]])
                  ) %>% 
      addCircles(data = tri_df_filtered, lng = ~longitude, lat = ~latitude, radius = ~air_onsite_release, group = "air_releases") %>% 
      addCircles(data = tri_df_filtered, lng = ~longitude, lat = ~latitude, radius = ~water_onsite_release, group = "water_releases") %>% 
      addCircles(data = tri_df_filtered, lng = ~longitude, lat = ~latitude, radius = ~land_onsite_release, group = "land_releases") %>% 
      addLayersControl(overlayGroups = c("chemicals","cancer_slope","air_releases","water_releases","land_releases")) %>% 
      addLegend("bottomright", pal = chemical_pal, values = state_chem[["log_total_rel"]], title = "log total releases (log pounds)") %>% 
    addLegend("bottomleft", pal = cancer_slope_pal, values = cancer_sf_joined_lm[["estimate"]]*1e5, title = "slope estimate")  
    
    return(leafletplot)
  })
output$state_line_plot <- renderPlotly({
  input$cancer
  cancer_yearly = county_cancer_chem %>%
    filter(cancer == isolate(input$cancer)) %>%
    group_by(year, st) %>%
    summarize(new_cases = sum(n),
              pop_est = sum(pop_est)) %>%
    mutate(pop_est_thousands = pop_est/100000,
           incidence = new_cases/pop_est_thousands,
           st = toupper(st))
  st_ggplot <- cancer_yearly %>%
    ggplot(aes(x = year, y = incidence, color = st)) +
    geom_line() +
    labs(
      x = "Year",
      y = "Cancer Incidence per 100,000 people"
    ) + 
     theme_bw() 
  
  st_ggplot %>% plotly::ggplotly()
 })

output$stacked_yearly_release <- renderPlotly({
  input$chemical
  input$state
stacked_yearly_release = tri_df %>% 
  filter(chemical == isolate(input$chemical),
         state == (isolate(input$state) %>%
                     toupper()) ) %>% 
  group_by(year) %>% 
  summarize(air = round(sum(air_onsite_release)/1000000, digits = 3),
            water = round(sum(water_onsite_release)/1000000, digits = 3),
            land = round(sum(land_onsite_release, na.rm = TRUE)/1000000, digits = 3),
            offsite = round(sum(off_site_release_total)/1000000, digits = 3),
            total_release = round(sum(total_releases)/1000000, digits = 3)) %>% 
  arrange(-total_release) %>% 
  gather(key = waste_release_route, value = release, air:offsite) %>% 
  mutate(waste_release_route = fct_relevel(waste_release_route, 
                                           "offsite","land", "water", "air")) %>%
   ggplot(aes(x = year, y = release, fill = waste_release_route)) +
  geom_area(position = 'stack') + 
  scale_color_viridis_c() + 
  labs(
    y = "Waste Release (Million Pounds)",
    x = "Year"
  ) +
  theme_bw()
  plotly::ggplotly(stacked_yearly_release)
})
output$lung_chem_standalone <- renderUI({
  #browser()
  plotly_iframe <- tags$iframe(src = "lung_chem_scatter_plotly.html", height=1024, width=768)
  print(plotly_iframe)
  plotly_iframe
})
output$click_plot <- renderPlotly({
  #browser()
  
})

})
