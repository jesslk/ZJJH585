library(shiny)
library(tidyr)
library(leaflet)
library(tidyverse)

## Extract dataset
basic_url <- "https://data.iowa.gov/resource/m3tr-qhgy.json"
full_url = paste0(basic_url, "?County=Story")


story_info <- jsonlite::read_json(full_url)


new.distinct.names <- story_info %>% 
  purrr::map(.x, 
             .f=~names(rbind.data.frame(rlist::list.flatten(.x),0)))

unlisted <- story_info %>% 
  purrr::map(.f = ~rbind.data.frame(unlist(.x, recursive=T, use.names=T)))

unlisted.info <- purrr::map2(unlisted,
                             new.distinct.names,
                             .f= ~purrr::set_names(.x, .y))

story_df <- do.call(plyr::rbind.fill, unlisted.info)

story_df_new <- story_df %>% 
  mutate(new_date = gsub("T.*","", date),
         year=year(new_date),
         month=month(new_date),
         day=day(new_date))

### Shiny Attempt 2
ui <- fluidPage(
  
  titlePanel("Sales Trade by Stores"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create time series plot for time vs. stores' sale."),
      
      selectInput("Year",
                  label = "Year", 
                  choices = levels(as.factors(story_df_new$year)), 
                  selected = "2015")
    )
  ),
  
  selectInput("Month", 
              label = "Month",
              choices = levels(as.factors(story_new$month)),
              selected = NULL),
  
  selectInput("Stores", 
              label = "Store",
              choices = levels(story_new$name),
              selected = "Cyclone Liquors"))


server <- function(input, output) {
  
  #story_new_subset <- reactive({
  # story_new2 %>%
  #  filter(city == input$city)
  #})
  
  
  
#   output$map <- renderLeaflet({
#     leaflet(story_new2[story_new2$city == input$city, ]) %>%
#       addTiles() %>%
#       addMarkers(~lng, ~lat, popup = ~as.character(name))
#   })
#   
}


###
# Run the application 
shinyApp(ui = ui, server = server)


