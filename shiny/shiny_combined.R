
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
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

story_df$lng <- as.numeric.factor(story_df$store_location.coordinates1)
story_df$lat <- as.numeric.factor(story_df$store_location.coordinates2)


## dataset for time series 
story_df_new <- story_df %>% 
  mutate(new_date = gsub("T.*","", date),
         year=year(new_date),
         month=month(new_date),
         day=day(new_date)) 



story_df_new1 <- story_df_new %>% 
  group_by(name, year, month) %>% 
  summarise(sale_sum = sum(as.numeric(sale_dollars)))
##Remove NAs

story_df2<- story_df[!is.na(story_df$lng) & !is.na(story_df$lat),]


##### Shiny

ui <- fluidPage(
  titlePanel("Story County Liquor Sales"),
    mainPanel(
      tabsetPanel(
        tabPanel("Liquor Sales Map",   sidebarPanel(
          selectInput("city", label = "city", choices = levels(unique(story_df2$city)), selected = "AMES")
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel("Cornfield", leafletOutput("map"))
          )
        )), 
        tabPanel("Liquor Sales Trend Analysis ",
                 # Sidebar layout with input and output
                 sidebarLayout(
                   
                   # Sidebar Panel for inputs
                   sidebarPanel(
                     
                     helpText("Create Bar Plot for Time vs. Stores' Sale."),
                     
                     # Input: Select for choosing dataset
                     selectInput("Stores",
                                 label = "Stores",
                                 choices = levels(story_df_new1$name),
                                 selected = "Cyclone Liquors"),
                     
                     selectInput("Year", 
                                 label = "Year",
                                 choices = levels(as.factor(story_df_new1$year)),
                                 selected = NULL),
                     
                     selectInput("Month",
                                 label = "Month",
                                 choices = levels(as.factor(story_df_new1$month)),
                                 selected = NULL),
                     
                     selectInput("Day",
                                 label = "Day",
                                 choices = levels(as.factor(story_df_new$day)),
                                 selected = NULL)
                     
                     # Main Panel for displaying outputs
                   ),
                   mainPanel(
                     fluidRow( 
                       plotlyOutput("storePlot"),
                       plotlyOutput("storePlot2")
                     )
                   )
                 )
        )) 
      )
    )
  

  
  
  
  
  

  
  
  
server <- function(input, output) {
  
  
  output$map <- renderLeaflet({
    leaflet(story_df2[story_df2$city == input$city, ]) %>%
      addTiles() %>%
      addMarkers(~lng, ~lat, popup = ~as.character(name))
  })
  
  
  output$storePlot <- renderPlotly({
    
    story_df_new1 %>% filter(name == input$Stores, year == input$Year) %>%
      mutate(month = factor(month)) %>%
      ggplot(aes(x = month, y = sale_sum))+geom_col(colour="black", fill="#DD8888")
    
  }
  ) 
  
  
  output$storePlot2 <- renderPlotly({
    
    story_df_new %>% filter(name == input$Stores, year == input$Year, month == input$Month) %>%
      mutate(day = as.factor(day), month = as.factor(month), year = as.factor(year), sale_dollars = as.numeric(sale_dollars)) %>%
      ggplot(aes(x = day, y = sale_dollars)) + geom_col(colour = "black", fill = "#7fcdbb")
    
    
  })
}
  
  
  
  
  
  
  
  
  
  
  





# Run the application 
shinyApp(ui = ui, server = server)



