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


##



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$table <- DT::renderDataTable(
    
    DT::datatable({
      selectedzip <- input$location
      selectedpet <- input$animal
      
      key <- "key="
      base_url <- "http://api.petfinder.com/"
      method <- "pet.find"
      
      query <- paste0("animal=", selectedpet, "&", "location=", selectedzip)
      url <- sprintf("%s%s?%s&%s", base_url, method, key, query)
      pets_list <- read_xml(url) %>% xml_nodes("pet")
      
      pet_df <- pets_list %>% purrr::map_df(pet_fun)
      
      pet_df
      
    }, options = list(pageLength = 20, dom = "tip"))
    
  )
}



####

ui <- navbarPage("Iowa Liquor Sales",
                    tabPanel("Locations of Liquor Stores", leafletOutput("map", width = "100%", height = "900px"),
                        absolutePanel(draggable = TRUE, top = 75, left = 1200, bottom = "auto",
                                        width = 330, height = "auto",
                                        selectInput("mapparent",
                                                    label = "City",
                                                    choices = levels(story_df$city),
                                                    selected = "AMES"
                                        )
                          )
                 ),                 
                 titlePanel("Sales Trade by Stores"),
                 
                 sidebarLayout(
                   sidebarPanel(
                     helpText("Create time series plot for time vs. stores' sale."),
                     
                     selectInput("Year",
                                 label = "Year", 
                                 choices = c("2012",
                                             "2013",
                                             "2014",
                                             "2015"), 
                                 selected = "2015")
                     )
                 ),
                 
                     selectInput("Month", 
                             label = "Month",
                             choices = levels(as.factors(story_new$month)),
                             selected = "1"),
                 
                     selectInput("Day",
                             label = "Day",
                             choices = levels(as.factors(story_new$day)), 
                             selected = "1"),
                 
                     selectInput("Stores", 
                                 label = "Store",
                                 choices = levels(story_new$name),
                                 selected = "Cyclone Liquors")
                 )





server <- function(input, output, session) {
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- dowmap_count[,1:4]
    if (input$parent != "All") {
      data <- data[data$parent == input$parent,]
    }
    if (input$year != "All") {
      data <- data[data$year == input$year,]
    }
    if (input$state != "All") {
      data <- data[data$state == input$state,]
    }
    data
  }, options = list(pageLength = 10)))
  
  output$map <- renderLeaflet({
    leaflet(dowmap_count[dowmap_count$year == input$mapyear & dowmap_count$parent == input$mapparent, ]) %>%
      setView(lat=10, lng=15, zoom=2) %>%
      addProviderTiles("Esri.WorldStreetMap") %>% addCircleMarkers(color = "red", radius = ~count,
                                                                   popup=~paste("Company:", parent, "<br>",
                                                                                "Country:", state, "<br>",
                                                                                "Year:", year, "<br>",
                                                                                "Number of Subsidiaries:", count))
  })
  
  output$points <- renderPlotly({
    if (input$us==TRUE) {dowmap_comb <- dowmap_comb} else {dowmap_comb <- dowmap_comb[dowmap_comb$state!="United States of America", ]}
    p <- ggplot(dowmap_comb, aes(state, sum_all, size=sum_all, color=state)) +
      geom_point(aes(text=paste("Country: ", state, "<br>Total Count: ", sum_all)))+ 
      theme(panel.background = element_blank(), plot.title = element_text(hjust = 0), axis.text.x = element_text(angle = 90, hjust = .5),
            axis.text.y = element_text(angle = 90, hjust = .1)) + 
      labs(title = "Total Count of Child Co. in a Country", x = "Country", y = "Count total", size = NULL, color = NULL)
    plotpoint <- ggplotly(p, tooltip = "text", source = "pointplot")
    plotpoint
  })
  
  output$bar <- renderPlotly({
    event <- event_data("plotly_click", source = "pointplot")
    validate(need(!is.null(event),
                  'Click each dot to populate this barplot for each country'))
    if (is.null(event)) {plotly_empty()} else {
      list <- as.list(event)
      country <- as.character(dowmap_comb$state[list$x])
      bars_dat <- dowmap_count[(dowmap_count$state==country), ]
      bars_dat$year <- as.factor(bars_dat$year)
      bars_dat <- bars_dat[order(bars_dat$year, decreasing=FALSE), ]
      bars_dat$year <- factor(bars_dat$year, levels = rev(levels(bars_dat$year)))
      
      bars <- ggplot(bars_dat, aes(reorder(parent, count, sum), count, fill=year)) + geom_bar(stat = "identity", 
                                                                                              aes(text = paste("Parent Co.: ", parent,
                                                                                                               "<br> Year: ", year,
                                                                                                               "<br> Count", count))) + 
        labs(title = country, x = "Parent Company", y="Count") +
        theme(panel.background = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, size = 10), 
              axis.text.y = element_text(angle = 90, hjust = .5))
      ggplotly(bars, tooltip = "text", source = "barplot")
    }
  })
  
  output$line <- renderPlotly({
    event <- event_data("plotly_click", source = "pointplot")
    if (is.null(event)) {plotly_empty()} else {
      list <- as.list(event)
      country <- as.character(dowmap_comb$state[list$x])
      bars_dat2 <- dowmap_count[(dowmap_count$state==country), ]
      
      bars_sum2 <- bars_dat2 %>% group_by(parent) %>%
        summarise(count = sum(count))
      bars_sum2 <- bars_sum2[order(bars_sum2$count, decreasing = FALSE), ]
      
      bars_dat2 <- bars_dat2[order(match(bars_dat2$parent, bars_sum2$parent)), ]
      
      event2 <- event_data("plotly_hover", source = "barplot")
      validate(need(!is.null(event2),
                    'Hover over the barplot to populate this line graph'))
      if (is.null(event2)) {plotly_empty()} else {
        list2 <- as.list(event2)
        parentco <- as.character(unique(bars_dat2$parent)[list2$x])
      }
      line_dat <- dowmap_count[(dowmap_count$state==country & dowmap_count$parent==parentco), ]
      line <- ggplot(line_dat, aes(year, count)) + geom_line(color="red") + labs(title = paste(parentco, "at" , country), x = "Years (2005 - 2015)", y="Count") +
        theme(panel.background = element_blank(), plot.title = element_text(hjust = 0), axis.text.x = element_text(hjust = .5),
              axis.text.y = element_text(angle = 90, hjust = 1)) + scale_x_continuous(breaks = c(2005:2015))
      ggplotly(line)
    }
  })
  
}


###
# Run the application 
shinyApp(ui = ui, server = server)