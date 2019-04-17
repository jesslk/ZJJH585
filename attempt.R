library(tidyverse)
library(stringr)
library(leaflet)
drink <- read.csv("https://data.iowa.gov/resource/m3tr-qhgy.csv", header = T)

drink2 <- drink[-c(1:5)]

testme <- drink2 %>% separate(`store_location`, c("point", "Long", "Lat"), sep = " ")  

testme$Long <- readr::parse_number(testme$Long)
testme$Lat <- readr::parse_number(testme$Lat)
sales <- subset(testme, select = -`point`)


leaflet(data = sales) %>% addTiles() %>% addMarkers(~Long, ~Lat, popup = ~as.character(category_name))
