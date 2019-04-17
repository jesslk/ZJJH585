library(tidyverse)
library(rjson)
library(jsonlite)
library(RCurl)
library(readr)
d <- read_csv("Iowa_Liquor_Sales-Story.csv")
view(d)

## procedure to get the same data as above
basic_url <- "https://data.iowa.gov/resource/m3tr-qhgy.json"
full_url = paste0(basic_url, "?County=Story")


full_url
story <- jsonlite::read_json(full_url)
story
story <- as.data.frame(story)
dim(story)