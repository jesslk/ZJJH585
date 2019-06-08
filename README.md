README
================
Zhenzhen Chen, Jessica Kueon, Hana Lee, Jing Zhao
April 17, 2019

Link
----

<https://github.com/hnlee1428/ZJJH585>

#### Objective: Make a R Shiny to visualize Iowa Liquor Sales dataset <https://data.iowa.gov/Sales-Distribution/Iowa-Liquor-Sales/m3tr-qhgy>

#### 1. Importing the subset of data in the Json form and converting it into a dataframe in R.

``` r
library(shiny)
library(tidyr)
library(mapview)
```

    ## Warning: package 'mapview' was built under R version 3.5.3

``` r
library(leaflet)
```

    ## Warning: package 'leaflet' was built under R version 3.5.3

``` r
library(plotly)
```

    ## Loading required package: ggplot2

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(tidyverse)
```

    ## -- Attaching packages -------------------------------------------------------------- tidyverse 1.2.1 --

    ## v tibble  2.0.1     v dplyr   0.7.8
    ## v readr   1.3.1     v stringr 1.3.1
    ## v purrr   0.3.0     v forcats 0.3.0

    ## -- Conflicts ----------------------------------------------------------------- tidyverse_conflicts() --
    ## x lubridate::as.difftime() masks base::as.difftime()
    ## x lubridate::date()        masks base::date()
    ## x dplyr::filter()          masks plotly::filter(), stats::filter()
    ## x lubridate::intersect()   masks base::intersect()
    ## x dplyr::lag()             masks stats::lag()
    ## x lubridate::setdiff()     masks base::setdiff()
    ## x lubridate::union()       masks base::union()

``` r
## procedure to get the same data as above
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

head(story_df)
```

    ##              address bottle_volume_ml category
    ## 1     640 LINCOLNWAY              500  1011300
    ## 2     305 AIRPORT RD              750  1011500
    ## 3   2515 CHAMBERLAIN             1000  1081340
    ## 4    419 LINCOLN WAY              750  1081200
    ## 5 3800 W LINCOLN WAY              750  1081900
    ## 6   2515 CHAMBERLAIN              750  1032200
    ##                        category_name city county county_number
    ## 1                 TENNESSEE WHISKIES AMES  Story            85
    ## 2              STRAIGHT RYE WHISKIES AMES  Story            85
    ## 3                 ROOT BEER SCHNAPPS AMES  Story            85
    ## 4                     CREAM LIQUEURS AMES  Story            85
    ## 5 MISC. AMERICAN CORDIALS & LIQUEURS AMES  Story            85
    ## 6              IMPORTED VODKA - MISC AMES  Story            85
    ##                      date                            im_desc
    ## 1 2015-08-19T00:00:00.000 Jack Daniels Old #7 Black Lbl Mini
    ## 2 2012-09-17T00:00:00.000                      Templeton Rye
    ## 3 2012-01-12T00:00:00.000        Phillips Root Beer Schnapps
    ## 4 2014-04-23T00:00:00.000      Bailey's Original Irish Cream
    ## 5 2015-11-11T00:00:00.000            Paramount Melon Liqueur
    ## 6 2012-01-31T00:00:00.000            Pinnacle Cherry Whipped
    ##   invoice_line_no itemno                        name pack sale_bottles
    ## 1    S27407400005  26821           Hy-vee  #2 / Ames   12            1
    ## 2    S07743200026  27102      Sam's Club 6568 / Ames    6            6
    ## 3    S03538200068  84617             A J'S LIQUOR II   12            6
    ## 4    S18598700064  68036          Almost Always Open   12            3
    ## 5    S29016200248  76526 Hy-Vee Food Store #1 / Ames   12            1
    ## 6    S03818000099  35770             A J'S LIQUOR II   12            3
    ##   sale_dollars  sale_gallons sale_liters state_bottle_cost
    ## 1        13.59 0.13208602617         0.5              9.06
    ## 2       162.78 1.18877423561         4.5             18.08
    ## 3        43.98 1.58503231414           6              4.89
    ## 4        58.47  0.5943871178        2.25                13
    ## 5         7.95 0.19812903926        0.75               5.3
    ## 6        35.43  0.5943871178        2.25              7.87
    ##   state_bottle_retail store store_location.type
    ## 1               13.59  2501               Point
    ## 2               27.13  3524               Point
    ## 3                7.33  4509               Point
    ## 4               19.49  4987               Point
    ## 5                7.95  2500               Point
    ## 6               11.81  4509               Point
    ##   store_location.coordinates1 store_location.coordinates2
    ## 1                  -93.619455                   42.022848
    ## 2                   -93.61365                   42.001123
    ## 3                  -93.650965                    42.02146
    ## 4                  -93.616517                   42.022916
    ## 5                  -93.715664                   42.023145
    ## 6                  -93.650965                    42.02146
    ##   store_location_address store_location_city store_location_zip
    ## 1         640 LINCOLNWAY                AMES              50010
    ## 2         305 AIRPORT RD                AMES              50010
    ## 3       2515 CHAMBERLAIN                AMES              50010
    ## 4        419 LINCOLN WAY                AMES              50010
    ## 5     3800 W LINCOLN WAY                AMES              50010
    ## 6       2515 CHAMBERLAIN                AMES              50010
    ##                 vendor_name vendor_no zipcode
    ## 1  Brown-Forman Corporation        85   50010
    ## 2       Wilson Daniels Ltd.       255   50010
    ## 3 Phillips Beverage Company       380   50010
    ## 4           Diageo Americas       260   50010
    ## 5            Luxco-St Louis       434   50010
    ## 6           Jim Beam Brands        65   50010

#### Data cleaning

``` r
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
story_df$lng <- as.numeric.factor(story_df$store_location.coordinates1)
story_df$lat <- as.numeric.factor(story_df$store_location.coordinates2)


## Create dataset for time series 
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
story_total2 <- story_df2 %>% group_by(name, city, lng, lat) %>% summarise(Total_Sales_Dollars = sum(as.numeric(sale_dollars))) 





### Categorize sales


story_total2 <- story_total2 %>%
  mutate(Total_Sales = ifelse(Total_Sales_Dollars < 30000, "Less than 30000",
                                        ifelse(Total_Sales_Dollars >= 30000 & Total_Sales_Dollars <= 50000, "Less than 50000",
                                               ifelse(Total_Sales_Dollars >=50001 & Total_Sales_Dollars <= 10000, "Less than 10000",
                                                      ifelse(Total_Sales_Dollars >= 10001 & Total_Sales_Dollars <= 30000, "Less than 30000",

                                                                                                                          ifelse(Total_Sales_Dollars >= 30000 & Total_Sales_Dollars <= 60000, "Less than 60000", NA))))))
###
```
