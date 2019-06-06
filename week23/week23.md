---
title: "second_tidytue"
author: "liujian"
date: "2019/6/5"
output: 
  html_document:
    keep_md: true
---




```r
library(tidyverse)
library(countrycode)
library(sp)
library(maps)
library(maptools)
library(leaflet)
ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")
```

```r
glimpse(ramen_ratings)
```

```
## Observations: 3,180
## Variables: 6
## $ review_number <dbl> 3180, 3179, 3178, 3177, 3176, 3175, 3174, 3173, ...
## $ brand         <chr> "Yum Yum", "Nagatanien", "Acecook", "Maison de C...
## $ variety       <chr> "Tem Tem Tom Yum Moo Deng", "tom Yum Kung Rice V...
## $ style         <chr> "Cup", "Pack", "Cup", "Cup", "Tray", "Cup", "Pac...
## $ country       <chr> "Thailand", "Japan", "Japan", "France", "Japan",...
## $ stars         <dbl> 3.75, 2.00, 2.50, 3.75, 5.00, 3.50, 3.75, 5.00, ...
```

```r
sapply(ramen_ratings, function(x) length(unique(x)))
```

```
## review_number         brand       variety         style       country 
##          3178           456          2971             9            44 
##         stars 
##            40
```

```r
sapply(ramen_ratings, range, na.rm = T)
```

```
##      review_number brand           
## [1,] "1"           "1 To 3 Noodles"
## [2,] "3180"        "Zow Zow"       
##      variety                                            style  country    
## [1,] "\"A\" Series Artificial Chicken"                  "Bar"  "Australia"
## [2,] "三養<U+B77C><U+BA74> (Samyang Ramyun) (South Korean Version)" "Tray" "Vietnam"  
##      stars
## [1,] "0"  
## [2,] "5"
```

```r
unique(ramen_ratings$country)[order(unique(ramen_ratings$country))]
```

```
##  [1] "Australia"     "Bangladesh"    "Brazil"        "Cambodia"     
##  [5] "Canada"        "China"         "Colombia"      "Dubai"        
##  [9] "Estonia"       "Fiji"          "Finland"       "France"       
## [13] "Germany"       "Ghana"         "Holland"       "Hong Kong"    
## [17] "Hungary"       "India"         "Indonesia"     "Italy"        
## [21] "Japan"         "Malaysia"      "Mexico"        "Myanmar"      
## [25] "Nepal"         "Netherlands"   "New Zealand"   "Nigeria"      
## [29] "Pakistan"      "Philippines"   "Phlippines"    "Poland"       
## [33] "Russia"        "Sarawak"       "Singapore"     "South Korea"  
## [37] "Sweden"        "Taiwan"        "Thailand"      "UK"           
## [41] "Ukraine"       "United States" "USA"           "Vietnam"
```

```r
update_coutry <- ramen_ratings %>% 
  mutate(country = recode(country,
                          "Phlippines" = "Philippines",
                          "Hong Kong" = "China",
                          "Taiwan" = "China",
                          "Holland" = "Netherlands",
                          "Dubai" = "United Arab Emirates",
                          "Sarawak" = "Malaysia"))

update_coutry %>% 
  filter(is.na(country))
```

```
## # A tibble: 0 x 6
## # ... with 6 variables: review_number <dbl>, brand <chr>, variety <chr>,
## #   style <chr>, country <chr>, stars <dbl>
```

```r
unique(update_coutry$country)[order(unique(update_coutry$country))]
```

```
##  [1] "Australia"            "Bangladesh"           "Brazil"              
##  [4] "Cambodia"             "Canada"               "China"               
##  [7] "Colombia"             "Estonia"              "Fiji"                
## [10] "Finland"              "France"               "Germany"             
## [13] "Ghana"                "Hungary"              "India"               
## [16] "Indonesia"            "Italy"                "Japan"               
## [19] "Malaysia"             "Mexico"               "Myanmar"             
## [22] "Nepal"                "Netherlands"          "New Zealand"         
## [25] "Nigeria"              "Pakistan"             "Philippines"         
## [28] "Poland"               "Russia"               "Singapore"           
## [31] "South Korea"          "Sweden"               "Thailand"            
## [34] "UK"                   "Ukraine"              "United Arab Emirates"
## [37] "United States"        "USA"                  "Vietnam"
```


```r
#https://stackoverflow.com/questions/33041266/r-rworldmap-map-issue-and-leaflet-application
world <- map("world", fill=TRUE, plot=FALSE)
world_map <- map2SpatialPolygons(world, sub(":.*$", "", world$names))
world_map <- SpatialPolygonsDataFrame(world_map,
                                      data.frame(country=names(world_map), 
                                                 stringsAsFactors=FALSE), 
                                      FALSE)

cnt <- unique(update_coutry$country)

target <- subset(world_map, country %in% cnt)
# bins <- c(0, 1000, 1500, 2000, 2200, 2500, 2700, 3000, 3300)
# pal <- colorBin("viridis", domain = update_coutry$review_number, bins = bins)
# 
# leaflet(target) %>% 
#   addTiles() %>% 
#   addPolygons(fillColor = ~pal(update_coutry$review_number),
#               weight = 2,
#   
#   color = "white",
#   dashArray = "3",
#   fillOpacity = 0.7)
no_of_reviews <- update_coutry %>% 
  group_by(country) %>% 
  summarise(sum_review = sum(review_number, na.rm = T)) %>% 
  arrange(sum_review)

target@data <- target@data %>% 
  left_join(no_of_reviews)
pal <- colorBin("RdYlBu", domain = target$sum_review, bins = 5, reverse = TRUE)
leaf <- leaflet(target) %>% 
  addTiles() %>% 
  addPolygons(fillColor = ~pal(sum_review),
              weight = 2,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7) %>% 
  addLegend(pal = pal, values = ~sum_review, opacity = 0.7, title = NULL,
  position = "bottomleft")

library(htmlwidgets)
 library(htmltools)

 rr <- tags$div(
   HTML('<a> Number of reviews for Ramen rating in 38 countries</a>')
 ) 

leaf  %>%
   addTiles() %>%
   
   addControl(rr, position = "bottomright")
```

<!--html_preserve--><div id="htmlwidget-e35290ed2165121c943b" style="width:672px;height:480px;" class="leaflet html-widget"></div>
