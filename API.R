# https://developers.zomato.com/documentation

rm(list = ls())

library(memoise)
library(stringr)
library(RCurl)
library(curl)
library(jsonlite)
library(rjson)
library(tm)
library(wordcloud)

key     <<- '07b65c0fd9ac93e8c6bb905f468eaa14'
header  <<- c(Accept="application/json", 'user_key' = key)
# res_id  <<- 16774318

########################## COMMON ##########################

##### CITIES #####
cities <- function(city_name = NULL, 
                   lat = NULL,
                   lon = NULL,
                   city_ids = NULL,
                   count = 100){
 
  if(is.null(city_ids) == FALSE) city_ids <- paste(city_ids,collapse = ',')
  
  if(length(unlist(strsplit(city_name, ' '))) > 1) city_name <- paste(unlist(strsplit(city_name, ' ')), collapse = '%20')
  
  url <- paste('https://developers.zomato.com/api/v2.1/cities?q=', city_name,
               '&lat=', lat, 
               '&lon=', lon, 
               '&city_ids=', city_ids,
               '&count=', count, 
               sep = "")
  
  data <- fromJSON(getURL(url, httpheader = header))
  data
}
  
##### COLLECTIONS #####
collections <- function(city_id, 
                   lat = NULL,
                   lon = NULL,
                   count = 100){
 
  if (missing(city_id)) stop("Need to specify city_id")
  
  url <- paste('https://developers.zomato.com/api/v2.1/collections?city_id=', city_id,
               '&lat=', lat, 
               '&lon=', lon,
               '&count=', count, 
               sep = "")
  
  data <- fromJSON(getURL(url, httpheader = header))
  data
}
##### CUISINES #####
cuisines <- function(city_id, 
                     lat = NULL,
                     lon = NULL){
  
  if (missing(city_id)) stop("Need to specify city_id")
  
  url <- paste('https://developers.zomato.com/api/v2.1/cuisines?city_id=', city_id,
               '&lat=', lat, 
               '&lon=', lon, 
               sep = "")
  
  data <- fromJSON(getURL(url, httpheader = header))
  data
}
##### ESTABLISHMENTS #####
establishments <- function(city_id, 
                     lat = NULL,
                     lon = NULL){
  
  if (missing(city_id)) stop("Need to specify city_id")
  
  url <- paste('https://developers.zomato.com/api/v2.1/establishments?city_id=', city_id,
               '&lat=', lat, 
               '&lon=', lon, 
               sep = "")
  
  data <- fromJSON(getURL(url, httpheader = header))
  data
}

##### GEOCODE #####
geocode <- function(lat,
                    lon){
  
  if (missing(lat)) stop("Need to specify lat")
  if (missing(lon)) stop("Need to specify lon")
  
  url <- paste('https://developers.zomato.com/api/v2.1/geocode?lat=', lat, 
               '&lon=', lon, 
               sep = "")
  
  data <- fromJSON(getURL(url, httpheader = header))
  data
}


########################## LOCATION ##########################

##### LOCATION #####
location <- function(city_name, 
                     lat = NULL, 
                     lon = NULL, 
                     count = 100){
  
  if (missing(city_name)) stop("Need to specify city_name")
  
  if(length(unlist(strsplit(city_name, ' '))) > 1) city_name <- paste(unlist(strsplit(city_name, ' ')), collapse = '%20')
  
  url <- paste('https://developers.zomato.com/api/v2.1/locations?query=', city_name,
               '&lat=', lat, 
               '&lon=', lon, 
               '&count=', count, 
               sep = "")
  
  data <- fromJSON(getURL(url, httpheader = header))
  data
}

##### LOCATION DETAILS #####
location_details <- function(entity_id, 
                             entity_type = c('city', 'subzone', 'zone', 'landmark', 'metro', 'group')){
  
  if (missing(entity_id)) stop("Need to specify entity_id")
  if (missing(entity_type)) stop("Need to specify entity_type in ('city', 'subzone', 'zone', 'landmark', 'metro', 'group')")
  
  url <- paste('https://developers.zomato.com/api/v2.1/location_details?entity_id=', entity_id,
               '&entity_type=', entity_type,
               sep = "")
  
  data <- fromJSON(getURL(url, httpheader = header))
  data
}
########################## RESTAURANT ##########################

##### RESTAURANT #####
resturant <- function(resturant_id){
  
  if (missing(resturant_id)) stop("Need to specify resturant_id")
  
  url <- paste('https://developers.zomato.com/api/v2.1/restaurant?res_id=', resturant_id, 
               sep = "")
  
  data <- fromJSON(getURL(url, httpheader = header))
  data
}

##### REVIEWS #####
reviews <- function(resturant_id, 
                    start = 0,
                    count = 20){
  
  if (missing(resturant_id)) stop("Need to specify resturant_id")
  
  url <- paste('https://developers.zomato.com/api/v2.1/reviews?res_id=', resturant_id,
               '&start=', start,
               '&count=',count ,sep = "")
  
  data <- fromJSON(getURL(url, httpheader = header))
  data
}

##### SEARCH #####
search <- function(entity_id = NULL, 
                   entity_type = c('city', 'subzone', 'zone', 'landmark', 'metro', 'group'),
                   q = NULL, 
                   start = 1,
                   count = 100,
                   lat = NULL,
                   lon = NULL,
                   radius = NULL,
                   cuisines = NULL,
                   establishment_type = NULL, 
                   collection_id  = NULL,
                   sort = c('cost', 'rating', 'real_distance'),
                   order = c('asc', 'desc')){
  
  if (missing(entity_type)) stop("Need to specify entity_type in ('city', 'subzone', 'zone', 'landmark', 'metro', 'group')")
  if (missing(sort)) stop("Need to specify sort in ('cost', 'rating', 'real_distance')")
  if (missing(order)) stop("Need to specify order in ('asc', 'desc')")
  
  if(length(unlist(strsplit(q, ' '))) > 1) q <- paste(unlist(strsplit(q, ' ')), collapse = '%20')
  
  url <- paste('https://developers.zomato.com/api/v2.1/search?entity_id=', entity_id, 
               '&entity_type=', entity_type,
               '&q=', q,
               '&start=', start, 
               '&count=', count,
               '&lat=', lat,
               '&lon=', lon,
               '&radius=', radius,
               '&cuisines=', cuisines,
               '&establishment_type=', establishment_type, 
               '&collection_id=', collection_id,
               '&sort=', sort, 
               '&order=', order, 
               sep = "")
  
  data <- fromJSON(getURL(url, httpheader = header))
  data
}
##############################################################################
