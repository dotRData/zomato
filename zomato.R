# https://developers.zomato.com/documentation

library(RCurl)
library(curl)
library(jsonlite)
library(rjson)
library(tm)
library(wordcloud)

key     <- '07b65c0fd9ac93e8c6bb905f468eaa14'
header  <- c(Accept="application/json", 'user_key' = key)

########################## COMMON ##########################

##### CITIES #####

##### COLLECTIONS #####

##### CUISINES #####

##### ESTABLISHMENTS #####

##### GEOCODE #####


########################## LOCATION ##########################

##### LOCATION #####
r_city_name <- 'bangalore'
lat <- 10
lon <- 10
count <- 10
url_location <- paste('https://developers.zomato.com/api/v2.1/locations?query=', r_city_name, '&lat=', lat, '&lon=', lon, '&count=', count, sep = "")

##### LOCATION DETAILS #####
r_entity_id <- 4
r_entity_type <- 'city'
url_location_details <- paste('https://developers.zomato.com/api/v2.1/location_details?entity_id=', r_entity_id,'&entity_type=', r_entity_type, sep = "")

########################## RESTAURANT ##########################

##### RESTAURANT #####
r_res_id  <- 16774318
url_restaurant <- paste('https://developers.zomato.com/api/v2.1/restaurant?res_id=', r_res_id, sep = "")

##### REVIEWS #####
start <- 1
count <- 10
r_res_id  <- 16774318
url_reviews <- paste('https://developers.zomato.com/api/v2.1/reviews?res_id=', r_res_id, '&start=', start, '&count=',count ,sep = "")

##### SEARCH #####
# entity_type <- c('city', 'subzone', 'zone', 'landmark', 'metro', 'group')
# sort <- c('cost', 'rating', 'real_distance')
# order <- c('asc', 'desc')
entity_id <- 4
entity_type <- 'city'
q <- 
start <- 
count <- 
lat <- 
lon <- 
radius <- 
cuisines <- 
establishment_type <- 
collection_id <- 
r_sort <- 'rating'
r_order <- 'desc'
url_search <- paste('https://developers.zomato.com/api/v2.1/search?entity_type=', r_entity_type,'&sort=', r_sort,'&order=', r_order, sep = "")

##############################################################################

data    <- fromJSON(getURL(url, httpheader = header))

for(i in 1:length(data$user_reviews)){
    print(data$user_reviews[[i]]$review$review_text)
}




