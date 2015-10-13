# https://developers.zomato.com/documentation

rm(list = ls())

library(ggplot2)
library(memoise)
library(stringr)
library(RCurl)
library(curl)
library(jsonlite)
library(rjson)
library(tm)
library(wordcloud)

key        <<- '07b65c0fd9ac93e8c6bb905f468eaa14'
header     <<- c(Accept="application/json", 'user_key' = key)
zomato_url <<- 'https://developers.zomato.com/api/v2.1'

########################## COMMON ##########################

##### CITIES #####
cities <- function(city_name = NULL, 
                   lat = NULL,
                   lon = NULL,
                   city_ids = NULL,
                   count = 100){
 
  if(is.null(city_ids) == FALSE) city_ids <- paste(city_ids,collapse = ',')
  
  if(length(unlist(strsplit(city_name, ' '))) > 1) city_name <- paste(unlist(strsplit(city_name, ' ')), collapse = '%20')
  
  url <- paste(zomato_url,'/cities?q=', city_name,
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
  
  url <- paste(zomato_url,'/collections?city_id=', city_id,
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
  
  url <- paste(zomato_url,'/cuisines?city_id=', city_id,
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
  
  url <- paste(zomato_url,'/establishments?city_id=', city_id,
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
  
  url <- paste(zomato_url,'/geocode?lat=', lat, 
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
  
  url <- paste(zomato_url,'/locations?query=', city_name,
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
  
  url <- paste(zomato_url,'/location_details?entity_id=', entity_id,
               '&entity_type=', entity_type,
               sep = "")
  
  data <- fromJSON(getURL(url, httpheader = header))
  data
}
########################## RESTAURANT ##########################

##### RESTAURANT #####
resturant <- function(resturant_id){
  
  if (missing(resturant_id)) stop("Need to specify resturant_id")
  
  url <- paste(zomato_url,'/restaurant?res_id=', resturant_id, 
               sep = "")
  
  data <- fromJSON(getURL(url, httpheader = header))
  data
}

##### REVIEWS #####
reviews <- function(resturant_id, 
                    start = 0,
                    count = 20){
  
  if (missing(resturant_id)) stop("Need to specify resturant_id")
  
  url <- paste(zomato_url,'/reviews?res_id=', resturant_id,
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
  
  url <- paste(zomato_url,'/search?entity_id=', entity_id, 
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

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


