
library(tidyverse)
library(xlsx)
library(httr)
library(RJSONIO)
library(r2d3)
library(sf)
library(ggplot2)
theme_set(theme_bw())
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)


# NOAA Weather Station Data -----------------------------------------------

# Read NOAA data
prac <- read.csv('data/shasta19.csv')

# View all days where precipitation was greater than 2
prac %>%
    select(DATE, DailyPrecipitation) %>%
    filter(as.numeric(prac$DailyPrecipitation) > 2) 


# Conflict Data ------------------------------------------------------

# get data
prac <- fromJSON('https://ucdpapi.pcr.uu.se/api/gedevents/17.2?pagesize=10000&StartDate=2016-01-01&EndDate=2020-12-18')

# extract results list
result <- prac$Result

# Null to NA function to apply through
null_to_na <- function(x) {
    
    for(i in 1:length(x))
        if(is.null(x[[i]])){
            x[[i]] <- NA
        } else {
            next
        }
    return(x)
    
}

# Set nulls to NA
result <- lapply(result, null_to_na)

# Initialize data frame with first element of results
c_df<- data.frame(result[[1]])

# Add the rest w/loop
for (i in 2:length(result)){
    c_df <- rbind(c_df, data.frame(result[[i]]))
}

# get next URL
URL <- prac$NextPageUrl
url_list <- c(URL, rep(NA, 50))

# Discovered there's 39 pages, or so it seems, should list URL's
# it would be best if I figured out what the last 'URL' was and
# ifelsed on that. 
for( i  in 2:39){
    listing <- fromJSON(URL)
    if (listing$NextPageUrl != ""){
        url_list[i] <- listing$NextPageUrl
        Sys.sleep(.2)
        URL <- listing$NextPageUrl
    } else {
        break
    }
}

# Manually subset to remove NA's
url_list <- url_list[!is.na(url_list)]

# This function should take a vector of URL's that return JSON, and
# give back data frames of data
get_all_data <- function(x) {
    # get Data
    data <- fromJSON(x)
    
    # subset Data
    data <- data$Result
    
    # Turn Nulls to NA's
    data <- lapply(data, null_to_na)
    
    # Initialize data frame with first element of results
    c_df<- data.frame(data[[1]])
    
    # Add the rest w/loop
    for (i in 2:length(data)){
        c_df <- rbind(c_df, data.frame(data[[i]]))
    }
    
    return(c_df)
}

# lappy over our URL with get_all_data
yes <- lapply(url_list, get_all_data)

# Collapse list of df's to single df
yes <- bind_rows(yes)

# Add page 1
c_df <- rbind(c_df, yes)

#
save(c_df, file = "data/conflict_16.Rds")

## Data has been saved above, now if we want to reload and explore our data,

data1 <- load('data/conflict_10_15.Rds')
data2 <- load('data/conflict_16.Rds')

c_df <- rbind(data1, data2)

# Fast Exploration --------------------------------------------------------

# USA involved
View(yes %>%
    filter(side_b_new_id == 769 | side_a_new_id == 769))

# ------------- It doesn't appear to be only USA options. Needs to do 
# ------------- more exploring. 

# Mapping Conflict Data ---------------------------------------------------

# Working from here - https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html

# Create globe
world <- ne_countries(scale = "medium", returnclass = "sf")
names(world)
world$geounit
# Pass it to ggplot
ggplot(data = world) +
    geom_sf(aes(fill = pop_est)) +
    coord_sf() +
    xlab('Longitude') +
    ylab('Latitude') +
    ggtitle('World Map', subtitle = paste0("(", length(unique(world$geounit)), " countries)"))


