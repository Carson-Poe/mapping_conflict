
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
prac <- fromJSON('https://ucdpapi.pcr.uu.se/api/gedevents/17.2?pagesize=10000&StartDate=2010-01-01&EndDate=2015-12-31',
                 simplifyDataFrame = simplifyVector)

# extract results list
result <- prac$Result

# Take first element in list to initalize data.frame
z <- data.frame(result[[1]])

# Add the rest w/loop
for (i in 2:length(result)){
    z <- rbind(z, data.frame(result[[i]]))
}


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

