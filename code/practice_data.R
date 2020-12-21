
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
library(viridis)
library(skimr)
library(GGally)


# NOAA Weather Station Data -----------------------------------------------

# Read NOAA data
prac <- read.csv('data/shasta19.csv')

# View all days where precipitation was greater than 2
prac %>%
    select(DATE, DailyPrecipitation) %>%
    filter(as.numeric(prac$DailyPrecipitation) > 2) 



# Retrieving and Cleaning Data -------------------------------------------

# get data
prac <- fromJSON('https://ucdpapi.pcr.uu.se/api/gedevents/20.1?pagesize=1000&StartDate=2019-01-01')

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
save(c_df, file = "data/conflict_19.Rds")



# Loading Data ------------------------------------------------------------

# Data saved in steps above, now if we want to load said data and explore...

# 2010 - 2016

load('data/conflict_10_15.Rds')

c_df1 <- c_df

load('data/conflict_16.Rds')

c_df <- rbind(c_df1, c_df)

rm(c_df1)

# 2019 only

load('data/conflict_19.Rds')


# Fast Exploration --------------------------------------------------------

# USA involved
View(c_df %>%
    filter(side_b_new_id == 769 | side_a_new_id == 769))

# Look at all
View(c_df)

# Wow, this function is nice
skim(c_df)

# And this 
names(c_df)

c_df %>%
    mutate(region = factor(region))

unique(c_df$region)

# This ggpairs fucntion is great, can really throw stuff in and see what sticks
c_df %>%
    select(type_of_violence, region, best) %>%
    filter(best < 100) %>%
    ggpairs(mapping = aes(color = region))

# Need type of to be factor
c_df$type_of_violence <- as.factor(c_df$type_of_violence)

# This is beautiful. Easy ggplot for histogram of deaths, w/ Region Facet
c_df %>%
    filter(best <50) %>%
    ggplot(aes(x = best)) +
    geom_histogram(bins = 50, aes(fill=region)) + 
    facet_wrap(~region) +
    xlab('Deaths Best Estimate')

# Practice Plots
ggplot(c_df, aes(group = type_of_violence, y=best, x = date_end)) +
    geom_line()

ggplot(c_df, aes(group = type_of_violence, y=best, x = type_of_violence, fill = type_of_violence)) +
    geom_bar(stat = 'identity') +
    scale_color_viridis(discrete = TRUE, option = "D") +
    scale_fill_viridis(discrete = TRUE)

ggplot(c_df, aes(group = type_of_violence, y=best, x = type_of_violence, fill = type_of_violence)) +
    geom_boxplot(stat = 'identity') +
    scale_color_viridis(discrete = TRUE, option = "D") +
    scale_fill_viridis(discrete = TRUE)

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


