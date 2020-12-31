
extrafont::loadfonts(device="win")
library(tidyverse)
library(xlsx)
library(httr)
library(RJSONIO)
library(r2d3)
library(sf)
library(ggplot2)
theme_set(theme_bw())
library(skimr)
library(GGally)
library(maps)
library(scales)
library(gganimate)
library(plotly)
library(ggplot2)
library(sf)
library(lubridate)

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

load("C:/Users/Car/Desktop/Messing Around/Exploration Mapping/mapping_conflict/data/conflict_19.Rds")


# Fast Exploration --------------------------------------------------------

# tibble
c_df <- c_df %>% 
    as_tibble(c_df)

# USA involved
View(c_df %>%
    filter(side_b_new_id == 769 | side_a_new_id == 769))

# Look at all
View(c_df)

# Wow, this function is nice
skim(c_df)

# Create a civilian deaths categorical variable
c_df <- c_df %>% 
    mutate(civ_cat = case_when(deaths_civilians > 0 ~ 'yes', TRUE ~ 'no'))

# How many?
c_df %>% 
    count(civ_cat)

# This would make a great variable to predict. 

# This is looking better already. Starting to see some interesting stuff. Ish
# Way better since I've been watching Julia Silge and David Robinson

# Okay, loving this plot, group by country and rounded dates, sum deaths, plot
# Filtering by more than 300 events here.
c_df %>% 
    add_count(country) %>% 
    filter(n > 300) %>% 
    mutate(rounded_date = floor_date(as.Date(date_start), unit = 'month')) %>% 
    group_by(country, rounded_date) %>% 
    mutate(sum_deaths = sum(best)) %>% 
    select(country, sum_deaths, rounded_date) %>% 
    ggplot(aes(rounded_date, sum_deaths, color = country)) +
    geom_line() +
    theme(axis.text.x = element_text(angle = 45, vjust = .5), legend.position = 'none') +
    labs(x = 'Dates', y = 'Total Deaths') +
    facet_wrap(~country, scales = 'free_y')

# Okay, really cool plot. Doesn't show st dev, but shows average deaths, plus sample
# size, ordered by most
c_df %>% 
    select(country, best, date_start, region, best) %>% 
    add_count(country) %>% 
    filter(n > 100) %>% 
    group_by(country) %>% 
    mutate(average = (sum(best)/n), sum_deaths = sum(best)) %>% 
    select(-best, -date_start) %>% 
    # Seems like a cheater way to limit number of rows
    summarise(region = unique(region),
          n = max(n), 
          average = max(average),
          sum_deaths = max(sum_deaths)) %>% 
    mutate(country = fct_reorder(country, -average)) %>% 
    ggplot(aes(country, average, fill = country))+
    geom_col() +
    geom_text(aes(label = paste0('n= ', n), angle = 90), hjust = 'top') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position =  'none') +
    labs(x = 'Country', y = 'Average Deaths per Violent Encounter 2019') +
    scale_y_continuous(n.breaks = 8)


# This ggpairs fucntion is great, can really throw stuff in and see what sticks
c_df %>%
    add_count(region) %>% 
    filter(best < 100) %>% 
    filter(n > 100) %>% 
    select(civ_cat, region, best) %>%
    ggpairs(mapping = aes(color = region))

# Need type of to be factor
c_df$type_of_violence <- as.factor(c_df$type_of_violence)

# This is beautiful. Easy ggplot for histogram of deaths, w/ Region Facet
c_df %>%
    filter(best < 50) %>%
    ggplot(aes(x = best)) +
    geom_histogram(bins = 50, aes(fill=region)) + 
    facet_wrap(~region) +
    xlab('Deaths Best Estimate') +
    labs(title = 'Are some regions events more deadly?', y ='Number of Events')

# Mapping Conflict Data ---------------------------------------------------

# One is state based, two is non-state based, three is one sided
# OMG THIS IS IT. THE WORLD MAP! Thank you JULIA SILGE! 
world <- map_data('world')

# 
ggplot() +
    geom_map(data = world, map = world, aes(long, lat, map_id = region), 
             color = 'white', fill = 'gray50', alpha = .2) +
    geom_point(data = c_df, aes(longitude, latitude, color = as.factor(type_of_violence)), 
               alpha = .8) +
    theme(legend.title = element_text('Type of Violence')) +
    labs(title = 'Global Violence') +
    scale_color_brewer(palette = 'Set1', labels = c('State Based', 'Non-State', 'One-Sided')) +
    guides(color = guide_legend('Type of Violence'))


type_of_violence


#Type of UCDP organized violence:
    #1: state-based conflict
    #2: non-state conflict
    #3: one-sided violence

# Okay, kinda like, civilian deaths mapped globally
ggplot() +
    geom_map(data = world, map = world, aes(long, lat, map_id = region), 
             color = 'white', fill = 'gray50', alpha = .3) +
    geom_point(data = c_df, aes(longitude, latitude, 
                                color = as.factor(civ_cat), 
                                group = id), alpha = .8) +
    theme() +
    scale_color_brewer(palette = 'Dark2') +
    labs(title = 'Conflict Civilian Deaths 2019', x ='', y= '') +
    guides(color = guide_legend('Civilian Deaths'))


# January only, change to dates
c_jan_19 <- c_df %>% 
    mutate(date_start = as.Date(date_start), date_end = as.Date(date_end)) %>% 
    filter(date_start <= '2019-1-31' & date_start >= '2019-1-1') 

# Create world data set
world <- map_data('world')

# OMG It's working, had to group by ID becuase it was grouping by some other
# factor. Now can we make our transitions smoother. As well as go moth to month?

# Global over January '19
z <- ggplot() +
    geom_map(data = world, map = world, aes(long, lat, map_id = region), 
             color = 'white', fill = 'gray50', alpha = .2) +
    geom_point(data = c_jan_19, aes(longitude, latitude, group = id, color = type_of_violence), 
               alpha = .8) +
    transition_time(date_start) +
    theme(legend.background = element_rect(fill = 'grey50', 
                                           size = .5, linetype = 2)) +
    labs(x = 'Longitude', 
         y = 'Latitude', 
         title = 'A Month In Conflict: { frame_time }') +
    enter_fade()
    
animate(z, height = 800, width = 1000, nframes = 100)


# Afghanistan -------------------------------------------------------------

# Set up data frame
# January only, Afghanistan only, change to dates, select specific columns
c_jan_19_af <- c_df %>% 
    filter(country == 'Afghanistan') %>%
    select(id, best, latitude, longitude, side_a, side_b, date_start, date_end) %>% 
    mutate(date_start = as.Date(date_start), date_end = as.Date(date_end)) %>% 
    filter(date_start <= '2019-1-31' & date_start >= '2019-1-1') 

# Get shapefile -- https://hub.arcgis.com/datasets/2b63527870ef416bacf83bcaf388685f_0/data
afg_sf <- read_sf('data/afghanistan')

# Plot afg animated and save to object
afg <- ggplot() +
    geom_sf(data = afg_sf, fill = 'gray50', alpha =.1) +
    geom_point(data = c_jan_19_af, aes(longitude, latitude, group = id,
                                       frame = date_start), 
               alpha = .8, color = 'red', size = 2) +
    transition_time(date_start) +
    labs(x = 'Longitude', 
         y = 'Latitude',
         title = 'Afghanistan Conflict: {frame_time}') 
    #enter_fade() + 
    #exit_recolor(color = 'black')
    #shadow_mark(color = 'black') # Keeps points on map

# Useful cheat sheet - https://ugoproto.github.io/ugo_r_doc/pdf/gganimate.pdf

# Animate afg object to the right size
animate(afg, height = 800, width = 1000, nframes = 100)

c_jan_19_af %>% 
    count(id) 

# Beautiful, needed ids and frame for plotly. Frame needs to be in numeric or prob character
# maybe as.Date as.char
afg <- 
    ggplot(data = c_jan_19_af) +
    geom_sf(data = afg_sf, fill = 'gray50', alpha =.1) +
    geom_point(data = c_jan_19_af, aes(longitude, latitude, ids = id,
                                       frame = as.character(date_start)),
               alpha = .8, color = 'darkred', size = 1) +
    labs(x = 'Longitude', 
         y = 'Latitude',
         title = 'Conflict in Afghanistan January 2019')

# Plotly instead?
afg <- ggplotly(afg, width = 500, height = 500)

# Need to re-run to see if this fixes sizing - 
afg %>% 
    animation_opts(1000, easing = "linear", redraw = FALSE) 




c_jan_19_af

# save it and we shall see
htmlwidgets::saveWidget(afg, 'afg.html')


#  India -------------------------------------------------------------------

# Set up data frame
# January only, India only, change to dates, select specific columns
c_jan_19_in <- c_df %>% 
    filter(country == 'India') %>%
    select(id, best, latitude, longitude, side_a, side_b, date_start, date_end) %>% 
    mutate(date_start = as.Date(date_start), date_end = as.Date(date_end)) %>% 
    filter(date_start <= '2019-1-31' & date_start >= '2019-1-1') 

# Get shapefile -- https://hub.arcgis.com/datasets/2b63527870ef416bacf83bcaf388685f_0/data
india_sf <- read_sf('data/india')

# Beautiful, needed ids and frame for plotly. Frame needs to be in numeric or prob character
# maybe as.Date as.char
ind <- 
    ggplot(data = c_jan_19_in) +
    geom_sf(data = india_sf, fill = 'gray50', alpha =.1) +
    geom_point(data = c_jan_19_in, aes(longitude, latitude, ids = id,
                                       frame = as.character(date_start)),
               alpha = .8, color = 'darkred', size = 1) +
    labs(x = 'Longitude', 
         y = 'Latitude',
         title = 'Conflict in India January 2019')

# Plotly instead?
ind <- ggplotly(ind, width = 500, height = 500)

# Add opts
ind %>% 
    animation_opts(1000, easing = "linear", redraw = FALSE) 



# save it and we shall see
htmlwidgets::saveWidget(ind, 'india.html')


# Modeling ----------------------------------------------------------------

# Can we predict whether there were civilian deaths? 

# Use strata

# How many?
c_df %>% 
    count(civ_cat)

