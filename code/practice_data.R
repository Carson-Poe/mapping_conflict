
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

load("C:/Users/Car/Desktop/Messing Around/Exploration Mapping/mapping_conflict/data/conflict_19.Rds")


# Fast Exploration --------------------------------------------------------

# USA involved
View(c_df %>%
    filter(side_b_new_id == 769 | side_a_new_id == 769))

# Look at all
View(c_df)

# Wow, this function is nice
skim(c_df)

# This is looking better already. Starting to see some interesting stuff. Ish
# Way better since I've been watching Julia Silge and David Robinson
# Get count by country
c_df %>% 
    select(country, best, date_start, region, best) %>% 
    add_count(country) %>% 
    filter(n > 100) %>% 
    ggplot(aes(date_start, best, color = country)) +
    geom_line() +
    facet_wrap(~country, scales = 'free_y')

# The problem with the above graph is it does up and down lines, like, the 
# 'best estimate' isn't a great variable to plot here, we could do boxplots

c_df %>% 
    select(country, best, date_start, region, best) %>% 
    add_count(country) %>% 
    filter(n > 100, best < 300) %>% 
    ggplot(aes(y = best, x = country)) +
    geom_boxplot() +
    labs(y = 'Deaths', x = 'Country') +
    facet_wrap(~country, scales = 'free')

# That looks a bit better, also, it seems like plotting by best isn't a great idea
# maybe average by country, first attempt didn't work, I couldn't pass the mutate
# into the geom_ properally, I had multi rows per country. I wonder, gggplot might
# not allow for grouped to be passed in. 
# Solution was to create a new df, though I probably could have done all the summarize stuff
# down there. Actually, lets try that

# Okay, really cool plot. Doesn't show st dev, but shows average deaths, plus sample
# size, ordered by most
c_df %>% 
    select(country, best, date_start, region, best) %>% 
    add_count(country) %>% 
    filter(n > 100) %>% 
    group_by(country) %>% 
    mutate(average = (sum(best)/n), sum_deaths = sum(best)) %>% 
    select(-best, -date_start) %>% 
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
    scale_y_continuous( n.breaks = 8)


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


# Try some of those Shiny world maps
# No wait
str(c_df)

conflict_plot <- c_df %>%
    select(country, type_of_violence, latitude, longitude, best) %>% 
    mutate(type_of_violence = factor(type_of_violence))

conflict_plot %>% 
    ggplot() +
    geom_polygon_interactive(size = .1, aes(x = longitude, y = latitude, size = best,
                                            tooltip = country)) +
    scale_fill_gradientn(colors = brewer.pal(3, 'RdBu'))



# OMG THIS IS IT. THE WORLD MAP! Thank you JULIA SILGE! 
world <- map_data('world')
View(c_df)
ggplot() +
    geom_map(data = world, map = world, aes(long, lat, map_id = region), 
             color = 'white', fill = 'gray50', alpha = .2) +
    geom_point(data = c_df, aes(longitude, latitude, color = as.factor(type_of_violence)), 
               alpha = .8) +
    theme() +
    scale_color_brewer(palette = 'Set1')

# Size is deaths, world map
world <- map_data('world')
View(c_df)
ggplot() +
    geom_map(data = world, map = world, aes(long, lat, map_id = region), 
             color = 'white', fill = 'gray50', alpha = .2) +
    geom_point(data = c_df, aes(longitude, latitude, color = as.factor(type_of_violence),
                                size = best), 
               alpha = .8) +
    theme() +
    scale_color_brewer(palette = 'Set1')

# Now to map over time, see the David Robinson big mac series
# Should be the same

big_mac %>%
    filter(!is.na(gdp_dollar)) %>% 
    ggplot(aes(gdp_dollar, usd_adjusted))+
    geom_point() +
    geom_text_repel(aes(label = country)) +
    geom_smooth(method = 'lm') + 
    transition_time(date) +
    labs(x = 'GDP per capita (dollars)', 
         y = 'Raw Big Max Index relative to USD',
         title = '{ frame_time }')

# tibble
c_df <- c_df %>% 
    as_tibble()

# January only, change to dates
c_jan_19 <- c_df %>% 
    mutate(date_start = as.Date(date_start), date_end = as.Date(date_end)) %>% 
    filter(date_start <= '2019-1-31' & date_start >= '2019-1-1') 

# Create world data set
world <- map_data('world')

# OMG It's working, had to group by ID becuase it was grouping by some other
# factor. Now can we make our transitions smoother. As well as go moth to month?

z <- ggplot() +
    geom_map(data = world, map = world, aes(long, lat, map_id = region), 
             color = 'white', fill = 'gray50', alpha = .2) +
    geom_point(data = c_jan_19, aes(longitude, latitude, group = id, color = type_of_violence), 
               alpha = .8) +
    scale_color_brewer(palette = 'Set1') +
    transition_time(date_start) +
    theme(legend.background = element_rect(fill = 'grey50', 
                                           size = .5, linetype = 2)) +
    labs(x = 'Longitude', 
         y = 'Latitude', 
         title = '{ frame_time }') +
    enter_fade()
    
animate(z, height = 800, width = 1000, nframes = 100)

# Really love this plot, how can we immitate?
data <- data.frame(
    x = 1:10,
    y = runif(10),
    begin = runif(10, 1, 100),
    length = runif(10, 5, 20),
    enter = runif(10, 5, 10),
    exit = runif(10, 5, 10)
)

anim <- ggplot(data, aes(x, y)) +
    geom_col() +
    transition_events(start = begin,
                      end = begin + length,
                      enter_length = enter,
                      exit_length = exit) +
    enter_grow() +
    exit_drift(x_mod = 11) +
    exit_fade()
