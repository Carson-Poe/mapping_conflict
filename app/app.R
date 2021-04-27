#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
library(leaflet)
library(leaflet.providers)
library(lubridate)
# Read data from rds



load(file = 'data/conflict_19.Rds')

# Define UI for application that draws a histogram
ui <- fluidPage(
        h3('A Dashboard Demo'),
        p(),
        
        sidebarLayout(position = 'left',
                sidebarPanel(img(src = 'university_logo.png', height = 200, width = 250),
                             sliderInput('n', 'Number of Points to Display', value = 5, min = 1, max = 1000),
                            selectInput("country", "Select Country", selected = 'Ukraine', unique(c_df$country)),
                             selectInput('civ', 'Civilian Deaths', selected = 'yes', c('yes', 'no', 'both')),
                             selectizeInput('side', 'Actor', selected = 'Government of Ukraine', choices = unique(c('All', c_df$side_a, c_df$side_b)))), #This is where the avg would go
                mainPanel(leafletOutput("mymap"),
                          tableOutput('mytable'),
                          plotOutput(outputId = "plot")
        )
))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
        
    
    
        plot_points <- reactive({
                
                x <- function(data) {
                        if (input$civ == 'yes') {
                                return(filter(data, civ_cat == input$civ))
                        } else if (input$civ == 'no'){
                                return(filter(data, civ_cat == input$civ))
                        } else {
                                return(filter(data))
                        }
                }
                
                if (input$side == 'All') {
                    c_df %>% 
                        filter(country == input$country) %>% 
                        arrange(desc(best)) %>%
                        x() %>% 
                        head(input$n) %>% 
                        mutate(date_start = sub('T.+', '', as.character(date_start)))
                } else {
                    c_df %>% 
                            filter(country == input$country) %>% 
                            arrange(desc(best)) %>%
                            filter(side_a %in% input$side | side_b %in% input$side) %>% 
                            x() %>% 
                            head(input$n) %>% 
                            mutate(date_start = sub('T.+', '', as.character(date_start)))
                }
        })
        
        
        side.choices <- reactive({
                za <- c_df %>% filter(country %in% input$country)
                unique(c('All', za$side_a, za$side_b))
        })
        
        
        observe({
                updateSelectizeInput(session, 'side', choices = side.choices())
        })
        
        
        output$mymap <- renderLeaflet({
                
                
                leaflet(plot_points()) %>% 
                        addMarkers(data = plot_points(), lng = plot_points()$longitude, lat = plot_points()$latitude) %>% 
                        addProviderTiles(providers$Stamen.TonerLite,
                                         options = providerTileOptions(noWrap = TRUE))
        })
        
        output$mytable <- renderTable({plot_points() %>%
                select(date_start, country, side_a, side_b, deaths = best, deaths_civilians)},  
                                      hover = TRUE,  
                                      align = 'c',  
                                      digits = 2) 
     

        
}

# Run the application 
shinyApp(ui = ui, server = server)
