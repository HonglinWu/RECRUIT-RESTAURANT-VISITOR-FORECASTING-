#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(tibble)
library(tidyr)
library(lubridate)
library(readr)
library(tidyverse)
library(plotly)
library(waffle)
library(leaflet)

# Source helper functions -----
source("www/functions/guestAnalysisFun.R")


visit_date.df <- read_csv("C:/Users/wendy/OneDrive/Documents/Homework/COE/Data Visualization/final/recruit-restaurant-visitor-forecasting/air_visit_data.csv")
store_info.df <- read_csv("C:/Users/wendy/OneDrive/Documents/Homework/COE/Data Visualization/final/recruit-restaurant-visitor-forecasting/air_store_info.csv")
date_info.df <- read_csv("C:/Users/wendy/OneDrive/Documents/Homework/COE/Data Visualization/final/recruit-restaurant-visitor-forecasting/date_info.csv")

names(date_info.df)[1] <- "visit_date"
date_join <- merge(visit_date.df, date_info.df, by = "visit_date")

# Define server logic required to draw plots
shinyServer(function(input, output, session) {

  
  #Plot the total visitors during the whole date range
  output$totalPlot <-renderPlotly({
    
    totalPlot <- visit_date.df %>%
    group_by(visit_date) %>%
    summarise(all_visitors = sum(visitors)) %>%
    ggplot(aes(visit_date,all_visitors)) +
    geom_line(col = "#32CD32", size = 0.8) +
    labs(y = "All visitors", x = "Date", title = "Total Visitors During the Whole Date Range");
    
   
  });
  
  
  #Print the info of each point
  #output$hover <- renderPrint({
    
    #d <- event_data("plotly_hover");
    #if (is.null(d)) " " else d
    
    #paste("Visit_Date =", as.Date(input$plot_click$x, origin = "1970-01-01"),
    #     "All Visitors =", input$plot_click$y)
  #});
  
  
  
  #Plot the month bar chart
  output$monthPlot <- renderPlotly({
    
     

     monthPlot <- visit_date.df %>%
     mutate(month = month(visit_date, label = TRUE)) %>%
     group_by(month) %>%
     summarise(visits = median(visitors)) %>%
     ggplot(aes(month, visits, fill = month)) +
     geom_col() +
     coord_flip() +
     theme(legend.position = "none") +
     labs(x = "Month", y = "Median visitors", title = "Median Visitors of Each Month");
          
     
     
  });
  
  
  #Plot the week day bar chart
  output$weekDayPlot <- renderPlotly({
    
    weekDay <- date_join %>%
      group_by(day_of_week) %>%
      summarise(visits = median(visitors)) %>%
      ggplot(aes(reorder(day_of_week, visits, FUN = desc), visits, fill = day_of_week)) +
      geom_col() +
      coord_flip() +
      theme(legend.position = "none", axis.text.y = element_text(angle=45, hjust=1, vjust=0.9)) +
      labs(x = "Day of the week", y = "Median visitors", title = "Median Visitors of Each Week Day")

  
  });
  
  
  #Plot the line chart for each store during selected date range
  output$storePlot <- renderPlotly({
    
    store_id <- input$store_id;
    
    visit_date_range <- seq(from = as.Date('2016-01-01'), 
                            to = as.Date('2017-04-22'),
                            by = '1 day');
    
    visit_date_selected <- visit_date_range[visit_date_range >= format(input$visit_date_range[1])&
                                            visit_date_range <= format(input$visit_date_range[2])];
    
    sub_visit.df <- visit_date.df[(visit_date.df$air_store_id == store_id) & 
                                 as.Date(visit_date.df$visit_date) %in% visit_date_selected,];
    
    
    
    storePlot <- sub_visit.df %>%
    separate(visit_date, into = c("year", "month", "day"), sep = "-") %>%
    unite(year_month, year, month, sep = "-") %>%
    group_by(year_month) %>%
    summarise(all_visitors = sum(visitors)) %>%
    ggplot(aes(year_month, all_visitors)) +
    geom_line(aes(group = 1), color="#000099") +
    geom_point(col = "#8E388E", size = 4) +
    theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9)) +
    labs(x = "Year_Month", y = "All visitors", 
        title = paste("The sum of visitors of store", store_id, 
    "within date range from", input$visit_date_range[1], "to", 
        input$visit_date_range[2]));
   
    
  });
  
  
  #Plot the map for restaurants
  output$genre_map <- renderLeaflet({
    
    genre_selected <- input$res_genre;
    
    sub_genre.df <- store_info.df[store_info.df$air_genre_name %in% genre_selected,]
    
   # c_color <- colorFactor(c("#00CD66","#0000CD","#EEEE00","#FFB90F", "#FF0000", "#B03060",       
   #                       "#9400D3", "#8B5742", "#EE6A50", "#8B8378", "#FF1493",
   #                      "#0000FF","#363636","#008B8B"), 
   #    domain = store_info.df$air_genre_name)
    
   # c_color <- colorFactor("plasma", domain = store_info.df$air_genre_name)
    
    leaflet(sub_genre.df) %>%
      setView(lng = 137.0000, lat = 38.0000, zoom = 5) %>%
      addTiles() %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      addCircleMarkers(~longitude, ~latitude, #color = ~c_color(store_info.df$air_genre_name),
                 popup = ~air_store_id, label = ~air_genre_name, radius = 15,
                 clusterOptions = markerClusterOptions())
  })
    
  
  #Plot the bar chart to count different kinds of restaurants
    output$store_genre_count <- renderPlotly({
      
      genre_selected <- input$res_genre;
      
      sub_genre.df <- store_info.df[store_info.df$air_genre_name %in% genre_selected,]
      
      sotre_genre_count <- sub_genre.df %>%
        group_by(air_genre_name) %>%
        count() %>%
        ggplot(aes(reorder(air_genre_name, n, FUN = desc), n, fill = air_genre_name)) +
        geom_col() +
        theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9)) +
        labs(x = "Type of restaurant", y = "Number of restaurants",
             title = "Number of restaurant for selected restaurant type")
      
      
    });
  
    #Plot the waffle chart of the type of restaurant for different area
    output$area_genre <- renderPlot({
      
      #area_selected <- input$res_area;
      
      #sub_area.df <- store_info.df[store_info.df$air_area_name %in% area_selected,]
      
      #genre_list <- sub_area.df %>%
      #  group_by(air_genre_name) %>%
       # count() 
      
     # genre_vec <- unlist(genre_list[2]) 

      #names(genre_vec) <- unlist(genre_list[1])
        
      #waffle(genre_vec,rows = 4, size = 1, colors = c('#a6cee3','#1f78b4','#b2df8a','#33a02c',
      #                                  '#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6', 
       #                               '#8B8378', '#FF1493', '#0000FF','#363636','#008B8B'),
      #       title = paste("Waffle Chart of the Type of Restaurant for Area", area_selected))

     guestAnalysisFun(store_info.df, input$res_area)
     
    })
  
})
