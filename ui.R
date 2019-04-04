#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

source("www/functions/guestAnalysisFun.R")

visit_date.df <- read_csv("C:/Users/wendy/OneDrive/Documents/Homework/COE/Data Visualization/final/recruit-restaurant-visitor-forecasting/air_visit_data.csv")
store_info.df <- read_csv("C:/Users/wendy/OneDrive/Documents/Homework/COE/Data Visualization/final/recruit-restaurant-visitor-forecasting/air_store_info.csv")

# Define UI for application 
shinyUI(

  fluidPage(
  
  
  # Application title
  titlePanel("Guest Analysis"),
  
   fluidPage(
     fluidRow( 
       h3("General View of the Dataset", align = "center"),
       
       column(5, 
              
              fluidRow(
                
                plotlyOutput(outputId = "totalPlot")
                #verbatimTextOutput(outputId = "hover")
                
                )
              
              ),
 
       column(7,   
              
                column(6, plotlyOutput(outputId = "monthPlot")),
                
                column(6, plotlyOutput(outputId = "weekDayPlot"))
                
              )
    )
  ),
  
     br(),
  
     fluidPage(
       h3("Detail View of the Dataset", align = "center"),
      sidebarLayout(
        sidebarPanel(
          
          # textInput(inputId = "store_id", label = "Please enter a store ID:", 
          #          value = "air_00a91d42b08b08d9")
          
          #id_list <- unique(visit_date.df[,1]),
          #visit_date.df %>% distinct(air_store_id)
          
          selectInput(inputId = "store_id", label = "Please select a store ID:",
                      choices = visit_date.df %>% distinct(air_store_id), 
                      selected = "air_ba937bf13d40fb24"),
          br(),
          br(),
          br(),
          
          dateRangeInput(inputId = "visit_date_range", label = "Please enter the date 
                     range you want to check out:", start = "2016-01-01",
                         end = "2017-04-22", min = "2016-01-01", max = "2017-04-22")
        ),
      

      mainPanel( 
           plotlyOutput(outputId = "storePlot")
         )
           
     )
     ),      
     
     fluidPage(
       
       sidebarLayout(
         
      sidebarPanel(
 
      
      awesomeCheckboxGroup(inputId = "res_genre", label = "Please select at least 
                         one store genre:", 
           choices =  unique(store_info.df$air_genre_name),
           selected = c("Italian/French", "Dining bar", "Yakiniku/Korean food",
                        "Cafe/Sweets","Izakaya", "Okonomiyaki/Monja/Teppanyaki",
                        "Bar/Cocktail", "Japanese food", "Western food", 
                        "Creative cuisine", "International cuisine", "Asian",
                        "Karaoke/Party", "Other")),
      
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br()

      
      ),

      mainPanel(
        fluidRow(
          leafletOutput(outputId = "genre_map")
        ),
        
        fluidRow(

         plotlyOutput(outputId = "store_genre_count")

      )
      
       ))),
   
  
  fluidPage(
    
    sidebarLayout(
      sidebarPanel(

          selectInput(inputId = "res_area", label = "Please select an area:", 
                      choices =  unique(store_info.df$air_area_name),
                      selected = "Hyogo-ken Kobe-shi Kumoidori")
      ),
      mainPanel(
       
        plotOutput(outputId = "area_genre" , height = 200, width = 900)
      )
    
    
  )
)
  
))
