#' @title: guestAnalysisFun

#' @author Dataholic

#'
#' @source source("www/functions/guestAnalysisFun.R")
#'
#' 
#' 
#' Loard libs ----
#' 


library(tidyverse)
library(waffle)

guestAnalysisFun <- function(df, area_selected){
  
  if(is.null(df))
    stop("")

  sub_area.df <- df[df$air_area_name %in% area_selected,]
  
  genre_list <- sub_area.df %>%
    group_by(air_genre_name) %>%
    count() 
  
  genre_vec <- unlist(genre_list[2]) 
  
  names(genre_vec) <- unlist(genre_list[1])
  
  wafflePlot <- waffle(genre_vec,rows = 4, size = 1, colors = c('#a6cee3','#1f78b4','#b2df8a','#33a02c',
                                                  '#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6', 
                                                  '#8B8378', '#FF1493', '#0000FF','#363636','#008B8B'),
         title = paste("Waffle Chart of the Type of Restaurant for Area", area_selected))
  
  return(wafflePlot)
}

