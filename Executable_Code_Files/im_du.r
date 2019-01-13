library(dplyr)
library(magrittr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(plotly)
library(shiny)
library(DT)
library(reshape2)
library(splitstackshape)
library(stringr)
library(corrplot)
library(corrr)
library(ggthemes)


#################################################################
setwd("C:/Program Files/RStudio/bin")
data<- read.csv("movie_metadata.csv")
#DURATION VS IMDB SCORE (NUMBER OF USER REVIEWS)

data$`Number of User Reviews` <- data$num_user_for_reviews 
data <- select(data, -num_user_for_reviews)

data$movie_title %<>% stringr::str_replace("?",replacement="") 

Score_Dur <- ggplot(data, aes(x=duration, y=imdb_score,
                              text=paste("Movie Title: ", test$movie_title, "<br>Duration in Hours and Minutes: ",
                                         duration %>%
                                           minutes() %>%
                                           period_to_seconds() %>%
                                           seconds_to_period() %>%
                                           as.character() %>%
                                           stringr::str_replace(" 0S", replacement = ""),
                                         sep=""))) +
  geom_point(size=1, aes(colour=`Number of User Reviews`)) +
  scale_colour_gradientn(colours = rev(rainbow(6))) +
  theme(plot.title = element_text(face = "bold")) +
  labs(title = "Duration Vs. IMDB Score (and Number of User Reviews)", 
       x = "Duration", y = "IMDB Score")

ggplotly(Score_Dur, hoverinfo="text")

