library(ggplot2)
library(GGally)
library(dplyr)
library(tree)
library(rpart)
library(rpart.plot)
library(plotly)
library(corrplot)
library(corrr)
library(ggthemes)
library(magrittr)
library(lubridate)
library(reshape2)
library(corrgram)
library(randomForest)

################################################################

# DIVIDING DATA INTO TEMP AND TEST RUNTIME
setwd("C:/Program Files/RStudio/bin")
data<- read.csv("movie_metadata.csv")
train<-sample_frac(data, 0.7) #train data which has 70% of data
sid<-as.numeric(rownames(train)) #Because rownames() returns character
test<-data[-sid,] #test data which has 30% of data 

################################################

#TO EXTRACT THE NUMERICAL DATA FROM DATASET

columns <- c()
for(k in 1:dim(Sorted_data)[2])
{
  print(k)
  if(is.numeric(Sorted_data[,k])|| is.integer(Sorted_data[,k]))
  {
    columns[k]=T
  }
  else
  {
    columns[k]=F
  }
}
#na.omit returns the object with incomplete cases removed like missing data eliminated columns wise.
#TEMP DATAFRAME HOLDS THE NUMERICAL DATA
temp <- na.omit(Sorted_data[,columns])


s1<-within(temp, rm(genres,actor_3_name,imdb_score,actor_1_name,director_name,actor_2_name,color,num_critic_for_reviews,duration,gross,num_voted_users,facenumber_in_poster,plot_keywords,movie_imdb_link,num_user_for_reviews,language,country,content_rating,budget,aspect_ratio))

attach(s1)
#################################################################

#Calculating top  hit movies

k<-1
o<-0
yearlist<-list()

for(i in 1:2000)
{
  if(identical(s1[i,5],s1[i+1,5]) == FALSE)
  {
    # unique_years[length(unique_years) + 1] <- s1[i,5]
    yearlist[k]<-s1[i,5]
    k=k+1
  }
}
print(yearlist)
movielist<- list()
m <- 1
for(g in 1:59)
{
  #likes <- likes + (year_likes[q,1]) + (year_likes[q,2]) + (year_likes[q,3]) + (year_likes[q,4]) + (year_likes[q,6]) + (year_likes[q,7])
  o = s1$director_facebook_likes[g] + s1$actor_3_facebook_likes[g] + s1$actor_1_facebook_likes[g] + s1$cast_total_facebook_likes[g] + s1$actor_2_facebook_likes[g] + s1$movie_facebook_likes[g]
  movielist[m] = o
  print(movielist[m])
  print(s1[g,5])
  m=m+1
}
print(movielist)
class(movielist)
plot(x=yearlist,y=movielist, type = "h", xlab = "year" , ylab = "movies")


#Num_likes_year_wise <- matrix(Num_likes_per_year,nrow=58,ncol=1)


################################################
#Facebook likes
################################################

movielikes <- list()
moviebudget <- list()

for(v in 1 : 500)
{
  movielikes[v] <- Sorted_data$movie_facebook_likes[v]
}
print(movielikes)
for(f in 1:500)
{
  if(is.na(Sorted_data$budget[f]))
  {
    moviebudget[f] <- 0
  }
  moviebudget[f] <- Sorted_data$budget[f]
}
print(moviebudget)

plot(x = movielikes, y = moviebudget, type="h", xlab ="movies_facebook_likes",ylab ="Movie_budget")



