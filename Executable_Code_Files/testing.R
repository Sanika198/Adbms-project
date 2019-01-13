# packages#
help('plot')
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
#To install missing packages
#install.packages('randomForest')

########################
# DIVIDING DATA INTO TEMP AND TEST RUNTIME
setwd("C:/Program Files/RStudio/bin")
data<- read.csv("movie_metadata.csv")
train<-sample_frac(data, 0.7) #train data which has 70% of data
sid<-as.numeric(rownames(train)) #Because rownames() returns character
test<-data[-sid,] #test data which has 30% of data 

################################################
#The attach() function in R can be used to make objects within dataframes accessible in R with fewer keystrokes
attach(test) 
#order is used to sort data by default it is ascending and for desc [-column name,]
Sorted_data<-test[order(title_year),] 
class(Sorted_data)
#################################
#rm() to remove columns that are not required
#s1<-within(Sorted_data, rm(genres,actor_3_name,imdb_score,actor_1_name,director_name,actor_2_name,color,num_critic_for_reviews,duration,gross,num_voted_users,facenumber_in_poster,plot_keywords,movie_imdb_link,num_user_for_reviews,language,country,content_rating,budget,aspect_ratio))
###############################

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

##########################################
#LINEAR MODEL
##########################################
# Correlation to find the  x and y value relation or relation of 1 variable with all other

correlation <- c()
for(i in 1:dim(temp)[2])
{
  correlation[i] <- cor(temp[,i],temp[,'imdb_score'])
  
}
print(colnames(temp))
columns_data<-data.frame(colnames(temp))
correlation #prints correlation results
x<-as.vector(columns_data)
y<-as.vector(correlation)
z<-data.frame(x,y)
print(z)
plot(x=x,y=y,type="b",col="red")

####################################################

ggplot(temp, aes(x=num_voted_users, y=imdb_score)) + geom_point(colour="blue") +
  stat_smooth(method=lm, se=FALSE, colour="black")+ggtitle(paste('R:',correlation[7]))


#scipen fun helps to display all numbers after decimal point
options(scipen = 999) 
lmfit <- lm(imdb_score ~ num_voted_users,data=test)
summary(lmfit)
rSquared <- summary(lmfit)$r.squared

pred <- predict(lmfit,test)
#plot(pred)
set.seed(100)
a<-mean((temp$imdb_score-pred)^2) #MSE
print(a)
#help("predict")

###################################################################
## Regression trees
###################################################################
library(rpart)
#install.packages("rpart")
temp
set.seed(3)
train <- sample(dim(temp)[1],dim(temp)[1]*0.9)
train
temp_train <- temp[train,]
temp_test <- temp[-train,]
m.rpart <- rpart(imdb_score~.,data=temp_train)
m.rpart


rpart.plot(m.rpart,digits = 3)
## Testing this model

#We apply this model to the test data.


p.rpart <- predict(m.rpart,temp_train)

p.rpart_df<-data.frame(p.rpart)
act_imdb<-temp_train$imdb_score
act_imdb

#How do the actual and predicted models differ? We visualize this by the use of histograms.


tree_dataframe <- data.frame(p.rpart,temp_train$imdb_score)
pred_actual<-data.frame(tree_dataframe)



#View(pred_actual)
ggplot(tree_dataframe, aes(x=p.rpart)) + geom_histogram(fill="blue", colour="green")
ggplot(tree_dataframe, aes(x=temp_train.imdb_score)) + geom_histogram(fill="red", colour="orange")



#We see that the distributions of the predictions are in no way close to that of the distributions of the  real values.
#What is the correlation between the predicted and the actual values?


reg_corr<-cor(p.rpart_df,act_imdb)
reg_corr


#This is a good correleation score. But it goes not give a measure of how the predicted values deviate from the
#actual values.

## MSE

#We therefore calculate the mean squared error


reg_mean<-mean((p.rpart_df-act_imdb)^2)
reg_mean

###########################################################

#Calculating top 20 directors

library("ggplot2")
library("RColorBrewer")
library("data.table")
library("corrgram")
#install.packages("data.table")

color_scheme = brewer.pal(7, "Accent")
help("brewer.pal")
# Subset the directors from entire dataset
director = test['director_name']

# Count how many times each director is in the dataset
director = data.frame(table(director))

# Sort the dataset by the frequency each director appears
director = director[order(director$Freq,decreasing=TRUE),]

# Remove the row without a director name
director = director[-c(1),]

# Plot the top 20 directors with the most movies
ggplot(director[1:20,], aes(x=reorder(factor(director), Freq), y=Freq, alpha=Freq)) + 
  geom_bar(stat = "identity", fill=color_scheme[7]) + 
  xlab("Director") + 
  ylab("Number of Movies") + 
  ggtitle("Top 20 Directors with the most movies") + 
  coord_flip() # to flip vertical to horizontal


