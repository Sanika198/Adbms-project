#install.packages('shinythemes')
library(shiny)
library(shinythemes)
library(rsconnect)
library(plotly)
bootstrapPage(theme = shinytheme("superhero"),
              navbarPage("Movie Prediction System", inverse="TRUE",fluid = "TRUE"),
              sidebarLayout(
                sidebarPanel(h4("Linear Model"),
                selectInput("linear", label = h3("Select to view Results below"), 
                            choices = list("R^2" = rSquared,
                                           "MSE" = a), selected = 1),
                submitButton("Submit"),
                br(),
                h3("Linear Model Result:"),
                textOutput("Linear_res"),
               # actionButton("btn","Click to display Actual IMDB score Graph table", click="btn1")
               br(),
               h4("Regression Model"),
               selectInput("reg", label = h3("Select to view Results below"), 
                           choices = list("COR" = reg_corr,
                                          "MSE" = reg_mean), selected = 1),
               submitButton("Submit"),
               br(),
               h3("Regression Model Result:"),
               textOutput("reg_res")
               
                ),
               
               
                
                mainPanel("Results",
                          tabsetPanel(
                          
                         tabPanel("Correlation Table",dataTableOutput('mytable')),
                         tabPanel("Correlation Graph",plotOutput('plot1')),
                         tabPanel("IMDB_Prediction Graph",plotOutput('plot2')),
                         tabPanel("IMDB_actual Graph",plotOutput('plot3')),
                         tabPanel("Predicted vs Actual Table",dataTableOutput('mytable1'))
                         
                         ))
               
    
            )   
               
              
              )
            

              
              
              