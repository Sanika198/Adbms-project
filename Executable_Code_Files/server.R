library(shiny)
library(plotly)
shinyServer(
  function(input,output){
   # output$linear<-renderPlot({plot(z$x,z$y)})
    
      output$Linear_res<-renderText(input$linear)
      output$reg_res<-renderText(input$reg)
        output$mytable<-renderDataTable({z})
        output$mytable1<-renderDataTable({pred_actual})
        #col<-as.numeric(x)
        #row<-as.numeric(y)
        #output$mygraph<-renderPlot({hist(mytable)})
        output$plot1 <- renderImage({
          # When input$n is 1, filename is ./images/image1.jpeg
          filename <- normalizePath(file.path('D:/R studio/RStudio/bin/',
                                              paste('Lin_Cor_Graph', input$n, '.png', sep='')))
          
          # Return a list containing the filename
          list(src = filename)
        }, deleteFile = FALSE)
        
        #output$plot2 <- renderImage({
          # When input$n is 1, filename is ./images/image1.jpeg
         # filename <- normalizePath(file.path('D:/R studio/RStudio/bin/',
       #                                       paste('Pred_actual_plot', input$n, '.png', sep='')))
          
          # Return a list containing the filename
          #list(src = filename)
      #  }, deleteFile = FALSE)
        output$plot2<-renderPlot({
           hist(p.rpart,col = "light green",border = "black")
          
          #hist(act_imdb,col = "red",border = "black")
        }
          
        )
        output$plot3<-renderPlot({
          #hist(p.rpart,col = "light green",border = "black")
          
          hist(act_imdb,col = "red",border = "black")
        }
        
        )
      
      }
  
)