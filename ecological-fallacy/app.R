
# rm(list=ls())
# library(tidyverse)
# library(ggthemes)
# library(metafor)
# library(shiny)
# library(DT)
# library(xtable)
# 
# #------------------------------------------------
# # Simulate Data 
# #------------------------------------------------
# 
# set.seed(12345)
# n=1000
# r=-.5
# Z=rmnorm(n,c(0,0),matrix(c(1,r,r,1),2,2))
# X=Z[,1]
# E=Z[,2]
# Y=3+2*X+E
# 
# #------------------------------------------------
# # Inputs for Shiny App
# #------------------------------------------------
# 
# #Plot parameters
# yticks <- c( 0.50, 0.75, 1.00, 1.33, 2.00, 4.00, 8.00)
# yticks2 <- c( 0.50, 0.75, 0.91, 1.00, 1.10, 1.33, 2.00, 4.00, 8.00)
# yticks_cont <- c(-20:20/10)
# 
# tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
#                "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")
# tableau11 <- c("Black","#1F77B4","#FF7F0E","#2CA02C","#D62728",
#                "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")
# Ylab <- "Relative Risk"
# scaleFUN <- function(x) sprintf("%.2f", x)
 theme_set(theme_bw())

ui <- bootstrapPage(
  
  
   sliderInput("cor", "Correlation:",
              min = -0.9, max = 0.9,
              value = -0.5, step = 0.1),
    
 
   plotOutput(outputId = "main_plot", height = "300px"),
   plotOutput(outputId = "facet_plot", height = "300px")
    
  )

server <- function(input, output){
  
  selectedData <- reactive({

    set.seed(12345)
    n=1000
    r=input$cor
    Z=rmnorm(n,c(0,0),matrix(c(1,r,r,1),2,2))
    X=Z[,1]
    E=Z[,2]
    Y= 7 + 2*X + 2*E + rnorm(n, 0, 3)

    
    # X= rnorm(n, 5, 1)
    # Y= 3+2*X+ E + rnorm(n)
    # E= r*Y + r*X


    I=cut(Z[,2],qnorm(seq(0,1,by=.05)), labels=
            c(
              "Austria",
              "Italy",
              "Belgium",
              "Bulgaria",
              "Lithuania",
              "Croatia",
              "Netherlands",
              "Denmark",
              "Estonia",
              "Poland",
              "Portugal",
              "Finland",
              "Romania",
              "France",
              "Germany",
              "Slovenia",
              "Greece",
              "Spain",
              "Hungary",
              "Sweden"
            ))
    Yg=tapply(Y,I,mean)
    Xg=tapply(X,I,mean)

    df <- data.frame(X,Y, Yg, Xg,  I)

    df
  })

 
    
  output$main_plot <- renderPlot({

    df <- selectedData()
    ggplot(df, aes(x=Xg, y=Yg)) +geom_point() + geom_smooth(method="lm")   +
      stat_regline_equation(label.y = 8) + stat_cor(label.y = 10)

  })
  output$facet_plot <- renderPlot({
    
    df <- selectedData()
    ggplot(df, aes(x=X, y=Y)) +geom_point() + geom_smooth(method="lm") + 
      facet_wrap(~I) +
      stat_regline_equation(label.y = 8) + stat_cor(label.y = 10)
    
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

