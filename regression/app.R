
#https://bookdown.org/paulcbauer/idv2/8-20-example-a-simple-regression-app.html

#To do:
#Make factor variables group variables in ggplot
#Allow multiple variables


library(shiny)

df<-readRDS(here::here("regression/washb_data.RDS"))

#"laz","tr","momage","hfias","sex","elec"

ui <- fluidPage(
  titlePanel("Regression Model (Dataset: WASH Benefits)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("outcome", label = h3("Outcome"),
                  choices = list("laz" = "laz"), selected = 1),
      
      selectInput("indepvar", label = h3("Explanatory variable"),
                  choices = list("hfias" = "hfias",
                                 "momage" = "momage",
                                 "tr" = "tr",
                                 "sex" = "sex",
                                 "elec" = "elec"), selected = 1)
      
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Scatterplot", plotOutput("scatterplot")), # Plot
                  tabPanel("Distribution", # Plots of distributions
                           fluidRow(
                             column(6, plotOutput("distribution1")),
                             column(6, plotOutput("distribution2")))
                  ),
                  tabPanel("Model Summary", verbatimTextOutput("summary")), # Regression output
                  tabPanel("Data", DT::dataTableOutput('tbl')) # Data as datatable
                  
      )
    )
  ))



# SERVER
server <- function(input, output) {
  
  # Regression output
  output$summary <- renderPrint({
    fit <- lm(df[,input$outcome] ~ df[,input$indepvar])
    names(fit$coefficients) <- c("Intercept", input$var2)
    summary(fit)
  })
  
  # Data output
  output$tbl = DT::renderDataTable({
    DT::datatable(df, options = list(lengthChange = FALSE))
  })
  
  
  # Scatterplot output
  output$scatterplot <- renderPlot({
    plotdf = data.frame(x=df[,input$indepvar], y=df[,input$outcome])
    ggplot(plotdf, aes(x=x, y=y)) +
      geom_point() + geom_smooth(method = "lm")
    
    # plot(df[,input$indepvar], df[,input$outcome], main="Scatterplot",
    #      xlab=input$indepvar, ylab=input$outcome, pch=19)
    # abline(lm(df[,input$outcome] ~ df[,input$indepvar]), col="red")
    # lines(lowess(df[,input$indepvar],df[,input$outcome]), col="blue")
  }, height=400)
  
  
  # Histogram output var 1
  output$distribution1 <- renderPlot({
    hist(df[,input$outcome], main="", xlab=input$outcome)
  }, height=300, width=300)
  
  # Histogram output var 2
  output$distribution2 <- renderPlot({
    hist(df[,input$indepvar], main="", xlab=input$indepvar)
  }, height=300, width=300)
}

shinyApp(ui = ui, server = server)