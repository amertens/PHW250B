

#Too add:
#Need to Show calculations of PAF from prev and RR
#And maybe add a 2x2 table to show calculation of PAF from 2x2
#Also show a treemap with non-diseased numbers


library(epitools)
library(treemap)

ui <- bootstrapPage(
  
  
  numericInput("RR", "Relative Risk:", value = 2, min = 1, max = 50, step = 1),
  numericInput("prev", "Exposure Prevalence:", value = 0.3, min = 0.01, max = 0.99, step = 0.1),
  
  plotOutput(outputId = "main_plot", height = "300px", width = "300px"),
  textOutput("PAF"),
  textOutput("PAF_perc")

)

server <- function(input, output){
  
  selectedData <- reactive({
    
    AR <- (input$RR-1)/input$RR
    PAF <- (input$prev*(input$RR-1))/(input$prev*(input$RR-1)+1)
    
    prop_cases_exp <- PAF/AR
    
    unexposed <- 1-prop_cases_exp
    exposed_unattr <- 1 - unexposed - PAF
    
    
    # Create data
    group <- c("Unexposed", "Exposed" , "Exposed and attributable")
    value <- c(unexposed, exposed_unattr, exposed_attr)
    data <- data.frame(group,value)
    data
  })
  
  output$PAF <- renderText({
    
    PAF <- (input$prev*(input$RR-1))/(input$prev*(input$RR-1)+1)
    PAF <- paste0(" The population attributable fraction is: ",round(PAF,2))
    PAF
  })
  
  output$PAF_perc <- renderText({
    
    PAF <- (input$prev*(input$RR-1))/(input$prev*(input$RR-1)+1)
    PAF <- paste0(round(PAF,2)*100,"% of cases are attributable to the exposure, 
                  and would not have occurred if exposure was removed")
    PAF
  })
  
  
  
  output$main_plot <- renderPlot({
    
    data <- selectedData()
    
    # treemap
    treemap(data,
            index="group",
            vSize="value",
            type="index")
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

