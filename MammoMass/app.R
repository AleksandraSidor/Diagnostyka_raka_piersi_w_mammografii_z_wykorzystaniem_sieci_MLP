#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
setwd("C:/Users/Admin/Studia/Semestr 6/PADR/R")
# Ladowanie danych obliczonych w pliku '2.R'
source("2.R")

stackedBarPlotYLabs <- c(stack='Number of cases',
                         fill='% of cases')


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Mammographic Mass Dataset Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          fluidRow(selectInput(inputId = 'feature',
                               label = 'Select dataset feature to display:',
                               choices = list(`BI-RADS assesment`="bi_rads",
                                              `Patient's age`="age",
                                              `Mass shape`="shape",
                                              `Mass margin`="margin",
                                              `Mass density`="density",
                                              `Mass severity`="severity"),
                               selected = "bi_rads")),
          
          fluidRow(selectInput(inputId = 'metric',
                               label = 'Select metric of stacked bar plot:',
                               choices = list(`Number of cases`='stack',
                                              `% of cases`="fill"),
                               selected = "stack"))
        ),


        mainPanel(
          fluidRow(
            plotOutput("barPlot")
          ),
          fluidRow(
            plotOutput("benVSmagPlot")
          )
        )
    )
)


server <- function(input, output) {
  
    output$barPlot <- renderPlot({
        chosen_feature <- input$feature
        
        ggplot(data_factorized, aes_string(chosen_feature)) +
          geom_bar(fill=featureColors[chosen_feature]) +
          ggtitle(paste("Histogram for", chosen_feature, sep=" ")) +
          theme_minimal() +
          theme(plot.title = element_text(size=20, face="bold"))
          
    })
    
    output$benVSmagPlot <- renderPlot({
        chosen_feature <- input$feature
        chosen_metric <- input$metric
        
        ggplot(data=data_factorized, aes_string(fill='severity', x=chosen_feature)) +
          geom_bar(stat = 'count', position = chosen_metric) +
          ylab(stackedBarPlotYLabs[chosen_metric]) +
          ggtitle(paste("Distribution of mass severity with respect to", chosen_feature, sep=" ")) +
          theme_minimal() +
          theme(plot.title = element_text(size=20, face="bold"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
