#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

stackedBarPlotYLabs <- c(stack='Number of cases',
                         fill='% of cases')

setwd("C:/Users/aleks/OneDrive/Pulpit/sem6/PADR/R-main/R-main")
# Ladowanie danych obliczonych w pliku '2.R'
source("2.R")

# Define UI for application that draws a histogram
ui <- navbarPage(
  title = span("Title",style="color:red"),
  tabPanel("Overview of neural network",
           
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
                 plotlyOutput("barPlot")
               ),
               fluidRow(
                 plotlyOutput("benVSmagPlot")
               )
             )
           )),
  tabPanel("Try network",
           
           sidebarLayout(
             mainPanel(),
             
             sidebarPanel(width=12, style = 'background-color: #D8E9E9', 
                          p("TRY NETWORK", style='font-size: 36px', align = 'center'),
                          p("tutaj  jakis ladny tekscik ze uzytkownik sb moze wpisac wartosci imaginary albo prawdziwe i sb zobaczyc swoja diagnoze",
                            style='font-size: 28px', align = 'center' )
                          
             )
           ),
           fluidRow(
             sidebarPanel(width=8, style = 'background-color: #FFFFFF',
                          fluidRow(column(5, align = 'center',
                                          numericInput(inputId = 'age',
                                                                    label = "Patient's age",
                                                                    value = 50,
                                                                    min = 18)),
                                   column(5, offset=1, align = 'center',
                                          selectInput(inputId = 'shape',
                                                         label = 'Mass shape',
                                                         choices = c('round - 1' = 1, 'oval - 2' = 2, 'lobular - 3' = 3, 'irregular - 4' = 4)))),
                          
                          fluidRow(column(5, align = 'center',
                                          selectInput(inputId = 'margin',
                                                                  label = 'Mass margin',
                                                                  choices = c('circumscribed - 1' = 1, 'microlobulated - 2' = 2, 'obscured - 3' = 3, 'ill-defined - 4' = 4, 'spiculated - 5' = 5))),
                                   column(5, offset=1, align = 'center',
                                          selectInput(inputId = 'density',
                                                         label = 'Mass density',
                                                         choices = c('high - 1' = 1, 'iso - 2' = 2, 'low - 3' = 3, 'fat-containing - 4' = 4))))
                          
                          
                          
                          # fluidRow(column(5, offset=8, actionButton(inputId = 'submit', label = "Sumbit", width = '200px', style = 'background-color: #D8E9E9')))
                          
                          
                          
             ),
             sidebarPanel(width = 4, style = 'background-color: #FFFFFF',
                          fluidRow(column(10, offset = 1, align = 'center',
                                          selectInput(inputId = 'layers',
                                                label = 'Number of hidden layers',
                                                choices = c('1' = 1, '2' = 2)))),
                          fluidRow(column(5, offset = 1, align = 'center',
                                          numericInput(inputId = 'first_layer',
                                                          label = 'Number of neurons in 1st layer',
                                                          value = 3,
                                                          min = 1,
                                                          max = 8, 
                                                          step = 1)),
                                   column(5, align = 'center',
                                          numericInput(inputId = 'second_layer',
                                                          label = 'Number of neurons in 2nd layer',
                                                          value = 3,
                                                          min = 1,
                                                          max = 8, 
                                                          step = 1))))),
           fluidRow(style = 'padding: 10px', align = 'center', actionButton(inputId = 'submit', label = "Sumbit", width = '400px', style = 'background-color: #D8E9E9')),

           fluidRow(
             column(style = 'padding:50px', width = 6, align = 'right',  "Your diagnose: "),
           column(style = 'padding:50px', width = 6, textOutput('selected_var')))
            
  )

  
)

# Define server logic required to draw a histogram

server <- function(input, output) {
  # observeEvent(input$layers, {
  #   toggleState(second_layer, condition = input$layers == 2)
  # })
  # 
  observeEvent(input$submit, {v <-c(input$age, as.numeric(input$shape), as.numeric(input$margin), as.numeric(input$density))
  v2 <- c(input$first_layer, input$second_layer)
  nn <- neuralnet(severity ~ age+shape+margin+density, data=data_stand,
                  err.fct = "sse", hidden =v2 , act.fct = "logistic")
  pred <-predict(nn, scale(t(v), center = scaled_centers, scale = scaled_scales))
  if(pred>= 0.5) {
    output$selected_var <- renderText({paste(pred, 'MALIGNANT')})
  }
  else {
    output$selected_var <- renderText({paste(pred, 'BENIGN')})
  }
  })
  
  output$barPlot <- renderPlotly({
    chosen_feature <- input$feature
    
    p <- ggplot(data_factorized, aes_string(chosen_feature)) +
      geom_bar(fill=featureColors[chosen_feature]) +
      ggtitle(paste("Histogram for", chosen_feature, sep=" ")) +
      theme_minimal() +
      theme(plot.title = element_text(size=20, face="bold")) +
      scale_x_discrete(drop=FALSE)
    
    if(chosen_feature=="age") {
      p <- p + scale_x_discrete(breaks=seq(15,100,5))
    }
    
    ggplotly(p)
    
  })
  
  output$benVSmagPlot <- renderPlotly({
    chosen_feature <- input$feature
    chosen_metric <- input$metric
    
    
    p <- ggplot(data=data_factorized, aes_string(fill='severity', x=chosen_feature)) +
      geom_bar(stat = 'count', position = chosen_metric) +
      ylab(stackedBarPlotYLabs[chosen_metric]) +
      ggtitle(paste("Distribution of mass severity with respect to", chosen_feature, sep=" ")) +
      theme_minimal() +
      theme(plot.title = element_text(size=20, face="bold"))
    
    if(chosen_feature=="age") {
      p <- p + scale_x_discrete(breaks=seq(15,100,5))
    }
    
    ggplotly(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)