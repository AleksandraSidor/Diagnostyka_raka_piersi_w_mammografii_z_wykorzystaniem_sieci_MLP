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
library(shinydashboard)
library(shinyjs)

status <- reactiveVal();

stackedBarPlotYLabs <- c(stack='Number of cases',
                         fill='Fraction of cases')

# Ladowanie danych obliczonych w pliku '3.R'
#setwd("C:/Users/aleks/OneDrive/Pulpit/sem6/PADR/R-main")
setwd("C:/Users/Admin/Studia/Semestr 6/PADR/R-main")
source("3.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = span("Neural network")),
                    dashboardSidebar(
                      sidebarMenu(style = 'background-color: #FFFFFF',
                                  menuItem("Overview", tabName = "Overview", icon = icon('table', class = 'menu_icon')),
                                  menuItem("Control panel", tabName = "Try", icon = icon('magic', class = 'menu_icon'))
                      )
                    ),
                    dashboardBody(tags$head(tags$style(HTML('
.skin-green {
  background-color: rgb(121, 180, 183);
}


.skin-green .main-header .logo {
  background-color: rgb(121, 180, 183);
}

.skin-green .main-header .logo:hover {
  background-color: rgb(121, 180, 183);
}
                          
.skin-green .main-header .navbar {
  background-color: rgb(121, 180, 183);
}
                          
.skin-green .main-sidebar {
  background-color: 	#D3D3D3;
}

.skin-green .main-sidebar .sidebar .sidebar-menu{
  background-color:	rgb(25, 76, 47);
  font-family: helvetica, sans-serif;
  font-size: 1.5em
}

.menu_icon {
  margin-right: 5px;
}
                
.skin-green .main-sidebar .sidebar .sidebar-menu .active a{
  background-color:	rgb(157, 157, 157);
}

.skin-green .main-sidebar .sidebar .sidebar-menu a:hover{
  background-color: hsl(0, 0%, 32%);
  cursor: pointer;
}

.skin-green .main-header .navbar .sidebar-toggle:hover{
         background-color: gray;
}
                          
.content-wrapper, .right-side {
  background-color: #FFFFFF;
}

.small-box {
  height: 150px;
  overflow: hidden;
}

.small-box.bg-green { 
  background-color: #c8f5b0 !important;
  color: rgba(0,0,0,0.5) !important;
  height: 200px;
}

.small-box.bg-red { 
  background-color: #EEB0B0 !important;
  color: rgba(0,0,0,0.5) !important;
  height: 200px;
}

.small-box.bg-blue {
  background-color: #DEF3F4!important;
  color: rgba(0,0,0,0.5) !important
}

.small-box.bg-olive {
  background-color: #E0EFD8 !important;
  color: rgba(0,0,0,0.5) !important
}

.small-box.bg-purple {
  background-color: #C0D1E8 !important;
  color: rgba(0,0,0,0.5) !important;
}

.boxIcon {
  font-size: 2em;
}
'))),
tags$script("
      Shiny.addCustomMessageHandler('status-color', function(color) {
        document.getElementById('prediction').style.color = color;

      });
    "),
                      tabItems(
                        tabItem(tabName = "Overview", style = 'background-color: #FFFFFF',
                                tabBox(title="Overview panel", width="100%", height="100%", 
                                       tabPanel(title = "Histograms",
                                sidebarLayout(
                                  sidebarPanel(width = 3,
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
                                                                                   `Fraction of cases`="fill"),
                                                                    selected = "stack"))
                                  ),
                                  
                                  
                                  mainPanel(width = 9, height = '200%', useShinyjs(),
                                            
                                            fluidRow(
                                              plotlyOutput("benVSmagPlot", height = '850px')
                                            )
                                  )
                                )),
                                tabPanel(title = "Feature correlation matrix",
                                         mainPanel(width = 12, height = "200%",
                                                   plotlyOutput("CorrelationMatrix", height = '850px', width='auto'))
                                         ),
                                
                                tabPanel(title = "Neural network structure",
                                         fluidRow(style="padding: 20px",sidebarPanel(width = 5,
                                                               strong("MLP Structure", align="left",
                                                                  style="font-size:250%; font-family: 'arial'"),
                                                               br(),
                                                               br(),
                                                               p("Multilayer perceptron used for malignancy prediction of a tumor detected in mammography, has a presented structure.", style="font-size:120%; font-family: 'arial'"),
                                                               br(),
                                                               p("It consists of: ", style="font-size:120%; font-family: 'arial'"),
                                                               p("- ", strong("Input layer"), "with 4 neurons,", style="font-size:120%; font-family: 'arial'"),
                                                               p("- One ", strong("hidden layer"), "with 2 neurons,", style="font-size:120%; font-family: 'arial'"),
                                                               p("- ", strong("Output layer"), "with 1 neuron representing mass severity.", style="font-size:120%; font-family: 'arial'"),
                                                               p("As an output, neural network returns value from the range (0,1). A number smaller than a treshold = 0.5 is considered as benign, while a number bigger or equal to 0.5 - as a malignant tumor.", style="font-size:120%; font-family: 'arial'"),
                                                               br(),
                                                               p("Results from a agiven range are provided by the activation function - ", strong("unipolar sigmoid function."), style="font-size:120%; font-family: 'arial'")),
                                                              
                                                  mainPanel(width = 7,offset = 2, img(src = "/MLP.png", height = 500, width = 700, align = "center"))),
                                         fluidRow(plotlyOutput("sigmoide"))
                                        ))),
                                
                        tabItem(tabName = "Try",
                                
                                sidebarLayout(
                                  
                                  sidebarPanel(width=12, style = 'background-color: white; border-color: #D3D3D3; border-bottom: 3px solid rgb(121, 180, 183); border-top: 3px solid rgb(121, 180, 183); border-left:0px solid white; border-right: 0px solid white; margin-bottom: 40px',
                                               h1(strong("Neural network control panel"), align = "center", style = "font-family: 'Inria Magic', sans-serif; color: rgba(0,0,0,0.7);"),
                                               h3("Use the panel on the left to enter your own mass measurements and hit 'Submit' to see it's severity prediction. Use the panel on the right to change the structure of the neural network and hit 'Retrain' to apply.",
                                                  align = 'center', style="color: rgba(0,0,0,0.5)")
                                               
                                  ),
                                  mainPanel(width=12,
                                  fluidRow(
                                  box(width=6
                                        ,fluidRow(column(5, align = 'center',
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
                                                                           choices = c('high - 1' = 1, 'iso - 2' = 2, 'low - 3' = 3, 'fat-containing - 4' = 4)))),
                                               
                                               
                                               
                                               fluidRow(column(style = 'padding: 10px',
                                                               width = 12,
                                                               align = 'center',
                                                               actionButton(inputId = 'submit', label = "Submit", width = '300px', height = "200px" ,style = ' color: #fff; background-color: rgb(121, 180, 183); font-size:120%')))
                                               
                                               
                                               
                                               
                                  ),
                                  box(width=6,
                                               fluidRow(column(10, offset = 1, align = 'center',
                                                               selectInput(inputId = 'layers',
                                                                           label = 'Number of hidden layers',
                                                                           choices = c('1' = 1, '2' = 2),
                                                                           selected = '2'))),
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
                                                                            step = 1))),
                                               fluidRow(column(style = 'padding: 10px',
                                                               width = 12,
                                                               align = 'center',
                                                               actionButton(inputId = 'retrain', label = "Retrain", width = '300px', height = "200px" ,style = 'color: #fff; background-color: rgb(121, 180, 183); font-size:120%')))
                                               )),
        
                                
                                
                                fluidRow(valueBoxOutput('accuracyValueBox'),
                                         valueBoxOutput('sensitivityValueBox'),
                                         valueBoxOutput('specificityValueBox')
                                ),
                          
                                fluidRow(
                                  column(valueBoxOutput('predictionValueBox', width = 12), width = 8, offset = 2)
                                )
                        ))))))
                      

# Define server logic required to draw a histogram

server <- function(input, output, session) {
  
  output$accuracyValueBox <- renderValueBox(valueBox(
    paste(round(confMatrix_ts$overall['Accuracy']*100, 2), "%"), tags$p("Accuracy", style='font-size: 150%'), icon = icon("bullseye", class="boxIcon"),
    color = "blue"
  ))
  
  output$sensitivityValueBox <- renderValueBox(valueBox(
    paste(round(confMatrix_ts$byClass['Sensitivity']*100, 2), "%"), tags$p("Sensitivity", style='font-size: 150%'), icon = icon("eye", class="boxIcon"),
    color = "olive"
  ))
  
  output$specificityValueBox <- renderValueBox(valueBox(
    paste(round(confMatrix_ts$byClass['Specificity']*100, 2), "%"), tags$p("Specificity", style='font-size: 150%'), icon = icon("virus-slash", class="boxIcon"),
    color = "purple"
  ))
  
  observeEvent(input$layers, {
    if(input$layers == 1){
      disable('second_layer')
    }
    else {
      enable('second_layer')
    }
    
  })

  observeEvent(input$submit, {v <-c(input$age, as.numeric(input$shape), as.numeric(input$margin), as.numeric(input$density))
  # scaled_centers <- attr(v, 'scaled:center')
  # scaled_scales <- attr(v, 'scaled:scale')
  # scaled_centers
   #v_s <- scale(t(v), center = scaled_centers[2:5], scale = scaled_scales[2:5])
   
  
  pred1 <-round(predict(nn, scale(t(v), center = scaled_centers, scale = scaled_scales)), 2)
  
  if(pred1>= 0.5) {
    # diagnose <- paste("Your diagnose:" ,pred1, 'MALIGNANT')
    # session$sendCustomMessage("status-color", 'red')
    vBox <- renderValueBox(valueBox(
      pred1, tags$p("Malignant", style='font-size: 150%'), icon = icon("alert", lib = "glyphicon", class="boxIcon"),
      color = "red"
    ))
  }
  else {
    #diagnose <- paste("Your diagnose:", pred1, 'BENIGN')
    # session$sendCustomMessage("status-color", 'green')
    vBox <-  renderValueBox(valueBox(
      pred1, tags$p("Benign", style='font-size: 150%'), icon = icon("heartbeat", class="boxIcon"),
      color = "green"
    ))
  }
    output$predictionValueBox <- vBox
  })
  
  observeEvent(input$retrain, {
  v2 <- c(input$first_layer, input$second_layer)
  
  withProgress(message="Retraining neural network...", value = 0.5, {
    nn <<- neuralnet(training_sev ~ age+shape+margin+density, data=data_stand_tr,
                    err.fct = "sse", hidden = v2, act.fct = "logistic")
    
    incProgress(0.45, message = "Neural network successfully retrained")
    Sys.sleep(1)
    incProgress(0.05)
    })
  
  pred <-round(predict(nn, scale(testing_set, center = scaled_centers, scale = scaled_scales)), 2)
  
  predsVStarget_ts <-data.frame(case = rownames(testing_set), 
                                predictions=factor(round(pred, digits = 0), labels = c('benign', 'malignant')),
                                target = factor(data_ts$testing_sev, labels = c('benign', 'malignant')))
  
  confMatrix_tr <- confusionMatrix(data=predsVStarget_tr$predictions, reference=predsVStarget_tr$target, positive = "malignant")
  confMatrix_ts <- confusionMatrix(data=predsVStarget_ts$predictions, reference=predsVStarget_ts$target, positive = "malignant")
  
  output$accuracyValueBox <- renderValueBox(valueBox(
    paste(round(confMatrix_ts$overall['Accuracy']*100, 2), "%"), tags$p("Accuracy", style='font-size: 150%'), icon = icon("bullseye", class="boxIcon"),
    color = "blue"
  ))
  
  output$sensitivityValueBox <- renderValueBox(valueBox(
    paste(round(confMatrix_ts$byClass['Sensitivity']*100, 2), "%"), tags$p("Sensitivity", style='font-size: 150%'), icon = icon("eye", class="boxIcon"),
    color = "olive"
  ))
  
  output$specificityValueBox <- renderValueBox(valueBox(
    paste(round(confMatrix_ts$byClass['Specificity']*100, 2), "%"), tags$p("Specificity", style='font-size: 150%'), icon = icon("virus-slash", class="boxIcon"),
    color = "purple"
  ))
  
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
  
  output$CorrelationMatrix <- renderPlotly({
    # Create a ggheatmap
    ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
      geom_tile(color = "white")+
      scale_fill_gradient2(low = "black", high = "firebrick2", mid = "dodgerblue4", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson\nCorrelation") +
      theme_minimal()+ # minimal theme
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 12, hjust = 1))+
      theme(axis.text.y = element_text(size = 12)) + 
      coord_fixed()
    
    ggplotly(ggheatmap + 
      geom_text(aes(Var2, Var1, label = value), color = "white", size = 4) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
      guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                   title.position = "top", title.hjust = 0.5)))
  })
  
  output$sigmoide <- renderPlotly({
    sigmoid = function(x) {
      1 / (1 + exp(-x))
    }
    x <- seq(-10, 10, 0.1)
    xx<-sigmoid(x)
    xxx <- data.frame(xx)
    
    f = expression(1 / (1 + exp(-x)))
    d<-D(f,'x')
    x <- seq(-10, 10, 0.1)
    wynik <- eval(d)
    melted_data <- data.frame(x = x, y1 = xx, y2 = wynik)
    
    p <- ggplot(melted_data, aes(x=x)) +
      geom_line(aes(y=y1), colour = '#79B4B7', size = 1) +
      geom_line(aes(y=y2), colour = 'green', linetype = 'dashed', color = '#DEEF99') +
      ggtitle("Unipolar sigmoide" ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 20, face = "bold", family = "arial"))
    

    ggplotly(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)