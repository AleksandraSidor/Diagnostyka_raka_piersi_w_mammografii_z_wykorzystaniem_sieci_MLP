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

# Faktoryzacja danych w data_clean na konkretne labele
# shape: [1,2,3,4] -> ["round", "oval", "lobular", "irregular"]
# margin: [1,2,3,4,5] -> ["circumscribed", "microlobulated", "obscured", "ill-defined", "spiculated"]
# density: [1,2,3,4] -> ["high", "iso", "low", "fat-containing"]
# severity: [0,1] -> ["benign", "malignant"]

data_factorized <- data.frame(bi_rads=factor(data_clean$bi_rads),
                            age=factor(data_clean$age),
                            shape=factor(data_clean$shape, labels = c("round", "oval", "lobular", "irregular")),
                            margin=factor(data_clean$margin, labels = c("circumscribed", "microlobulated", "obscured", "ill-defined", "spiculated")),
                            density=factor(data_clean$density, labels = c("high", "iso", "low", "fat-containing")),
                            severity=factor(data_clean$severity, labels= c("benign", "malignant")))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Mammographic Mass Dataset Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = 'feature',
                        label = 'Select dataset feature to display',
                        choices = list(`BI-RADS assesment`="bi_rads",
                                       `Patient's age`="age",
                                       `Mass shape`="shape",
                                       `Mass margin`="margin",
                                       `Mass density`="density",
                                       `Mass severity`="severity"),
                        selected = 'bi_rads')
        ),


        mainPanel(
           plotOutput("barPlot")
        )
    )
)


server <- function(input, output) {

    output$barPlot <- renderPlot({
        chosen_feature <- input$feature
        ggplot(data_factorized, aes_string(chosen_feature)) + geom_bar(fill=featureColors[chosen_feature])
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
