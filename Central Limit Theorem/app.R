#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(sliderInput("samplesize",
                             "Sample Size:",
                             min = 100,
                             max = 10000,
                             value = 1000)),
    mainPanel(plotOutput("distPlot"))
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) { output$distPlot <- renderPlot({
  hist(rnorm(input$samplesize),
       col='darkorchid',
       xlab="Sample",
       main="Normally Distributed Sample")},
  height=300
) 
}

# Run the application 
shinyApp(ui = ui, server = server)
