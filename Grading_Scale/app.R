#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
theme_set(theme_classic())


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Daily Evaluation Grading Scale"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fixedRow(
        column(8,
               sliderInput("Rise_time",
                  "Rise time (min):",
                  min = -240,
                  max = 240,
                  value = 30),
          sliderInput("Screen_time",
                    "Screen Time (min):",
                    min = 1,
                    max = 300,
                    value = 60),
          sliderInput("Phone_pickups",
                    "Number of Phone pickups:",
                    min = 1,
                    max = 200,
                    value = 50),
          sliderInput("Todo_total",
                    "Total To-do Lists:",
                    min = 0,
                    max = 20,
                    value = 15),
          sliderInput("Todo_finished",
                    "Total To-do Lists Completed:",
                    min = 0,
                    max = 20,
                    value = 15))),
      fixedRow(
        column(8,
               sliderInput("Meditation",
                  "Meditation (min):",
                  min = 0,
                  max = 40,
                  value = 20),
                sliderInput("Reading",
                            "Reading (min):",
                            min = 0,
                            max = 100,
                            value = 30),
                sliderInput("Multiple",
                            "Multiple (Subjective Evaluation 1-5):",
                            min = 1,
                            max = 5,
                            value = 5,
                            step = .1),
                sliderInput("Run",
                            "Total distance ran (km):",
                            min = 0,
                            max = 5,
                            value = 1),
                radioButtons("Drink",
                             "Drink (T/F):",
                             choices = c("Drank", "Sober"),
                             inline = TRUE)))
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  data <- reactive({
    
    data <- data.frame(Reading = input$Reading, 
                       Meditation = input$Meditation,
                       Rise_time = input$Rise_time,
                       Screen_time = input$Screen_time,
                       Phone_pickups = input$Phone_pickups,
                       Todo_total = input$Todo_total,
                       Todo_finished = input$Todo_finished,
                       Drink = input$Drink,
                       Run = input$Run,
                       Multiple = input$Multiple)
    
    data <- data %>%
      mutate(Rise_time = if_else(Rise_time > 0, 
                               round((1 / (1 + exp(0.004 * Rise_time)) - 0.5),3), 
                               round((1 / (1 + exp(0.004 * Rise_time)) - 0.5),3)),
             Screen_time = round((1000 - Screen_time) / 4.3e+5 * 1e+5) / 1000,
             Phone_pickups = round((340 - Phone_pickups) / 5) / 1000,
             Work_finished = if_else(Todo_total < 12, 
                               round(Todo_finished / (3.06 * Todo_total),3), 
                               round(Todo_finished / (2.94 * Todo_total),3)),
             Meditation = round(Meditation / 140,3),
             Reading = case_when(Reading > 0 ~ round(Reading / 450,3), 
                                 Reading == 0 ~ -0.03),
             Drink = if_else(Drink == "Drank", -0.03, 0),
             Run = if_else(Run != 0, round(Run / 85,3), -0.003),
             Multiple = Multiple * (2.3 + 6 / Multiple) / 100,
             Total = (Rise_time + Screen_time + Work_finished + Meditation +
               Reading + Drink + Run + Phone_pickups + Multiple)*100)
    return(data)
       
       
  })
    
    output$distPlot <- renderPlot({
      
      ggplot(data()) +
        geom_col(aes(x = 0, y = Total, fill = "orange")) +
        ylim(0,100) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "None")
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
