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
library(shinythemes)
theme_set(theme_classic())
load("month_dat.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("sandstone"),
  
  # Sidebar with a slider input for number of bins 
    column(10,
    sidebarPanel(
      fluidRow(
           sliderInput("Rise_time",
                  "Rise time (min):",
                  min = -100,
                  max = 240,
                  value = mean(month_dat$Rise_time)),
               h5("Negative value: earlier rise than intended\nPositive value: later rise than intended"),
               hr(),
          sliderInput("Work_finished",
                     "Work_finished (%):",
                     min = 0,
                     max = 1,
                     value = mean(month_dat$work_finished)/100,
                     step = .01),
          sliderInput("Meditation",
                      "Meditation (min):",
                      min = 0,
                      max = 30,
                      value = mean(month_dat$Meditation)),
          sliderInput("Reading",
                      "Reading (min):",
                      min = 0,
                      max = 60,
                      value = mean(month_dat$Reading)),
          sliderInput("Screen_time",
                     "Screen Time (min):",
                     min = 1,
                     max = 500,
                     value = mean(month_dat$Screen_time)),
          sliderInput("Phone_pickups",
                     "Number of Phone pickups:",
                     min = 1,
                     max = 200,
                     value = mean(month_dat$Phone_pickups)),
          sliderInput("Multiple",
                      "Multiple (Subjective Evaluation 1-5):",
                      min = 1,
                      max = 5,
                      value = mean(month_dat$Multiple),
                      step = .1),
          radioButtons("Drink",
                       "Drink (T/F):",
                       choices = c("Sober", "Drank"),
                       inline = TRUE))),
    # Show a plot of the generated distribution
      column(2, offset = 2,
      mainPanel(
      plotOutput("distPlot")))
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
                       Work_finished = input$Work_finished,
                       Drink = input$Drink,
                       Multiple = input$Multiple)
    
    data <- data %>%
      mutate(Rise_time = if_else(Rise_time > 0, 
                               round((1 / (1 + exp(0.004 * Rise_time)) - 0.5),3), 
                               round((1 / (1 + exp(0.004 * Rise_time)) - 0.5),3)),
             Screen_time = round((1000 - Screen_time) / 4.3e+5 * 1e+5) / 1000,
             Phone_pickups = round((340 - Phone_pickups) / 5) / 1000,
             Work_finished = round(Work_finished / (3.06),3),
             Meditation = round(Meditation / 140,3),
             Reading = case_when(Reading > 0 ~ round(Reading / 450,3), 
                                 Reading == 0 ~ -0.03),
             Drink = if_else(Drink == "Drank", -0.03, 0),
             Multiple = Multiple * (2.3 + 6 / Multiple) / 100,
             Total = (Rise_time + Screen_time + Work_finished + Meditation +
               Reading + Drink + Phone_pickups + Multiple)*100) %>%
      mutate(Total = case_when(
                      Total > 100 ~ 100,
                      Total <= 100 ~ Total
      ))
    return(data)
       
       
  })
  
  
    output$distPlot <- renderPlot({
      
      ggplot(data()) +
        geom_col(aes(x = 0, y = Total, fill = "orange")) +
        ylim(0,100) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "None",
              plot.title = element_text(size=22)) +
        labs(title = paste0("Total: ",as.character(data()$Total), "%"))

      
      }, height = 700, width = 300)
}

# Run the application 
shinyApp(ui = ui, server = server)
