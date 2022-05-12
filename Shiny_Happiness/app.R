## Goal: Enable filtering by storm status.

library(shiny)
library(dplyr)
library(ggplot2)
load("/Volumes/Programming/Spring 2022/DANL 310/my_website/aLin-96.github.io/h_p_dat.RData")
theme_set(theme_classic())
h_p_dat <- h_p_dat %>%
  dplyr::filter(Region != "NA")

ui <- fluidPage(
  
  selectInput("hpnsContinent",
              label = "Filter by Continent: ",
              choices = unique(h_p_dat$Continent)),
  
  "The plot below shows GDP vs Happiness Scores by Continent",
  plotOutput("nameDist"))

server <- function(input, output, session) {
  output$nameDist <- renderPlot({
    h_p_dat$Life_expectancy_F <- factor(h_p_dat$Life_expectancy_F,      
                                       levels = c("High Life Expectancy",
                                                  "Medium Life Expectancy",
                                                  "Low Life Expectancy"))
    ggplot(dplyr::filter(h_p_dat, Continent == input$hpnsContinent), 
           aes(x = GDP, y=H_score, color = Life_expectancy_F,
               size = Freedom)) +
      geom_point() +
      theme_classic()+
      labs(title = "Happiness Scores vs GDP by Region\n")},
    width = 800, height = 500
  )

}

shinyApp(ui, server)
