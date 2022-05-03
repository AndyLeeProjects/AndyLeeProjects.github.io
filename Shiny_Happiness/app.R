## Goal: Enable filtering by storm status.

library(shiny)
library(dplyr)
library(ggplot2)


load("h_alldat.RData")
theme_set(theme_classic())
h_alldat <- h_alldat %>%
  dplyr::filter(Region != "NA")


ui <- fluidPage(
  
  selectInput("hpnsRegion",
              label = "Filter by Region: ",
              choices = unique(h_alldat$Region)),
  
  "The plot below shows GDP vs Happiness Scores by Region",
  plotOutput("nameDist")
)

server <- function(input, output, session) {
  
  output$nameDist <- renderPlot({
    
    str(input$stormStatus)
    ggplot(dplyr::filter(h_alldat, Region == input$hpnsRegion), 
           aes(x = GDP, y=H_score, color = Region)) +
      geom_point() +
      theme_classic()+
      labs(title = "Happiness Scores vs GDP by Region\n")
    
  },
  
  width = 700, height = 500)
  
}

shinyApp(ui, server)