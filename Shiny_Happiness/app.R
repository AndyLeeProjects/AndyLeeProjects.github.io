library(shiny)
library(ggplot2)
library(tidyverse)

load("h_p_dat.RData")
theme_set(theme_classic())
h_p_dat <- h_p_dat %>%
  dplyr::filter(Region != "NA")

ui <- fluidPage(
  
  tags$head(tags$style('
     #my_tooltip {
      position: absolute;
      width: 300px;
      z-index: 100;
     }
  ')),
  tags$script('
    $(document).ready(function(){
      // id of the plot
      $("#plot1").mousemove(function(e){ 

        // ID of uiOutput
        $("#my_tooltip").show();         
        $("#my_tooltip").css({             
          top: (e.pageY + 5) + "px",             
          left: (e.pageX + 5) + "px"         
        });     
      });     
    });
  '),
  selectInput("hpnsContinent",
              label = "Filter by Continent: ",
              choices = unique(h_p_dat$Continent)),
  plotOutput("plot1", hover = hoverOpts(id = "plot_hover", delay = 0)),
  uiOutput("my_tooltip")
)

server <- function(input, output) {
  
  data <- reactive({
    h_p_dat
  })
  
  output$plot1 <- renderPlot({
    req(input$hpnsContinent)
    ggplot(filter(h_p_dat, Continent == input$hpnsContinent), 
           aes(x = GDP, y=H_score, color = Life_expectancy_F,
               size = Freedom)) +
      geom_point() +
      theme_classic()+
      labs(title = "Happiness Scores vs GDP by Region\n")
  }, width = 800, height = 500)
  
  output$my_tooltip <- renderUI({
    hover <- input$plot_hover 
    #y <- nearPoints(data(), input$plot_hover)[ ,c("hpnsContinent", input$hpnsContinent)]
    y <- nearPoints(data(), input$plot_hover)[c('Year','Country','H_score')]
    req(nrow(y) != 0)
    verbatimTextOutput("vals")
  })
  
  output$vals <- renderPrint({
    hover <- input$plot_hover 
    #y <- nearPoints(data(), input$plot_hover)[ , c("hpnsContinent", input$hpnsContinent)]
    y <- nearPoints(data(), input$plot_hover)[c('Year','Country','H_score')]
    req(nrow(y) != 0)
    # y is a data frame and you can freely edit content of the tooltip 
    # with "paste" function 
    y
  })
}
shinyApp(ui = ui, server = server)