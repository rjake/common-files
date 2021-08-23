library(shiny)
library(shinyWidgets)
library(tidyverse)

# input <- list(slider = 20)
order <- 10:30
step <- 5
rand_numbers <- rnorm(1000)

shinyApp(
  ui = 
    fluidPage(
      sidebarLayout(
        sidebarPanel = sidebarPanel( 
          fluidRow(
            column(6),
            column(1, uiOutput("slider_down"), style = "margin-top: 3px;"),
            column(1, uiOutput("slider_up"), style = "margin-top: 3px;"),
            column(4)
          ),
          br(),
          uiOutput("slider"),
          width = 3
        ),
        mainPanel = mainPanel( plotOutput("plot") )
      )
    ),
  server = function(input, output, session) {
    output$plot <- renderPlot({
      tibble(x = rand_numbers) |>  
        ggplot(aes(x)) +
        geom_histogram(color = "white", bins = input$slider)
    })
    
    output$slider <- renderUI({
       sliderInput(
        inputId = "slider",
        label = NULL, 
        min = min(order),
        max = max(order),
        value = max(order) - step,
        step = step,
        ticks = FALSE,
        width = "75%"
      )
      
    })
    
    output$slider_down <- renderUI({
      if (input$slider > min(order)) {
        actionBttn(
          inputId = "slider_down",
          label = "<",
          size = "xs",
          style = "pill"
        )
      }
    })
    
    output$slider_up <- renderUI({
      if (input$slider < max(order)) {
        actionBttn(
          "slider_up",
          ">",
          size = "xs",
          style = "pill"
        )
      }
    })
    
    observeEvent(
      eventExpr = input$slider_up,
      handlerExpr = 
        updateSliderInput(
          session = session, 
          inputId = "slider",
          value = input$slider + step
        )
    )
    
    observeEvent(
      eventExpr = input$slider_down,
      handlerExpr = 
        updateSliderInput(
          session = session, 
          inputId = "slider",
          value = input$slider - step
        )
    )
  }
)
