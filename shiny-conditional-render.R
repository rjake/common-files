# show different rendered output types based on the user selection

library(shiny)
shiny::reactiveConsole(TRUE) # for debugging

shinyApp(
  ui = fluidPage(
    radioButtons(
      inputId = "type",
      label = NULL,
      choices = c("plot", "table", "text")
    ),
    uiOutput("res")
  ),
  server = function(input, output, session) {
    output$res <- renderUI({
      switch(
        input$type,
        "plot" = plot(mtcars$mpg, mtcars$cyl) |> renderPlot(),
        "table" = renderTable(mtcars),
        "text" = paste(row.names(mtcars), collapse = " | ") |> renderText()
      )
    })
  }
) |>
  runApp(launch.browser = rstudioapi::viewer)
