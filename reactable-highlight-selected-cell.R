# workspace ----
library(shiny)
library(reactable)
library(glue)
library(tidyverse)
library(stringr)

dummy_input <-
  list(
    cell_col = list(name = "estimate.income"),
    cell_row = list(index = 1, value = "Alaska")
  )

df <-
  us_rent_income |>
  filter(str_detect(NAME, "^A")) |>
  rename_all(tolower)

id_col <- "name"
use_names_from <- "variable"
use_values_from <- c("estimate", "moe")


prep_df <-
  df |>
  pivot_wider(
    id_cols = all_of(id_col),
    names_from = all_of(use_names_from),
    names_sep = ".",
    values_from = all_of(use_values_from)
  )

# app -----
{
  ui <-
    fluidPage(
      titlePanel("reactable example"),
      reactableOutput("table"),
      hr(),
      h3("results"),
      tableOutput("selected_data")
    )

  server <- function(input, output, session) {
    output$table <- renderReactable({
      # fns
      highlight_selected <- function(value, index, name) {
        if (!is.null(input$cell_info$row_index)) {
          if (input$cell_info$column_name == name & input$cell_info$row_index == index) {
            list(
              border = "1px solid",
              borderColor = "#bbb",
              backgroundColor = "#eee",
              fontWeight = "bold"
            )
          }
        }
      }

      assign_selected_values <- {
        glue(
          # find name for <id_col>
          .open = "<",
          .close = ">",
          "function(rowInfo, column) {
            if (window.Shiny) {
              let column_info = column.name
              Shiny.setInputValue(
                'cell_info',
                {
                  row_index: rowInfo.index + 1,
                  column_name: column.name,
                  orig_column: column_info.replace(/\\..*/i, ''),
                  orig_value_filter: column_info.replace(/.*\\./i, ''),
                  id: rowInfo.values['<id_col>']
                  // global var:       ^^^^^^
                }
              )
            }
          }"
        ) |>
          JS()
      }

      # table
      reactable(
        prep_df,
        defaultColDef = colDef(
          style = function(value, index, name) {
            highlight_selected(value, index, name)
          }
        ),
        onClick = assign_selected_values,
      )
    })

    
    # selected_data
    output$selected_data <- renderTable({
      selected_id <- input$cell_info$id
      orig_column <- input$cell_info$orig_column
      orig_value_filter <- input$cell_info$orig_value_filter

      glue("selected_id: {selected_id}, orig_column: {orig_column}, orig_value_filter: {orig_value_filter}") |>
        print()

      if (!length(orig_column)) { # '' when nothing is selected
        df
      } else if (orig_column == id_col) { # NA when only ID column selected
        df |>
          filter(id_col == selected_id)
      } else {
        df |>
          filter(
            get(id_col) == selected_id,
            get(use_names_from) == orig_value_filter
          ) |>
          select(id_col, orig_column, use_names_from)
      }
    })
  } # end server

  shinyApp(ui, server)
}
