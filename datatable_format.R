library(DT)
library(tidyverse)

custom_data_table <- function(df) {
  datatable(
    df,
    extensions = c("Buttons", "FixedHeader", "Scroller"),
    filter = "top",
    rownames = FALSE,
    escape = FALSE,
    options =
      list(
        dom = "Bfrtip",
        deferRender = FALSE,
        scrollY = 540,
        scroller = TRUE,
        scrollX = TRUE,
        fixedHeader = FALSE,
        buttons = c("copy", "csv", "excel", "pdf", "print"),
        pageLength = nrow(df),
        lengthChange = FALSE
      )
  )
}


nice_dates <- function(dt, column_index) {
  dt %>%
    formatDate(
      columns = column_index,
      method = "toLocaleDateString",
      params = list(
        "en-US",
        list(
          year = "numeric",
          day = "numeric",
          month = "numeric"
        )
      )
    )
}


tibble(
  date = as.Date("2021-01-01") + (1:10 * 7),
  group = letters[1:10]
) %>%
  custom_data_table() %>%
  nice_dates(1)
