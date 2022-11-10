# allows hover info to use data not in plot
library(tidyverse)
library(plotly)
library(glue)

# scatter plot
economics |> 
  plot_ly() |> 
  add_trace(
    type = "scatter",
    mode = "markers",
    x = ~date,
    y = ~uempmed,
    hoverinfo = "text",                  # <-----
    hovertext =                          # <-----
      ~glue(
        "Population: {pop}
        Unemployment: {uempmed}%"
      )
  )

# stacked bar chart
diamonds |> 
  count(cut, color) |>
  add_count(color, wt = n, name = "total") |> 
  plot_ly(
    type = "bar",
    x = ~color,
    y = ~n,
    name = ~cut,
    hoverinfo = "text",                    # <-----
    hovertext = ~paste(cut, n, "/", total) # <-----
  ) |> 
  layout(barmode = "stack")
