# allows hover info to use data not in plot
library(plotly)
library(glue)

economics |> 
  mutate(
    text = glue(
      "Population: {pop}
      Unemployment: {uempmed}%"
    )
  ) |> 
  plot_ly() |> 
  add_trace(
    x = ~date,
    y = ~uempmed,
    text = ~text,
    hoverinfo = ~text,
    type = "scatter",
    hovertemplate = "%{text}"
  )
