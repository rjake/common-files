library(tidyverse)
library(ggiraph)

spc_ggiraph <- function(df) {
  hover_css <- #girafe_css_bicolor(primary = NA, secondary = "black")
    girafe_css(
      css = "fill:NA; stroke:black; cursor:pointer;",
      point = "stroke-width:2px"
    )
  
  selection_css <- "fill:black; stroke:gray; r:5pt;"

  set_girafe_defaults(
    opts_hover = opts_hover(css = hover_css),
    opts_selection = opts_selection(css = selection_css, only_shiny = FALSE),
    opts_sizing = opts_sizing(rescale = TRUE),
    opts_toolbar = opts_toolbar(saveaspng = FALSE, position = "bottom", delay_mouseout = 5000),
    opts_tooltip = opts_tooltip(css = "padding:3px;background-color:#333333;color:white;"),
    opts_zoom = opts_zoom(min = 1, max = 4)
  )
  
  p <- 
    ggplot(
      data = df,
      mapping = aes(
        x = x, 
        group = 1,
        # here we add iteractive aesthetics
        tooltip = glue(
          "{format({x}, '%D')}
          {y}% ({sum} / {n})
          Centerline: {cl}%
          Upper: {ucl}%
          Lower: {lcl}%"
        ),
        data_id = x
      )
    ) +
    geom_line(aes(y =  cl), linewidth = 0.5, linetype = "dashed", color = "grey80") +
    geom_line(aes(y = ucl), linewidth = 0.5, linetype = "dashed", color = "grey80") +
    geom_line(aes(y = lcl), linewidth = 0.5, linetype = "dashed", color = "grey80") +
    geom_line(aes(y = y), linewidth = 1, color = "#00BFC4", alpha = 0.6) +
    geom_point_interactive(
      aes(y = y, color = sigma.signal), 
      size = 3, 
      hover_nearest = TRUE
    )
  
  girafe(ggobj = p)
}


df <- 
  qicharts2::qic(
    x = i, 
    y = r,
    n = n,
    data = qicharts2::nhs_accidents,
    return.data = TRUE,
    chart = "p"
  ) |>
  mutate(
    sum = y.sum,
    x = Sys.Date() + x,
    across(c(y, cl, lcl, ucl), ~round(.x * 100, 1))
  )

spc_ggiraph(df)
