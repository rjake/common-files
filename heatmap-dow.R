# heatmap for day of the week with M T W H F S S on X-axis

library(tidyverse)

expand_grid(
  date = Sys.Date() + -10:10,
  group = letters[1:5]
) |> 
  mutate(
    n = row_number() %% 4,
    week = 
      lubridate::floor_date(date, "week", week_start = 1) |> 
      format("%m/%d")
  ) |> 
  ggplot(aes(date, group, fill = n, label = n)) +
  facet_grid(
    cols = vars(week),
    space = "free",           # <----
    scales = "free_x"         # <----
  ) +
  geom_tile(alpha = 0.8) +
  geom_text() +
  scale_fill_gradient(
    low = "white", 
    high = "dodgerblue"
  ) +
  scale_x_date(
    date_breaks = "1 day",      # <----
    labels = 
      ~format(.x, "%a") |>      # <----
      str_sub(1,1) 
  )

