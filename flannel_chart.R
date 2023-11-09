library(tidyverse)

df <- 
  mpg |> 
  distinct(
    x = trans,
    y = as.character(cyl)
  )


prep_df <- 
  df |>
  add_count(x, name = "n_y") |> # how many of y values use x
  add_count(y, name = "n_x") |> # how many of x values use y
  filter(n_y > 1) |> 
  transmute(
    x = fct_reorder(x, n_y) |> fct_rev(),
    y = fct_reorder(y, n_x),
    n_x,
    n_y
  ) |>
  arrange(y) |> # most common y variable
  group_by(x) |> 
  mutate(
    y_ord = row_number(), # sequence
    pattern = paste(y, collapse = "/"), # paste all x values as 1 string
    y_1 = as.character(y[y_ord == 1]) |> max(), # 1st value, most common table (from arrange)
    y_2 = as.character(y[y_ord == 2]) |> max() |> replace_na("") # 2nd value
  ) |> 
  group_by(pattern) |> 
  mutate(n_pattern = n_distinct(x)) |> # how common is this pattern sequence
  ungroup() |> 
  arrange(y_1, desc(n_pattern), y_2, pattern) |> 
  mutate(x = fct_inorder(x, ordered = TRUE)) # final order of x


prep_df |> 
  ggplot(aes(x, y, color = pattern)) +
  geom_line(aes(group = x), size = 1) +
  geom_line(aes(group = y), size = 1, alpha = 0.2) +
  geom_point(size = 5) +
  theme(
    panel.background = element_rect("white"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none"
  )
