# another method for sankey plots, associated with this issue: https://github.com/thomasp85/ggforce/issues/136#issuecomment-965944266
library(tidyverse)
library(ggforce)

df <-
  tibble::tribble(
    ~round_1, ~round_2, ~round_3, ~n,
    "A",      "D",      "G",       2,
    "A",      "E",      "G",       3,
    "B",      "D",      "F",       2,
    "B",      "D",      "G",       2,
    "B",      "C",      "F",       3
  ) |> 
  mutate(fill = round_2)


make_plot <- function(title, ...) {
  df %>%
    ggforce::gather_set_data(1:3) |>
    mutate(y = fct_relevel(y, ...)) |> 
    ggplot(aes(x, id = id, split = y, value = n)) +
    geom_parallel_sets(aes(fill = fill), alpha = 0.3, sep = 0.01) +
    geom_parallel_sets_axes(fill = "grey80", axis.width = 0.15, sep = 0.01) +
    geom_parallel_sets_labels(angle = 0, sep = 0.01) +
    theme_void() +
    labs(title = title) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)
    )
}

gridExtra::grid.arrange(
  make_plot("Default order\n(alphabetical)"),
  make_plot("Prioritize\nFlatness", c("B", "A", "C", "D", "E", "F", "G")),
  make_plot("Prioritize height\n(biggest on bottom)", c("A", "B", "C", "E", "D", "F", "G")),
  nrow = 1
)
