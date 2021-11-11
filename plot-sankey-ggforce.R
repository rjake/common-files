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



# One more example ----
library(ggforce)   # geom_parallel_*
library(ggplotify) # as.ggplot  as.grob
library(tidyverse)

df <-
  tibble::tribble(
    ~round_1, ~round_2, ~round_3,   ~N ,
    "A",      "C",      "Excluded", 300,
    "A",      "D",      "Included", 200,
    
    "A",      "E",      "Included", 50,
    "A",      "F",      "Excluded", 30,
    "B",      "D",      "Excluded", 60,
    "B",      "F",      "Excluded", 250
  ) |>
  rename_all(tolower)


df_prep <-
  df |>
  ggforce::gather_set_data(1:3) |> 
  arrange(round_3, n) |> 
  mutate(
    fill = (round_3 == "Included"),
    y_sort = 
      fct_inorder(y, ordered = TRUE) |> 
      fct_relevel(
        "B", "A",
        "F", "C", "D", "E",
        "Excluded", "Included"
      )  #|> fct_rev()
  ) |> 
  print()

p <-
  ggplot(df_prep, aes(x, id = id, split = y_sort, value = n)) +
  # ribbons
  geom_parallel_sets(aes(fill = fill), alpha = 0.3, axis.width = 0.1, sep = 0.01) +
  # bars
  geom_parallel_sets_axes(fill = "grey80", axis.width = 0.15, sep = 0.01) +
  # text for labes
  geom_parallel_sets_labels(angle = 0, hjust = "left", size = 3.5, sep = 0.01) +
  # text for counts
  geom_parallel_sets_labels(angle = 0, hjust = "center", size = 4, sep = 0.01) +
  scale_fill_manual(values = c("grey60", "lightseagreen")) +
  labs(x = NULL) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(), 
    axis.ticks = element_blank()
  )

p

# edit values
cleanup_plot <- function(p) {
  gg <- ggplot_build(p)

  gg$data[[4]]$label <- gg$data[[4]]$value
  gg$data[[3]]$x <- gg$data[[3]]$x + 0.1

  color_lookup <-
    gg$data[[1]] |>
    distinct(label = as.character(split), value, fill) |>
    count(label, fill, wt = value) |>
    arrange(desc(n)) |>
    group_by(label) |>
    slice(1) |>
    ungroup() |>
    select(-n) |>
    inner_join(
      distinct(gg$data[[2]], label = as.character(label), value)
    ) #|> print()

  gg$data[[2]]$fill <- NULL

  gg$data[[2]] <- left_join(gg$data[[2]], color_lookup)

  # return
  gg
}


# print
cleanup_plot(p) |>
  ggplot_gtable() |> #plot()
  as.grob() |>
  as.ggplot()


save_plot <- function(type, w = 10, h = 5) {
  ggsave(
    filename =
      paste0(
        str_replace_all(Sys.time(), "[^0-9]+", ""),
        ".",
        type
      ),
    device = type,
    width = 10,
    height = 5,
    units = "in"
  )
}
# save_plot("png")
# save_plot("svg")
