library(tidyverse)
library(scales)

df <- 
  USJudgeRatings |> 
  as_tibble(rownames = "y") |> 
  gather(x, fill, -y) |> 
  mutate(
    x = fct_reorder(x, fill, mean),
    y = fct_reorder(y, fill, sum, .desc = FALSE)
  ) |> 
  print()

# number of columns for plot
n_col <- 2

# separate into columns and keep factor order
plot_prep <- 
  df |> 
  mutate(
    y_order = as.integer(y),
    max_rows = ceiling(n_distinct(y) / n_col),
    facets = (-y_order %/% max_rows) # needs something to facet on, need '-' to keep highest values on left
  ) |> 
  print()

# plot
p <- 
  plot_prep |> 
  ggplot(aes(x, y, fill = fill)) +
  facet_wrap(~facets, ncol = n_col, scales = "free_y") +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_stepsn(breaks = breaks_pretty(6), colours = viridis_pal()(5)) +
  theme(
    strip.background.y = element_blank(),
    strip.text = element_blank(),
    axis.text = element_text(size = 7)
  )

p + theme(aspect.ratio = 1)

# identify aspect ratio
aspect <- 
  plot_prep |> 
  select(x, y) |> 
  summarise_all(n_distinct) |> 
  mutate(ratio = y / x/ n_col) |> 
  print() 

p + theme(aspect.ratio = aspect$ratio) # <------ fixed 1:1 size
