library(tidyverse)
library(broom)

mpg |>
  select(
    x = displ,
    y = hwy,
    group = class
  ) |>
  ggplot(aes(x, y, colour = group, label = group)) +
  geom_count(alpha = 0.1) +
  stat_smooth(alpha = 0.6, method = lm, geom = "line", se = FALSE) +
  geom_text(
    aes(y = .fitted),
    size = 3,
    hjust = 0,
    nudge_x = 0.1,
    data = ~{
      nest_by(.x, group) %>%
      mutate(mod = list(lm(y ~ x, data = data))) |>
      summarize(broom::augment(mod)) |>
      arrange(x) |> 
      slice_tail(n = 1)
    }
  ) +
  scale_x_continuous(expand = expansion(add = c(0, 1))) +
  theme_minimal()
