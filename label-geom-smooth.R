library(tidyverse)
library(broom)

mpg |>
  ggplot(aes(displ, hwy, colour = class, label = class)) +
  geom_count(alpha = 0.1) +
  stat_smooth(alpha = 0.6, method = lm, geom = "line", se = FALSE) +
  geom_text(
    aes(y = .fitted), size = 3, hjust = 0, nudge_x = 0.1,
    data = ~{
      nest_by(.x, class) |>
        summarize(broom::augment(lm(hwy ~ displ, data = data))) |>
        arrange(displ) |> 
        slice_tail(n = 1)
    }
  ) +
  scale_x_continuous(expand = expansion(add = c(0, 1))) +
  theme_minimal()


# OR make a function
last_lm_points <- function(df, formula, group) {
  df |> 
    nest_by({{group}}) |> 
    summarize(broom::augment(lm(formula, data = data))) |>
    arrange(1) |> 
    slice_tail(n = 1)
}

last_lm_points(mpg, hwy~displ, class)

mpg |>
  ggplot(aes(displ, hwy, colour = class, label = class)) +
  geom_count(alpha = 0.1) +
  stat_smooth(alpha = 0.6, method = lm, geom = "line", se = FALSE) +
  geom_text(
    aes(y = .fitted), size = 3, hjust = 0, nudge_x = 0.1,
    data = ~last_lm_points(.x, hwy~displ, class)
  ) +
  scale_x_continuous(expand = expansion(add = c(0, 1))) +
  theme_minimal()
