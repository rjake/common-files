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
        slice_max(order_by = displ, n = 1)
    }
  ) +
  scale_x_continuous(expand = expansion(add = c(0, 1))) +
  theme_minimal()


# OR make a function
#' @examples
#' last_lm_points(df = mpg, formula = hwy~displ, group = class)
last_lm_points <- function(df, formula, group) {
  # df <- mpg; formula <- as.formula(hwy~displ); group <- as.symbol("class");
  x_arg <- formula[[3]]
  df |> 
    nest_by({{group}}) |> 
    summarize(broom::augment(lm(formula, data = data))) |>
    slice_max(order_by = get(x_arg), n = 1)
}

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
