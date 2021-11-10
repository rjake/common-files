library(tidyverse)
theme_set(theme_bw())

n <- 100

set.seed(1234)
df <- 
  tibble(
    y = runif(n),
    ord = 1:n,
    x = sample(1:1000, n, replace = TRUE),
    se = sqrt((y * (1 - y)) / (x)),
    ## lower and upper limits for 95% and 99.9% CI, based on FEM estimatorimator
    mean = weighted.mean(y, 1 / se^2),
    estimator = sqrt((mean * (1 - mean)) / (ord)),
    lcl_95 = mean - 1.96 * estimator,
    ucl_95 = mean + 1.96 * estimator,
    lcl_999 = mean - 3.29 * estimator,
    ucl_999 = mean + 3.29 * estimator,
    in_bounds = (y >= lcl_95 & y <= ucl_95)
  ) |> 
  filter(
    y >= lcl_999 - 0.05,
    y <= ucl_999 + 0.05
  ) |> 
  print()


## draw plot
ggplot(aes(ord, y), data = df) +
  geom_point(aes(color = in_bounds)) +
  geom_line(aes(y = lcl_95)) +
  geom_line(aes(y = ucl_95)) +
  geom_line(aes(y = lcl_999), linetype = "dashed") +
  geom_line(aes(y = ucl_999), linetype = "dashed") +
  geom_hline(aes(yintercept = mean)) +
  scale_color_manual(values = c("grey70", "dodgerblue")) +
  scale_y_continuous(limits = c(0, 1))

# write_csv(df, "funnel_plot.csv")

df_long <- 
  df |> 
  select(ord, y, mean, contains("cl")) |> 
  full_join(tibble(ord = 1:n)) |>
  arrange(ord) |> 
  #filter(ord > 110) |> 
  mutate(
    across(
      .cols = matches("cl_9|mean"), 
      .fns = ~approx(ord, .x, ord)$y)
  ) |> 
  pivot_longer(-ord, names_to = "metric") |> 
  mutate(
    size = ifelse(metric == "y", 2, 1), 
    color = ifelse(metric == "y", "dodgerblue", "grey30"),
    alpha = case_when(
      str_detect(metric, "999") ~ 0.25,
      str_detect(metric, "95") ~ 0.6,
      TRUE ~ 1
    )
  ) |>
  print()

ggplot(aes(ord, value), data = df_long) +
  geom_point(aes(color = color, size = size, alpha = alpha)) +
  scale_size(range = c(1, 3)) +
  scale_alpha(range = c(0.25, 1)) +
  scale_color_identity()

# write_csv(df_long, "funnel_plot_long.csv")
