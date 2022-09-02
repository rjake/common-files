library(tidyverse)
library(lubridate)
library(gt)

fy <- \(x) quarter(x, with_year = TRUE, fiscal_start = 7) |> str_extract("^\\d+")
fq <- \(x) paste0("Q", quarter(x, fiscal_start = 7))


df <- 
  gt::pizzaplace |> 
  transmute(
    date = ymd(date),
    group = type,
    subgroup = fct_reorder(size, price)
  ) |> 
  print()



df |> 
  arrange(date) |> 
  mutate(
    year = fy(date),
    quarter = fq(date),
    month = month(date, label = TRUE),
    date_group = paste(year, quarter, month, sep = "-")
  ) |> 
  count(date_group, group, subgroup) |> 
  pivot_wider(
    id_cols = c(group, subgroup),
    names_from = date_group,
    values_from = n,
    values_fill = 0
  ) |> 
  group_by(group) |> 
  gt() |> 
  tab_spanner_delim(delim = "-") |> 
  tab_options(row_group.as_column = TRUE) 
