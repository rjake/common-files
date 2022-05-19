library(gt)
library(tidyverse)
library(glue)
library(fontawesome)

my_css <- # not needed if CSS In yml of dashboard
  read_file("https://raw.githubusercontent.com/twbs/bootstrap/main/dist/css/bootstrap.min.css")

# check boxes ----
# fontawesome
my_icon <- function(x, color, ...) {
  fa(x, fill = color, ...) |> 
    as_glue()
}

# fake data
tibble(
  website = c("Google", "Twitter", "RStudio"),
  url = c("www.google.com", "www.twitter.com", "www.rstudio.com"),
  date = Sys.Date() + 1:3,
  test_and_implement_ind = c(0:1, NA),
  sustain_and_spread_ind = rev(test_and_implement_ind)
) |> 
  # prep data
  mutate(website = glue("[{website}]({url})")) |> 
  select(-url) |> 
  mutate(
    date = format(date, "%m/%d/%y"),
    across(
      ends_with("_ind"),
      ~recode(
        .x,
        "0" = my_icon("times", color = "salmon"),
        "1" = my_icon("check", color = "cadetblue"),
        .missing = my_icon("minus", color = "lightgrey")
      )
    )
  )|> 
  rename_all(
    ~str_replace(.x, "_and_", " + ") |> 
      str_remove("_ind$") |> 
      str_replace("_", " ") |> 
      str_to_title()
  ) |> 
  # present, these all come from gt::
  gt() |>
  tab_options(table.font.color = "grey") |> 
  fmt_markdown(columns = everything()) |> 
  cols_width(matches("\\+") ~ px(80)) |>
  cols_align(matches("\\+"), align = "center") |>
  opt_css(my_css)

# heatmap ----
mpg_stats <- 
  mpg |> 
  filter(cyl != 5) |> 
  count(class, cyl) |> 
  arrange(cyl) |> 
  mutate(class = fct_reorder(class, n, sum, .desc = TRUE))

mpg_stats |> 
  pivot_wider(
    names_from = cyl,
    values_from = n,
    names_prefix = "cyl: "
  ) |>
  arrange(class) |> 
  gt::gt() |> 
  gt::tab_options(table.align = "left", table.margin.left = 50) |> 
  gt::data_color(
    columns = starts_with("cyl"),
    colors = scales::col_numeric(
      palette = c("lightblue", "cadetblue"),
      domain = range(mpg_stats$n),
      na.color = "lightgrey"
    )
  )
