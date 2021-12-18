library(tidyverse)
library(glue)
library(rlang)

tibble::tribble(
  ~ n, ~ min, ~ max,
  1,     0,     1,
  2,    10,   100,
  3,   100,  1000
) |> 
  pmap(runif)


make_mappable <- function(fn, ...) {
  # return a function that takes named inputs
  function(x, ...) {
    # browser()

    dots <- dots_values(...)

    f <- fn
    formal_names <- names(formals(f))


    if (inherits(x, "data.frame")) {
      use_list <-
        x |>
        select(one_of(formal_names)) |>
        as.list() |>
        suppressWarnings()
    } else {
      use_list <- x
    }

    use_list |>
      as.list() |>
      append(dots) |>
      do.call(what = f) |>
      suppressWarnings()
  }
}

# a mappable plot function
map_plot <- make_mappable(plot)

# with list
list(y = 1:5 * 10, x = 1:5) |> 
  map_plot()

# with df
tibble(y = 1:5 * 10, x = 1:5) |> 
  map_plot(xlab = 'x', ylab = "y", col = "orange", pch = "@")

# a custom print function
print_order <- function(x, y, z, ...) {
  paste(x, y, z, ...)
}

print_order(1, 2, 3, 4, 5)

# make mappable
map_print <- make_mappable(print_order)

# with df input
tibble(x = 1:2, y = 2:3, z = 4:5) |> 
  map_print()

# with extra values (...)
list(x = 1:2, y = 2:3, z = 4:5) |> 
  map_print(c(1:2))
