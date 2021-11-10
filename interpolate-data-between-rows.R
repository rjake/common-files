library(tidyverse)

df <- 
  tibble(
    seq = 1:5, 
    v1 = c(1, NA, 3, NA, 5),
    v2 = c(40, NA, 60, NA, 70),
    v3 = c(10, NA, NA, NA, 100)
  ) |> 
  print()

  # seq   v1   v2    v3 
  # ----  ---  ---  ----
  #   1    1   40    10 
  #   2   NA   NA    NA 
  #   3    3   60    NA 
  #   4   NA   NA    NA 
  #   5    5   70   100 


# approx() can lineraly interpolate the values between two points
# it returns a list of 'x' and 'y'
# spline() can return non-linear values: https://stackoverflow.com/a/31845035/4650934
approx(
  x = 1:3,
  y = c(1, NA, 5),
  xout = 1:3 # can also do n = 3
)

# you can apply it like this:
df |> 
  mutate(
    v1_ext = approx(seq, v1, seq)$y, 
    v2_ext = approx(seq, v2, seq)$y,
    v3_ext = approx(seq, v3, seq)$y
  ) |> 
  knitr::kable("pandoc")

# seq   v1   v2    v3   v1_ext   v2_ext   v3_ext
# ----  ---  ---  ----  -------  -------  -------
#   1    1    40    10        1       40     10.0
#   2   NA    NA    NA        2       50     32.5
#   3    3    60    NA        3       60     55.0
#   4   NA    NA    NA        4       65     77.5
#   5    5    70   100        5       70    100.0
 

# or do it all at once using a function + across()
interpolate <- function(x, y) {
  approx(x, y, n = length(x))$y # the '$y' is different than the input var 'y'
}

df |> 
  mutate(
    across(
      .cols = starts_with("v"),
      .fns = ~interpolate(seq, .x),
      .names = "{.col}_est"
    )
  ) |> 
  knitr::kable("pandoc")

