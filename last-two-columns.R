# subtract the last column from the 2nd to last column
library(tidyverse)

# c_across last_col()
mtcars |> 
  rowwise() |> 
  mutate(
    diff = diff(c_across(last_col(offset = 0:1)))
  ) |> 
  ungroup()


# using '.'
mtcars |> 
  rowwise() %>% # needs to be magrittr pipe
  mutate(
    diff = diff(c_across(rev(names(.))[1:2]))
  ) |> 
  ungroup()


# lambda syntax
mtcars |> 
  (\(df) {
    df |> 
      mutate(x = df[[ncol(df)-1]] - df[[ncol(df)]])
  })()
