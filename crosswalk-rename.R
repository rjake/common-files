# rename a data frame based on a crosswalk table
# some columns are used to populate multiple fields

library(tidyverse)

# mock data ----
crosswalk <- 
  tibble(
    base_name = c("hwy", "hwy", "cty", "displ", NA),
    system_a_name = c("Highway MPG", "Highway", "City MPG", "Displacement", "Something Else"),
    system_b_name = c("MPG Highway", "Highway", "MPG City", "Displacement", "Other")
  )

system_a_order <- sort(crosswalk$system_a_name)
system_b_order <- sort(crosswalk$system_b_name)

df <- mpg

# the real stuff -----
select_data <- function(system) {
  # system <- "system_a"
  crosswalk_col <- paste0(system, "_name") # from df
  col_order <- get(paste0(system, "_order")) # from global env
  
  rename_list <- 
    set_names(
      x = crosswalk$base_name,
      nm = crosswalk[[crosswalk_col]]
    ) |> 
    discard(is.na)
  
  
  df |> 
    select(rename_list) |> 
    select(one_of(col_order)) |> 
    suppressWarnings()
}


select_data("system_a")
#   `City MPG` Displacement Highway `Highway MPG`
#        <int>        <dbl>   <int>         <int>
# 1         18          1.8      29            29
# 2         21          1.8      29            29

select_data("system_b")
#   Displacement Highway `MPG City` `MPG Highway`
#          <dbl>   <int>      <int>         <int>
# 1          1.8      29         18            29
# 2          1.8      29         21            29
