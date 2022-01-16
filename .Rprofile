# options ----
options(
  digits = 3 ,
  readr.read_lazy = FALSE, # prevents files from locking
  repos = c(CRAN = "https://cran.rstudio.com/"),
  scipen = 999,
  tibble.print_max = 30,
  warnPartialMatchArgs = TRUE
)

#' opens r profile
assign(".rprof", function() file.edit("C://Users/rileyj3/.Rprofile"), envir = globalenv())


# make FALSE when needing to update devtools, rstudioapi, shinyobjects, etc
if (TRUE) {
  library(shinyobjects)
  source("~/r_functions.R")
  .custom_functions()
}
