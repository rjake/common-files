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
assign(".rprof", \() file.edit("~/.Rprofile"), envir = globalenv())


# make FALSE when needing to update devtools, rstudioapi, shinyobjects, etc
if (TRUE) {
  library(shinyobjects)
  source("~/r_functions.R")
  .custom_functions()
}


# print a fun message
setHook("rstudio.sessionInit", function(newSession) {
  # cat("\014") # clears console
  cowsay::say(
    what = paste(
      httr::content(httr::GET("https://tedlassoquotes.com/v1/quote"))[1:2],
      collapse = " "
    ),
    by = "random",
    what_color = randomcoloR::randomColor(),
    by_color = randomcoloR::randomColor(luminosity = "bright")
  )
}, action = "append")
