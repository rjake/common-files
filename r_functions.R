# workspace ----
# set working directory to file location
.set_here <- function() {
  location <- rstudioapi::getActiveDocumentContext()$id
  is_console <- (location  == "#console")

  context <- rstudioapi::getSourceEditorContext()
  wd <- paste0('setwd("', dirname(context$path), '")')

  if (is_console) {
    rstudioapi::sendToConsole(
      code = wd,
      execute = TRUE
    )
  } else {
    rstudioapi::modifyRange(
      location = context$selection[[1]]$range,
      text = wd,
      id = context$id
    )
  }
}

#' keeps only the objects that match the regex pattern
#' @examples
#' a <- 0; b <- 1; x <- 2; y <- 1;
#' .ls_keep("^[xy]")
.ls_keep <- function(regex, negate = FALSE) {
  obj_list <- ls(envir = globalenv())
  to_remove <- obj_list[!stringr::str_detect(obj_list, regex, negate = negate)]
  rm(list = to_remove, envir = globalenv())
}


#' args to environment
#' sends function innards to global environment
#' highlight this code: my_fn(a = 1, b = 2)
#' run .fn_to_env() in console
.fn_to_env <- function(fn = NULL) {
  context <- rstudioapi::getSourceEditorContext()
  old_code <- parse(text = context$selection[[1]]$text)
  
  # update primary function to list
  old_code[[1]][[1]] <- as.symbol("list")
  
  new_code <-
    old_code |> 
    as.character() |> 
    paste(" |> list2env(envir = globalenv())")

  rstudioapi::sendToConsole(
    code = new_code,
    execute = TRUE
  )
}


# reprex ----
#' surrounds selected reprex with ticks
.reprex_ticks <- function() {
  reprex::reprex_selection(venue = "gh")
}

#' copies code as is (no ticks)
.reprex_code <- function() {
  reprex::reprex_selection(venue = "r")
}


# print ----
#' print all rows
.print <- function(x) {
  print(x, n = Inf)
}


# package development ----
#' opens package helpers
.pkg_helpers <- function() {
  file.edit("~/github/package_helpers.r")
}

#' runs these two functions together
.devload <- function() {
  devtools::document()
  devtools::load_all()
}


# other ----
#' opens r profile
.rprof <- function(edit_r_profile) {
  usethis::edit_r_profile()
}

#' creates beep
.beep <- function() {
  beepr::beep(5)
}


# show list at start ----
#' prints list of functions in this file
.custom_functions <- function() {
  r_profile <- parse("~/r_functions.R")
  fn_code <- as.character(r_profile[grep("^\\.", r_profile)])
  new_fn <- stringr::str_remove(
    stringr::str_extract(fn_code, "^[^\\{]*"),
    " <- function"
  )

  # stringr::str_remove_all(fn_code, " .*|\\n|\\}")
  message("\nYou added these functions:")
  print(
    styler::style_text(
      text = trimws(new_fn)
    )
  )
}


.custom_functions()

# invisible({
#   # devtools::install_github("gaborcsardi/notifier")
#   notify_long_running <- function(second_cutoff = 20) {
#     last <- proc.time()[1]
#     function(expr, value, ok, visible) {
#       duration <- proc.time()[1] - last
#       if (duration > second_cutoff) {
#         notifier::notify(msg = paste0(collapse = " ", deparse(expr)), title = sprintf("Completed in %.02f (s)", duration))
#       }
#       last <<- proc.time()[1]
#       TRUE
#     }
#   }
#
#   addTaskCallback(notify_long_running())
# })
#
# .flash <- function(beep = FALSE) {
#   # powershell:
#   # system2(# uses backslashes
#   #   "powershell",
#   #   args = c(
#   #     "-file",
#   #     "C:\\Users\\rileyj3\\Documents\\flash_window.ps1"
#   #   )
#   # )
#
#   # c++
#   # https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-flashwindow
#   rstudio <- getWindowsHandles("all", pattern = "RStudio")[1]
#
#   #dyn.load("C:/Windows/SysWOW64/user32.dll")
#   dyn.load("C:/Windows/System32/user32.dll")
#   code <-
#     glue(
#       'BOOL FlashWindow(
#         HWND {rstudio},
#         BOOL TRUE
#       );'
#     )
# }
#
# template
# .new_rmd <- function(name, template, pkg) { #.generate_file_from_template
#   file <- ifelse(grepl("\\.[Rr]md$", name), name, paste0(name, ".Rmd"))
#
#   try( # throws a weird error from slash in css in yaml
#     rmarkdown::draft(file, template = template, package = pkg)
#   )
#
#   file.edit(file)
# }
