# workspace ----
# set working directory to file location
.set_here <- function(generic = FALSE) {
  context <- rstudioapi::getSourceEditorContext()
  location <- rstudioapi::getActiveDocumentContext()$id

  if (generic) {
    wd <- "setwd(dirname(.rs.api.getSourceEditorContext()$path))\n\n"
  } else {
    wd <- paste0('setwd("', dirname(context$path), '")\n\n')
  }

  is_console <- (location  == "#console")


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
#' highlight this code: boxplot.stats(1:100)
#' run .fn_to_env() in console
#' then run .fn_to_env(TRUE) to include defaults
#' @examples
#' # this becomes x = 1,2,3... 
#'   boxplot.stats(1:100)
#' 
#' # can handle lazy eval, this example would extramt 'gear' to a symbol so 
#' # you could debug something like dplyr::count(data, cols = {{cols}})
#'   tidyr::pivot_longer(mtcars, gear) 
#'   
.fn_to_env <- function(include_defaults = FALSE) {
  context <- rstudioapi::getSourceEditorContext()
  clean_text <- gsub("#'", "", x = context$selection[[1]]$text)
  expr <- rlang::parse_expr(clean_text)
  fn <- eval(expr[[1]])
  
  call_list <- # bring back all arguments as a list
    expr |>
    as.call() |>
    rlang::call_match(
      fn = fn,
      defaults = include_defaults
    ) |>
    as.list()
  
  args_only <- call_list[-1] # drop function symbol

  get_symbols <- # evaluate objects in environment/search path
    purrr:::modify_if(
      .x = args_only,
      .p = ~exists(deparse(.x)),
      .f = ~get(deparse(.x))
    )
  
  update_symbols <- # if column names are used, deparse the arguments, need to test more
    purrr:::modify_if(
      .x = get_symbols,
      .p = ~inherits(.x, "name") && length(.x) == 1,
      .f = ~rlang::sym(deparse(.x))
    )
  
  eval_results <- # evaluate arguments like x = 1:100
    purrr:::modify_if(
      .x = update_symbols,
      .p = ~!(inherits(.x, "name") && length(.x) == 1),
      .f = ~eval(.x)
    )

  eval_results |>
    list2env(envir = globalenv())
}

# quarto ----
.render_quarto  <- function(x = rstudioapi::getSourceEditorContext()$path) {
  quarto::quarto_render(x, output_format = "all")
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

#' show one or more records vertically
#' @param df defaults to last thing printed
#' @param n # of records
#' @param width column width
#' @param rows length of rows in the console (after flipped)
#'
#' @examples
#' ggplot2::midwest
#' .show_n()
#' .show_n(ggplot2::midwest, n = 5, rows = 10)
#' .show_n(tidyr::billboard, n = 3, width = 10, rows = 20)
.show_n <- function(df = .Last.value, n = 3, width = 90 / n, rows = Inf) {
  df |>
    head(n) |>
    t() |>
    as.data.frame() |>
    dplyr::as_tibble(rownames = "column") |>
    dplyr::mutate_all(
      ~as.character(.x) |>
        substr(start = 1, stop = as.integer(width)) |>
        trimws()
    ) |>
    print(n = rows)
}


# package development ----
#' opens package helpers
.pkg_helpers <- function() {
  rstudioapi::navigateToFile("~/github/common-files/package_helpers.R")
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

.alerts <- function(activate = TRUE) {
  alert_active <- ("beep" %in% getTaskCallbackNames())

  if (activate & alert_active) {
    return(message("alerts: already on"))
  }

  # activate if not on
  if (activate & !alert_active) {
    options(error = function() beepr::beep(9))

    addTaskCallback(
     function(expr, value, ok, visible) {
       beepr::beep(5)
       TRUE
     },
     name = "beep"
    )
    return(message("alerts: on"))
  }

  # deactivate if on
  if (!activate & alert_active) {
    while ("beep" %in% getTaskCallbackNames()) {
      removeTaskCallback("beep")
    }
    return(
      message(
        "alerts: off\n",
        "To reset options(error): Debug > On Error > Error Inspector"
      )
    )
  }
}


# show list at start ----
#' prints list of functions in this file
.custom_functions <- function() {
  r_profile <- parse("~/GitHub/common-files/r_functions.R")
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
#   rstudioapi::navigateToFile(file)
# }
