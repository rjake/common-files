# options ----
options(
    repos = c(CRAN = "https://cran.rstudio.com/"),
    scipen = 999,
    digits = 3,
    warnPartialMatchArgs = TRUE
)


# functions ----
# open R Profile
.rprof <- function(variables) {
  usethis::edit_r_profile()
}


# remove items from global env with regex
.rm_ls <- function(regex, negate = FALSE) {
  # x <- 1;x2 <- 2; y <- 1;regex <- "^[xy]"; negate <- FALSE
  obj_list <- ls(envir = globalenv())
  to_remove <- obj_list[stringr::str_detect(obj_list, regex, negate = negate)]
  rm(list = to_remove, envir = globalenv())
}
# .rm_ls("^[xy]")


# generate Rmd or flexdb
.generate_file_from_template <- function(name, template, pkg) {
  file <- ifelse(grepl("\\.[Rr]md$", name), name, paste0(name, ".Rmd"))
  
  try( # throws a weird error from slash in css in yaml
    rmarkdown::draft(file, template = template, package = pkg)
  )
  
  file.edit(file)
}


.new_markdown <- function(name, template, pkg) {
  .generate_file_from_template(name, template, pkg)
}


.new_dashboard <- function(name, 
                          template = "flex_dashboard", 
                          pkg = "flexdashboard"
                          ) {
  rmarkdown::draft(name, template = template, package = pkg)
}


# set working directory to file location
# option to print to console vs add to source editor
.set_here <- function(console = FALSE) {
  context <- rstudioapi::getSourceEditorContext()
  wd <- paste0('setwd("', dirname(context$path), '")')
  if (console) {
    cat(wd)
  } else {
    context
    rstudioapi::modifyRange(
      context$selection[[1]]$range,
      wd,
      id = context$id
    )
  }
}


# list all files
.all_functions <- function() {
  # list all invisible fns
  r_profile <- parse("~/.Rprofile")  
  fn_code <- as.character(r_profile[grep("^\\.", r_profile)])
  # remove everything before the '{' then slide fn name over (rm assignment)
  new_fn <-
    stringr::str_remove(
      stringr::str_extract(fn_code, "^[^\\{]*"), 
      " <- function"
    )
  # tell me about it
  message("\nYou added these functions:")
  print(
    styler::style_text(
      text = trimws(new_fn)
    )
  )
}

# the list of all new functions will run with each new R session
.all_functions()
