.rs.restartR(); rm(list = ls());
#library(devtools)
#library(pkgdown)
#library(beepr)

# documentation and checks
devtools::document(); beepr::beep(5);
devtools::load_all(); beepr::beep(5);
pkgdown::build_site(lazy = TRUE); beepr::beep(5);
#devtools::test(); beepr::beep(5);
devtools::check(); beepr::beep(5);
#devtools::build_vignettes()
#updatePackageVersion()


devtools::load_all()
load_reactive_objects("inst/Rmd/flexdashboard_demo.Rmd")
load_reactive_objects("inst/Rmd/test_dashboard.Rmd")
load_reactive_objects("inst/Rmd/test_dashboard_missing_inputs.Rmd")
load_reactive_objects("inst/rmd/test_dashboard_no_inputs.Rmd")
load_reactive_objects("inst/shiny/app.R")
load_reactive_objects("inst/shiny/server.R")
load_reactive_objects("inst/shiny/ui.R")

devtools::install_github("rjake/shinysim")


# GETTING STARTED ----
usethis::create_package(getwd())
usethis::use_pipe()

devtools::install_github("r-lib/devtools")
#3devtools::install_github("r-lib/usethis")
# install.packages("covr")
# install.packages("spelling")

library(devtools)
library(roxygen2)
library(usethis)
has_devel()
use_r()
use_testthat()
use_spell_check()
use_data_raw()
use_package_doc()
use_roxygen_md()

use_travis()
#use_appveyor()
use_coverage(type = c("codecov"))

# errors:

use_news_md()


# add tests
use_test("show_colors")


# updating release ----
# Bump version in DESCRIPTION:
desc::desc_bump_version("minor")
#Double check vignettes/reference pages in pkgdown:
devtools::install(); pkgdown::build_site()
# Add additional contributors/authors
file.edit("DESCRIPTION")
# Double check _pkgdown.yml for new functions
file.edit("_pkgdown.yml")
# Check test coverage: Make sure it's 100%
covr::package_coverage(type = "tests")
# Update NEWS.md: Follow format from previous releases
file.edit("NEWS.md")
# Run package spell check:
spelling::spell_check_package()
# may need to add/edit words
file.edit("inst/WORDLIST")
# Lint package:
#library(magrittr)
files_to_lint <-
  tibble::tibble(
    file = list.files(pattern = "\\.R$", recursive = TRUE),
    edit = file.info(file)$mtime
  ) magrittr::`%>%`
dplyr::filter(edit > "2019-08-03")

lapply(files_to_lint$file, lintr::lint)

# rename package
library(magrittr)
files_to_check <-
  list.files(recursive = TRUE) %>% 
  .[grep("^(?!docs|man|inst|LICENSE)", ., perl = T)] %>% 
  c(., ".Rbuildignore")


replace_text <- function(x, oldname, newname) {
  #i = 1; oldname = "shinyloadr"; newname = "shinysim"
  file <- x
  
  file_text <- readr::read_lines(file)
  
  new_text <- 
    stringr::str_replace_all(file_text, oldname, newname)

  write(new_text, file)
}

for(i in seq_along(files_to_check)) {
  replace_text(
    files_to_check[i],
    oldname = "shinyloadr",
    newname = "shinysim"
  )
}

files_to_check <-
  list.files(recursive = TRUE) %>% 
  .[grep("\\.R$", ., perl = T)]


find_text <- function(x, phrase) {
  file <- x
  
  tibble(
    file = x,
    text = readr::read_lines(file),
    has = stringr::str_detect(text, phrase)  
  ) %>% 
    mutate(line = row_number()) %>% 
    filter(has) %>% 
    select(file, line, text)
}


purrr::map_dfr(files_to_check, find_text, phrase = "nocov")

