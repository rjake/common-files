.rs.restartR(); rm(list = ls());
#library(devtools)
#library(pkgdown)
#library(beepr)

updatePackageVersion <- function(packageLocation ="."){
  ## Read DESCRIPTION file
  desc <- readLines(file.path(packageLocation, "DESCRIPTION"))

  ## Find the line where the version is defined
  vLine <- grep("^Version\\:", desc)

  ## Extract version number
  vNumber <- gsub("^Version\\:\\s*", "", desc[vLine])

    ## Split the version number into two; a piece to keep, a piece to increment
    versionNumber <- strsplit(vNumber, "\\.")[[1]]
  versionParts <- length(versionNumber)
  vNumberKeep <- paste(versionNumber[1:(versionParts-1)], sep= "", collapse= ".")
  vNumberUpdate <- versionNumber[versionParts]

  ## Replace old version number with new one (increment by 1)
  oldVersion <- as.numeric(vNumberUpdate)
  newVersion <- oldVersion + 1

  ## Build final version number
  vFinal <- paste(vNumberKeep, newVersion, sep = ".")

  ## Update DESCRIPTION file (in R)
  desc[vLine] <- paste0("Version: ", vFinal )

  ## Update the actual DESCRIPTION file
  writeLines(desc, file.path(packageLocation, "DESCRIPTION"))

  ## Return the updated version number to screen
  return(vFinal)
}

# restart R

# documentation and checks
devtools::document(); beepr::beep(5);
devtools::load_all(); beepr::beep(5);
pkgdown::build_site(lazy = TRUE); beepr::beep(5);
#devtools::test(); beepr::beep(5);
devtools::check(); beepr::beep(5);
#devtools::build_vignettes()
#updatePackageVersion()


devtools::install_github("rjake/shinyloadr")



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

