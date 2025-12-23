# for each PR ----
# restart R
rm(list = ls())
.rs.restartR()
# confirm status of renv
renv::status(dev = TRUE)

# Update documentation (NAMESPACE) for functions
devtools::document() # devtools::install_version("roxygen2", version = "7.1.2", repos = "http://cran.us.r-project.org")
# Load functions
devtools::load_all()
# Run tests first without package style
# devtools::test(); .beep()
devtools::test(stop_on_failure = TRUE) # filter = "package-style|summary", invert = TRUE,
system("msg * pkg tests are done")
# devtools::test(filter = "package-style|summary")
system("msg * pkg tests are done")
# check test coverage
.rs.restartR()
x <- covr::report() # if fails, restart, may also need covr::package_coverage(quiet = FALSE, clean = FALSE)
covr::report(x)

# install
devtools::install(build_vignettes = TRUE)
# build readme from Rmd if applicable
devtools::build_readme()
# fs::dir_copy("README_files", "docs")
# rebuild site
pkgdown::build_site(devel = TRUE, lazy = TRUE)
# confirm styling applied
pkgdown::init_site()
pkgdown::preview_site()
system("msg * pkg site is done")

#  Check if there are any package issues
devtools::check(env_vars = c("_R_CHECK_SYSTEM_CLOCK_" = 0), vignettes = !FALSE) # cran = TRUE)
system("msg * pkg checks complete")


# update news
file.edit("NEWS.md")


# other pkgdown functions commonly used
pkgdown::build_reference()
pkgdown::build_article(name = "Intro.Rmd")
pkgdown::build_articles()



# updating release ----
# Bump version in DESCRIPTION:
desc::desc_bump_version("patch")
# Double check vignettes/reference pages in pkgdown:
devtools::install()
pkgdown::build_site()
# Add additional contributors/authors
file.edit("DESCRIPTION")
# Double check _pkgdown.yml for new functions
file.edit("_pkgdown.yml")
# Update NEWS.md: Follow format from previous releases
file.edit("NEWS.md")
# Run package spell check:
spelling::spell_check_package()
# may need to add/edit words
# dir.create("inst"); file.create("inst/WORDLIST")
file.edit("inst/WORDLIST")



# send to CRAN ----
# additional checks
devtools::check_rhub() # rhub::validate_email(email = "rjake@sas.upenn.edu")
devtools::check_win_devel()
devtools::check_win_release()
# add comments for CRAN
rstudioapi::navigateToFile("cran-comments.md", line = 12)
# check tags >> remotes::install_github("thinkr-open/checkhelper")
tags <- checkhelper::find_missing_tags()
tags |>
  select(filename, topic, where(is.logical), starts_with("test")) |>
  filter(test_has_export_and_return != "ok" | test_has_export_or_has_nord != "ok") |>
  dplyr::rename_all(stringr::str_remove_all, "test_|has_") #|> names()

# release to CRAN, will need to confirm an email when done
devtools::release()

# also helpful https://github.com/ThinkR-open/prepare-for-cran

# fn web ----
package_to_analyze <- "simplecolors"
library(package_to_analyze, character.only = TRUE)

tibble::tibble(
  fn = unclass(lsf.str(envir = asNamespace(package_to_analyze), all.names = TRUE)),
  exported = fn %in% unclass(lsf.str(paste0("package:", package_to_analyze), all.names = TRUE))
)

# mvbutils::foodweb(
#   where = asNamespace(package_to_analyze),
#   # descendents = FALSE,
#   # ancestors = FALSE,
#   cex = 0.8,
#   # prune = "sc_within", # specific function of interest
#   # boxcolor = "grey90",
#   color.lines = TRUE
# )
