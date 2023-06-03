usethis::create_package(getwd())

# every pkg
usethis::edit_file("DESCRIPTION")
usethis::use_mit_license(copyright_holder = "Jake Riley")
usethis::use_coverage(type = "codecov")
covr::codecov(token = "7bf0c157-517d-486e-92ab-8fc99315f73c")
# facetteer
# CODECOV_TOKEN=7bf0c157-517d-486e-92ab-8fc99315f73c


# README
usethis::use_readme_rmd()
# add badge
.rs.api.navigateToFile("README.Rmd", line = 19)
# knit README
devtools::build_readme()

# fix pre-commit hook
# change first line to: #!/bin/sh    instead of: #!/bin/bash
usethis::edit_file(".git/hooks/pre-commit")


usethis::use_news_md()


# actions
usethis::use_github_action_check_standard()
file.copy(
  "~/github/headliner/.github/workflows/test-coverage.yaml",
  ".github/workflows"
)


# description
write(
  file.path("BugReports: https://github.com", usethis:::target_repo_spec(), "issues"),
  file = "DESCRIPTION",
  append = TRUE
)

desc::desc_add_urls(
  usethis:::target_repo()$url |> tools::file_path_sans_ext(),
  normalize = TRUE
)

desc::desc_get_author(file = "../headliner") |>
  desc::desc_set_authors(normalize = TRUE)

usethis::use_roxygen_md()


# when ready
usethis::use_data()
pkgdown::build_favicon()



library(headliner)
make_headline <- function(x = 10, y = 20, todays_date = Sys.Date()) {
  z <- 123
  glue::glue(
    x,
    y,
    "{todays_date}: {x} vs. {y} {z}",
    formatted_date = format(todays_date, "%B %Y"),
    .envir = rlang::current_env()
  )
}

make_headline(25)
#> Error in format(todays_date, "%B %Y"): object 'todays_date' not found


# This does
make_headline <- function(x = 10, y = 20, todays_date = Sys.Date()) {
  headline(x,
           y,
           "In the month to {formatted_date}, {trend} of {delta} ({orig_values})",
           formatted_date = format(dynGet("todays_date"), "%B %Y"))
}

make_headline()
#> In the month to September 2022, decrease of 10 (10 vs. 20)
