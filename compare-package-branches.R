package <- "headliner"
old_branch <- "main"
new_branch <- "feature/some-dev"


# code runs from here
library(tidyverse)
library(glue)
library(diffr)
library(htmlwidgets)
library(htmltools)
library(reactable)

# switch to main branch
system(paste("git checkout", old_branch))
devtools::load_all()
old_pkg <- asNamespace(package)

# switch to dev branch
system(paste("git checkout", new_branch))
devtools::load_all()
new_pkg <- asNamespace(package)


#' get body of functions
#' @examples
#' get_body(env = "old_pkg", x = "sum")
#' get_body(env = "new_pkg", x = "sum")
get_body <- function(env, x) {
  fn <- get(env)[[x]]
  attr(fn, "srcref") |>    #another method:  capture.output(fn)
    glue_collapse("\n") |>
    paste0("\n") |> # add trailing new line
    as_glue()
}


# pull fn contents
all_fn <-
  tibble(
    branch = c(old_branch, new_branch),
    env = c("old_pkg", "new_pkg")
  ) |>
  mutate(
    fn = map(
      .x = env,
      .f = ~ls(envir = get(.x))
    )
  ) |>
  unnest(fn) |>
  mutate(code = map2_chr(env, fn, possibly(get_body, "")))


# find changes
pkg_changes <-
  all_fn |>
  group_by(fn) |>
  mutate(
    n = n(),
    n_versions = n_distinct(code)
  ) |>
  ungroup() |>
  arrange(fn) |>
  mutate(
    status = case_when(
      n == 2 & n_versions == 1 ~ "same",
      n_versions == 2 ~ "updated",
      env == "old_pkg" ~ "deleted",
      env == "new_pkg" ~ "added"
    )
  ) |>
  group_by(status) #sets up group_split() & group_keys()

# counts
count(pkg_changes, status)

# which fns are different
distinct(pkg_changes, status, fn) |>
  filter(status != "same") |>
  arrange(status, fn)


# turn to list
changes_list <-
  pkg_changes |>
  group_split() |>
  set_names(group_keys(pkg_changes)$status)


changes_list$updated


#' view changes between new & old functions
#' #' @examples
#' view_delta(fn_name = "change", old = "a\nb\nc\n", new = "a\nc\nd\n")
view_delta <- function(fn_name, old, new) {
  x <- tempfile()
  y <- tempfile()

  write_file(old, x)
  write_file(new, y)

  title <-
    htmltools::tags$code(
      style = "font-size:20px; font-weight:bold; color:#888;",
      glue("{fn_name}()")
    )

  branches <-
    tibble(
      existing = paste(old_branch),
      proposed = paste(new_branch)
    ) |>
    reactable::reactable(
      width = 1000,
      defaultColDef = reactable::colDef(align = "center"),
      theme = reactable::reactableTheme(style = list(fontFamily = "Segoe UI"))
    )

  diff <-
    diffr::diffr(
      x,
      y,
      wordWrap = FALSE,
      before = "",
      after = "",
      width = 1000 # doesn't work?
    )

  unlink(x)
  unlink(y)

  htmlwidgets::prependContent(
    x = diff,
    # these are stacked on top
    title,
    branches,
    htmltools::tags$br(),
    htmltools::tags$br()
  )
}

# one change
with(
  changes_list$updated,
  view_delta(fn[1], code[1], code[2])
)


# show all differences (opens in viewer)
changes_list$updated |>
#  slice(1) |>
  select(env, fn, code) |>
  spread(env, code) |>
  mutate(
    viz = pmap(
      .l = list(fn, new_pkg, old_pkg),
      .f = view_delta
    )
  ) |>
  pull(viz)



# alternative approaches:
fn <- "sum"
waldo::compare(old_pkg[[fn]], new_pkg[[fn]])
diffobj::diffChr(get_body("old_pkg", fn), get_body("new_pkg", fn))
diffobj::diffDeparse(old_pkg[[fn]], new_pkg[[fn]])
