# library(codetools)
library(tidyverse)
library(glue)
library(networkD3)


# can change branch with:  system("git checkout feature/new_branch")
pkg_env <- devtools::load_all()$env
# pkg_env <-  asNamespace("simplecolors")


#' find symbols
#' searches for functions but may also find object names, argument names,
#' regex makes this less robust than rlang methods, will miss anything that
#' doesn't start with a letter, period or underscore (special pipes)
#' @examples
#' find_symbols(expr = ggplot2::theme_bw)
#' find_symbols(expr = ggplot2::theme_bw , env = getNamespace("ggplot2"))
find_symbols <- function(expr, env = NULL) {

  unique_steps <-
    body(expr)[-1] |>
    as.character() |>
    str_replace_all("\\\n", " ") |>
    str_remove_all('"[^"]+"') |>
    str_remove_all("'[^']+'") |>
    str_replace_all("[\\(\\),~\\!]", " ") |>
    str_replace_all("(?<=[a-zA-Z])\\[", " ") |>
    str_split(" ") |>
    flatten_chr() |>
    unique() |>
    keep(~length(.x) > 0)

  args <-  formals(expr) |> names()

  symbol_pattern <- "^[\\._a-zA-Z%][\\w\\._]*"

  if (missing(env)) {
    relevant_steps <-
      unique_steps |>
      keep(str_detect, symbol_pattern) |>
      discard(str_detect, "\\$\\w")
  } else {
    env_list <- ls(envir = env)

    relevant_steps <-
      unique_steps |>
      keep(~(.x %in% env_list))
  }

  relevant_steps |>
    discard(~(.x %in% c(args, "FALSE", "NA", "NULL", "TRUE")))
}

#' Use NAMESPACE file to find exports
#' @example
#' find_exports(ns_path = "NAMESPACE")
#' find_exports(ns_path = system.file("NAMESPACE", package = "whereiation"))
find_exports <- function(ns_path) {
  ns <-
    read_lines(
      file = ns_path,
      skip = 2
    )

  ns |>
    keep(str_detect, "export") |>
    str_remove_all("export\\(|\\)")
}

# # examples
#find_symbols(find_symbols)
#find_symbols(expr = pkg_env[[ ls(pkg_env)[1] ]])
#find_symbols(expr = pkg_env[[ ls(pkg_env)[1] ]], env = pkg_env)

export_list <- find_exports(ns_path = "NAMESPACE")

df <-
  tibble(
    fn = as.list(pkg_env) |> names(),
    used = as.list(pkg_env) |> map(find_symbols, env = pkg_env)
  ) |>
  #slice(17) |>
  unnest(used, keep_empty = TRUE) |>
  #filter(str_detect(used, "[a-zA-Z]") | is.na(used)) %>%
  #filter(used %in% fn) |>
  mutate(id = row_number()) |>
  pivot_longer(-id) |>
  add_count(value) |>
  mutate(n = ifelse(name == "used", NA, n)) |>
  group_by(id) |>
  fill(n) |>
  pivot_wider(names_from = name, values_from = value) |>
  #mutate(used = ifelse(n == 1, "(not found)", used)) |>
  drop_na() |>
  print()


# df <-
#   tibble(
#     fn = as.list(pkg_env) |> names(),
#     used = as.list(pkg_env) |> map(find_symbols, pkg_env)
#   ) |>
#   unnest(used, keep_empty = TRUE) |>
#   mutate(
#     used = ifelse(!used %in% fn & !fn %in% used, "???", used)
#   ) |>
#   drop_na() |>
#   # filter(str_detect(used, "[a-zA-Z]")) |>
#   # filter(used %in% fn) |>
#   mutate_all(str_replace_all, "_", " ") |>
#   print()


# function to create tables for network
prep_for_network <- function(from, to, link_size = 3) {
  #exported = fn %in% unclass(lsf.str(paste0("package:", package_to_analyze), all.names = TRUE))
  node_levels <-
    c(from, to) |>
    na.omit() |>
    unique() |>
    sort() |>
    factor()

  nodes <-
    tibble(
      name = node_levels,
      factor_id = as.integer(node_levels) - 1,
      group = node_levels %in% export_list
    ) |>
    arrange(name) |>
    as.data.frame()

  links <-
    tibble(
      source_name = from,
      target_name = to
    ) |>
    drop_na(source_name) |>
    mutate(
      source_name = factor(source_name, levels = levels(node_levels)),
      target_name = factor(target_name, levels = levels(node_levels)),
      source = as.integer(source_name) - 1,
      target = as.integer(target_name) - 1,
      n = link_size
    ) |>
    arrange(source_name) |>
    as.data.frame()

  list(
    nodes = nodes,
    links = links
  )
}


# build link & node tables (list object)
network <-
  prep_for_network(
    from = df$used,
    to = df$fn,
    link_size = 7
  )

# network$nodes |>
#   left_join(
#     name
#   )


# create network chart
forceNetwork(
  Links = network$links,
  Nodes = network$nodes,
  Source = "source",
  Target = "target",
  Value = "n",
  NodeID = "name",
  Group = "group",
  arrows = TRUE,
  fontSize = 14,
  colourScale = JS('d3.scaleOrdinal(["grey", "dodgerblue"])'),
  opacityNoHover = 1,
  linkDistance = 50,
  height = 700,
  width = 1000,
  bounded = TRUE,
  charge = -400,
  zoom = TRUE
)


simpleNetwork(
  df,
  Source = "used",
  Target = "fn",
  height = 500,
  width = 1000,
  zoom = TRUE
)



# END ----







# old approaches
#
# find functions used in body of another function
# original here: https://stackoverflow.com/a/11878961/4650934

#' find_functions <- function(fn) {
#'   if (!is.function(fn)) {
#'     return()
#'   }
#'
#'   leaf <- function(expr, walker) {
#'     res <- try(eval(expr), silent = TRUE)
#'     if (!is.null(res) && is.function(res)) {
#'       fns_used <<- c(fns_used, as.character(expr))
#'     }
#'   }
#'
#'   call <- function(expr, walker) {
#'     walkCode(expr[[1]], walker)
#'
#'     for (e in as.list(expr[-1])) {
#'       if (!missing(e)) {
#'         walkCode(e, walker)
#'       }
#'     }
#'   }
#'
#'   # loop through and collect fns used
#'   fns_used <- c()
#'
#'   walkCode(
#'     body(fn),
#'     makeCodeWalker(
#'       call = call,
#'       leaf = leaf,
#'       write = cat
#'     )
#'   )
#'
#'   # return unique functions used
#'   unique(fns_used)
#' }
#'
#'
#'
#' define_cols(pkg_env$compare_columns)
#' find_symbols <- function(expr) {
#'   # extract referenced columns
#'   store <- list()
#'
#'   #' @examples
#'   #' store <- list()
#'   #' extract_symbols(x = expr( y > min(c(x, z) )))
#'   #' store
#'   extract_symbols <- function(x) {
#'     e <- x
#'     for (i in seq_along(e)) {
#'       if (is.symbol(e[[i]])) store <<- append(store, e[[i]])
#'       else extract_symbols(e[[i]])
#'     }
#'   }
#'
#'   extract_symbols(expr)
#'   store
#' }
#'
#' fn_symbols <- function(fn) {
#'   fn_body <- body(fn)[-1]
#'   map(fn_body, find_symbols)
#'   fn_body[[1]] |> find_symbols()
#' }
#'
#' define_cols(expression("sum(1 + 1)"))
