library(tidyverse)
library(glue)
library(networkD3)
library(htmlwidgets)

pkg <- "shiny"
pkg_env <- getNamespace(pkg)
pkg_fns <- names(pkg_env) |> sort()
exported_fns <- getNamespaceExports(pkg) |> sort()
#fn_list <- pkg_fns
expr <- rlang::env_get

#' @examples 
#' explore_fn(fn = "env_get",    env = getNamespace("rlang") )
#' explore_fn(fn = "geom_point", env = getNamespace("ggplot2") )
explore_fn <- function(fn, env = NULL) {
  expr <- env[[fn]]
  
  if (!is.function(expr)) {
    return()
  }
  
  all_args <- formals(expr)
  
  keep_args <- 
    all_args |> 
    keep(~class(.x) == "call")
  
  # exclude_arg_names <- setdiff(
  #   names(all_args), # remove these
  #   c(fn_list, names(keep_args)) # if included here
  # )
  
  token_df <- 
    expr |>
    body() |>
    deparse() |>
    parse(text = _) |> 
    utils::getParseData() |> 
    as_tibble()
  
  
  df <- 
    token_df |> 
    filter(
      token %in% c("SPECIAL", "SYMBOL", "SYMBOL_FUNCTION_CALL"),
      !text %in% names(all_args)
    ) |> 
    distinct(used = text)
  
  if (length(keep_args)) {
    df <-
      df |>
      bind_rows(
        tibble(
          used =
            keep_args |>
              as.character() |>
              str_remove_all("\\(.*")
        )
      )
  }
  
  if (!missing(env)) {
    df <- filter(df, used %in% names(env))
  }
  
  df |> 
    mutate(
      fn = fn, 
      .before = everything()
    )
}

all_fns <- 
  map_dfr(
    .x = pkg_fns,
    .f = possibly(explore_fn, NA),
    env = pkg_env
  ) |> 
  print()


all_nodes <- 
  all_fns |> 
  mutate(id = row_number()) |>
  #filter(str_detect(paste(fn, used), "%..%")) |> 
  pivot_longer(-id) |>
  # prioritize the count of fn
  add_count(value) |>
  mutate(n = ifelse(name == "used", NA, n)) |>
  group_by(id) |>
  fill(n) |>
  ungroup() |> 
  pivot_wider(
    names_from = name, 
    values_from = value
  ) |>
  #mutate(used = ifelse(n == 1, "(not found)", used)) |>
  print()


# function to create tables for network
prep_for_network <- function(from, to, export_list = exported_fns, link_size = 3) {
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

#save.image(file = "~/github/one-off-projects/R/shiny-codebase-app/.RData")




# build link & node tables (list object)
network <-
  prep_for_network(
    from = all_nodes$used,
    to = all_nodes$fn,
    link_size = 20
  )

# network$nodes |>
#   left_join(
#     name
#   )


# create network chart
networkD3::forceNetwork(
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
  #height = 1500,
  #width = 2500,
  #bounded = TRUE,
  charge = -50,
  zoom = TRUE
) 
