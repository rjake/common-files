library(codetools)
library(tidyverse)
library(networkD3)


# can change branch with:  system("git checkout feature/new_branch")
pkg_env <- devtools::load_all()$env
# pkg_env <-  asNamespace("simplecolors")


# find functions used in body of another function
# original here: https://stackoverflow.com/a/11878961/4650934
find_functions <- function(fn) {
  if (!is.function(fn)) {
    return()
  }

  leaf <- function(expr, walker) {
    res <- try(eval(expr), silent = TRUE)
    if (!is.null(res) && is.function(res)) {
      fns_used <<- c(fns_used, as.character(expr))
    }
  }

  call <- function(expr, walker) {
    walkCode(expr[[1]], walker)

    for (e in as.list(expr[-1])) {
      if (!missing(e)) {
        walkCode(e, walker)
      }
    }
  }

  # loop through and collect fns used
  fns_used <- c()

  walkCode(
    body(fn),
    makeCodeWalker(
      call = call,
      leaf = leaf,
      write = cat
    )
  )

  # return unique functions used
  unique(fns_used)
}


# examples
find_functions(pkg_env$abc)


df <-
  tibble(
    fn = as.list(pkg_env) |> names(),
    used = as.list(pkg_env) |> map(find_functions)
  ) |>
  unnest(used) |>
  filter(str_detect(used, "[a-zA-Z]")) |>
  filter(used %in% fn) |>
  mutate_all(str_replace_all, "_", " ") |>
  print()


# function to create tables for network
prep_for_network <- function(from, to, link_size = 3) {
  nodes <-
    tibble(
      name = factor(sort(unique(c(from, to)))),
      factor_id = as.integer(name),
      group = 1
    )

  links <-
    tibble(
      source_name = from,
      target_name = to,
      source = factor(source_name, levels(nodes$name)) |> as.integer() - 1,
      target = factor(target_name, levels(nodes$name)) |> as.integer() - 1,
      n = link_size
    )

  nodes$group <-
    factor(
      nodes$name %in% links$target_name
    )

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
  height = 500,
  width = 900,
  bounded = TRUE,
  charge = -250,
  zoom = TRUE
) |>
  suppressMessages()



