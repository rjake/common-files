library(tidyverse)
library(igraph)

# functions ----
subgraph_root <- function(g, root, direction = "out") {
  subgraph(
    graph = g,
    vids = subcomponent(g, root, direction)
  )
}


find_all_paths <- function(g, root, direction = "out") {
  sg <- subgraph_root(g, root, direction)

  leafnodes <- 
    sapply(
      X = V(sg), 
      FUN = \(x) length(neighbors(sg, x)) == 0
    )
  
  get.all.shortest.paths(
    graph = sg, 
    from = V(sg)[root], 
    to = leafnodes
  )$res
}


subgraph_path_to_table <- function(paths, sep = " -> ", to_cols = FALSE) {
  df <- 
    tibble(
      chain_id = seq_along(paths),
      node = 
        paths |> 
        map(names) |> 
        map_chr(paste, collapse = sep),
      
    ) |>
    # split to rows for next step
    separate_longer_delim(node, delim = sep) |> 
    # create cumulative paths A  A-B  A-B-C  A-B-C-D
    group_by(chain_id) |> 
    transmute(
      node,
      path = accumulate(node, ~ paste(.x, .y, sep = sep)),
    ) |> 
    ungroup() |> 
    distinct(node, path)
  
  if (!to_cols) {
    df
  } else {
    df |> 
      mutate(
        level = str_count(path, sep) + 1,
        .before = node
      ) |> 
      separate_wider_delim(
        cols = path,
        delim = sep,
        names_sep = "_", 
        too_few = "align_start"
      )
  }
}


# in action ----
## prep ----
raw_data <- 
  make_tree(30, 2) |> 
  as_data_frame() |> 
  bind_rows( tibble(from = c(24, 27), to = c(11, 14)) ) |> 
  print()

network_edges <- 
  raw_data |>
  mutate_all(~paste0("p", .x)) |> 
  add_count(from, name = "n_link") |> 
  print()

network_vertices <-
  tibble(name = c(network_edges$from, network_edges$to)) |>
  count(name)

g <- 
  graph_from_data_frame(
    d = network_edges, 
    directed = TRUE, 
    vertices = network_vertices
  ) 

plot(g, layout = layout_as_tree(g))


## subset ----
root <- "p6" 
sg <- subgraph_root(g, root, direction = "out")

V(g)$color <- "grey80"
V(g)$color[V(g)$name == root] <- "darkorange"
V(g)$color[V(g)$name %in% V(sg)$name] <- "darkgoldenrod1"
E(g)$color <- "grey80"
E(g)$color[
  str_detect( str_extract(attr(E(g), "vnames"), "^[^|]+"), glue_collapse(V(sg)$name, "|") ) 
  & str_detect( str_extract(attr(E(g), "vnames"), "[^|]+$"), glue_collapse(V(sg)$name, "|") )
] <- "black"

plot(g, layout = layout_as_tree(g), edge.arrow.size = 1, vertex.size = 15)
plot(sg, layout = layout_as_tree(sg))


## show cols ----
paths <- find_all_paths(sg, root) |> print()

paths |> subgraph_path_to_table() 
paths |> subgraph_path_to_table(sep = "--") 
paths |> subgraph_path_to_table(to_cols = TRUE) 

## ggnetwork ----
library(ggplot2)
library(ggnetwork)

sg |>
  ggnetwork::fortify(arrow.gap = 0.04) |> 
  as_tibble() |> #view()
  #pull(name)
  mutate(
    color = ifelse(name == root, "white", "black"),
    fill = ifelse(name == root, "black", "grey80")
  ) |> 
  #view()
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "gray50", arrow = arrow(length = unit(6, "pt")), curvature = 0.1) +
  geom_nodelabel(
    aes(label = name, fill = fill, color = color, alpha = n), 
    fontface = "bold", size = 2.5
  ) +
  scale_fill_identity() +
  scale_color_identity() +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  )



# scratch ----

## example plot ----
g <- 
  make_tree(20, children = 2) %>%
  set_vertex_attr(name = "name", value = seq(vcount(.)))

V(g)$color <- "grey80"
V(g)$color[V(g)$name == 4] <- "darkorange"
V(g)$color[V(g)$name %in% c(8:9,16:19)] <- "darkgoldenrod1"
E(g)$color <- "grey80"
E(g)$color[c(7,8,15:18)] <- "black"
plot(g, layout = layout_as_tree(g), edge.arrow.size = 1, vertex.size = 15)


subgraph_plot <- function(sg, label_size = 8, warn = TRUE, ...) {
  n_path <- length(sg)
  if (warn && n_path > 10) {
    res <- 
      menu(
        title = paste("There are ", n_path, " nodes. What do you want to do?"),
        choices = c("Continue with plot", "Cancel")
      )
    
    if (res == 2) {
      stop(
        call. = FALSE,
        "Consider using sq |> find_all_paths(root) |> subgraph_path_to_table()"
      )
    }
  }
  
  layout <- # horizontal
    -layout.reingold.tilford(sg)[,2:1] |> 
    norm_coords()
  
  # add vertex attributes
  V(sg)$label.cex <- label_size
  
  plot(
    sg,
    layout = layout,
    edge.arrow.width = NA,
    ...,
    rescale = !TRUE
  ) |> 
    suppressWarnings() # weird stuff in plot.default(...)
}


subgraph_plot(
  sg,
  warn = FALSE, 
  edge.curved = seq(0.5, -0.5, length = ecount(sg)),
  vertex.shape = "rectangle",
  vertex.color = "grey90",
  edge.color = "grey80",
  vertex.frame.color = NA,
  vertex.label.color = "black",
  vertex.label.cex = 7 / 10,
  #vertex.size = strwidth(V(sg)$name) * 15,
  #vertex.size2 = strheight("I") * 30,
  ylim = c(-1,1),
  xlim = c(-1, 1.2),
  asp = 6 / 10
)


## ggraph ----

library(ggraph)
ggraph(sg) +
  geom_edge_link() +
  geom_node_label(aes(label = name)) +
  ggforce::theme_no_axes()
