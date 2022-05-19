library(tidyverse)
library(patchwork)

hexbin_distributions <- function(df,
                                 x,
                                 y,
                                 geoms = geom_histogram(bins = bins, fill = "lightblue", color = "white"),
                                 bins = 15,
                                 discrete_colors = NULL,
                                 heatmap_specs = NULL) {
  # df <- mpg; x <- as.symbol("cty"); y <- as.symbol("hwy")

  p <-
    ggplot(df, aes({{x}}, {{y}})) +
    geom_hex(bins = bins) +
    theme(
      panel.grid.major = element_blank(),#element_line("grey90"),
      #panel.grid.minor = element_line(NULL),
      panel.background = element_rect("white", "white")
    ) +
    heatmap_specs +
    theme(plot.margin = margin(t = 0, r = 0, b = 10, l = 10))

  #p
  #pb <- ggplot_build(p)

  #hex_layer <-
    # map_lgl(
    #   pb$plot$layers,
    #   ~inherits(.x$geom, "GeomHex")
    # ) |>
    # which() |>
    # min()

  # hex_df <- pb$data[hex_layer][[1]]

  marginal_distribution <- function(df, var, geom) {
    # df <- mpg; var <- as.symbol("cty"); geom = geom_histogram; group = NULL

    p <- suppressWarnings(
      df |>
        #mutate(group = ifelse(is.null(grouping), "", get(grouping))) |>
        ggplot(aes({{var}})) +
        geom +
        theme_void() +
        theme(plot.margin = margin()) +
        guides(fill = "none")
    )

    if (!is.null(discrete_colors)) {
      p <- suppressWarnings(
         p +
         scale_color_manual(values = discrete_colors) +
         scale_fill_manual(values = discrete_colors)
      )
    }

    p
  }

  use_geoms <- (
    if (length(geoms) == 2) {
      geoms
    } else {
      use_geoms <- list(geoms, geoms)
    }
  )

  #p1 <- p + theme(legend.position = "none")
  hist_top <-
    marginal_distribution(df, {{x}}, use_geoms[[1]])#, color)

  hist_side <-
    marginal_distribution(df, {{y}}, use_geoms[[2]]) +
    coord_flip()

  (
    hist_top + plot_spacer() +
    p + hist_side
  ) +
    plot_layout(
      ncol = 2,
      heights = c(1, 5),
      widths = c(5, 1),
      guides = "collect"
    )
}



# example ----
local({
  test_df <- {
    set.seed(1)
    tibble(
      x = rbeta(2000, 2, 7) * 100,
      y = rbeta(2000, 7, 2) * 100
    ) |>
    arrange(x/y) |>
    mutate(groups = rep(letters[1:2], each = 1000))
  }
  # df <- test_df; x <- as.symbol("x"); y <- as.symbol("y"); group = as.symbol("g")

  test_df |>
    hexbin_distributions(x, y) +
    plot_annotation(title = "All together")

  test_df |>
    hexbin_distributions(x, y, bins = 10) +
    plot_annotation(title = "Bins = 10")

  test_df |>
    hexbin_distributions(
      x, y,
      geoms = c(
        geom_histogram(bins = 20),
        geom_boxplot()
      )
    ) +
    plot_annotation(title = "Bins = 10")

  test_df |>
    hexbin_distributions(
      x, y,
      geoms = geom_density(aes(fill = groups), adjust = 1, alpha = 0.6),
      discrete_colors = c("red", "grey")
    ) +
    plot_annotation(title = "As density by group")


  test_df |>
    hexbin_distributions(
      x, y, #group = "g",
      bins = 20,
      geoms = c(
        geom_histogram(aes(fill = groups), bins = 20, position = "identity", alpha = 0.6, color = "white"),
        geom_boxplot(aes(color = groups))
      ),
      discrete_colors = c("red", "grey"),
      heatmap_specs  =
        list(
          labs(fill = "# obs"),
          scale_fill_viridis_b(),
          scale_y_continuous(limits = c(0, NA))
        )
    ) +
    plot_annotation(title = "All the works")
})


