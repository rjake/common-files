library(tidyverse)

test_data <-
  msleep %>%
  mutate(
    order = fct_lump(order, 5) %>% as.character(),
    vore = replace_na(vore, "other")
  ) %>%
  select(order, vore, genus, name) %>%
  arrange(order, vore, genus, name)



#' @examples 
#' 
make_sankey_df <- function(df, col_1, col_2, ord_by = NA, sep = 0.02, x = 1) {
  sigmoid <- 
    c(0, (1 / (1 + exp(-(-4:4)))), 1)
  
  gap <- sep * nrow(df)

  df_set <-
    df %>%
    # assign variables
    select_(
      col_1 = col_1,
      col_2 = col_2
    ) %>%
    mutate(group = paste(col_1, col_2)) %>%
    # roll up data
    group_by(group, col_1, col_2) %>%
    summarise(n = n()) %>%
    group_by(col_1) %>%
    mutate(n_col_1 = sum(n)) %>%
    group_by(col_2) %>%
    mutate(n_col_2 = sum(n)) %>%
    ungroup() %>%
    # line up y-positions for col_1
    arrange(n_col_1, col_1, n_col_2) %>%
    mutate(
      add_gap_1 = (col_1 != lag(col_1, default = first(col_1))),
      y1 = cumsum(n + ifelse(add_gap_1, gap, 0))
    ) %>%
    ungroup() %>%
    # repeat for col_2
    arrange(n_col_2, y1) %>%
    mutate(
      add_gap_2 = (col_2 != lag(col_2, default = first(col_2))),
      y2 = cumsum(n + ifelse(add_gap_2, gap, 0))
    )


  df_final <-
    df_set %>%
    # bump up col_1 y values
    mutate(y1 = y1 + (max(y2) - max(y1))) %>%
    mutate(height = y2 - y1) %>%
    mutate(
      y_curve = list(sigmoid),
      x = list(1:11)
    ) %>%
    unnest(c(y_curve, x)) %>% 
    mutate(
      high = y1 + (y_curve * height),
      low = high - n
    ) %>%
    mutate()
}


#' @examples 
#' 
sankey_label <- function(df, col, side) {
  find_x <- ifelse(side == "left", min(df$x), max(df$x))

  df %>%
    group_by_(col) %>%
    filter(x == find_x) %>%
    summarise(
      x = find_x,
      high = max(high)
    ) %>% 
    ungroup()
}


#' @examples 
#' 
plot_sankey <- function(df, col_1, col_2) {
  prep_df <- make_sankey_df(test_data, col_1 = col_1, col_2 = col_2)


  ggplot(prep_df) +
    geom_ribbon(aes(x = x, ymax = high, ymin = low, group = group, fill = col_1),
      alpha = 0.25
    ) +
    geom_rect(
      data = filter(prep_df, x %in% c(min(x), max(x))),
      aes(
        xmin = x - 0.1, xmax = x + 0.1,
        ymin = low, ymax = high, group = group
      ),
      fill = "black"
    ) +
    geom_text(
      data = sankey_label(prep_df, "col_1", "left"),
      aes(x = x, y = high - 1, label = col_1), hjust = 1, nudge_x = -0.2
    ) +
    geom_text(
      data = sankey_label(prep_df, "col_2", "right"),
      aes(x = x, y = high - 1, label = col_2), hjust = 0, nudge_x = 0.2
    ) +
    scale_x_continuous(limits = c(0, 13), expand = c(0, 0)) +
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "white"),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )
}

# try it out ----
test_data %>% 
  plot_sankey(col_1 = "vore", col_2 = "order")
