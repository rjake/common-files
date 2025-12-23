"-----------------------------------------------------------
documentation and tests for package data
only uses ggplot2, otherwise base R for minimal dependencies
------------------------------------------------------------"


# R/ ----
library(ggplot2)

#' Create plots to show distribution of column values
#' function to plot data
#' @examples
 plot_column_summary(
   df = surgeries,
   char = c("day_of_week", "service", "surgery_type", "ed_ind", "opioid_rx_ind"),
   date = c("hospital_discharge_date", "surgery_date"),
   num = c("log_id", "postop_los_days")
 )

 
 plot_column_summary <- function(df, char = NULL, date = NULL, num = NULL, max_n = 12) {
  # df <- carData::MplsStops
  # df <- msleep
  plots <- list()
  
  plot_types <- 
    data.frame(
      name = names(df),
      class = sapply(df, \(x)class(x)[1])
    )
  
  plot_types$plot <- NA_character_
  
  plot_types$plot[grepl(x = plot_types$class, "char|factor")] <- "character"
  plot_types$plot[grepl(x = plot_types$class, "Date|POSIX")] <- "date"
  plot_types$plot[grepl(x = plot_types$class, "numeric|integer")] <- "numeric"
  # default to character
  plot_types$plot[is.na(plot_types$plot)] <- "character"
  
  plot_types$plot[plot_types$name %in% date] <- "date"
  plot_types$plot[plot_types$name %in% char] <- "character"
  plot_types$plot[plot_types$name %in% num] <- "numeric"
  
  cols_plot_c <- plot_types$name[plot_types$plot == "character"] |> sort()
  cols_plot_d <- plot_types$name[plot_types$plot == "date"] |> sort()
  cols_plot_n <- plot_types$name[plot_types$plot == "numeric"] |> sort()

  # characters/indicators
  if (length(cols_plot_c)) {
    use_df <- data.frame()
    for (col in cols_plot_c) {
      new_df <- 
        data.frame(
          name = col, 
          value = as.character(df[[col]]) |> substr(1, 20)
        ) #|> head()
      
      new_df$value[is.na(new_df$value)] <- "<NA>"
      value_n <- table(new_df$value) |> sort(decreasing = TRUE)
      
      new_df$value[
        !new_df$value %in% head(names(value_n), max_n)
      ] <- paste0("Other(", max(c(length(value_n) - max_n), 0), ")")
      
      use_df <- rbind(use_df, new_df)
    }
    
    new_plot <-
      use_df |>
      ggplot(aes(y = value)) +
      geom_bar(fill = "#799a3e", color = "white") +
      facet_wrap(~name, scales = "free") +
      scale_x_continuous(expand = expansion(c(0, 0.1))) +
      labs(y = NULL)
    
    plots$as_character <- new_plot
  }
  
  # dates
  if (length(cols_plot_d)) {
    use_df <- data.frame()
    for (col in cols_plot_d) {
      new_df <- data.frame(name = col, value = df[[col]])
      new_df$n <- 
      if (length(unique(new_df$value)) > 20) {
        
      }
      
      use_df <- rbind(use_df, new_df)
    }
    
    new_plot <-
      use_df |>
      ggplot(aes(x = value)) +
      geom_histogram(fill = "#33a0bb", color = "white") +
      facet_wrap(~name) +
      scale_y_continuous(expand = expansion(0)) +
      labs(x = NULL)
    
    plots$as_date <- new_plot
  }
  
  # numeric
  if (length(cols_plot_n)) {
    use_df <- data.frame()
    for (col in cols_plot_n) {
      new_df <- data.frame(name = col, value = df[[col]])
      use_df <- rbind(use_df, new_df)
    }
    
    new_plot <-
      use_df |>
      ggplot(aes(x = value)) +
      geom_histogram(fill = "#d01c65", color = "white") +
      facet_wrap(~name, scales = "free") +
      scale_y_continuous(expand = expansion(c(0, 0.1))) +
      labs(x = NULL)
    
    plots$as_numeric <- new_plot
  }
  
  plots
 }
 

theme_set(
  theme(
    panel.background = element_rect(fill = "white", color = "grey80"),
    panel.grid = element_blank()
  )
)

plot_cgd <- plot_column_summary(survival::cgd)


for (p in plot_cgd) print(p)

plot_email <-
  plot_column_summary(
    df = openintro::email50,
    char = 
      c(
        "spam", 
        "to_multiple", 
        "from", 
        "cc", 
        "sent_email", 
        "format", 
        "re_subj", 
        "urgent_subj"
      )
  )


for (p in plot_email) print(p)


plot_oscars <- plot_column_summary(openintro::oscars)

for (p in plot_oscars) print(p)


# Tests/ ----
## test-definitions-listed.R ----
get_names <- function(file) {
  test_path(file) |>
    readLines() |>
    grep(pattern = "\\item\\{", value = TRUE) |>
    gsub(pattern = ".*item\\{([^\\}]+)\\}.*", replacement = "\\1")
}

data_1_defs <-
  list(
    data = names(ed_fractures),
    docs = get_names("../../R/data-1.R")
  )

data_2_defs <-
  list(
    data = names(sepsis),
    docs = get_names("../../R/data-2.R")
  )

test_that("all names accounted for", {
  expect_equal(data_1_defs$data, data_1_defs$docs)
  expect_equal(data_1_defs$data, data_1_defs$docs)
})
