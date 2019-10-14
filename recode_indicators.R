library(tidyverse)


fake_data <-
  tibble(
    id = letters[1:6],
    ind_group_a = c(rep(0:1, times = 2), NA, NA),
    ind_group_b = rep(1:0, each = 3)
  )


replace_in_list <- function(list, name, value) {
  if (!name %in% names(list)) {
    warning(
      glue('defaulting to binary_values = list({name} = "{value}") as {name} is unspecified'),
      call. = FALSE)
    list[[name]] <- value
    list
  } else {
    list
  }
}




roll_up_indicators <- function(x,
                               binary_values =
                                 #list(yes = "yes", no = "no"),
                                 #formals(recode_indicator),#
                                 list(if_1 = "yes", if_0 = "no", if_na = NA_character_),
                               ...) {

  ind_cols <- grep("^ind_", names(x))

  binary_values <- replace_in_list(binary_values, "if_1", "yes")
  binary_values <- replace_in_list(binary_values, "if_0", "no")
  binary_values <- replace_in_list(binary_values, "if_na", NA_character_)

  recode_vals <- list(
    "1" = binary_values$if_1,
    "0" = binary_values$if_0,
    .default = "",
    .missing = binary_values$if_na)

  df <-
    x %>%
    rename_at(ind_cols, str_remove, "^ind_") %>%
    mutate_at(
      ind_cols,
      list(~recode(.,!!!recode_vals))
      #partial(recode, recode_values)
      #partial(recode_indicator, !!!binary_values)   #partial(ifelse, !!!binary_values)
    ) %>%
    group_by_at(ind_cols) %>%
    count() %>%
    ungroup()

  knitr::kable(df, ...)
}


fake_data %>%
  roll_up_indicators()


fake_data %>%
  roll_up_indicators(list(if_1 = "has"))

fake_data %>%
  roll_up_indicators(list(if_na = "(missing)"))

fake_data %>%
  roll_up_indicators(list(if_1 = "has", if_0 = "missing"))


