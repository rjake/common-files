# Find the file and line numbers where tables are used

library(tidyverse)

table_names <- "x|(db.*y)" # regular expression

use_files <-
  list.files(
    pattern = ".sql", 
    recursive = T
  )


path_info <-
  tibble(path = use_files) %>% 
  mutate(
    folder = str_extract(path, "\\w+"),
    query = str_extract(path, "\\w+(?=.sql)")
  )


find_table_references <- function(i) {
  # i = 2
    raw_text <- # convert file text to data frame
      read_lines(path_info$path[i]) %>% 
      str_remove_all("\\t") %>% 
      tibble(
        folder = path_info$folder[i],
        query = path_info$query[i],
        text = .,
        alias = str_extract(text, "[a-z]{2}(?=\\.)") # works only for 2 letter aliases
      ) %>% 
      mutate(line = row_number())
    
    
    find_tables <- # find alias names
      raw_text %>% 
      unnest(table = str_split(text, " ")) %>% 
      filter(str_detect(table, "[[:alpha:]]")) %>% 
      mutate(# find the lines where the alias is defined
        keep = str_detect(table, table_names),
        alias = lead(table, 1)
      ) %>% 
      filter(
        keep == TRUE,
        alias != "as"
      ) %>% 
      select(alias, table)
    
    final_search  <- # keep only rows with aliases
      raw_text %>% 
      filter(
        !str_detect(text, "join "),
        alias %in% find_tables$alias
      ) 
    
    if (nrow(final_search) > 0) { 
     
      final_search %>% 
        mutate(field = str_extract_all(text, paste0("(?<=", alias, "\\.)[^\\s]*"))) %>% 
        unnest(field) %>% 
        mutate(field = str_remove_all(field, "[\\.,\\(\\)]")) %>% 
        left_join(find_tables) %>% 
        select(folder, query, table, alias, line, field) %>% 
        distinct()
    }
    
}

final_results <-
  map_dfr(seq_along(all_files$path), find_table_references)

sapply(final_results, n_distinct)
