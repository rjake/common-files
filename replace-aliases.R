require(tidyverse)
require(rstudioapi)
require(glue)


test_code <- function() {
  tmp <- tempfile()
  writeLines(
    con = tmp,
    text = glue::glue(
      "select
        ee.firstName,
        ee.lastName,
        co.company_name
      from
        employees    ee
        join company co on co.company_key = ee.company_key"
    )
  )

  file.edit(tmp)
}


ask_and_wait <- function(text) {
  invisible(readline(prompt = text))
}


find_statement <- function(statement) {
  from_statements <- rstudioapi::getSourceEditorContext()
  need_statement <- paste(
    "Please highlight", statement, "and press [enter] to continue"
  )

  if (!nchar(from_statements$selection[[1]]$text)) {
    ask_and_wait(need_statement)
    use_statements <- rstudioapi::getSourceEditorContext()
    #find_statement(statement)
  } else {
    ready <-
      menu(
        choices = c("Sure is!", "No, let me highlight the right text"),
        title = paste("Is the selected text", statement, "?")
      )
    if (ready == 1) {
      use_statements <- rstudioapi::getSourceEditorContext()
    } else {
      ask_and_wait(need_statement)
      use_statements <- rstudioapi::getSourceEditorContext()
    }
  }
  return(use_statements)
}


find_from_patterns <- function(x) {
  df <-
    tibble(orig_text = str_split(x, "\\n")[[1]]) %>%
    filter(nchar(orig_text) > 0) %>%
    mutate(
      search =
        str_remove_all(orig_text, "^(\\w+ )?join ") %>%
        str_remove_all(" on .*") %>%
        trimws(),
      alias = str_extract(search, "\\w+$"),
      alias_alone = str_c("((as)? \\b", alias, "(\\n| |$))" ),
      alias_appended = str_c("\\b", alias, "\\b"),
      table = str_extract(search, "(\\w+)(?=[ ]+\\w+$)"),
      has_join = !str_detect(orig_text, "\\bjoin\\b")
    )

  list(
    orig_text = df$orig_text,
    replace_appended = set_names(df$table, df$alias_appended),
    replace_alone = paste0(df$alias_alone, collapse = "|"),
    remove = df$alias_alone[df$has_join],
    statements = glue("{df$alias} -> {df$table}")
  )
}


replace_aliases <- function() {
  from_statement <-
    find_statement(statement = "the 'from' statements you want to use")
  from_text <- from_statement$selection[[1]]$text
  #cat(from_text)

  use_patterns <- find_from_patterns(from_text)

  print(use_patterns$statements)
  ask_and_wait("Are these replacements correct? press [enter] to continue or [esc] to exit")

  # clear selection
  rstudioapi::setSelectionRanges(
    from_statement$selection[[1]]$range[[1]],
    id = from_statement$id
  )

  whole_query <- find_statement(statement = "your whole query")
  whole_query_text <- whole_query$selection[[1]]$text



  new_text <-
    whole_query_text %>%
    str_replace_all(use_patterns$remove, "\n") %>%
    str_replace_all(use_patterns$replace_alone, " ") %>%
    str_replace_all(na.omit(use_patterns$replace_appended))

  rstudioapi::modifyRange(
    location = whole_query$selection[[1]]$range,
    text = new_text,
    id = whole_query$id
  )
}


test_code()
replace_aliases()
