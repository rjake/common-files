library(tidyverse)

q <- 
  "select 
    x.this1, --end comment
    x.this2, 
    (x.this3, x.this3, and /*mid-code comment*/ x.this3) as w, 
    x.this4, 
    case when (1 + 2) then 1 else 0 end as z,
    y.*, 
    1 as N,
    cast(2.3 as float) as B
  from test x
  join y on x.a = y.a
  where (this1 = this2 or (1 < 2)) and 3 between 4 and 5"


commands <- "select|from|where|group by|having|order by|limit"

a <-
  q %>% 
  str_replace_all("\n", "\\\\n") %>% 
  str_extract_all("(,?)(\\s?)[^\\s\\,]+") %>% 
  unlist()

a %>% 
  str_replace_all(", \\\\n", "\\\\n,")

df <-
  tibble(
    breakout = 
      str_replace_all(q, "\n", " ") %>% 
      str_replace_all("\\s{2,}", " ") %>% 
      str_extract_all("(,?)(\\s?)[^\\s\\,]+") %>% 
      #str_extract_all(q, "(,?)(\\s?)[^\\s\\,]+") %>% 
      unlist() %>% 
      trimws()
  ) %>% 
    mutate(
      line = 1:n(),
      command = str_extract(breakout, "select|from|where|group by|having|order by|limit"),
      open_p = str_count(breakout, "\\("),
      close_p = -str_count(lag(breakout, default = 0), "\\)")
    ) %>% 
    fill(command) %>%
    mutate(
      command = fct_inorder(factor(command)),
      c_open = cumsum(open_p),
      c_close = cumsum(close_p),
      c_both = (c_open + c_close) == 0,
      section = cumsum(
        c_both | c_both != lag(c_both, default = first(c_both))
      )
    ) %>% 
    select(-c(open_p:c_both)) %>% 
    group_by(command, section) %>% 
    summarise(
      text = 
        glue_collapse(breakout, " ") %>% str_replace_all(" , ", ", ")
    ) %>% 
    ungroup() 


sql_select <-
  df %>% 
  filter(command == "select") %>% 
  mutate(
    collapse = case_when(
      row_number() == 2 & text == "distinct" ~ 0,
      row_number() == 2 ~ 1,
      str_detect(text, "^,|when|end") ~ 1,
      TRUE ~ 0
    ),
    lines = cumsum(collapse)
  ) %>%
  group_by(lines) %>% 
  summarise(text = glue_collapse(text, sep = " ")) %>% 
  ungroup() %>% 
  mutate(
    text = str_replace(text, "(when|end)", "    \\1"),
    text = ifelse(text == "select", text, paste0("    ", text))
  )
  

