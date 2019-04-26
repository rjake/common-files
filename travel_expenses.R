library(tidyverse)
library(lubridate)
library(googlesheets)
library(qicharts2)

options(scipen = 999)

# PREP ----
# from google takeout
{
all_files <- 
  list.files(
    "C:/Users/foxtr/Desktop/Takeout/Fit/Daily Aggregations", 
    full.names = TRUE
  )

read_steps <- function(path) {
  read.csv(path) %>% 
    mutate(date = str_remove_all(path, ".*/|\\.csv$")) %>% 
    group_by(date) %>% 
    summarise(steps = sum(Step.count, na.rm = TRUE)) %>%
    ungroup()
}

# steps <- map_dfr(all_files, read_steps)
# write.csv(steps, "steps.csv", row.names = F)
}


# get oauth
# options(httr_oob_default=TRUE) 
# gs_auth(new_user = TRUE) 

# google sheet to read in
google_key <- 
  gs_key("1gwyoKqypFLGImreco5YhHrKzT6DcDoT-ixIvujfhwzE")

# get data from googlesheets
septa <-
  gs_read(
    google_key,
    ws = "Septa",
    lookup = TRUE,
    check.names = FALSE
  ) %>% 
  data.frame() %>%
  set_names(tolower) %>% 
  mutate(
    source = "SEPTA",
    date = mdy_hms(time) %>% as.Date(),
    cost = cost.ea,
    n = 1
  ) %>% 
  select(source, date, cost, n)


lyft <-
  gs_read(
    google_key,
    ws = "Lyft",
    lookup = TRUE,
    check.names = FALSE
  ) %>% 
  data.frame() %>%
  set_names(tolower) %>% 
  mutate(
    source = "Lyft",
    date = ymd_hms(time) %>% as.Date(),
    cost = as.numeric(str_remove(total, "\\$")),
    n = 1
  ) %>% 
  select(source:n)

steps <-
  gs_read(
    google_key,
    ws = "Steps",
    lookup = TRUE,
    check.names = FALSE, 
    col_types = cols(
      date = col_date(format = "%m/%d/%Y")
    )
  ) %>% 
  data.frame() %>%
  set_names(tolower) %>% 
  filter(steps > 0) %>% 
  mutate(
    source = "Steps",
    date = ymd(date),
    cost = 0,
    n = steps
  ) %>% 
  select(source, date, cost, n)


all_travel <-
  bind_rows(septa, lyft, steps) %>% 
  mutate(month = floor_date(date, "month")) %>% 
  filter(month > "2018-01-01") %>%
  group_by(source, month) %>%
  summarise(
    volume = sum(n),
    cost = sum(cost)
  ) %>%
  ungroup() %>%
  gather(measure, value, volume:cost) %>%
  filter(!(source == "Steps" & measure == "cost")) %>% 
  filter((source == "Steps" & month > "2017-12-01") | measure == "cost") %>% 
  group_by(source, measure) %>%
  mutate(mean = mean(value)) %>%
  ungroup() %>% 
  mutate(label = paste0(source, " - ", measure))


format_si <- function(...) {
  function(x) {
    limits <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12,
                1e-9,  1e-6,  1e-3,  1e0,   1e3,
                1e6,   1e9,   1e12,  1e15,  1e18,
                1e21,  1e24)
    prefix <- c("y",   "z",   "a",   "f",   "p",
                "n",   "µ",   "m",   " ",   "k",
                "M",   "G",   "T",   "P",   "E",
                "Z",   "Y")
    
    # Vector with array indices according to position in intervals
    i <- findInterval(abs(x), limits)
    
    # Set prefix to " " for very small values < 1e-24
    i <- ifelse(i==0, which(limits == 1e0), i)
    
    paste0(format(round(x/limits[i], 1),
                 trim=TRUE, scientific=FALSE, ...),
          prefix[i])
  }
}


ggplot(all_travel, aes(month)) +
  geom_vline(
    xintercept = as.Date(c("2019-02-01")), #"2018-04-01", 
    #linetype = "dotted"
    color = "lightcyan", size = 3
  ) +
  geom_line(aes(y = value), color = "dodgerblue", size = 1) +
  geom_point(aes(y = value), color = "dodgerblue", size = 3) +
  geom_line(aes(y = mean), linetype = "dashed") +
  facet_wrap(~label, scales = "free_y", ncol = 1) +
  scale_y_continuous(
    labels = format_si(),#scales::comma,#unit_format(unit = "K"), 
    limits = c(0, NA)
  ) +
  theme(
    panel.background = element_rect(fill = "white", color = "grey75"),
    strip.text = element_text(size = 12)
  )




spc_cost <-
  qic(
    month, cost,
    data = all_travel, 
    facets = ~source, 
    scales = "free_y"
  )$data %>% 
  mutate(measure = "Cost")

travel_volume <-
  all_travel %>% 
  group_by(source, month) %>%
  summarise(
    volume = sum(n),
    n = 1
  )

spc_volume <-
  qic(
    month, volume, n,
    data = travel_volume, 
    facets = ~source, 
    scales = "free_y"
  )$data %>% 
  mutate(measure = "Volume")

spc_both <-
  bind_rows(spc_cost, spc_volume) %>% 
  mutate(
    lcl = ifelse(cl - y.sd < 0, 0, cl - y.sd),
    ucl = cl + y.sd
  )

ggplot(spc_both, aes(x)) +
  geom_point(aes(y = lcl)) +
  #geom_line(aes(y = y.mean)) +
  #geom_line(aes(y = cl)) +
  facet_wrap(facet1 ~ facet2, scales = "free_y")



# not using ####################################################
library(tidyverse)
library(lubridate)
library(reticulate)
library(mboxr)

py_config()

emails <- read_mbox("C:/Users/foxtr/Desktop/Travel.mbox")

reticulate::use_condaenv(condaenv = "3", conda = "C:/ProgramData/Anaconda3/condabin")

reticulate::use_condaenv(condaenv = 'py37', required = TRUE)

emails <-
  read_lines("Travel.mbox")


emails_collapsed <-
  str_replace(emails, "(^Delivered-To: yakemaker@gmail.com)", "||\\1") %>% 
  paste(collapse = "")


find_details <-
  tibble(text = trimws(emails)) %>% 
  mutate(text = str_remove_all(text, "(=20)+")) %>% 
  mutate(
    message = cumsum(str_detect(text, "^Delivered-To: "))
  ) %>% 
  filter(message == 1) %>% 
  mutate(id = row_number()) %>% 
  group_by(message) %>% 
  mutate(
    date = max(str_extract(text, "^Date:.*"), na.rm = TRUE),
    table = cumsum(str_detect(text, "<td>")),
    row = cumsum(str_detect(text, "<tr>"))
  ) %>% 
  #filter(table == 1) %>% 
  ungroup()

    str_detect(text, "^Date:|\\$|Shared Discount") |
      lead(str_detect(text, "^at")),
    !str_detect(text, "2offupto10rides")
  ) %>% 
  mutate(type = 
           case_when(             
             str_detect(text, "^Date:") ~ "date_orig",
             str_detect(text, "^\\$") ~ "total_orig",
             lead(str_detect(text, "^\\$")) #& str_detect(text, "\\-\\$") 
                                      ~ "tip_orig",
             str_detect(text, "Shared") ~ "shared",
             TRUE ~ NA_character_
            )
  ) %>% 
  filter(!is.na(type)) %>% 
  distinct() %>% 
  spread(type, text)


final_lyft <-
  find_details %>% 
  mutate(
    date = dmy_hms(str_extract(date_orig, "(?<=,).*(?=\\+)")) + hours(5),
    day = as.Date(date),
    time = hour(date),
    shared = !is.na(shared),
    tip = str_extract(tip_orig, "(?<=\\$)."),
    total = str_extract(total_orig, "(?<=\\$)[\\d\\.]+")
  )


