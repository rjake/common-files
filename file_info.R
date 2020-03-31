library(tidyverse)
library(qicharts2)

# find files
files <-
  list.files(
    pattern = "*.csv", 
    recursive = TRUE
  ) 


# get info (slow)
file_info <-
  files %>% 
  #head() %>% 
  file.info() 


# build data frame
df <-
  file_info %>% 
  rownames_to_column(var = "path") %>% 
  rename(
    create_date = ctime,
    modify_date = mtime,
    last_access_date = atime
  ) %>% 
  mutate(
    file_dir = dirname(path),
    file_name = basename(path),
    file_type = str_extract(file_name, "[^\\.]+$"),
    size_mb = size / 2^20
  ) %>% 
  select(
    starts_with("file"), 
    ends_with("date"),
    size_mb
  )


# make chart
df %>% 
  mutate(n = 1) %>% 
  qic(
    data = .,
    x = create_date,
    y = n,
    x.period = "month",
    chart = "c"
  )
