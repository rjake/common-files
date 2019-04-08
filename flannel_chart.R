library(tidyverse)

movie_order <-
  c(
    "Phantom Menace", "Attack of the Clones", "Revenge of the Sith",
    "New Hope", "Empire Strikes Back", "Return of the Jedi",
    "Force Awakens"
  )

raw_data <-
  starwars %>% 
  select(films, name, species) %>% 
  mutate(species = fct_lump(species, 2)) %>% 
  unnest(films) %>%
  filter(complete.cases(.)) %>% 
  mutate(
    films = str_remove(films, "^(The|A) "),
    films = fct_relevel(films, movie_order)
  ) %>% 
  select(
    x = films,
    y = name,
    color = species
  )

raw_data <- 
  mpg %>% 
  distinct(manufacturer, class) %>% 
  select(
    y = manufacturer, 
    x = class
  ) %>% 
  mutate(color = 1)

raw_data <- 
  msleep %>% 
  select(
    x = vore, 
    y = order
  ) %>% 
  distinct() %>% 
  filter(complete.cases(.)) %>% 
  mutate(color = 1)

raw_data <-
  tibble(
    y = str_remove(fruit, " fruit"),
    x = str_split(y, ""),
    color = 1
    ) %>% 
  #sample_n(10) %>% 
  unnest(x) %>% 
  filter(
    str_detect(y, "[sfwz]"), 
    x != " "
  ) %>% 
  distinct()

raw_data <-
  nycflights13::planes %>% 
  distinct(manufacturer, engines = as.character(engines)) %>%
  #sample_n(60) %>% 
  select(x = engines, y = manufacturer) %>% 
  mutate(color = 1)
#sapply(nycflights13::planes, n_distinct);head(nycflights13::planes)

raw_data <-
  storms %>% 
  distinct(status, month = as.character(month)) %>%
  #sample_n(60) %>% 
  select(x = month, y = status) %>% 
  mutate(color = 1)
  

df <-
  raw_data  %>% 
  add_count(x, name = "n_y") %>% 
  add_count(y, name = "n_x") %>% 
  #filter(n_x > 1) %>% 
  arrange(n_x) %>%
  mutate(x = fct_reorder(x, n_y, max)) %>% 
  mutate(x_int = str_pad(as.integer(factor(x)), 2, "left", "0")) %>% 
  group_by(y) %>%
  mutate(
    cat = paste(unique(x_int) %>% sort(decreasing = TRUE), collapse = ",")
  ) %>%
  ungroup() %>%
  mutate(y = fct_reorder(y, cat, max))

ggplot(df, aes(x, y, color = color)) +
  geom_line(aes(group = x), size = 3, color = "white") +
  geom_line(aes(group = y), size = 3, alpha = 0.2) +
  geom_point(size = 4) +
  labs(color = "") +
  #coord_fixed(ratio = 0.5) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0),
    axis.text = element_text(size = 12)
  )
