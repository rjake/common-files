# oulining spatial distributions

library(tidyverse)
library(geosphere)
library(colorspace)

color_hls <- function(...) {
  # both from colorspace
  hex(HLS(...))
}

raw_data <-
  quakes %>% 
  mutate(
    long = long-180,
    app_x = round(long, 1),
    app_y = round(lat, 1)
  ) %>% 
  group_by(app_x, app_y) %>% 
  summarise(n = n()) %>% 
  ungroup()



main_x <- mean(raw_data$app_x)
main_y <- mean(raw_data$app_y)

ggplot(raw_data, aes(app_x, app_y)) + 
  geom_point(alpha = .2) +
  geom_point(x = main_x, y = main_y, shape = "★", size = 7)


final_distances <-
  raw_data %>% 
  rowwise() %>% 
  mutate(dist = distHaversine(c(app_x, app_y), c(main_x, main_y), r = 3958.8)) %>% 
  ungroup() %>% 
  mutate(cat = cut(dist, breaks = (0:5 * 250), include.lowest = TRUE, right = FALSE, dig.lab = 4)) %>% 
  filter(!is.na(cat))#, cat != "[50,60]") 


table(final_distances$cat)
hist(final_distances$dist)


# states <-
#   map_data("state", region = c("pennsylvania", "new jersey", "maryland", "delaware", "new york"))

find_quadrant <- function(x, y) {
  case_when(
    sign(x) == 1 & sign(y) == 1  ~ 1000,
    sign(x) == 1 & sign(y) == -1  ~ 2000,
    sign(x) == -1 & sign(y) == -1  ~ 3000,
    sign(x) == -1 & sign(y) == 1  ~ 4000,
    TRUE ~ 0
  )
}

find_quadrant(-30, 30)


hull <-
  final_distances %>%
  group_by(cat) %>%
  slice(chull(app_x, app_y)) %>% 
  mutate(
    id = row_number(),
    dist_x = app_x - main_x,
    dist_y = app_y - main_y,
    rank_x = dist_x * sign(dist_y),
    rank_y = dist_y * sign(dist_x),
    quad = find_quadrant(dist_x, dist_y)
  ) %>% 
  arrange(quad, desc(rank_y), rank_x) %>%
  mutate(
    ord = row_number()
  ) %>% 
  ungroup()

change_range <- function(x, new_min, new_max) {
    (x - min(x))/(max(x)-min(x)) * (new_max - new_min) + new_min 
}


max_distances <-
  final_distances %>% 
  #group_by(cat) %>% 
  summarise(
    max_x = max_x
  )
  arrange(desc(dist)) %>% 
  slice(1) %>% 
  #ungroup() %>% 
  mutate(
    main_x = main_x,
    main_y = main_y,
    diff_x = abs(app_x - main_x),
    diff_y = abs(app_y - main_y)
  )

max_distances

outlines <-
  final_distances %>%
  mutate(
    min_x = min(app_x),
    min_y = min(app_y),
    max_x = max(app_x),
    max_y = max(app_y)
  ) %>% 
  group_by(cat, min_x, min_y, max_x, max_y) %>% 
  summarise(
    max = max(dist)
  ) %>% 
  ungroup() %>% 
  unnest(pts = list(1:4*90)) %>% #list(0:36*10)) %>%
  mutate(
    circ_x = (sin((pts)*2*pi/360)*max)+main_x,
    circ_y = (cos((pts)*2*pi/360)*max)+main_y
  ) %>% 
  mutate(
    x = change_range(circ_x, min_x, max_x),
    y = change_range(circ_y, min_y, max_y)
  )
  
ggplot(final_distances, aes(app_x, app_y)) +
  #coord_map(xlim = c(-74.5, -78), ylim = c(39.5, 42.1)) +
  # geom_polygon(
  #   data = states, aes(x = long, y = lat, group = group), 
  #   fill = "white", color = "grey"
  # ) +
  geom_polygon(data = outlines, aes(circ_x, circ_y, fill = fct_rev(cat))) +
  #geom_path(data = outlines, aes(x, y, color = cat), alpha = .4, size = 6) +
  #geom_point(aes(app_x, app_y, size = n, color = cat)) +
  geom_point(x = main_x, y = main_y, shape = "★", size = 6) 

+
  scale_size(range = c(1,4), breaks = c(1,5,10)) +
  scale_fill_manual(
    values = c(
      color_hls(180, .8, .5),
      color_hls(180, .7, .5),
      color_hls(180, .5, .5),
      color_hls(180, .4, .5),
      color_hls(180, .3, .5)
    )
  ) +
  scale_color_manual(
    values = c(
      color_hls(180, .3, .7),
      color_hls(180, .4, .7),
      color_hls(180, .5, .7),
      color_hls(180, .7, .7),
      color_hls(180, .8, .7)
    )
  ) +
  theme(
    panel.background = element_rect(color = "grey", fill = NULL),
    #axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(
    color = "Miles"
  )


