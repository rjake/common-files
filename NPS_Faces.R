library(tidyverse)

#################################################
#   WORKS
#################################################
range_new <- 
  function(x, newMin, newMax){
    (x - min(x))/(max(x)-min(x)) * (newMax - newMin) + newMin 
  }

a <- 
  data.frame(join = 1, 
             x = seq(2, 7, length.out = 41)) %>% 
  mutate(y = sin(x) ) %>% 
  filter(y < 0) %>% 
  mutate(x = range_new(x, -1, 1),
         y = range_new(y, -1, 1),
         y0 = range_new(y, 0, 1))

plot(a$x, a$y0)

b <-
  data.frame(team = letters[1:11],
             NPS = seq(-100, 100, length.out = 11),
             join = 1) %>%
  right_join(a) %>%
  mutate(absy = y0*100,
         absNPS = abs(NPS)) %>% 
  filter(absy <= absNPS) %>%
  mutate(dir = ifelse(NPS < 0, -1, 1),
         width = absNPS/101,
         y2 = y*dir,
         x2 = ifelse(x < 0, 
                     x - abs(x)*width, 
                     x + abs(x)*width)
         )

  ggplot(b)+
    facet_wrap(~ team, nrow = 3) +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill = "white")) +
    geom_point(aes(x = .5, y = .25, fill = NPS), shape = 21, size = 35) +
    scale_fill_continuous(low = "darkorange", high = "yellow") +
    geom_point(aes(x = -.5, y = 1.5), shape = 21, size = 3) +
    geom_point(aes(x = .5, y = 1.5), shape = 21, size = 3) +
    geom_line(aes(x = x2, y = y2)) +
    ylim(-2.5, 2.5) +
    xlim(-2.5, 2.5) +
    coord_fixed()

##################### historical ##############################  

set.seed(124)
df <-
  expand.grid(month = 1:12,
              team = LETTERS[1:4],
              KEEP.OUT.ATTRS = F) %>% 
  rowwise() %>% 
  mutate(NPS = ifelse(month == 1,
                      sample(c(-50:80), 1),
                      sample(c(-15:15), 1)),
         join = 1) %>% 
  ungroup() %>% 
  group_by(team) %>% 
  mutate(NPS = cumsum(NPS)) %>% 
  ungroup() %>%
  right_join(a) %>%
  mutate(absy = y0*100,
         absNPS = abs(NPS)) %>% 
  filter(absy <= absNPS) %>%
  mutate(dir = ifelse(NPS < 0, -1, 1),
         width = absNPS/101,
         y2 = y*dir,
         x2 = ifelse(x < 0, 
                     x - abs(x)*width, 
                     x + abs(x)*width)
  )

ggplot(df, aes(month, NPS, color = team, group = team))+
  geom_line(size = 2)

ggplot(df)+
  facet_grid(team ~ month) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey90")) +
  geom_point(aes(x = .5, y = .25, fill = NPS), 
             shape = 21, size = 25, color = "black", stroke = 1) +
  scale_fill_continuous(low = "darkorange", high = "yellow") +
  geom_point(aes(x = -.5, y = 1.5), shape = 21, size = 3) +
  geom_point(aes(x = .5, y = 1.5), shape = 21, size = 3) +
  geom_line(aes(x = x2, y = y2), size = 1) +
  ylim(-2.5, 2.5) +
  xlim(-2.5, 2.5) +
  coord_fixed()

df %>% 
  filter(team == "C") %>% 
  ggplot()+
  geom_hline(aes(yintercept = 0), size = 1, alpha = .3, linetype = "dashed") +
  geom_line(aes(month, NPS), size = 5, alpha = .3) +
  geom_point(aes(x = month, y = NPS, fill = NPS), 
             shape = 21, size = 20, color = "black", stroke = 1) +
  scale_fill_continuous(low = "darkorange", high = "yellow") +
  scale_x_continuous(breaks = seq(1,12,1)) +
  geom_point(aes(x = (month -.35), y = (NPS + .5)), shape = 21, size = 3) +
  geom_point(aes(x = (month + .35), y = (NPS + .5)), shape = 21, size = 3) +
  geom_line(aes(x = (month + x2), 
                y = ifelse(NPS > 0, NPS + y2, NPS - (1-y2)), 
                group = month), size = 1) +
  ylim(-20, 20) +
  theme(panel.grid.minor = element_blank())
