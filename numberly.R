#original: https://github.com/zonination/perceptions/blob/master/percept.R
library(tidyverse)
options(scipen = 999)

#Import files, load plot and data packages, fire up the number machine.
probly <- 
  read.csv("https://raw.githubusercontent.com/zonination/perceptions/master/probly.csv", 
           stringsAsFactors = F) %>% 
  gather(key = variable, value = value) %>% 
  mutate(variable = gsub("[.]", " ", variable))


numberly <- 
  read.csv("https://raw.githubusercontent.com/zonination/perceptions/master/numberly.csv", 
           stringsAsFactors = F) %>% 
  gather(key = variable, value = value) %>% 
  mutate(variable = gsub("[.]", " ", variable))

#library(scales)
#library(RColorBrewer)
probly_ord <-
  probly %>% 
  group_by(variable) %>% 
  summarise(value = median(value)) %>% 
  arrange(value) %>% 
  ungroup()

numberly_ord <-
  numberly %>% 
  group_by(variable) %>% 
  summarise(value = median(value)) %>% 
  arrange(value) %>% 
  ungroup()


probly$variable <-
  factor(probly$variable, probly_ord$variable)

numberly$variable <-
  factor(numberly$variable, numberly_ord$variable)

#Plot probability data
#png(file='plot1.png', width = 800, height = 800)
ggplot(probly,aes(variable,value))+
  geom_point(aes(color=variable), size=2, alpha=.2)+
  geom_boxplot(aes(fill=variable),alpha=.5)+
  coord_flip()+
  guides(fill=FALSE,color=FALSE)+
  xlab("Phrase")+
  ylab("Assigned Probability (%)")+
  scale_y_continuous(breaks=seq(0,100,10))+
  ggtitle("Perceptions of Probability")

#Plot numberly data
#png(file='plot2.png', width = 800, height = 500)
axis_range <- c(0.01, 0.1, 1, 10, 100, 1000, 10000)

ggplot(numberly,
       aes(variable,value))+
  geom_jitter(aes(color=variable),size=2, alpha=.2, width = 0)+
  #geom_point(aes(fill=variable),alpha=0.5, width = .3, outlier.shape = NA)+
  geom_boxplot(aes(fill=variable), alpha=.5, width = .3)+
  geom_text(data = numberly_ord, 
            aes(variable, value,label = value), 
            size = 3, vjust = -1.5) +
  scale_y_log10(labels = as.character(axis_range),
                breaks = axis_range,
                limits = c(-1, max(axis_range)))+
  guides(fill=FALSE,color=FALSE)+
  xlab("Phrase")+
  ylab("Assigned Number (log10)")+
  coord_flip()+
  theme(panel.grid.minor = element_blank()) +
  ggtitle("Perceptions of Numbers")
