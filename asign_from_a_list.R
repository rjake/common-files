library(tiryverse)

# make your plots
plot1 <- ggplot(mpg, aes(cty, hwy)) + geom_count()
plot2 <- plot1 + theme_void()
plot3 <- plot1 + theme_bw()
plot4 <- plot1 + theme_dark()

# put them in a list
list_plots <- mget(x = ls(pattern = "^plot"), envir = .GlobalEnv)

# you can call them like this
list_plots$plot1
list_plots$plot2
list_plots$plot3
list_plots$plot4

# or in your new script you can assign them into the environment
rm(list = ls(pattern = "^plot"))

for (i in seq_along(list_plots)) {
  assign(names(list_plots[i]), list_plots[[i]], .GlobalEnv)
}

# same thing using purrr
purrr::walk2(
  .x = names(list_plots),
  .y = list_plots, 
  .f = ~assign(x = .x, value = .y, envir = .GlobalEnv)
)
