list_of_packages <- c(
  #"alluvial",
  #"animation",
  "corrplot",
  "data.table",
  "datapasta",
  "dbscan",
  "devtools",
  "doParallel",
  "DT",
  "flexdashboard",
  "geosphere",
  #"gganimate", #may need: devtools::install_github("dgrtwo/gganimate"",
  "ggmap",
  "ggrepel",
  "ggthemes",
  "glue",
  "googlesheets",
  "gridExtra",
  "highcharter",
  "httr",
  "imager",
  "knitr",
  "lubridate",
  "mapproj",
  "maptools",
  "nycflights13",
  "odbc",
  "pkgdown",
  "plotly",
  "raster",
  "RColorBrewer",
  "RCurl",
  "rgdal",
  "rgeos",
  "scales",
  "sf",
  "shiny",
  "shinydashboard",
  "shinyWidgets",
  "SnowballC",
  "stringi",
  "styler",
  "tidyverse",
  "tm",
  #"TSP
  "viewxl",
  "zoo"
)


existing <- as.data.frame(installed.packages())

new_packages <- list_of_packages[!list_of_packages %in% existing$Package]

install.packages(new_packages)

devtools::install_github("rjake/simplecolors")
devtools::install_github("rjake/shinyobjects")
devtools::install_github("rjake/whereiation")
