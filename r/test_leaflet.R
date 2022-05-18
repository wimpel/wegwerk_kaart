library(leaflet)
library(leaflet.extras2)
library(sf)
library(tidyverse)

#remotes::install_github('trafficonese/leaflet.extras2')

routes <- readRDS("./output/routes.rds")
grens <- sf::st_bbox(routes) %>% as.vector()

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addArrowhead(data = routes,
               color = ~kleur,
               fillColor = ~kleur,
               opacity = 1,
               options = arrowheadOptions(size = "15px",frequency = "endonly")
               )
  