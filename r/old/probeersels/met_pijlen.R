#https://github.com/slutske22/leaflet-arrowheads

library( leaflet )
library( sf )
library(geojsonsf)
library( htmlwidgets )
library( htmltools )

mapview::mapview(lijnen)

arrowHead <- htmlDependency(
  "leaflet-arrowheads",
  "0.1.2",
  src = normalizePath(".\\script"),
  #src = "./script",
  script = "leaflet-arrowheads.js"
)
geometryutil <- htmlDependency(
  "leaflet.geometryutil",
  "0.1.2",
  src = normalizePath(".\\script"),
  #src = "./script",
  script = "leaflet.geometryutil.js"
)
registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}



geo_lijnen <- sf_geojson(lijnen)
geojsonio::geojson_write( input = geo_lijnen, file = "./test.geoJSON" )


leaflet() %>%
  registerPlugin(arrowHead) %>%
  registerPlugin(geometryutil) %>%
  addTiles() %>%
  #addGeoJSON(geo_lijnen) %>%
  onRender("function(el, x, data) {
    L.geoJSON(data, {
  arrowheads: { 
    frequency: 'endonly',
    size: '50px'
  } 
}).addTo(this);
  }", data = geo_lijnen)
  
  


leaflet(geo_lijnen) %>%
  addTiles() %>%

  addPolylines(data = lijnen, color = "orange", weight = 10, opacity = 1)
  
  
  
mapview::mapview(lijnen)

str(lijnen)
