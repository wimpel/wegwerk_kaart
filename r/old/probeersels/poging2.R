##
#idee: https://github.com/jjimenezshaw/Leaflet.Control.Layers.Tree

# CBM A1
library(osrm)
library(leaflet)
library(geojsonsf)

#get coordinates to build routes with
hinder.start <- rev(c(52.341823, 7.254037))
hinder.eind <- rev(c(52.284989, 6.745750))
route.start <- rev(c(lon = 52.340234, lat = 7.271095))
route.start2 <- rev(c(52.349596, 7.258845))
route.eind <- rev(c(lon = 52.302415, lat = 6.715246))
route.via <- rev(c(lon = 52.193045, lat = 7.102321))
#build routes using osrm routing
omleiding <- as.data.frame(rbind(route.start, route.via, route.eind))
omleiding2 <- as.data.frame(rbind(route.start2, route.via, route.eind))
hinder_route <- osrmRoute(src = hinder.start, dst = hinder.eind, overview = "full", returnclass = "sf") %>%
  mutate(groep = "groups.stremming")
omleiding_route <- osrmRoute(loc = omleiding, overview = "full", returnclass = "sf") %>%
  mutate(groep = "groups.omleiding")
omleiding_route2 <- osrmRoute(loc = omleiding2, overview = "full", returnclass = "sf") %>%
  mutate(groep = "groups.omleiding")
# get boundaries for map
grens <- dplyr::bind_rows(hinder_route, omleiding_route, omleiding_route2) %>%
  sf::st_bbox(df) %>% as.vector()
# put routes in a names list and convert to JSON
dataList <- lapply(
  tibble::lst(hinder_route, omleiding_route, omleiding_route2),
  sf_geojson)
# build map
leaflet() %>%
  registerPlugin(arrowHead) %>%
  registerPlugin(geometryutil) %>%
  registerPlugin(groupedlayercontrol) %>%
  addProviderTiles(providers$CartoDB.Positron, group = "kaart") %>%
  fitBounds( grens[1], grens[2], grens[3], grens[4]) %>%
  onRender("function(el, x, data) {
  var stremmingStyle = {'color': 'red', 'weight': 10, 'opacity': 1, 'dashArray': '20, 20'};
  var omleidingStyle = {'color': 'seagreen', 'weight': 10, 'opacity': 0.75};
  var hinder = data.hinder_route
  var route1 = data.omleiding_route
  var route2 = data.omleiding_route2
  var groups = {
          stremming: new L.LayerGroup(),
          omleiding: new L.LayerGroup()
        };
  L.geoJSON(hinder, {
    style: stremmingStyle
  })
  .on('mouseover', function (e) {e.target.setStyle({weight: 15});})
  .on('mouseout', function (e) {e.target.setStyle({weight: 10});})
  .addTo(groups.stremming);
  L.geoJSON(route1, {
    style: omleidingStyle,
    arrowheads: {frequency: 'endonly', yawn: 45, size: '30px', fill: true}
  })
  .on('mouseover', function (e) {e.target.setStyle({weight: 15, opacity: 1 });})
  .on('mouseout', function (e) {e.target.setStyle({weight: 10, opacity: 0.75});})
  .addTo(groups.omleiding);
  L.geoJSON(route2, {
    style: omleidingStyle,
    arrowheads: {frequency: 'endonly', yawn: 45, size: '30px', fill: true}
  })
  .on('mouseover', function (e) {e.target.setStyle({weight: 15, opacity: 1 });})
  .on('mouseout', function (e) {e.target.setStyle({weight: 10, opacity: 0.75});})
  .addTo(groups.omleiding);
  var groupedOverlays = {
    'A30 >> A1': {
      'stremmingen': groups.stremming,
      'omleidingen': groups.omleiding
      }
    };
  var options = {
    groupCheckboxes: false,
    collapsed:false
  };
  L.control.groupedLayers(null, groupedOverlays, options).addTo(this);
  }", data = dataList) %>%
  addScaleBar( position = "bottomleft" ) 



##################################################################################################
# start PLUGIN SECTIE
##################################################################################################
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
groupedlayercontrol <- htmlDependency(
  "leaflet.groupedlayercontrol",
  "0.6.1",
  src = normalizePath(".\\script"),
  #src = "./script",
  script = "leaflet.groupedlayercontrol.js",
  stylesheet = "leaflet.groupedlayercontrol.css"
)
registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}
##################################################################################################
# einde PLUGIN SECTIE
##################################################################################################