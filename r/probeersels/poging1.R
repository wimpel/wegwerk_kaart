# CBM A1
library(osrm)
library(leaflet)
library(geojsonsf)

hinder.start <- rev(c(52.341823, 7.254037))
hinder.eind <- rev(c(52.284989, 6.745750))
route.start <- rev(c(lon = 52.340234, lat = 7.271095))
route.start2 <- rev(c(52.349596, 7.258845))
route.eind <- rev(c(lon = 52.302415, lat = 6.715246))
route.via <- rev(c(lon = 52.193045, lat = 7.102321))

omleiding <- as.data.frame(rbind(route.start, route.via, route.eind))
omleiding2 <- as.data.frame(rbind(route.start2, route.via, route.eind))

hinder.route <- osrmRoute(src = hinder.start, dst = hinder.eind, overview = "full", returnclass = "sf")
omleiding.route <- osrmRoute(loc = omleiding, overview = "full",returnclass = "sf")
omleiding.route2 <- osrmRoute(loc = omleiding2, overview = "full",returnclass = "sf")

# mapview::mapview(hinder.route)
# mapview::mapview(omleiding.route)

leaflet() %>%
  registerPlugin(arrowHead) %>%
  registerPlugin(geometryutil) %>%
  registerPlugin(groupedlayercontrol) %>%
  addProviderTiles(providers$CartoDB.Positron, group = "kaart") %>%
  addWMSTiles( "https://service.pdok.nl/hwh/luchtfotorgb/wms/v1_0?",
               layers = "Actueel_ortho25",
               options=WMSTileOptions(format="image/jpeg",
                                      transparent=TRUE ) ,
               group = "luchtfoto"
  ) %>%
  onRender("function(el, x, data) {
  var stremmingStyle = {'color': 'red', 'weight': 10, 'opacity': 1, 'dashArray': '20, 20'};
  L.geoJSON(data, {
    style: stremmingStyle
  })
  .on('mouseover', function (e) {e.target.setStyle({weight: 15});})
  .on('mouseout', function (e) {e.target.setStyle({weight: 10});})
  .addTo(this);
  }", data = sf_geojson(hinder.route)) %>%
  onRender("function(el, x, data) {
  var omleidingStyle = {'color': 'seagreen', 'weight': 10, 'opacity': 0.75};
  L.geoJSON(data, {
    style: omleidingStyle,
    arrowheads: {frequency: 'endonly', yawn: 45, size: '30px', fill: true}
  })
  .on('mouseover', function (e) {e.target.setStyle({weight: 15, opacity: 1 });})
  .on('mouseout', function (e) {e.target.setStyle({weight: 10, opacity: 0.75});})
  .addTo(this);
  }", data = sf_geojson(omleiding.route)) %>%
  onRender("function(el, x, data) {
  var omleidingStyle = {'color': 'seagreen', 'weight': 10, 'opacity': 0.75};
  L.geoJSON(data, {
    style: omleidingStyle,
    arrowheads: {frequency: 'endonly', yawn: 45, size: '30px', fill: true}
  })
  .on('mouseover', function (e) {e.target.setStyle({weight: 15, opacity: 1});})
  .on('mouseout', function (e) {e.target.setStyle({weight: 10, opacity: 0.75});})
  .addTo(this);
  }", data = sf_geojson(omleiding.route2)) %>%
  addScaleBar( position = "bottomleft" ) %>%
  addLayersControl(baseGroups = c( "kaart", "luchtfoto" ), 
                   options = layersControlOptions(collapsed = FALSE))

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