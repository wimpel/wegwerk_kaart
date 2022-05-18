##
#idee: https://github.com/jjimenezshaw/Leaflet.Control.Layers.Tree
library(data.table)
library(tidyverse)
library(readxl)
library(osrm)
library(leaflet)
library(geojsonsf)
library(htmltools)
library(htmlwidgets)

# read excel file
bestand <- "./data/twitterkaartjes.xlsx"
bladen <- readxl::excel_sheets(bestand)
xldata <- lapply(bladen, function(x) {
  readxl::read_excel(path = bestand, sheet = x,
                     col_types = c(rep(c("numeric", "text"), 2), rep("numeric", 2), rep("text", 2), "numeric"))
})
names(xldata) <- bladen
#vullen latlon op basis van ndw
xldata <- lapply(xldata, function(x) {
  temp <- setDT(x)
  #temp <- setDT(xldata[[1]]) #  !! <-- voor testdoeleinden --> !!
  temp[, id := .I]
  temp2 <- temp[is.na(lat), ]
  nwb.temp <- rwsvwm::nwb_match(id = temp2$id, 
                                wegnaam_ = temp2$wegnaam, rijbaan = temp2$rijbaan, hectometer_ = temp2$hectometer, 
                                rds_nwb_ = "i:/brondata/nwb/20201101-nwb_bewerkt.rds")[[1]]
  temp[is.na(lat), c("lat", "lon") := 
         nwb.temp[temp[is.na(lat), ], .(latitude.nwb, longitude.nwb), on = .(id)]]
  temp[, id := NULL]
  setDF(temp)
  return(temp)
})

# split individual routes (will become polylines later on)
routes <- lapply(xldata, function(x) split(x, f = x$route))

# create real routes, using  osm routing
df <- 
  dplyr::bind_rows(
    lapply(seq.int(routes), function(i) {
    dplyr::bind_rows(
      lapply(seq.int(lengths(routes)[i]), function(j) {
          temp <- osrmRoute(loc = as.data.frame(routes[[i]][[j]][, c("lon", "lat")]),
                        overview = "full", returnclass = "sf",
                        osrm.profile = "car") %>%
            mutate(naam = paste0(bladen[i], "_", routes[[i]][[j]][1,2], routes[[i]][[j]][1,1])) %>%
            mutate(groep = bladen[i]) %>%
            mutate(groepVol = paste0("groups.",bladen[i])) %>%
            mutate(type = ifelse(grepl("stremming", naam), "stremming", "omleiding"))
      }))
    })
  )
#df
# mapview::mapview(df)
# get boundaries for map
grens <- sf::st_bbox(df) %>% as.vector()
# create named list of geojson routes
# plotdata <- lapply(split(df, f = df$naam), sf_geojson)
#  PLUGIN SECTION
# from: https://github.com/slutske22/leaflet-arrowheads
arrowHead <- htmlDependency(
  "leaflet-arrowheads",
  "0.1.2",
  src = normalizePath(".\\script"),
  #src = "./script",
  script = "leaflet-arrowheads.js"
)
# from https://github.com/makinacorpus/Leaflet.GeometryUtil
geometryutil <- htmlDependency(
  "leaflet.geometryutil",
  "0.1.2",
  src = normalizePath(".\\script"),
  #src = "./script",
  script = "leaflet.geometryutil.js"
)
# from: https://github.com/ismyrnow/leaflet-groupedlayercontrol
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
# plot the map and layers
leaflet() %>%
  #register plugins
  registerPlugin(arrowHead) %>%
  registerPlugin(geometryutil) %>%
  registerPlugin(groupedlayercontrol) %>%
  # add basemap
  addProviderTiles(providers$CartoDB.Positron) %>%
  # addWMSTiles( "https://service.pdok.nl/hwh/luchtfotorgb/wms/v1_0?",
  #              layers = "Actueel_ortho25",
  #              options=WMSTileOptions(format="image/jpeg",
  #                                     transparent=TRUE ) ,
  #              group = "luchtfoto"
  # ) %>%
  # set map boundaries
  fitBounds( grens[1], grens[2], grens[3], grens[4]) %>%
  onRender("function(el, x, data) {
    // read data from the named list passd to onRender
    // data.name_from_list
    var routes = data.routes;
    var groups = data.groups;
    var types = groups;
    
    // function to define line color based on 
    //  feature.properties.type
    function getColor(d) {
      return d == 'stremming' ? 'red' :
             d == 'omleiding' ? 'seagreen' :
                                'black';
    }
    // funciton to define line dash based on 
    //  feature.properties.type
    function getDash(d) {
      // return d == 'stremming' ? '20' :
      return d == 'stremming' ? '' :
             d == 'omleiding' ? '' :
                                '';
    }
    // function to set style of polylines
    function newstyle(feature) {
      return {
          color: getColor(feature.properties.type),
          weight: 5,
          opacity: 1,
          dashArray: getDash(feature.properties.type),
          fillOpacity: 0.7
      };
    }
    
    // layerControl optioesn for groupedOverlays
    var options = {
      //exclusiveGroups: ['Stremming'],
      groupCheckboxes: false,
      collapsed: false
    };
    // add empty layercontrol
    var layerControl = L.control.groupedLayers(null, null, options).addTo(this);
    
    // iterate over types, filter by that type, and format the layer for that feature type
    types.forEach(function(type) {
      var layer = L.geoJson(routes, {
            filter: function(feature, layer) {
              return feature.properties.groep == type;
            },
            style: newstyle,
            arrowheads: {frequency: 'endonly', yawn: 60, size: '25px', fill: false}
          })
          .on('mouseover', function (e) {e.target.setStyle({weight: 15, opacity: 1 });})
          .on('mouseout', function (e) {e.target.setStyle({weight: 10, opacity: 0.75});})
      
      // all done with the layer, add it to the control
      layerControl.addOverlay(layer, type, 'Stremming');
    });
    
    }", data = list(routes = sf_geojson(df), groups = bladen)) 

