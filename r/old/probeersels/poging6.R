##
#idee: https://github.com/jjimenezshaw/Leaflet.Control.Layers.Tree

library(tidyverse)
library(readxl)
library(osrm)
library(leaflet)
library(geojsonsf)

# read excel file
bestand <- "./data/CBM_Schuttorf_Buren.xlsx"
bladen <- readxl::excel_sheets(bestand)
xldata <- lapply(bladen, function(x) {
  readxl::read_excel(path = bestand, sheet = x,
                     col_types = c(rep(c("numeric", "text"), 2), rep("numeric", 2)))
})
names(xldata) <- bladen

# split individual routes (will become polylines later on)
routes <- lapply(xldata, function(x) split(x, f = x$route))

# create real routes, using  osm routing
df <- 
  dplyr::bind_rows(
    lapply(seq.int(routes), function(i) {
    dplyr::bind_rows(
      lapply(seq.int(lengths(routes)[i]), function(j) {
          temp <- osrmRoute(loc = as.data.frame(routes[[i]][[j]][, c("lon", "lat")]),
                        overview = "full", returnclass = "sf") %>%
            mutate(naam = paste0(bladen[i], "_", routes[[i]][[j]][1,2], routes[[i]][[j]][1,1])) %>%
            mutate(groep = bladen[i]) %>%
            mutate(groepVol = paste0("groups.",bladen[i])) %>%
            mutate(type = ifelse(grepl("stremming", naam), "stremming", "omleiding"))
      }))
    })
  )

# get boundaries for map
grens <- sf::st_bbox(df) %>% as.vector()
# create named list of geojson routes
plotdata <- lapply(split(df, f = df$naam), sf_geojson)
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
  # set map boundaries
  fitBounds( grens[1], grens[2], grens[3], grens[4]) %>%
  onRender("function(el, x, data) {
    // funciton to define line color based on 
    //  feature.properties.type
    function getColor(d) {
      return d == 'stremming' ? 'red' :
             d == 'omleiding' ? 'seagreen' :
                                'black';
    }
    // funciton to define line dash based on 
    //  feature.properties.type
    function getDash(d) {
      return d == 'stremming' ? '20' :
             d == 'omleiding' ? '' :
                                '';
    }
    // function to set style of polylines
    function newstyle(feature) {
      return {
          color: getColor(feature.properties.type),
          weight: 10,
          opacity: 1,
          dashArray: getDash(feature.properties.type),
          fillOpacity: 0.7
      };
    }
    ///////////////////////////////////////
    //would like to make the code below this dynamic 
    //based on the groep-property in the JSON object
    //so A1L and A1R groups (and thereby the filtering)
    //are read in directly from the data object df 
    ///////////////////////////////////////
    // filtering
    function A1L(feature) {if (feature.properties.groep === 'A1L') return true} 
    function A1R(feature) {if (feature.properties.groep === 'A1R') return true}
    // crteation of layergroups
    var groups = {
            A1L: new L.LayerGroup(),
            A1R: new L.LayerGroup()
    };
    // create layers and add to groups
    var A1L = L.geoJSON(data, { 
      filter: A1L,
      style: newstyle,
      arrowheads: {frequency: 'endonly', yawn: 45, size: '30px', fill: true}
    })
    .on('mouseover', function (e) {e.target.setStyle({weight: 15, opacity: 1 });})
    .on('mouseout', function (e) {e.target.setStyle({weight: 10, opacity: 0.75});})
    .addTo(groups.A1L);
    var A1R = L.geoJSON(data, { 
      filter: A1R,
      style: newstyle,
      arrowheads: {frequency: 'endonly', yawn: 45, size: '30px', fill: true}
    })
    .on('mouseover', function (e) {e.target.setStyle({weight: 15, opacity: 1 });})
    .on('mouseout', function (e) {e.target.setStyle({weight: 10, opacity: 0.75});})
    .addTo(groups.A1R);
    /*
    // set the properties from the groupedOverlays-plugin
    var groupedOverlays = {
      Hinder: {
        'A1L': groups.A1L,
        'A1R': groups.A1R
      }
    };
    var options = {
      groupCheckboxes: false,
      exclusiveGroups: ['Hinder'],
      collapsed:false
    };
    // Add the groups
    L.control.groupedLayers(null, groupedOverlays).addTo(this);
    */
    
    var baseLayers = {
      'A1L': A1L,
      'A1R': A1R
    };
    
    var layerControl = L.control.layers(baseLayers, null, {collapsed: false}).addTo(this);
    baseLayers['A1L'].addTo(this);
    
    }", data = sf_geojson(df)) 



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
# groupedlayercontrol <- htmlDependency(
#   "leaflet.groupedlayercontrol",
#   "0.6.1",
#   src = normalizePath(".\\script"),
#   #src = "./script",
#   script = "leaflet.groupedlayercontrol.js",
#   stylesheet = "leaflet.groupedlayercontrol.css"
# )
registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}
##################################################################################################
# einde PLUGIN SECTIE
##################################################################################################





#####################################################################
# werkend
#####################################################################
# lijnkleur <- function(x) ifelse(grepl("stremming", x), "red", "seagreen")
# lijntype  <- function(x) ifelse(grepl("stremming", x), 20, 0)
# for (i in seq.int(df)) {
#      l <- l %>% addPolylines(data = df[i,], 
#                              layerId = ~naam,
#                              group = ~gsub("(.*)_.*", "\\1", naam),
#                              fill = FALSE, stroke = TRUE,
#                              weight= 5,
#                              opacity = 1,
#                              color = ~lijnkleur(naam),
#                              dashArray = ~lijntype(naam))
#   }
#####################################################################
# // werkend
#####################################################################



leaflet() %>%
  #register plugins
  registerPlugin(arrowHead) %>%
  registerPlugin(geometryutil) %>%
  # add basemap
  addProviderTiles(providers$CartoDB.Positron) %>%
  # set map boundaries
  fitBounds( grens[1], grens[2], grens[3], grens[4]) %>%
  onRender("function(el, x, data) {
        // funciton to define line color based on 
        //  feature.properties.type
        function getColor(d) {
          return d == 'stremming' ? 'red' :
                 d == 'omleiding' ? 'seagreen' :
                                    'black';
        }
        // funciton to define line dash based on 
        //  feature.properties.type
        function getDash(d) {
          return d == 'stremming' ? '20' :
                 d == 'omleiding' ? '' :
                                    '';
        }
        // function to set style of polylines
        function newstyle(feature) {
          return {
              color: getColor(feature.properties.type),
              weight: 10,
              opacity: 1,
              dashArray: getDash(feature.properties.type),
              fillOpacity: 0.7
          };
        }
        ///////////////////////////////////////
        //would like to make the code below this dynamic 
        //based on the groep-property in the JSON object
        //so A1L and A1R groups (and thereby the filtering)
        //are read in directly from the data object df 
        ///////////////////////////////////////
        // filtering
        function A1L(feature) {if (feature.properties.groep === 'A1L') return true} 
        function A1R(feature) {if (feature.properties.groep === 'A1R') return true}
        // crteation of layergroups
        var groups = {
                A1L: new L.LayerGroup(),
                A1R: new L.LayerGroup()
        };
        // create layers and add to groups
        var A1L = L.geoJSON(data, { 
          filter: A1L,
          style: newstyle,
          arrowheads: {frequency: 'endonly', yawn: 45, size: '30px', fill: true}
        })
        .on('mouseover', function (e) {e.target.setStyle({weight: 15, opacity: 1 });})
        .on('mouseout', function (e) {e.target.setStyle({weight: 10, opacity: 0.75});})
        .addTo(groups.A1L);
        var A1R = L.geoJSON(data, { 
          filter: A1R,
          style: newstyle,
          arrowheads: {frequency: 'endonly', yawn: 45, size: '30px', fill: true}
        })
        .on('mouseover', function (e) {e.target.setStyle({weight: 15, opacity: 1 });})
        .on('mouseout', function (e) {e.target.setStyle({weight: 10, opacity: 0.75});})
        .addTo(groups.A1R);

         var baseLayers = {
          'A1L': A1L,
          'A1R': A1R
        };
        
        var layerControl = L.control.layers(baseLayers, null, {collapsed: false}).addTo(this);
        baseLayers['A1L'].addTo(this);
        
        }", data = sf_geojson(df)) 


#############################################################################
# plot the map and layers
leaflet() %>%
  #register plugins
  registerPlugin(arrowHead) %>%
  registerPlugin(geometryutil) %>%
  registerPlugin(groupedlayercontrol) %>%
  # add basemap
  addProviderTiles(providers$CartoDB.Positron) %>%
  # set map boundaries
  fitBounds( grens[1], grens[2], grens[3], grens[4]) %>%
  onRender("function(el, x, data) {
    // funciton to define line color based on 
    //  feature.properties.type
    function getColor(d) {
      return d == 'stremming' ? 'red' :
             d == 'omleiding' ? 'seagreen' :
                                'black';
    }
    // funciton to define line dash based on 
    //  feature.properties.type
    function getDash(d) {
      return d == 'stremming' ? '20' :
             d == 'omleiding' ? '' :
                                '';
    }
    // function to set style of polylines
    function newstyle(feature) {
      return {
          color: getColor(feature.properties.type),
          weight: 10,
          opacity: 1,
          dashArray: getDash(feature.properties.type),
          fillOpacity: 0.7
      };
    }
    ///////////////////////////////////////
    //would like to make the code below this dynamic 
    //based on the groep-property in the JSON object
    //so A1L and A1R groups (and thereby the filtering)
    //are read in directly from the data object df 
    ///////////////////////////////////////
    // filtering
    function A1L(feature) {if (feature.properties.groep === 'A1L') return true} 
    function A1R(feature) {if (feature.properties.groep === 'A1R') return true}
    // crteation of layergroups
    var groups = {
            A1L: new L.LayerGroup(),
            A1R: new L.LayerGroup()
    };
    // create layers and add to groups
    var A1L = L.geoJSON(data, { 
      filter: A1L,
      style: newstyle,
      arrowheads: {frequency: 'endonly', yawn: 45, size: '30px', fill: true}
    })
    .on('mouseover', function (e) {e.target.setStyle({weight: 15, opacity: 1 });})
    .on('mouseout', function (e) {e.target.setStyle({weight: 10, opacity: 0.75});})
    .addTo(groups.A1L);
    var A1R = L.geoJSON(data, { 
      filter: A1R,
      style: newstyle,
      arrowheads: {frequency: 'endonly', yawn: 45, size: '30px', fill: true}
    })
    .on('mouseover', function (e) {e.target.setStyle({weight: 15, opacity: 1 });})
    .on('mouseout', function (e) {e.target.setStyle({weight: 10, opacity: 0.75});})
    .addTo(groups.A1R);
    /*
    // set the properties from the groupedOverlays-plugin
    var groupedOverlays = {
      Hinder: {
        'A1L': groups.A1L,
        'A1R': groups.A1R
      }
    };
    var options = {
      groupCheckboxes: false,
      exclusiveGroups: ['Hinder'],
      collapsed:false
    };
    // Add the groups
    L.control.groupedLayers(null, groupedOverlays).addTo(this);
    */
    
    var baseLayers = {
      'A1L': A1L,
      'A1R': A1R
    };
    
    var layerControl = L.control.layers(baseLayers, null, {collapsed: false}).addTo(this);
    baseLayers['A1L'].addTo(this);
    
    }", data = sf_geojson(df)) 
