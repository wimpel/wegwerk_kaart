library(readxl)
library(sf)
library(tidyverse)
library(data.table)
library(osrm)
library(leaflet)
library(htmlwidgets)
library(htmltools)

DT <- data.table::setDT(
  readxl::read_excel("./data/test02.xlsx", 
                     col_types = c(rep("text", 2), rep("numeric",4),"text", "numeric", rep("text", 2), "numeric", rep("text", 3))))

# id toevoegen voor makkelijk joinen
DT[, id := .I]
# rijbaan goedzetten
DT[rijbaan == "li", rijbaan := "Li"]
DT[rijbaan2 == "li", rijbaan2 := "Li"]
DT[rijbaan == "re", rijbaan := "Re"]
DT[rijbaan2 == "re", rijbaan2 := "Re"]
#koppelen aan nwb coordinaten
DT[rwsvwm::nwb_match(id_ = DT[is.na(lon_van), id], 
                     wegnaam_ = DT[is.na(lon_van), weg], 
                     rijbaan_ = DT[is.na(lon_van), rijbaan], 
                     hectometer_ = DT[is.na(lon_van), hm], 
                     rds_nwb_ = "i:/brondata/nwb/20201101-nwb_bewerkt.rds")[[1]],
   `:=`(lon_van = i.longitude.nwb, lat_van = i.latitude.nwb),
   on = .(id)]
DT[rwsvwm::nwb_match(id_ = DT[is.na(lon_tot), id], 
                     wegnaam_ = DT[is.na(lon_tot), weg2], 
                     rijbaan_ = DT[is.na(lon_tot), rijbaan2], 
                     hectometer_ = DT[is.na(lon_tot), hm2], 
                     rds_nwb_ = "i:/brondata/nwb/20201101-nwb_bewerkt.rds")[[1]],
   `:=`(lon_tot = i.longitude.nwb, lat_tot = i.latitude.nwb),
   on = .(id)]
#splits in lijnen en punten
punten <- DT %>%
  filter(is.na(weg2)) %>%
  mutate(color = dplyr::case_when(
    Type == "Tekstkar" ~ "yellow",
    Type == "MRS" ~ "red",
    Type == "Verkeersregelaar" ~ "darkblue"),
    ) %>%
  sf::st_as_sf(coords = c("lon_van", "lat_van"), crs = 4326)
lijnen <- DT %>%
  filter(Type == "Test") %>%
  mutate(color = "orange")
lijnen_van <- lijnen %>%
  sf::st_as_sf(coords = c("lon_van", "lat_van"), crs = 4326)
lijnen_tot <- lijnen %>%
  sf::st_as_sf(coords = c("lon_tot", "lat_tot"), crs = 4326)
lijnen <- lapply(seq.int(nrow(lijnen_van)), function(i) osrm::osrmRoute(lijnen_van[i,], 
                                                                        lijnen_tot[i,], 
                                                                        returnclass = "sf")) %>%
  dplyr::bind_rows() %>%
  mutate(color = "orange", 
         label = paste0("<p><b>",lijnen_van$Type,"</b></p><p>", lijnen_van$Opmerking, "</p><p>", 
                        lijnen_van$Verzorgingsplaats, "</p>"))

labels.punten <- DT %>%
  filter(is.na(weg2)) %>%
  mutate(label = paste0("<p><b>", Type, " - ", Omschrijving, "</b><br>", weg, rijbaan, " ", hm, "</p>",
                        "<p>", ifelse(is.na(Opmerking), "", Opmerking), "<br>", 
                        ifelse(is.na(Verzorgingsplaats), "", Verzorgingsplaats), "</p>")) %>%
  select(label) %>%
  map(as.list)

labels.lijnen <- DT %>%
  filter(!is.na(weg2)) %>%
  mutate(label = paste0("<p><b>", Type, " - ", Omschrijving, "</b><br>", 
                        weg, rijbaan, " ", hm, " tot ", weg2, rijbaan2, " ", hm2, "</p>",
                        "<p>", ifelse(is.na(Opmerking), "", Opmerking), "<br>", 
                        ifelse(is.na(Verzorgingsplaats), "", Verzorgingsplaats), "</p>")) %>%
  select(label) %>%
  map(as.list)



mapview::mapview(lijnen)

lapply(labels.punten$label, HTML)

leaflet() %>% 
  #addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "kaart") %>%
  addWMSTiles( "https://service.pdok.nl/hwh/luchtfotorgb/wms/v1_0?",
               layers = "Actueel_ortho25",
               options=WMSTileOptions(format="image/jpeg",
                                      transparent=TRUE ) ,
               group = "luchtfoto"
  ) %>%
  addScaleBar( position = "bottomleft" ) %>%
  addPolylines(data = lijnen, color = ~color, weight = 5, opacity = 1) %>%
  addLayersControl(baseGroups = c( "kaart", "luchtfoto" ))

l
