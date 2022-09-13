library(data.table)
library(tidyverse)
library(openxlsx)
library(readxl)
library(osrm)
library(leaflet)
library(geojsonsf)
library(htmltools)
library(htmlwidgets)
# read excel file
xldata <- setDT(
  # suppressWarnings(readxl::read_excel(path = "./data/werkzaamheden.xlsx",
  suppressWarnings(readxl::read_excel(path = "./data/20220912.xlsx", 
                     sheet = 1,
                     col_types = c(rep("text", 2), rep(c("numeric", "text"), 2), 
                                   rep("numeric", 2), rep("text", 2), "numeric", 
                                   rep("numeric", 4), rep("text", 2)))))
# create posix timestamps with start / end of work
xldata[, `:=`(van = openxlsx::convertToDateTime(datumVan + tijdVan),
              tot = openxlsx::convertToDateTime(datumTot + tijdTot))]
# create somerowid's
xldata[, id := .I]
# if no coordinates have been entered, try to merge them from nwb
nwb <- rwsvwm::nwb_match(id = xldata$id, 
                         wegnaam_ = xldata$wegnaam, 
                         rijbaan = xldata$rijbaan, 
                         hectometer_ = xldata$hectometer, 
                         rds_nwb_ = "./data/20201101-nwb_bewerkt.rds")[[1]]
xldata[nwb, 
       `:=`(lat = ifelse(is.na(lat), i.latitude.nwb, lat),
            lon = ifelse(is.na(lon), i.longitude.nwb, lon)), 
       on = .(id)]
# split individual routes (will become polylines later on)
routes <- split(xldata, by = c("werk", "route"))
# create real routes, using osm routing from osrm
df <- dplyr::bind_rows(
  lapply(seq.int(routes), function(i) {
  # create  osrm route
  temp <- osrmRoute(loc = as.data.frame(routes[[i]][, c("lon", "lat")]),
                    overview = "full", returnclass = "sf",
                    osrm.profile = "car")
  # join relevant data
  temp <- temp %>%
    dplyr::mutate(
      type = routes[[i]]$routeType[1],
      van = routes[[i]]$van[1],
      tot = routes[[i]]$tot[1],
      label = paste0("<p><b>", routes[[i]]$werk[1], "</b> - ", routes[[i]]$routeType[1], "<br>",
                     ifelse(is.na(routes[[i]]$omschrijving[1]), "geen omschrijving", routes[[i]]$omschrijving[1]), "<br>",
                     "<br>",
                     "van: ", as.character(routes[[i]]$van[1]), "<br>",
                     "tot: ", as.character(routes[[i]]$tot[1]), "<br>"),
      kleur = case_when(type == "stremming" ~ "red",
                        type == "omleiding" ~ "green", 
                        TRUE ~ "orange"),
      plaatje = routes[[i]]$image[1])
  return(temp)
  }))

saveRDS(df, "./output/routes.rds")
saveRDS(df, "./R/app/data/routes.rds")

# test
#leaflet() %>% addTiles() %>% addPolylines(data = df, color = ~kleur, label = ~label, popup = ~leafpop::popupImage(plaatje))
