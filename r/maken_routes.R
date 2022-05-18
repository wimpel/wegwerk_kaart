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
            mutate(type = ifelse(grepl("stremming", naam), "stremming", "omleiding")) %>%
            mutate(kleur = ifelse(type == "stremming", "red", "green"))
        }))
    })
  )

saveRDS(df, "./output/routes.rds")
saveRDS(df, "./R/app/data/routes.rds")
