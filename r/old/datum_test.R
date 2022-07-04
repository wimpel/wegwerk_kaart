# poging met datum en tijdvelden

# crosstalk met leaflet: https://rstudio.github.io/crosstalk/using.html
# datum select: https://stackoverflow.com/questions/70943105/how-to-dynamically-change-plotly-axis-based-on-crosstalk-conditions

library(data.table)
library(tidyverse)
library(readxl)
library(osrm)
library(leaflet)
library(geojsonsf)
library(htmltools)
library(htmlwidgets)
library(leaflet.extras2)

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

##############################################################################
test <- df 



werkzaamheden <- data.table(
  groep = unique(test$groep)[c(1,3)],
  van = seq(as.POSIXct("2022-01-01", tz = "Europe/Amsterdam"), by = "3 days", length.out = 2),
  tot = seq(as.POSIXct("2022-01-07", tz = "Europe/Amsterdam"), by = "7 days", length.out = 2)
)

uurvakken <- data.table(
  timestamp = seq(as.POSIXct("2022-01-01", tz = "Europe/Amsterdam"), 
                  as.POSIXct("2022-01-31", tz = "Europe/Amsterdam"), by = "1 hours")
)

temp <- setDF(
  werkzaamheden[uurvakken, on = .(van <= timestamp, tot >= timestamp)]
)

# toegeven van-tot aan spatial data
sharedRoutes <- SharedData$new(df %>%
                                 left_join(temp, by = "groep") %>%
                                 filter(!is.na(van)))


temp2 <- df %>%
  left_join(temp, by = "groep") %>%
  filter(!is.na(van))

bscols(widths = c(1,2),
  filter_slider("date", "Datum", sharedRoutes, ~van),
  leaflet(sharedRoutes) %>% addTiles() %>% addPolylines()
)




#toevegen datumvelden
test %>%
  mutate(van = seq(as.Date("2022-01-01"), by = "3 days", length.out = unique()))

df

leaflet() %>% addTiles() %>% addPolylines(data = df)


shared_quakes <- SharedData$new(quakes[sample(nrow(quakes), 100),])
bscols(
  leaflet(shared_quakes, width = "100%", height = 300) %>%
    addTiles() %>%
    addMarkers()
)
