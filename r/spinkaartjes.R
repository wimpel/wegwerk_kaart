# alpha test voor inlezen SPIN data
#  bron: \\ad.rws.nl\p-dfs01\appsdata\Sas_Data_P\levering\spin\archief
# met de kaartjes die OT maakt als verkeersverwachtingen
#  bron: G:\vwm\WV_OT\Algemeen\Afdeling OT\Overleggen OVM\KCO-VCNL (Operationeel Overleg)\Verkeersverwachtingen 2022\Verkeersverwachtingen 2022

library(data.table)
library(osrm)
# test periode
van = as.POSIXct("2022-07-11 00:00:00", tz = "Europe/Amsterdam")
tot = as.POSIXct("2022-07-18 23:59:59", tz = "Europe/Amsterdam")
# lees meest recente allcurrentmeasures
DT <- fread(tail(list.files("i:/brondata/spin/", 
                            pattern = "^AllCurrentMeasures.*\\.csv$", 
                            full.names = TRUE), 
                 n = 1),
            skip = 3, sep = ";", colClasses = "character")
# tijdstempels de juiste tijdzone geven
DT[, Start := as.POSIXct(Start, format = "%d-%m-%y %H:%M:%S", tz = "Europe/Amsterdam")]
DT[, Eind := as.POSIXct(Eind, format = "%d-%m-%y %H:%M:%S", tz = "Europe/Amsterdam")]
DT[, `Werkelijke start` := as.POSIXct(`Werkelijke start`, format = "%d-%m-%y %H:%M:%S", tz = "Europe/Amsterdam")]
DT[, `Werkelijk eind` := as.POSIXct(`Werkelijk eind`, format = "%d-%m-%y %H:%M:%S", tz = "Europe/Amsterdam")]
# filter op tijdperiode
DT <- DT[Start <= as.POSIXct("2022-07-10 23:59:59", tz = "Europe/Amsterdam") &
           Eind >= as.POSIXct("2022-07-04 00:00:00", tz = "Europe/Amsterdam"), ]
# filter verkeershindercategorie van C of hoger
DT <- DT[Verkeershindercategorie %in% c("C", "B", "A") |
           Verkeershinderklasse %in% c("10 - 30 min.", "> 30 min."), ]
setkey(DT, Start)
# maak routes
nwb <- rwsvwm::nwb_match(id = DT$Nummer, 
                         wegnaam_ = DT$`Weg van`, 
                         rijbaan = DT$`Zijde van`, 
                         hectometer_ = as.numeric(gsub(",", "\\.", DT$`km van`)), 
                         rds_nwb_ = "./data/20201101-nwb_bewerkt.rds")[[1]]
DT[nwb, 
   `:=`(lat.van = i.latitude.nwb, lon.van = i.longitude.nwb), 
   on = .(Nummer = id)]
nwb <- rwsvwm::nwb_match(id = DT$Nummer, 
                         wegnaam_ = DT$`Weg tot`, 
                         rijbaan = DT$`Zijde tot`, 
                         hectometer_ = as.numeric(gsub(",", "\\.", DT$`km tot`)), 
                         rds_nwb_ = "./data/20201101-nwb_bewerkt.rds")[[1]]
DT[nwb, 
   `:=`(lat.tot = i.latitude.nwb, lon.tot = i.longitude.nwb), 
   on = .(Nummer = id)]
# melt voor routes
routes <- melt(DT, id.vars = c("Nummer", "Start", "Eind"), 
               measure.vars = patterns(lon = "^lon\\....", 
                                       lat = "^lat\\...."))
routes.L <- split(routes, by = "Nummer")

df <- dplyr::bind_rows(
  pbapply::pblapply(seq.int(routes.L), function(i) {
    # create  osrm route
    Sys.sleep(0.5)
    temp <- osrmRoute(loc = as.data.frame(routes.L[[i]][, c("lon", "lat")]),
                      overview = "full", returnclass = "sf",
                      osrm.profile = "car")
    # join relevant data
    # temp <- temp %>%
    #   dplyr::mutate(
    #     type = routes[[i]]$routeType[1],
    #     van = routes[[i]]$van[1],
    #     tot = routes[[i]]$tot[1],
    #     label = paste0("<p><b>", routes[[i]]$werk[1], "</b> - ", routes[[i]]$routeType[1], "<br>",
    #                    ifelse(is.na(routes[[i]]$omschrijving[1]), "geen omschrijving", routes[[i]]$omschrijving[1]), "<br>",
    #                    "<br>",
    #                    "van: ", as.character(routes[[i]]$van[1]), "<br>",
    #                    "tot: ", as.character(routes[[i]]$tot[1]), "<br>"),
    #     kleur = case_when(type == "stremming" ~ "red",
    #                       type == "omleiding" ~ "green", 
    #                       TRUE ~ "orange"))
    return(temp)
  }))

library(leaflet)
leaflet() %>% addTiles() %>% addPolylines(data = df, opacity = 0.8)
