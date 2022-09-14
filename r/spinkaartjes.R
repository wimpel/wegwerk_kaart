# alpha test voor inlezen SPIN data
#  bron: \\ad.rws.nl\p-dfs01\appsdata\Sas_Data_P\levering\spin\archief
# met de kaartjes die OT maakt als verkeersverwachtingen
#  bron: G:\vwm\WV_OT\Algemeen\Afdeling OT\Overleggen OVM\KCO-VCNL (Operationeel Overleg)\Verkeersverwachtingen 2022\Verkeersverwachtingen 2022

library(data.table)
library(tidyverse)
library(osrm)
# test periode
van = as.POSIXct("2022-09-09 00:00:00", tz = "Europe/Amsterdam")
tot = as.POSIXct("2022-12-31 23:59:59", tz = "Europe/Amsterdam")
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
DT <- DT[Start <= as.POSIXct("2022-12-31 23:59:59", tz = "Europe/Amsterdam") &
           Eind >= as.POSIXct("2022-09-09 00:00:00", tz = "Europe/Amsterdam"), ]
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
routes <- melt(DT, id.vars = c("Nummer", "Status", "Wegwerktype", 
                               "Start", "Eind", "Verkeershinderklasse", "Verkeershindercategorie",
                               "Afsluiting", "Kenmerk Bestek", "Kenmerk Maatregel", "faseID"), 
               measure.vars = patterns(lon = "^lon\\....", 
                                       lat = "^lat\\...."))
routes.L <- split(routes, by = "Nummer")

df <- dplyr::bind_rows(
  pbapply::pblapply(seq.int(routes.L), function(i) {
    # slaaptijd om te voorkomen dat we de osrm server hammeren
    Sys.sleep(0.5)
    #bereken route
    osrm::osrmRoute(loc = as.data.frame(routes.L[[i]][, c("lon", "lat")]),
                      overview = "full", returnclass = "sf",
                      osrm.profile = "car") %>%
      # spin maatregelnummer ophalen voor joinen verderop in het proces
      dplyr::mutate(Nummer = routes.L[[i]]$Nummer[1])
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
    #return(temp)
  }))
# tussentijds opslaan zodat niet steeds opnieuw gegenereerd hoeft te worden
saveRDS(df, "./output/20220914-20221231-spin_routes.rds")
df <- readRDS("./output/20220914-20221231-spin_routes.rds")
# toevoegen extra informatie over de werkzaamheden
data.table::setDF(DT)
kaartdata <- df %>%
  # join relevante info bij de routes
  dplyr::left_join(DT, by = "Nummer") %>%
  # kleur van de route toevoegen
  dplyr::mutate(kleur = dplyr::case_when(Afsluiting == "Ja" ~ "red",
                                         Afsluiting == "Nee" ~ "orange",
                                         TRUE ~ "green")) %>%
  dplyr::mutate(type = dplyr::case_when(Afsluiting == "Ja" ~ "stremming",
                                         Afsluiting == "Nee" ~ "belemmering",
                                         TRUE ~ "omleiding")) %>%
  dplyr::mutate(van = Start, tot = Eind) %>%
  dplyr::mutate(label = paste0("<p><b>", Wegwerktype, "</b><br>",
                               paste0(`Kenmerk Bestek`," - ", `Kenmerk Maatregel`), "<br>",
                               "start:", paste0(`Weg van`, `Zijde van`," ", `km van`), "<br>",
                               "eind:", paste0(`Weg tot`, `Zijde tot`, " ", `km tot`), "<br>",
                               "Hinderklasse:", Verkeershinderklasse, "<br>",
                               "Hindercategorie:", Verkeershindercategorie, "<br>",
                               "van:", as.character(van), "<br>",
                               "tot:", as.character(tot), "<br>")) %>%
  dplyr::mutate(plaatje = "") %>%
  dplyr::select(src, dst, duration, distance, type, van, tot, label, kleur, plaatje)


saveRDS(kaartdata, "./output/spinroutes.rds")
saveRDS(kaartdata, "./R/app/data/spinroutes.rds")  
  
  



# library(leaflet)
# library(leaflet.extras2)
# library(leafpop)
# leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% 
#   #addPolylines(data = df, opacity = 0.8, color = "red")
#   addArrowhead(data = kaartdata,
#                color = ~kleur,
#                fillColor = ~kleur,
#                opacity = 1,
#                label = ~lapply(label, HTML),
#                #popup = leafpop::popupImage(plaatjes(), src = "local", embed = TRUE),
#                options = arrowheadOptions(size = "15px",frequency = "endonly"))
