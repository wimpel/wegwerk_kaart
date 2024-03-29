#maak een dummy set met werkzaamheden
library(data.table)
df <- readRDS("./output/routes.rds")
# werkzaamheden <- data.table(
#   groep = unique(df$groep)[c(1,3)],
#   van = seq(as.POSIXct("2022-01-01", tz = "Europe/Amsterdam"), by = "3 days", length.out = 2),
#   tot = seq(as.POSIXct("2022-01-07", tz = "Europe/Amsterdam"), by = "7 days", length.out = 2)
# )
werkzaamheden <- data.table(
  groep = unique(df$groep)[c(1,3)],
  van = seq(as.Date("2022-01-01", tz = "Europe/Amsterdam"), by = "3 days", length.out = 2),
  van_uur = c("22:00", "20:00"),
  tot = seq(as.Date("2022-01-07", tz = "Europe/Amsterdam"), by = "7 days", length.out = 2),
  tot_uur = c("03:00", "06:00")
)

saveRDS(werkzaamheden, "./output/werkzaamheden.rds")
saveRDS(werkzaamheden, "./R/app/data/werkzaamheden.rds")
