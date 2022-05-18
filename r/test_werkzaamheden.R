#maak een dummy set met werkzaamheden

df <- readRDS("./output/routes.rds")
werkzaamheden <- data.table(
  groep = unique(df$groep)[c(1,3)],
  van = seq(as.POSIXct("2022-01-01", tz = "Europe/Amsterdam"), by = "3 days", length.out = 2),
  tot = seq(as.POSIXct("2022-01-07", tz = "Europe/Amsterdam"), by = "7 days", length.out = 2)
)

saveRDS(werkzaamheden, "./output/werkzaamheden.rds")
