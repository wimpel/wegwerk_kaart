library(xml2)
library(tidyverse)

doc <- xml2::read_xml("../wegwerk/data/wegwerkzaamheden.xml")

nodes <- xml_find_all(doc, "//d1:situation")

df <- data.frame(situation = xml2::xml_attr(nodes, "id") ,
                 hindrance = xml2::xml_find_first(nodes, ".//d1:roadworkHindranceClass") %>% xml_text())

df.filter <- df %>%
  mutate(klasse = as.numeric(gsub(".*(.)", "\\1", hindrance))) %>%
  filter(klasse >= 3)
  
