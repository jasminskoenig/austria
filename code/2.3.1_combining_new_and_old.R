library(tidyverse)

G_sprueche <- readRDS("data/G_sprueche4.rds")
data2020 <- readRDS("data/data2020final.rds")

#binding with old data
dataincl2020 <- G_sprueche %>%
  select(-antrag_shortened, -sortout) %>%
  bind_rows(data2020) 

saveRDS(dataincl2020, "data/dataincl2020_final.rds")
