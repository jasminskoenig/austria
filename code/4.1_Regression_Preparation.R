Sys.setenv(LANG = "en")
library(dplyr)
library(tidyverse)
library(lubridate)

G_analysis <- readRDS("data/G_analysis.rds")
#tranforming variables to integer and adding information on populist shares of ministries

#df for regression analysis
G_analysis_R <- G_analysis %>%
  mutate(populist = as.integer(ifelse(populist == "populist", 1, 0))) %>%
  mutate(finances = as.integer(ifelse(policyarea == "Finances", 1, 0))) %>%
  mutate(security = as.integer(ifelse(policyarea == "Security", 1, 0))) %>%
  mutate(judicature = as.integer(ifelse(policyarea == "Judicature" ,1, 0))) %>%
  mutate(migration = as.integer(ifelse(policyarea == "Migration", 1, 0))) %>%
  mutate(mobility = as.integer(ifelse(policyarea == "Mobility", 1, 0))) %>%
  mutate(economy = as.integer(ifelse(policyarea == "Economy", 1, 0))) %>%
  mutate(taxation = as.integer(ifelse(policyarea == "Taxation", 1, 0))) %>%
  mutate(social = as.integer(ifelse(policyarea == "Social", 1, 0))) %>%
  mutate(decision = as.integer(ifelse(decision == "sustained", 1, 0))) %>%
  mutate(court = as.integer(ifelse(plaintiff_category == "Court", 1, 0))) %>%
  mutate(private = as.integer(ifelse(plaintiff_category == "Person", 1, 0))) %>%
  mutate(politician = as.integer(ifelse(plaintiff_category == "Politician", 1, 0))) %>%
  mutate(erkenntnis = as.integer(ifelse(type == "erkenntnis", 1, 0))) %>%
  mutate(beschluss = as.integer(ifelse(type == "beschluss", 1, 0))) %>%
  mutate(length = as.numeric(length)) %>%
  mutate(invalidated_length = str_count(invalidated)) %>%
  mutate(date = year(date_2)) %>%
  filter(norm != "NA") %>%
  select(-number, -plaintiff, -populist, -entscheidung, -catchwords, -index, -normnumber, -bgb, -policyarea, -invalidated, -normnumber) |> 
  mutate(kurz_dummy = if_else(government == "Kurz I", 1, 0))

#This is the document in our dropbox
saveRDS(G_analysis_R, "data/G_analysis_R.rds")

#Dataset for publication

G_analysis_R %>%
 select(-link, -type, -gov_brief, -length, -oralhearing, -pop_power, -gov_brief_by, -pop_department, -invalidated_length) ->
  vfgh_decisions_r

saveRDS(vfgh_decisions_r, "publication/vfgh_decisions_r.RDS")
