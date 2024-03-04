Sys.setenv(LANG = "en")

library(tidyverse)
library(lubridate)
library(readxl)
library(stringi)

dataincl2020 <- readRDS("data/dataincl2020_final_attacks.rds")
  
#longformat df to have only two variables for analysis
G_analysis2 <- dataincl2020 %>%
  select(-decision) %>% # deleting the decision on the case level, now needed only on norm level
  gather(number, decision, starts_with("decision")) %>%
  select(number, geschaeftszahl, decision, link, plaintiff, policyarea, plaintiff_category, type, gov_brief, length, oralhearing, entscheidung, catchwords, index, type, id_decision, contains("pr"), verkündung, verhandlung, court_president, session_id, start_break, start_session, end_break, end_session, contains("attack"), covid) %>%
  mutate(number = str_remove(number, "decision")) 

G_analysis3 <- dataincl2020 %>% 
  gather(number, invalidated, starts_with("invalidated")) %>% 
  select(number, invalidated, link) %>%
  mutate(number = str_remove(number, "invalidated"))

G_analysis <- dataincl2020 %>% 
  gather(number, norm, starts_with("norm")) %>% 
  select(number, norm, link, date) %>%
  mutate(number = str_remove(number, "norm")) %>%
  full_join(G_analysis2, by = c("link", "number")) %>%
  full_join(G_analysis3, by = c("link", "number"))

rm(G_analysis2, G_analysis3)
#---------------------------------------------------------------------------
# make legislature dataframe

#importing the dataset on governments
Governments <- read_excel("manual/Governments.xlsx") %>%
  mutate(Regierung = str_squish(Regierung)) %>%
  rename(government = Regierung) %>%
  mutate(across(
    c("Antritt", "Enthebung", "Abtritt"), 
    ~str_replace_all(., c("\\sJän(\\.|)\\s" = "01.", 
                          "\\sFeb\\.\\s" = "02.",
                          "\\sMärz\\s" = "03.",
                          "\\sApr\\.\\s" = "04.",
                          "\\sMai\\s" = "05.",
                          "\\sJuni\\s" = "06.",
                          "\\sOkt\\.\\s" = "10.",
                          "\\sNov\\.\\s" = "11.",
                          "\\sDez\\.\\s" = "12.")))) %>%
  mutate(across(
    c("Antritt", "Enthebung", "Abtritt"),
    ~dmy(.))) %>%
  mutate(Enthebung = if_else(government == "Nehammer", today(), Enthebung)) %>%
  mutate(inoffice = interval(ymd(Antritt), if_else(is.na(Abtritt), ymd(Enthebung), ymd(Abtritt))))

saveRDS(Governments, "data/governments.rds")

#defining governments + total number of laws per government
G_analysis <- G_analysis %>%
  mutate(norm = na_if(norm, "NA")) %>%
  filter(!is.na(norm)) %>% #deleting all cases in which I couldn't identify the norm
  mutate(year = str_extract(norm, "\\d{4}")) %>%
  mutate(bgb = case_when(
    str_detect(norm, "\\b\\d{1,3}\\/") ~ str_extract(norm, "\\b\\d{1,3}\\/"),
    str_detect(norm, "\\/\\d{1,3}\\b") ~ str_extract(norm, "\\/\\d{1,3}\\b"),
    TRUE ~ NA_character_
  )) %>% 
  mutate(bgb = str_remove(bgb, "\\/")) %>%
  mutate(year = as.numeric(year)) %>%
  # define governments via year of law and bgb number if there were multiple governments in one year
  mutate(government = case_when(
    year > 1980 & year < 1983 ~ "Kreisky IV",
    year == 1980 & bgb > 26 ~ "Kreisky IV",
    year == 1983 & bgb < 18 ~ "Kreisky IV",
    year == 1983 & bgb > 17 ~ "Sinowatz",
    year > 1983 & year < 1986 ~ "Sinowatz",
    year == 1986 & bgb < 26 ~ "Sinowatz",
    year == 1986 & bgb > 25 ~ "Vranitzky I",
    year == 1987 & bgb < 6 ~ "Vranitzky I",
    year == 1987 & bgb > 5 ~ "Vranitzky II",
    year > 1987 & year < 1990 ~ "Vranitzky II",
    year == 1990 & bgb < 69 ~ "Vranitzky II",
    year == 1990 & bgb > 68 ~ "Vranitzky III",
    year > 1990 & year < 1994 ~ "Vranitzky III",
    year == 1994 & bgb < 84 ~ "Vranitzky III",
    year == 1994 & bgb > 83 ~ "Vranitzky IV",
    year == 1995 ~ "Vranitzky IV",
    year == 1996 & bgb < 15 ~ "Vranitzky IV",
    year == 1996 & bgb > 14 ~ "Vranitzky V",
    year == 1997 & bgb < 5 ~ "Vranitzky V",
    year == 1997 & bgb > 4 ~ "Klima",
    year > 1997 & year < 2000 ~ "Klima",
    year == 2000 & bgb < 6 ~ "Klima",
    year == 2000 & bgb > 5 ~ "Schuessel I",
    year > 2000 & year < 2003 ~ "Schuessel I",
    year == 2003 & bgb < 9 ~ "Schuessel I",
    year == 2003 & bgb > 8 ~ "Schuessel II",
    year > 2003 & year < 2007 ~ "Schuessel II",
    year == 2007 & bgb < 2 ~ "Schuessel II",
    year == 2007 & bgb > 1 ~ "Gusenbauer",
    year == 2008 & bgb < 55 ~ "Gusenbauer",
    year == 2008 & bgb > 54 ~ "Faymann I",
    year > 2008 & year < 2013 ~ "Faymann I",
    year == 2013 & bgb < 73 ~ "Faymann I",
    year == 2013 & bgb > 72 ~ "Faymann II",
    year > 2013 & year < 2016 ~ "Faymann II",
    year == 2016 & bgb < 24 ~ "Faymann II",
    year == 2016 & bgb > 23 ~ "Faymann II",
    year == 2017 & bgb < 79 ~ "Kern",
    year == 2017 & bgb > 78 ~ "Kurz I",
    year == 2018 ~ "Kurz I",
    year == 2019 & bgb < 21 ~ "Kurz I",
    year == 2019 & bgb > 20 ~ "Bierlein",
    year == 2020 ~ "Kurz II",
    year == 2021 & bgb < 182 ~ "Kurz II",
    year == 2021 & bgb > 181 ~ "Schallenberg",
    year == 2021 & bgb > 197 ~ "Nehammer",
    TRUE ~ "NA"
  )) %>%
  # define how many laws each government passed in total
  mutate(lawstotal = case_when(
    government == "Kreisky IV" ~ 463,
    government == "Sinowatz" ~ 297,
    government == "Vranitzky I" ~ 53,
    government == "Vranitzky II" ~ 536,
    government == "Vranitzky III" ~ 627,
    government == "Vranitzky IV" ~ 123,
    government == "Vranitzky V" ~ 152,
    government == "Klima" ~ 450,
    government == "Schuessel I" ~ 386,
    government == "Schuessel II" ~ 531,
    government == "Gusenbauer" ~ 224,
    government == "Faymann I" ~ 645,
    government == "Faymann II" ~ 240,
    government == "Kern" ~ 228,
    government == "Kurz I" ~ 142,
    government == "Bierlein" ~ 62,
    government == "Kurz II" ~ 322,
    government == "Schallenberg" ~ 17,
    government == "Nehammer" ~ 61,
    TRUE ~ NA_real_
  ))%>%
  # interval of time in office
  left_join(Governments %>% select(government, inoffice), by = "government") %>%
  mutate(coalition = case_when(
    government == "Kreisky IV"  ~ "SPO",
    government == "Sinowatz" | government == "Vranitzky I"   ~ "SPO-FPO",
    government == "Vranitzky II" | government == "Vranitzky III" | government == "Vranitzky IV" | government == "Vranitzky V" | government == "Klima"  ~ "SPO-OVP",
    government == "Schuessel I" | government == "Schuessel II" ~ "OVP-FPO",
    government == "Gusenbauer" | government == "Faymann I" | government == "Faymann II" | government == "Kern"   ~ "SPO-OVP 2",
    government == "Kurz I" ~ "OVP-FPO 2",
    government == "Kurz II|Schallenberg|Nehammer" ~ "OVP-G")) %>%
  mutate(date_2 = case_when(
    str_detect(date, "\\d\\d\\.") ~ dmy(date),
    str_detect(date, "\\d{4}") ~ ymd(date)
  )) %>% 
  #define whether populists were in power when decision was taken
  mutate(pop_power = ifelse(date_2 %within% interval(ymd("2017-12-18"), ymd("2019-05-28")) | date_2 %within% interval(ymd("2000-02-04"), ymd("2007-01-11")), 1, 0)) %>%
  #define whether a government brief was written by the same government that issued the law
  mutate(gov_brief_by = ifelse(gov_brief == 1, ifelse(date_2 %within% inoffice, 1, 0), NA)) %>%
  mutate(populist = case_when(
    str_detect(government, "Kurz I\\b|Sinowatz|Vranitzky I\\b|Schuessel (I\\b|II\\b)") ~ "populist",
    TRUE ~ "nonpopulist")) %>%
  #populistvoteshare at time of review (includes both FPÖ and BZÖ)
  mutate(populistvoteshare = case_when(
    date_2 %within% interval(ymd("1979-06-05"), ymd("1983-04-23")) ~ 6.1,
    date_2 %within% interval(ymd("1983-04-24"), ymd("1986-11-22")) ~ 4.9,
    date_2 %within% interval(ymd("1986-11-23"), ymd("1990-10-06")) ~ 9.7,
    date_2 %within% interval(ymd("1990-10-07"), ymd("1994-10-08")) ~ 16.6,
    date_2 %within% interval(ymd("1994-10-09"), ymd("1995-12-17")) ~ 22.5,
    date_2 %within% interval(ymd("1995-12-18"), ymd("1999-10-02")) ~ 21.9,
    date_2 %within% interval(ymd("1999-10-03"), ymd("2002-11-23")) ~ 26.9,
    date_2 %within% interval(ymd("2002-11-24"), ymd("2006-09-30")) ~ 10,
    date_2 %within% interval(ymd("2006-10-01"), ymd("2008-09-27")) ~ 15.2,
    date_2 %within% interval(ymd("2008-09-28"), ymd("2013-09-28")) ~ 28.2,
    date_2 %within% interval(ymd("2013-09-29"), ymd("2017-10-14")) ~ 24,
    date_2 %within% interval(ymd("2017-10-15"), ymd("2019-09-28")) ~ 26,
    date_2 %within% interval(ymd("2019-09-29"), ymd(today()))  ~ 16.2,
    TRUE ~ NA_real_)) %>%
  #share of departments occupied by populists
  mutate(populistpart = as.numeric(case_when(
    government == "Sinowatz" ~ 21.43,
    government == "Vranitzky I" ~ 20.00,
    government == "Schuessel I" ~ 45.45,
    government == "Schuessel II" ~ 25.00,
    government == "Kurz I" ~ 41.67,
    government == "NA" ~ NA_real_,
    is.na(government) ~ NA_real_,
    TRUE ~ 0
  ))) %>%
  #this is from a very early version where we defined departments via policyarea
  mutate(populistministry = as.integer(case_when(
    str_detect(government, "Sinowatz|Vranitzky I\\b") & str_detect(policyarea,"Economy|Judicature") ~ 1,
    str_detect(government, "Schuessel I\\b") & str_detect(policyarea, "Judicature|Finances|Social|Family|Employment|Infrastructure") ~ 1,
    str_detect(government, "Schuessel II\\b") & str_detect(policyarea, "Judicature|Social|Family|Employment|Infrastructure")  ~ 1,
    str_detect(government, "Kurz I\\b") & str_detect(policyarea, "Security|Employment|Social|Infrastructure|Migration") ~ 1,
    government == "NA" ~ NA_real_,
    TRUE ~ 0
  ))) %>%
  #define populist dummy variable
  mutate(populistdummy = as.integer(case_when(
    government == "Sinowatz" ~ 1,
    government == "Vranitzky I" ~ 1,
    government == "Schuessel I" ~ 1,
    government == "Schuessel II" ~ 1,
    government == "Kurz I" ~ 1,
    government == "NA" ~ NA_real_,
    is.na(government) ~ NA_real_,
    TRUE ~ 0
  ))) %>%
  #which governments had a two-thirds majority
  mutate(twothirds = as.integer(case_when(
    str_detect(government, "Vranitzky II\\b|Vranitzky III\\b|Vranitzky V\\b|Klima|Gusenbauer") ~ 1,
    str_detect(government, "Kreisky IV\\b|Sinowatz|Vranitzky I\\b|Vranitzky IV\\b|Schuessel I\\b|Schuessel II\\b|Faymann|Kern|Kurz") ~ 0,
    TRUE ~ NA_real_
  )))

#vereinheitlichen der norm----

G_analysis <- G_analysis %>%
  mutate(normnumber = case_when(
    year > 1979 ~ str_remove(norm, "19[8|9][0-9]|20[0-2][0-9]"),
    year < 1980 ~ NA_character_
    )) %>% 
  mutate(normnumber = str_remove_all(normnumber, "[:punct:]|bgb")) %>%
  mutate(normnumber = str_squish(normnumber)) %>% 
  mutate(normnumber = na_if(normnumber, "NA")) %>%
  mutate(normnumber = na_if(normnumber, "")) %>%
  mutate(norm = ifelse(!is.na(normnumber) & !is.na(year),  paste(normnumber, year, sep = "/"), NA))

# merge with laws

laws <- readRDS("data/laws/laws.rds")

laws %>% 
  filter(pop_department == 1) %>%
  pull(norm) ->
  pop_dep

laws %>% 
  filter(pop_origin == 1) %>%
  pull(norm) ->
  pop_norm

G_analysis %>% 
  mutate(pop_origin = ifelse(norm %in% pop_norm, 1, 0)) %>%
  mutate(pop_department = ifelse(norm %in% pop_dep, 1, 0)) ->
  G_analysis

saveRDS(G_analysis, "data/G_analysis.rds")

G_analysis %>%
  select(-number, -link, -plaintiff, -length, -oralhearing, -entscheidung, -catchwords, -index, -invalidated, -bgb, -pop_power, -populistministry) ->
  vfgh_decisions

saveRDS(vfgh_decisions, "publication/vfgh_decisions.rds")

