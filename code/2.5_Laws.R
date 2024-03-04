Sys.setenv(LANG = "en")

# 2.1 CLEANING LAWS 

# libraries

library(tidyverse)
library(lubridate)
library(tidyverse)
library(readxl)
library(lubridate)

laws <- readRDS("data/laws/Laws_complete.rds")

# clean departments
kurz <- interval(ymd("2017-12-18"), ymd("2019-05-22"))
schuesseli <- interval(ymd("2003-02-28"), ymd("2007-01-11"))
schuesselii <- interval(ymd("2000-02-04"), ymd("2003-02-28"))
pop_parties <- c("FPO", "BZO")

laws %>%
  mutate(department = str_extract(department, ".*(bka|ministerium).*")) %>%
  mutate(department = ifelse(!is.na(department), department, case_when(
    str_detect(shorttitle, "Bundesfinanz(|ierungs)gesetz") | str_detect(law_id, "2021_I_(199|198|196|236)")  ~ "bmf",
    str_detect(law_id, "2021_I_229|2021_I_200") ~ "bmk",
    str_detect(law_id, "2021_I_210") ~ "bmsgpk",
    str_detect(law_id, "2021_I_206|2021_I_211") ~ "bmi",
    str_detect(law_id, "2021_I_202|2021_I_205") ~ "bka",
    str_detect(law_id, "2021_I_20") ~ "bmj"
  ))) %>%
  mutate(date = dmy(date)) %>%
  mutate(department = str_to_lower(str_squish(department))) %>%
  mutate(ausschuss = str_to_lower(str_squish(ausschuss))) %>%
  mutate(pop_department = case_when(
    initiated == "Regierungsvorlage" & str_detect(department, "bmoeds|sport") & date %within% kurz ~ 1,
    initiated == "Regierungsvorlage" & str_detect(department, "europa|integration") & date %within% kurz ~ 1,
    initiated == "Regierungsvorlage" & str_detect(department, "inneres|bmi") & date %within% kurz ~ 1,
    initiated == "Regierungsvorlage" & str_detect(department, "arbeit|sozial|gesundheit") & date %within% kurz ~ 1,
    initiated == "Regierungsvorlage" & str_detect(department, "landesverteidigung") & date %within% kurz ~ 1,
    initiated == "Regierungsvorlage" & str_detect(department, "verkehr|innovation|technologie") & date %within% kurz ~ 1,
    initiated == "Regierungsvorlage" & date %within% schuesseli & (str_detect(department, "bmöls|sport") | str_detect(ausschuss, "ausschuss für sportangelegenheiten|sport")) ~ 1,
    initiated == "Regierungsvorlage" & date %within% schuesseli & (str_detect(department, "bmj") | str_detect(ausschuss, "justiz")) ~ 1,
    initiated == "Regierungsvorlage" & date %within% schuesseli & (str_detect(department, "bmf\\b") | str_detect(ausschuss, "finanzausschuss"))  ~ 1,
    initiated == "Regierungsvorlage" & date %within% schuesseli & (str_detect(department, "bmsg") | str_detect(ausschuss, "gesundheit|soziale sicherheit und generationen")) ~ 1,
    initiated == "Regierungsvorlage" & date %within% schuesseli & (str_detect(department, "landesverteidigung|bmlv") | str_detect(ausschuss, "landesverteidigung")) ~ 1,
    initiated == "Regierungsvorlage" & date %within% schuesseli & (str_detect(department, "verkehr|innovation|technologie")| str_detect(ausschuss, "verkehr|innovation|technologie"))  ~ 1,
    initiated == "Regierungsvorlage" & str_detect(department, "bmj") & date %within% schuesselii ~ 1,
    initiated == "Regierungsvorlage" & str_detect(department, "bmsk") & date %within% schuesselii ~ 1,
    initiated == "Regierungsvorlage" & str_detect(department, "verkehr|innovation|technologie") & date %within% schuesselii ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(pop_legislator = case_when(
    initiated == "Initativantrag" & (date %within% kurz & party_1 %in% pop_parties | party_2 %in% pop_parties | party_3 %in% pop_parties| party_4 %in% pop_parties | party_5 %in% pop_parties | party_6 %in% pop_parties) ~ 1,
    initiated == "Initativantrag" & (date %within% schuesseli & party_1 %in% pop_parties | party_2 %in% pop_parties | party_3 %in% pop_parties| party_4 %in% pop_parties | party_5 %in% pop_parties | party_6 %in% pop_parties) ~ 1,
    initiated == "Initativantrag" & (date %within% schuesselii & party_1 %in% pop_parties | party_2 %in% pop_parties | party_3 %in% pop_parties| party_4 %in% pop_parties | party_5 %in% pop_parties | party_6 %in% pop_parties) ~ 1,
    TRUE ~ 0 
  )) %>%
  mutate(pop_origin = ifelse(pop_department == 1 | pop_legislator == 1, 1, 0)) ->
  laws

#cleaning ----
 laws %>%
  mutate_if(is.character, str_to_lower) %>%
  mutate(year = year(date)) %>%
  mutate(norm = str_remove_all(norm, "\\s|[:alpha:]|\\.")) %>%
  mutate(law_type = str_remove_all(law_type, "typ|\\s")) %>%
  mutate(law_id = str_remove_all(law_id, "dokumentnummer")) %>%
  mutate(name = str_remove_all(name, "titel")) %>%
  mutate(shorttitle = str_remove_all(shorttitle, "kurztitel")) %>%
  mutate(department = str_remove_all(department, "einbringende stelle")) %>%
  mutate_if(is.character, str_squish) %>%
  mutate(populism_exact = case_when(
    date < "1983-04-17" ~ "Non-Populist",
    date > "1983-04-16" & date < "1986-11-26" ~ "Populist",
    date > "1986-11-25" & date < "2000-02-05" ~ "Non-Populist",
    date > "2000-02-04" & date < "2007-01-12" ~ "Populist",
    date > "2007-01-11" & date < "2016-05-18" ~ "Non-Populist",
    date > "2016-05-17" & date < "2019-05-29" ~ "Populist",
    date > "2019-05-28" ~ "Non-Populist",
    TRUE ~ NA_character_
  )) %>%
  mutate(populism_rough = case_when(
    year < 1983 ~ "Non-Populist",
    year > 1982 & year < 1987 ~ "Populist",
    year > 1986 & year < 2000 ~ "Non-Populist",
    year > 1999 & year < 2007 ~ "Populist",
    year > 2006 & year < 2018 ~ "Non-Populist",
    year == 2018 ~ "Populist",
    year > 2018 ~ "Non-Populist",
    TRUE ~ NA_character_
  )) ->
  laws

saveRDS(laws, "data/laws/laws.rds")



  