# 2.1 CLEANING LAWS 

# libraries

library(tidyverse)
library(lubridate)

laws <- readRDS("data/laws/laws_2004_complete.rds")

# clean departments
kurz <- interval(ymd("2017-12-18"), ymd("2019-05-22"))
schuesseli <- interval(ymd("2003-02-28"), ymd("2007-01-11"))
schuesselii <- interval(ymd("2000-02-04"), ymd("2003-02-28"))

laws %>%
  filter(initiated == "Regierungsvorlage") %>% 
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
  mutate(pop_department = case_when(
    str_detect(department, "bmoeds|sport") & date %within% kurz ~ 1,
    str_detect(department, "europa|integration") & date %within% kurz ~ 1,
    str_detect(department, "inneres|bmi") & date %within% kurz ~ 1,
    str_detect(department, "arbeit|sozial|gesundheit") & date %within% kurz ~ 1,
    str_detect(department, "landesverteidigung") & date %within% kurz ~ 1,
    str_detect(department, "verkehr|innovation|technologie") & date %within% kurz ~ 1,
    str_detect(department, "bmöls|sport") & date %within% schuesseli ~ 1,
    str_detect(department, "bmj") & date %within% schuesseli ~ 1,
    str_detect(department, "bmf\\b") & date %within% schuesseli ~ 1,
    str_detect(department, "bmsg") & date %within% schuesseli ~ 1,
    str_detect(department, "landesverteidigung|bmlv") & date %within% schuesseli ~ 1,
    str_detect(department, "verkehr|innovation|technologie") & date %within% schuesseli ~ 1,
    str_detect(department, "bmj") & date %within% schuesselii ~ 1,
    str_detect(department, "bmsk") & date %within% schuesselii ~ 1,
    str_detect(department, "verkehr|innovation|technologie") & date %within% schuesselii ~ 1,
    TRUE ~ 0
  )) ->
  laws_regierungsvorlage
