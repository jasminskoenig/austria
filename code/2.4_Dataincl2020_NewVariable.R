Sys.setenv(LANG = "en")

library(tidyverse)
library(stringi)
library(readxl)
library(lubridate)
library(googlesheets4)

gs4_auth(cache = ".secrets", email = "j.s.koenig95@gmail.com")

# in some cases the norm wasn't extracted, let's check those cases

dataincl2020 <-  readRDS("data/dataincl2020_final.rds") 

dataincl2020 %>%
  filter(is.na(norm1))  ->
  norm1_na
  
norm1_na %>% 
  select(link, norm1, geschaeftszahl) %>%
  write_tsv("manual/norm1", na = "")

#lapply(norm1_na$link,function(x) browseURL(as.character(x)))

norm1_na_manual <- read_excel("manual/norm1na_manual.xlsx")

norm1_na %>%
  select(-norm1, -geschaeftszahl, -norm2) %>% 
  left_join(norm1_na_manual, by = "link") ->
  norm1_na

dataincl2020 %>%
  filter(!is.na(norm1))  ->
  norm1_notna

norm1_notna %>%
  bind_rows(norm1_na) ->
  dataincl2020

# TO GET RESULTS FROM PUBLISHED VERSION RUN THIS CODE
# dataincl2020 <-  readRDS("data/dataincl2020_final.rds") 

#do governments issue a government brief?

dataincl2020 <- dataincl2020 %>%
  mutate(govbrief = str_remove_all(entscheidung, "(?<=\\d)\\.")) %>%
  mutate(govbrief = str_extract_all(govbrief, "[^.!?;]*(bundesregierung)[^.!?;]*")) %>% 
  mutate(govbrief = str_squish(govbrief)) %>%
  mutate(gov_brief = case_when(
    str_detect(govbrief, "(?<=keine\\s)aeusserung") ~ 0,
    str_detect(govbrief, "von der erstattung einer meritorischen aeusserung abstand zu nehmen|verzichtete auf die abgabe einer meriotrischen aeusserung|von einer aeusserung in der sache (abgesehen|abesehe|abstand nehme)|von der erstattung einer aeusserung abstand") ~ 0,    
    str_detect(govbrief, "beantragt die bundesregierung primaer die zurueckweisung des gesetzespruefungsantrags|die bundesregierung haelt in ihrer aeusserung|tritt in der sache saemtlichen antraegen entgegen|die bundesregierung in ihrer aeusserung|erhob die bundesregierung einspruch|bestreitet die zulaessigkeit der antraege|die von der bundesregierung vorgelegten|die aeusserung der bundesregierung|in der sache fuehrt die bundesregierung|in der sache selbst haelt die bundesregierung die antraege fuer nicht begruendet|wie die bundesregierung richtig darlegt|die bundesregierung tritt den bedenken|der von der bundesregierung vorgetragene einwand|die bundesregierung stellt in einer aeusserung") ~ 1,    
    str_detect(govbrief, "^(?=.*(verteidigt))(?=.*(verfassungsmaessigkeit)).*") ~ 1,
    str_detect(govbrief, "^(?=.*(aeusserung|gegenschrift|stellungsnahme|antrag))(?=.*(erstattet|abgegeben|beantragt|entgegen|aeussert|begehrt|bestreitet|schildert|nimmt stellung|nimmt wie folgt stellung|trat|gab)).*") ~ 1,
    str_detect(govbrief, "^(?=.*(aeusserung))(?=.*(abstand|abgesehen)).*") ~ 0,
    TRUE ~ 0
  )) %>% 
  select(-govbrief)

#oral hearing -------------------------------------------------------

#usually the last sentence in a decision states whether 
#okay hier muss geprüft werden, ob die zuordnung so stimmt
dataincl2020 <- dataincl2020 %>%
  mutate(end = str_extract(entscheidung, "(\\.[^\\.]*){5}\\.$")) %>% 
  mutate(length = as.numeric(length)) %>%
  mutate(oralhearing = case_when(
    str_detect(end, "nichtoeffentlich|19 abs(3|4)|ohne muendliche|ohne vorangegangene muendliche|ohne durchfuehrung einer muendlichen verhandlung|ohne weiteres verfahren") ~ 0,
    str_detect(end, ".*(?=muendliche(|n) verhandlung)(?=.*abgesehen|entbehrlich).*") ~ 0,
    str_detect(entscheidung, "nichtoeffentlich") ~ 3,
    TRUE ~ 2
  )) %>% 
  mutate(oralhearing = case_when(
    length > 10000 & oralhearing == 2 ~ 1,
    oralhearing == 2 & length < 6000 ~ 0,
    str_detect(decision, "sustained") & oralhearing == 2 ~ 1,
    oralhearing == 0 ~ 0,
    str_detect(entscheidung, "ueber die moeglichkeit einer nichtoeffentlichen urteilsverkuendung im strafprozess") ~ 1, #Spezialfall, der durch den restlichen Code nicht abgedeckt wird
    oralhearing == 3 ~ 0,
    TRUE ~ NA_real_
  ))

# leftover cleaning ----

dataincl2020 <- dataincl2020 %>% 
  mutate_at(vars(contains('norm')), ~na_if(., "NA")) %>%
  distinct(link, .keep_all = TRUE)

# invalidated text -----------------

dataincl2020 %>%
  filter(decision == "sustained") %>%
  select(link, spruch, geschaeftszahl, contains("norm")) %>%
  mutate(spruch_i = str_remove(spruch, "ii\\..*")) %>%
  mutate(spruch_quote = str_extract_all(spruch_i, "\\p{quotation mark}.+?\\p{quotation mark}")) %>% 
  rowwise() %>%
  mutate(spruch_quote = paste(spruch_quote, collapse = ', ')) %>%
  mutate(spruch_w_quote = str_remove_all(spruch_i, "\\p{quotation mark}.+?\\p{quotation mark}")) %>%
  mutate(invalidated_par = str_extract_all(spruch_w_quote, "\\§\\d{1,3}(\\w|)((\\s+\\w+|)(\\s+\\w{1,3}\\d{1,3}|)(\\s+\\w+\\d+|)|)")) %>%
  mutate(invalidated = case_when(
    str_detect(spruch_w_quote, "(?<!i(n|m)) \\§") ~ str_extract_all(spruch_w_quote, "(?<!i(n|m)) \\§\\d{1,3}(\\w|)((\\s+\\w+|)(\\s+\\w{1,3}\\d{1,3}|)(\\s+\\w+\\d+|)|)"),
    !str_detect(spruch_w_quote, "(?<!i(n|m)) \\§") ~ str_extract_all(spruch_quote, ".*")
  )) %>% 
  mutate_at(vars(contains('invalidated')), ~paste(., collapse = ', ')) %>%
  mutate_at(vars(contains('invalidated')), ~str_remove_all(., '^c\\(|\\)')) %>%
  mutate_at(vars(contains('invalidated')), ~str_squish(.)) %>%
  mutate_at(vars(contains('spruch_')), ~na_if(., "")) %>%
  mutate_at(vars(contains('invalidated')), ~na_if(., "")) %>%
  ungroup() %>% 
  mutate(invalidated = ifelse(!is.na(spruch_quote) & spruch_quote != invalidated, ifelse(invalidated != invalidated_par & !is.na(invalidated_par), invalidated, spruch_quote), invalidated)) %>%
  mutate(invalidated = ifelse(is.na(invalidated), invalidated_par, invalidated)) %>%
  select(link, invalidated, invalidated_par, spruch_quote, geschaeftszahl) ->
  dataincl2020_inv

# again some cases must be coded manually

dataincl2020_inv %>%
  select(link, invalidated, geschaeftszahl) %>%
  filter(str_detect(invalidated, "\\§")) %>%
  write_tsv("manual/invalidation", na = "")

dataincl2020_inv_man <- read_excel("manual/invalidation_filled.xlsx") %>%
  select(-geschaeftszahl)

dataincl2020_inv %>%
  anti_join(dataincl2020_inv_man, by = "link") %>%
  select(link, invalidated) %>%
  bind_rows(dataincl2020_inv_man) ->
  dataincl2020_inv

dataincl2020_inv %>%
  left_join(dataincl2020, by = "link") %>%
  select(link, contains("norm"), invalidated) %>%
  filter(!is.na(norm2)) %>%
  filter(!is.na(invalidated)) %>% 
  mutate(invalidated = str_squish(invalidated)) %>%
  write_tsv("manual/invalidation_multiple")

dataincl2020_inv_man_mul <- read_excel("manual/invalidation_multiple_filled.xlsx") %>%
  select(-contains("norm"))

dataincl2020_inv %>%
  rename(invalidated1 = invalidated) %>%
  anti_join(dataincl2020_inv_man_mul, by = "link") %>%
  bind_rows(dataincl2020_inv_man_mul) %>% 
  mutate_at(vars(contains("invalidated")), ~str_remove_all(., "[:punct:]")) %>%
  mutate_at(vars(contains("invalidated")), ~str_squish(.)) ->
  dataincl2020_inv

dataincl2020 %>%
  left_join(dataincl2020_inv, by = "link") %>% 
  distinct(link, .keep_all = TRUE) %>% # due to an error before there's a duplicate in the manual data
  rowid_to_column("id_decision") ->
  dataincl2020

# add info on vfgh president
pres <- read_sheet("https://docs.google.com/spreadsheets/d/1m8-y_UbyuTEJ0fr8Ooy7u-b79Ai3UscGPAwn-XqDgks/edit#gid=0") |> 
  mutate(reign = interval(ymd(start), ymd(end)))

dataincl2020 |> 
  mutate(date_2 = dmy(date),
         date_2 = if_else(is.na(date_2), ymd(date), date_2)) |>
  select(-date2) |> 
  mutate(court_president = case_when(
    date_2 %within% pres$reign[1] ~ pres$president[1],
    date_2 %within% pres$reign[2] ~ pres$president[2],
    date_2 %within% pres$reign[3] ~ pres$president[3],
    date_2 %within% pres$reign[4] ~ pres$president[4],
    date_2 %within% pres$reign[5] ~ pres$president[5],
    date_2 %within% pres$reign[6] ~ pres$president[6],
  )) ->
  dataincl2020

dataincl2020 |> 
  mutate(covid = case_when(
    str_detect(entscheidung, "covid") ~ 1,
    str_detect(catchwords, "covid") ~ 1,
    str_detect(spruch, "covid") ~ 1,
    TRUE ~ 0
  )) ->
  dataincl2020

# save data ----

saveRDS(dataincl2020, "data/dataincl2020_final2.rds")


