Sys.setenv(LANG = "en")
library(tidyverse)
library(stringi)
library(magrittr)
library(readxl)
library(tm)
library(tidytext)

#loading the scraped data
G_sprueche <- readRDS("data/raw/G_sprueche.rds") #contains the sentence
G_entscheidung <- readRDS("data/raw/G_entscheidung_data.rds") #contains the judgement

#Basic cleaning G_sprueche
G_sprueche <- G_sprueche %>%
  mutate_if(is.character, str_to_lower) %>%
  mutate(geschaeftszahl = str_remove(geschaeftszahl, ".*zahl")) %>%
  mutate(spruch = stringi::stri_replace_all_fixed(spruch, c("ü","ä", "ö", "ß"), c("ue", "ae", "oe", "ss"), vectorize_all = FALSE)) %>%
  mutate(spruch = str_replace_all(spruch, "entscheidungsgruende.*$", "")) %>% #deleting the verdict, in cases which are formated wrongly online
  mutate(spruch = str_replace_all(spruch, "(bgb((l|i|I|)(\\.|)(\\s|)((I|i|)(\\s|)(nr|)(\\.|)(\\s|)))*)", "bgb")) %>% #uniforming the spelling of bgb; problem: whitespace is not uniform yet  mutate(spruch = str_replace_all(spruch, c("in der fassung" = "idf", "i\\.d\\.f\\." = "idf"))) 
  mutate(spruch = str_replace_all(spruch, c("in der fassung" = "idf", "i\\.d\\.f\\." = "idf"))) %>%
  mutate(catchwords = str_remove(catchwords, "schlagworte")) %>%
  mutate(catchwords = stringi::stri_replace_all_fixed(catchwords, c("ü","ä", "ö", "ß"), c("ue", "ae", "oe", "ss"), vectorize_all = FALSE)) %>%
  mutate_if(is.character, str_squish) %>%
  rename(applied = applied_norms) 

#Basic cleaning G_entscheidung
G_entscheidung <- G_entscheidung %>%
  mutate_if(is.character, str_to_lower)%>%
  mutate(entscheidung = stringi::stri_replace_all_fixed(entscheidung, c("ü","ä", "ö", "ß"), c("ue", "ae", "oe", "ss"), vectorize_all = FALSE)) %>%
  mutate(entscheidung = str_replace_all(entscheidung, "(bgb((l|i|)(\\.|)(\\s|)((I|i|)(\\s|)(nr|)(\\.|)(\\s|)))*)", "bgb")) %>% #uniforming the spelling of bgb; problem: whitespace is not uniform yet  mutate(entscheidung = str_replace_all(entscheidung, c("in der fassung" = "idf", "i\\.d\\.f\\." = "idf"))) %>%
  mutate(entscheidung = str_remove(entscheidung, "entscheidungsgruende")) %>%
  mutate(entscheidung = str_replace_all(entscheidung, c("in der fassung" = "idf", "i\\.d\\.f\\." = "idf"))) 


#shortening the verdict to the request - the easy version("antrag")
G_sprueche <- G_sprueche %>%
  left_join(G_entscheidung, by = "link") %>%
  mutate_if(is.character, str_squish) %>%
  mutate(antrag = str_replace_all(entscheidung, "ii\\..*$|ueber die antraege wurde erworgen.*$|ueber den antrag wurde erworgen.*$|zur begruendung fuehrte.*$|zur rechtslage.*$", "")) 

saveRDS(G_sprueche, "data/G_sprueche1.rds")

#--------------------------------------------------------------------
#filter for federal laws (sort out statelaw)

#spruch > antrag (has more authority)
G_sprueche <- G_sprueche %>%
  mutate(state = case_when(
    str_detect(spruch, "\\bbgb|bundeskanzler ist zur|\\babgb") & str_detect(spruch, "\\blgb|landesgesetz|landeshauptmann ist zur") ~ "contradictory",
    str_detect(spruch, "\\bbgb|bundeskanzler ist zur|\\babgb") & !str_detect(spruch, "\\blgb|landesgesetz|landeshauptmann ist zur") ~ "Bund",
    !str_detect(spruch, "\\bbgb|bundeskanzler ist zur|\\babgb") & str_detect(spruch, "\\blgb|landesgesetz|landeshauptmann ist zur") ~ "Land",
    str_detect(antrag, "\\bbgb|bundeskanzler ist zur|\\babgb") & str_detect(antrag, "\\blgb|landesgesetz|landeshauptmann ist zur") ~ "contradictory",
    str_detect(antrag, "\\bbgb|bundeskanzler ist zur|\\babgb") & !str_detect(antrag, "\\blgb|landesgesetz|landeshauptmann ist zur") ~ "Bund",
    !str_detect(antrag, "\\bbgb|bundeskanzler ist zur|\\babgb") & str_detect(antrag, "\\blgb|landesgesetz|landeshauptmann ist zur") ~ "Land",
    str_detect(spruch, "\\bmrg\\b|\\bstpo\\b|\\beuweg\\b|\\bvfgg\\b|\\bzpo\\b|\\bstgb\\b|\\brao\\b|\\bwehrg\\b|\\bgspg\\b|\\bgewo\\b|\\buwg\\b|\\basvg\\b|\\bgog\\b|\\bmschg\\b|\\basgg\\b|\\bfpg\\b|\\bjn\\b|\\beo\\b|\\bunistg\\b|\\bvwgg\\b|\\bstgg\\b|\\bkfg\\b|\\btabmg\\b|\\bgwg\\b|\\beiwog\\b|\\bestg\\b|\\beheg\\b") ~ "Bund",
    str_detect(antrag, "\\bmrg\\b|\\bstpo\\b|\\beuweg\\b|\\bvfgg\\b|\\bzpo\\b|\\bstgb\\b|\\brao\\b|\\bwehrg\\b|\\bgspg\\b|\\bgewo\\b|\\buwg\\b|\\basvg\\b|\\bgog\\b|\\bmschg\\b|\\basgg\\b|\\bfpg\\b|\\bjn\\b|\\beo\\b|\\bunistg\\b|\\bvwgg\\b|\\bstgg\\b|\\bkfg\\b|\\btabmg\\b|\\bgwg\\b|\\beiwog\\b|\\bestg\\b|\\beheg\\b") ~ "Bund",
    TRUE ~ "other"
  )) 

#2214 Bund, 665 Land, 123 contain buzzwords for both state
#levels, 92 cannot be coded
G_sprueche %>%
  dplyr::count(state)

#cases that cannot be coded automatically are exported
#for manual coding
G_sprueche %>%
  filter(state == "other"|state == "contradictory") %T>%
  write_tsv("manual/g_sprueche_m2.tsv") 

#importing manually coded data
Statelevel_manual <- read_excel("manual/state_level_manual2.xls") %>%
  mutate(date = as.character(date))

#replacing the uncoded cases with manually coded cases
G_sprueche <- G_sprueche %>%
  filter(state == "Bund"|state == "Land") %>%
  bind_rows(Statelevel_manual)

#2307 Bund, 747 Land, 12 affect both levels, 19 do not 
#contain any information and 8 affect laws that have been decided before the replublic of austria was founded
G_sprueche %>%
  dplyr::count(state)

#save df on verdicts on provincial law
G_Land <- G_sprueche %>%
  filter(state == "Land")

saveRDS(G_Land, "data/G_sprueche_land.rds")

#sorting out provincial laws
G_sprueche <- G_sprueche %>%
  filter(state =="Bund"|state == "Beides")

rm(Statelevel_manual)

#----------------------------------------------------------------------

#sorting out vorlagen (based on art267) 
#sorting out renotifications
G_sprueche <- G_sprueche %>%
  mutate(sortout = case_when(
    str_detect(spruch, "gerichtshof der europaeischen union") ~ "1",
    str_detect(antrag, "wiederverlautbarung") ~ "2",
    str_detect(spruch, "wiederverlautbarung") ~ "3",
    TRUE ~ "0")) %>%
  filter(sortout == 0) %>%
  distinct(entscheidung, .keep_all = TRUE) 

#------------------------------------------------------------------------

#creating the variable decision
G_sprueche <- G_sprueche %>%
  mutate(decision = case_when(
    str_detect(spruch, "kundmachung|kundzumachen") & str_detect(spruch, "abgewiesen|zurueckgewiesen|abgelehnt|nicht (als |)verfassungswidrig|(keine|nicht) folge gegeben") ~ "partly sustained",
    str_detect(spruch, "kundmachung|kundzumachen") & !str_detect(spruch, "abgewiesen|zurueckgewiesen|abgelehnt|nicht (als |)verfassungswidrig|(keine|nicht) folge gegeben") ~ "sustained",
    !str_detect(spruch, "kundmachung|kundzumachen") & str_detect(spruch, "abgewiesen|zurueckgewiesen|abgelehnt|nicht (als |)verfassungswidrig|(keine|nicht) folge gegeben") ~ "overruled",
    !str_detect(spruch, "kundmachung|kundzumachen") & !str_detect(spruch, "abgewiesen|zurueckgewiesen|abgelehnt|nicht (als |)verfassungswidrig|(keine|nicht) folge gegeben") & str_detect(spruch, "eingestellt") ~ "termination",
    TRUE ~ "other"
  )) 

G_sprueche %>%
  filter(decision == "other") %>%
  select(link, decision) %T>%
  write_tsv("manual/g_sprueche_decision.tsv") 

G_sprueche_decision_other <- G_sprueche %>%
  filter(decision == "other") 

#importing manually coded data
g_sprueche_decision_m <- read_excel("manual/g_sprueche_decision.tsv.xlsx") %>%
  select(link, decision)

G_sprueche_decision_other <- G_sprueche_decision_other %>%
  select(-decision) %>%
  full_join(g_sprueche_decision_m, by="link") 


#replacing the uncoded cases with manually coded cases
G_sprueche <- G_sprueche %>%
  filter(decision != "other") %>%
  bind_rows(G_sprueche_decision_other) %>%
  filter(decision != "termination") %>%
  filter(decision != "vorlage") %>%
  filter(decision != "NA") 

G_sprueche %>%
  dplyr::count(decision)


rm(G_sprueche_decision_other)
rm(g_sprueche_decision_m)

#------------------------------------------------------

#extracting the norms
#shortening the entscheidung to antrag (more precise than before)
#then everything between quotation marks is deleted, since the norms cannot defined within a quote
#next the paragraphs including bgb norms are extracted
#paragraphs are deleted which include buzzwords for being part of the justification not the request (important since the judges refer to other norms in their justifications)
G_sprueche <- G_sprueche %>%
  mutate(antrag_shortened = str_remove_all(entscheidung, regex("(ueber (die|den) antr(a|ae)(g|ge) wurde erworgen|hintergrund der rechtssprechung|unter der ueberschrift|mit erkenntnis vom|der verfassungsgerichtshof hat erwogen|staendig(e|en) rechtssprechung|die fuer die berurteilung|zur begruendung fuehrte|zur rechtslage|die rechtslage stellt sich wie folgt da|zur frage der zulaessigkeit|(der|die) antr(a|ae)(g|ge) (ist|sind) (unz|z)ulaessig|sei daher (unz|z)ulaessig|die bundesregierung|zu den bedenken|entwicklung der rechtslage|zur gemeinsamen beratung und entscheidung|aeusserungen und stellungnahmen|rechtsquellen|normativen zusammenhang|angefochten(e|en) bestimmun(g|gen) (ist|sind) hervorgehoben|die kundmachung|lauten wie folgt|antragslegitimation).*", dotall = TRUE))) %>%  
  mutate(antrag_shortened = str_extract_all(antrag_shortened, ".*((bgb(|\\s|\\si\\s)[0-9]{1,3}+(| )\\/[0-9]{4}+|bgb(|\\s|\\si\\s)[0-9]{4}+(| )\\/[0-9]{1,3}+)|(\\d{4}\\,(\\s|)(bgb(|\\s|\\si\\s|i\\s|i\\si\\s))(\\d{1,3}))).*")) %>%
  mutate(antrag_shortened = str_remove_all(antrag_shortened, "(anm\\.|erkenntnis vfslg\\.|diese entscheidung konnte gemaess|im sinne|judikatur|haelt der verfassungsgerichtshof fest|staendige rechtssprechung|beschreitung des gerichtsweges|fristsetzung|antragslegitimation).*")) 
  #there's an error message in the last step, but the output looks correct

saveRDS(G_sprueche, "data/G_sprueche2.rds")

#i've realized by now this should be in a function, but it works and i am too lazy to work on this again
#for not sustained cases, norms can only be extracted from the request
G_sprueche <- readRDS("data/G_sprueche2.rds")
G_sprueche_notsustained <- G_sprueche %>%
  filter(decision == "overruled")

G_sprueche_notsustained <- str_match_all(G_sprueche_notsustained$antrag_shortened, "((bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{1,3}+(| )\\/[0-9]{4}+)(.*?)(idf|novelle)(.*?)(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{1,3}+(| )\\/[0-9]{4}+))|((bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{4}+(| )\\/[0-9]{1,3}+)(.*?)(idf|novelle)(.*?)(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{4}+(| )\\/[0-9]{1,3}+))|((idf|novelle)(.*?)(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{1,3}+(| )\\/[0-9]{4}+))|((idf|novelle)(.*?)(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{4}+(| )\\/[0-9]{1,3}+))|(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{1,3}+(| )\\/[0-9]{4}+)(.*?)(idf|novelle)|(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{4}+(| )\\/[0-9]{1,3}+)(.*?)(idf|novelle)|(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{1,3}+(| )\\/[0-9]{4}+)|(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{4}+(| )\\/[0-9]{1,3}+)|(\\d{4}\\,(\\s|)(bgb(|\\s|\\si\\s|i\\s|i\\si\\s))(\\d{1,3}\\s))") %>%
  setNames(G_sprueche_notsustained$link) 

G_sprueche_notsustained <- map_df(G_sprueche_notsustained, ~as.data.frame(.x), .id="id") 

G_sprueche_notsustained <- G_sprueche_notsustained %>%
  select(1, 10, 26, 45, 48, 20, 32, 51) 

G_sprueche_notsustained <- G_sprueche_notsustained %>%
  mutate(V50 = str_replace_all(V50, "\\,", "\\/")) %>%
  mutate(norms_all = as.character(paste(V9, V25, V44, V47, V19, V31, V50, sep = ","))) %>%
  select(id, norms_all) %>%
  mutate(norms_all = str_remove_all(norms_all, "NA\\,|NA")) %>% 
  mutate(norms_all = ifelse(str_detect(norms_all, "(\\,$|)"),
                            paste0(norms_all, ""),
                            paste0(norms_all, ","))) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(norms_all = paste(norms_all, collapse = "")) %>%
  rename(link = id)

#for sustained cases it is more precise to extract the norms from spruch
G_sprueche_sustained <- G_sprueche %>%
  filter(decision == "sustained")

G_sprueche_sustained <- str_match_all(G_sprueche_sustained$spruch, "((bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{1,3}+(| )\\/[0-9]{4}+)(.*?)(idf|novelle)(.*?)(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{1,3}+(| )\\/[0-9]{4}+))|((bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{4}+(| )\\/[0-9]{1,3}+)(.*?)(idf|novelle)(.*?)(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{4}+(| )\\/[0-9]{1,3}+))|((idf|novelle)(.*?)(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{1,3}+(| )\\/[0-9]{4}+))|((idf|novelle)(.*?)(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{4}+(| )\\/[0-9]{1,3}+))|(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{1,3}+(| )\\/[0-9]{4}+)(.*?)(idf|novelle)|(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{4}+(| )\\/[0-9]{1,3}+)(.*?)(idf|novelle)|(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{1,3}+(| )\\/[0-9]{4}+)|(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{4}+(| )\\/[0-9]{1,3}+)|(\\d{4}\\,(\\s|)(bgb(\\s|)\\d{1,3}\\s))") %>%
  setNames(G_sprueche_sustained$link) 

G_sprueche_sustained <- map_df(G_sprueche_sustained, ~as.data.frame(.x), .id="id") 

G_sprueche_sustained <- G_sprueche_sustained %>%
  select(id, V9, V25, V44, V47, V19, V31, V50) 

G_sprueche_sustained <- G_sprueche_sustained %>%
  mutate(V50 = str_replace_all(V50, "\\,", "\\/")) %>%
  mutate(norms_all = as.character(paste(V9, V25, V44, V47, V19, V31, V50, sep = ","))) %>%
  select(id, norms_all) %>%
  mutate(norms_all = str_remove_all(norms_all, "NA\\,|NA")) %>% 
  mutate(norms_all = ifelse(str_detect(norms_all, "(\\,$|)"),
                            paste0(norms_all, ""),
                            paste0(norms_all, ","))) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(norms_all = paste(norms_all, collapse = "")) %>%
  rename(link = id)

#partly sustained cases need to be checked by antrag and spruch
G_sprueche_partlysustained <- G_sprueche %>%
  filter(decision == "partly sustained") %>%
  mutate(antrag_shortened = as.character(paste(antrag_shortened, spruch, sep = " "))) 

G_sprueche_partlysustained <- str_match_all(G_sprueche_partlysustained$antrag_shortened, "((bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{1,3}+(| )\\/[0-9]{4}+)(.*?)(idf|novelle)(.*?)(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{1,3}+(| )\\/[0-9]{4}+))|((bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{4}+(| )\\/[0-9]{1,3}+)(.*?)(idf|novelle)(.*?)(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{4}+(| )\\/[0-9]{1,3}+))|((idf|novelle)(.*?)(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{1,3}+(| )\\/[0-9]{4}+))|((idf|novelle)(.*?)(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{4}+(| )\\/[0-9]{1,3}+))|(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{1,3}+(| )\\/[0-9]{4}+)(.*?)(idf|novelle)|(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{4}+(| )\\/[0-9]{1,3}+)(.*?)(idf|novelle)|(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{1,3}+(| )\\/[0-9]{4}+)|(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{4}+(| )\\/[0-9]{1,3}+)|(\\d{4}\\,(\\s|)(bgb(|\\s|\\si\\s|i\\s|i\\si\\s))(\\d{1,3}\\s))") %>%
  setNames(G_sprueche_partlysustained$link) 

G_sprueche_partlysustained <- map_df(G_sprueche_partlysustained, ~as.data.frame(.x), .id="id") 

G_sprueche_partlysustained <- G_sprueche_partlysustained %>%
  select(1, 10, 26, 45, 48, 20, 32, 51) 

G_sprueche_partlysustained <- G_sprueche_partlysustained %>%
  mutate(V50 = str_replace_all(V50, "\\,", "\\/")) %>%
  mutate(norms_all = as.character(paste(V9, V25, V44, V47, V19, V31, V50, sep = ","))) %>%
  select(id, norms_all) %>%
  mutate(norms_all = str_remove_all(norms_all, "NA\\,|NA")) %>% 
  mutate(norms_all = ifelse(str_detect(norms_all, "(\\,$|)"),
                            paste0(norms_all, ""),
                            paste0(norms_all, ","))) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(norms_all = paste(norms_all, collapse = "")) %>%
  rename(link = id)

#putting sustained, partly sustained and not sustained cases in one df
norms_both <- bind_rows(G_sprueche_notsustained, G_sprueche_sustained, G_sprueche_partlysustained)

#putting the norm variable into the sprueche df
G_sprueche <- G_sprueche %>%
  full_join(norms_both, by = "link") 

rm(G_sprueche_sustained)
rm(G_sprueche_notsustained)
rm(norms_both)

#some cases contain information but have not been read for some reason
G_sprueche_missing <- G_sprueche %>%
  filter(is.na(norms_all))

G_sprueche_missing <- str_match_all(G_sprueche_missing$antrag_shortened, "((bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{1,3}+(| )\\/[0-9]{4}+)(.*?)(idf|novelle)(.*?)(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{1,3}+(| )\\/[0-9]{4}+))|((bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{4}+(| )\\/[0-9]{1,3}+)(.*?)(idf|novelle)(.*?)(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{4}+(| )\\/[0-9]{1,3}+))|((idf|novelle)(.*?)(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{1,3}+(| )\\/[0-9]{4}+))|((idf|novelle)(.*?)(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{4}+(| )\\/[0-9]{1,3}+))|(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{1,3}+(| )\\/[0-9]{4}+)(.*?)(idf|novelle)|(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{4}+(| )\\/[0-9]{1,3}+)(.*?)(idf|novelle)|(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{1,3}+(| )\\/[0-9]{4}+)|(bgb(|\\s|\\si\\s|i\\s|i\\si\\s)[0-9]{4}+(| )\\/[0-9]{1,3}+)|(\\d{4}\\,(\\s|)(bgb(|\\s|\\si\\s|i\\s|i\\si\\s))(\\d{1,3}\\s))") %>%
  setNames(G_sprueche_missing$link) 

G_sprueche_missing <- map_df(G_sprueche_missing, ~as.data.frame(.x), .id="id") 

G_sprueche_missing <- G_sprueche_missing %>%
  select(1, 10, 26, 45, 48, 20, 32, 51) 

G_sprueche_missing <- G_sprueche_missing %>%
  mutate(V50 = str_replace_all(V50, "\\,", "\\/")) %>%
  mutate(norms_all = as.character(paste(V9, V25, V44, V47, V19, V31, V50, sep = ","))) %>%
  select(id, norms_all) %>%
  mutate(norms_all = str_remove_all(norms_all, "NA\\,|NA")) %>% 
  mutate(norms_all = ifelse(str_detect(norms_all, "(\\,$|)"),
                            paste0(norms_all, ""),
                            paste0(norms_all, ","))) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(norms_all = paste(norms_all, collapse = "")) %>%
  rename(link = id)

G_sprueche <- G_sprueche %>%
  full_join(G_sprueche_missing, by ="link") %>% 
  mutate(norms_all = as.character(paste(norms_all.x, norms_all.y, sep = ""))) %>%
  mutate(norms_all = str_remove_all(norms_all, "NA")) %>%
  mutate(norms_all = str_remove_all(norms_all, "[:alpha:]")) %>%
  select(-norms_all.x, -norms_all.y)

rm(G_sprueche_missing)

#--------------------------------------------------------------
#organizing the new data

#removing duplicates within a string (norms variable, since often the norm under review is quoted more than once)
G_sprueche <- G_sprueche %>%
  mutate(norms_all = str_replace_all(norms_all, "\\s", "")) %>%
  mutate(norms_all = str_squish(norms_all)) 

#In a few cases, the "," is missing - inserting it manually
G_sprueche_fault <- G_sprueche %>%
  filter(str_detect(norms_all, "\\d{5}")) %T>%
  write_tsv("manual/g_sprueche_fault.tsv") 

G_sprueche_fault_corrected <- read_excel("manual/g_sprueche_fault.xlsx")

G_sprueche_fault <- G_sprueche_fault %>%
  select(-norms_all) %>%
  left_join(G_sprueche_fault_corrected, by = "link")

#Binding the corrected data with the existing one
G_sprueche <- G_sprueche %>%
  filter(!str_detect(norms_all, "\\d{5}")) %>%
  bind_rows(G_sprueche_fault)

rm(G_sprueche_fault)
rm(G_sprueche_fault_corrected)

#I do not remember what this step is for
G_sprueche$norms_all <- sapply(G_sprueche$norms_all, function(x) paste(unique(unlist(str_split(x, ","))), collapse = ", "))


#seperating norms into columns
G_sprueche <- G_sprueche %>%
  separate(norms_all, c("norm1", "norm2", "norm3", "norm4", "norm5", "norm6", "norm7", "norm8", "norm9", "norm10", "norm11"), sep = "([\\,])") 

#cleaning the new data
G_sprueche <- G_sprueche %>%
  mutate_at(vars(matches("norm")), ~str_squish(.)) %>% 
  mutate_at(vars(matches("norm")), list(~na_if(.,"")))

#-------------------------------------------------------------
#manually correcting the cases with many norms 
#(implies the shortening of antrag did not work)
G_sprueche_check <- G_sprueche %>%
  filter(!is.na(norm2))

G_sprueche_check %>%
  select(link, contains("norm")) %T>%
  write_tsv("manual/g_sprueche_check.tsv") 

G_sprueche_check_final <- read_excel("manual/g_sprueche_check_final.xls")

G_sprueche_check <- G_sprueche_check %>%
  select(-contains("norm")) %>%
  inner_join(G_sprueche_check_final, by = "link") %>%
  mutate(length = stri_length(entscheidung))

#manually inserting the missing norms 
G_sprueche_m <- G_sprueche %>%
  mutate(length = stri_length(entscheidung)) %>%
  filter(is.na(norm1)) %>%
  filter(decision != "sustained") %>%
  filter(length > 1000 | is.na(length))

G_sprueche_m %>% 
  select(link, contains("norm"), entscheidung, length) %>%
  write_tsv("manual/G_sprueche_manualnorms.tsv")

G_sprueche_manualnorms_final <- read_excel("manual/g_sprueche_manualnorms_final.xls")

G_sprueche_m <- G_sprueche_m %>%
  select(-contains("norm")) %>% 
  left_join(G_sprueche_manualnorms_final, by = "link") 

G_sprueche <- G_sprueche %>%
  mutate(length = stri_length(entscheidung)) %>%
  filter(is.na(norm1) & decision == "sustained"  | is.na(norm1) & is.na(length) | is.na(norm1) & length < 1000  | !is.na(norm1)) %>% 
  filter(is.na(norm2)) %>% 
  bind_rows(G_sprueche_m) %>%
  bind_rows(G_sprueche_check) 

rm(G_sprueche_check)
rm(G_sprueche_check_final)
rm(G_sprueche_m)
rm(G_sprueche_manualnorms_final)
#--------------------------------------------------------------------------------------

#defining the decisions for every single norm

#function to check whether norms are contained in spruch
check_partlysustained <- function(spruch, norm){
  map2(spruch, norm, str_detect) %>% unlist 
} 

#applying function to every norm
G_sprueche_partlysustained <- G_sprueche %>%
  filter(decision == "partly sustained") %>%
  mutate(spruch = str_remove_all(spruch, "[^.]*(nicht als verfassungswidrig|abgewiesen|zurueckgewiesen|nicht folge|eingestellt)[^.]*\\.")) %>% #sorting out sentences in which norms are declared not unconstitutional 
  mutate(decision1 = check_partlysustained(spruch, norm1)) %>%
  mutate(decision2 = check_partlysustained(spruch, norm2)) %>%
  mutate(decision3 = check_partlysustained(spruch, norm3)) %>%
  mutate(decision4 = check_partlysustained(spruch, norm4)) %>%
  mutate(decision5 = check_partlysustained(spruch, norm5)) %>%
  mutate(decision6 = check_partlysustained(spruch, norm6)) %>%
  mutate(decision7 = check_partlysustained(spruch, norm7)) %>%
  mutate(decision8 = check_partlysustained(spruch, norm8)) %>%
  mutate(decision9 = check_partlysustained(spruch, norm9)) %>%
  mutate(decision10 = check_partlysustained(spruch, norm10)) %>%
  mutate(decision11 = check_partlysustained(spruch, norm11)) %>%
  mutate_at(vars(matches("decision")), ~str_replace_all(., "TRUE", "sustained")) %>% 
  mutate_at(vars(matches("decision")), ~str_replace_all(., "FALSE", "overruled")) 

#Defining the decision for each norm in not partly sustained cases
G_sprueche <- G_sprueche %>%
  filter(decision != "partly sustained") %>%
  mutate(decision1 = case_when(
    !is.na(norm1) & str_detect(decision, "sustained") ~ "sustained",
    !is.na(norm1) & str_detect(decision, "overruled") ~ "overruled",
  ),
  decision2 = case_when(
    !is.na(norm2) & str_detect(decision, "sustained") ~ "sustained",
    !is.na(norm2) & str_detect(decision, "overruled") ~ "overruled",
  ),
  decision3 = case_when(
    !is.na(norm3) & str_detect(decision, "sustained") ~ "sustained",
    !is.na(norm3) & str_detect(decision, "overruled") ~ "overruled",
  ),
  decision4 = case_when(
    !is.na(norm4) & str_detect(decision, "sustained") ~ "sustained",
    !is.na(norm4) & str_detect(decision, "overruled") ~ "overruled",
  ),
  decision5 = case_when(
    !is.na(norm5) & str_detect(decision, "sustained") ~ "sustained",
    !is.na(norm5) & str_detect(decision, "overruled") ~ "overruled",
  ),
  decision6 = case_when(
    !is.na(norm6) & str_detect(decision, "sustained") ~ "sustained",
    !is.na(norm6) & str_detect(decision, "overruled") ~ "overruled",
  ),
  decision7 = case_when(
    !is.na(norm7) & str_detect(decision, "sustained") ~ "sustained",
    !is.na(norm7) & str_detect(decision, "overruled") ~ "overruled",
  ),
  decision8 = case_when(
    !is.na(norm8) & str_detect(decision, "sustained") ~ "sustained",
    !is.na(norm8) & str_detect(decision, "overruled") ~ "overruled",
  ),
  decision9 = case_when(
    !is.na(norm9) & str_detect(decision, "sustained") ~ "sustained",
    !is.na(norm9) & str_detect(decision, "overruled") ~ "overruled",
  ),
  decision10 = case_when(
    !is.na(norm10) & str_detect(decision, "sustained") ~ "sustained",
    !is.na(norm10) & str_detect(decision, "overruled") ~ "overruled",
  ),
  decision11 = case_when(
    !is.na(norm11) & str_detect(decision, "sustained") ~ "sustained",
    !is.na(norm11) & str_detect(decision, "overruled") ~ "overruled",
  )) 

#combining both into complete df (party sustained, sustained and overruled cases)
G_sprueche <- bind_rows(G_sprueche, G_sprueche_partlysustained)

rm(G_sprueche_partlysustained)

saveRDS(G_sprueche, "data/G_sprueche3.rds")

#--------------------------------------------------------------

#defining a variable on the plaintiff
G_sprueche <- readRDS("data/G_sprueche3.rds") %>%
  mutate(antrag_shortened = str_replace_all(antrag_shortened, "arbeits\\- und sozialgericht", "arbeitsundsozialgericht")) %>%
  mutate(entscheidung = str_remove_all(entscheidung, "\\(im folgenden(|\\:) (\\w+\\s\\w+|\\w+)\\)")) %>%
  mutate(entscheidung = str_replace_all(entscheidung, "\\s\\s", " ")) %>%
  mutate(plaintiff = case_when(
    str_detect(catchwords, "parteiantrag") ~ "parteiantrag",
    str_detect(catchwords, "individualantrag") ~ "individualantrag",
    str_detect(catchwords, "pruefungsbeschluss") | str_detect(entscheidung, "(im verfassungsgerichtshof bedenken|sind beim verfassungsgerichtshof einerseits bedenken|verfassungsgerichtshof von amts wegen|von amts wegen die verfassungsmaessigkeit)") ~ "vfgh",
    str_detect(antrag_shortened, "(abgeordnete(|n)|mitglieder) (zum nationalrat|des nationalrates)") ~ "nationalrat",
    str_detect(antrag_shortened, "(abgeordnete(|n)|mitglieder) (zum bundesrat|des bundesrates)") ~ "bundesrat",
    str_detect(entscheidung, "((stell(t|te|en)|beantrag(t|te|en)|begehr(t|te|en))\\sdie\\s\\w+(|\\.)\\slandesregierung|die\\s\\w+(|\\.)\\slandesregierung\\s(stell(t|te|en)|beantrag(t|te|en)|begehr(t|te|en)))") ~ "landesregierung",
    str_detect(antrag_shortened, "beim (verfassungsgerichtshof|vfgh) (sind|ist)") ~ "vfgh", #not part of the first coding of vfgh since it is not as safe, safer code is used first
    str_detect(entscheidung, "((stell(t|te|en)|beantrag(t|te|en)|begehr(t|te|en)|beschlos(s|sen))\\s(das|der|dieser)(\\s\\w+\\s|\\s)((\\w+|)(gericht|senat)(|shof)|uvs|ogh|vwgh|olg)|(das|der)(\\s\\w+\\s|\\s)((\\w+|)(gericht|senat)(|shof)|uvs|vwgh|ogh|olg)(\\s\\w+\\s|\\s)(stell(t|te|en)|beantrag(t|te|en)|begehr(t|te|en))|antrag des \\w+gericht)") ~ "gericht",
    str_detect(antrag_shortened, "((der|die) antragstell|einschreiter)") ~ "privat",
    str_detect(entscheidung, "(beim (verfassungsgerichtshof|vfgh) (sind|ist)|der verfassungsgerichtshof leitete)") ~ "vfgh", #not part of the first coding of vfgh since it is not as safe, safer code is used first
    str_detect(antrag_shortened, "anlassverfahren\\, antragsvorbringen und vorverfahren") ~ "gericht",
    str_detect(entscheidung, "(aus anlass (dieses verfahrens|(einer|der)bei ihm anhaengigen)|\\w+gericht(shof|) hat mit beschlu|(einschreitende|antragstellende)(\\s\\w+\\s|\\s)(\\w+|)(gericht|senat)|der unabhaengige verwaltungssenat)") ~ "gericht",
    str_detect(entscheidung, "((der|die) antragstell|einschreiter|beschwerdefuehrer)") ~ "privat",
    TRUE ~ "other"))

G_sprueche %>%
  dplyr::count(plaintiff)

#defining a variable with the more general category
#of plaintiff
G_sprueche <- G_sprueche %>%
  mutate(plaintiff_category = case_when(
    str_detect(plaintiff, "vfgh|gericht") ~ "Court",
    str_detect(plaintiff, "nationalrat|bundesrat|landesregierung") ~ "Politician",
    str_detect(plaintiff, "individualantrag|parteiantrag|privat") ~ "Person",
    TRUE ~ "Other"
  )) 

G_sprueche %>%
  dplyr::count(plaintiff_category)

G_sprueche %>%
  filter(decision != "overruled") %>%
  dplyr::count(plaintiff_category)

#------------------------------------------------
#analyzing the policy area through the index number

G_sprueche <- G_sprueche %>%
  mutate(applied_without_po = str_remove_all(applied, ".*(eo|aussstrg|vwgg|aussstrg|geg|vwgvg|zpo|gog).*")) %>% 
  mutate(policyarea = case_when(
    str_detect(index, "^(1003|1004|1006|1011|1012)") ~ "Political Rights",
    str_detect(index, "^(3105|44|66|82|65|67|68|6903|6905|2003)") | str_detect(applied, "gesundheit|asvg|bsvg|gsvg") & str_detect(index, "^1") |str_detect(catchwords, "versicherung|familienleben|kinderbetreuungsgeld") ~ "Social",
    str_detect(index, "^(2002|61)") | str_detect(catchwords, "familienleben") ~ "Family",
    str_detect(index, "^(4102|6904)") | str_detect(applied, "(asyl|fremd|aufenthaltsg)") & str_detect(index, "^1") | str_detect(catchwords, "auslaender|fremde|asyl|aufenthaltsrecht|staatsbuergerschaft") ~ "Migration",
    str_detect(index, "^(3101|3102|3103|3104|30|37|38|3901|32|35)") | str_detect(applied, "(kwg|kapitalmarkt|estg)") & str_detect(index, "^1") | str_detect(catchwords, "steuer") ~ "Finances",
    str_detect(index, "^(21|23|26|36|34|97|5|2006|2008)") | str_detect(applied, "io|uwg|insolvenz") & str_detect(index, "^1") |str_detect(catchwords, "gewerberecht|gewerbeberechtigung")~ "Economy",
    str_detect(index, "^(24|25|4101|4104|4107|4108|4109|4110|43|1013)") | str_detect(applied, "(stpo|stgb|stvg)") & str_detect(index, "^1") | str_detect(catchwords, "polizei")  ~ "Security",
    str_detect(index, "^(60|62|63|64|6901|6902)") | str_detect(applied, "bdg") & str_detect(index, "^1") |str_detect(catchwords, "arbeitsrecht") ~ "Employment",
    str_detect(index, "^16") | str_detect(applied, "orf-g") & str_detect(index, "^1") |str_detect(catchwords, "rundfunk") ~ "Media",
    str_detect(index, "^(70|71|72|73|75|76)") |str_detect(catchwords, "hochschulen") ~ "Education",
    str_detect(index, "^74") ~ "Religion",
    str_detect(index, "^(81|83|86|89|8902|8907|8908|8909|80|8901)") | str_detect(catchwords, "umweltschutz") ~ "Environment",
    str_detect(index, "^(2005|98|9901|9902|9903|9905|9906|90|93|94|96|92)") | str_detect(applied, "(mietrecht|wohnung|stadterneuerung|kfg)") & str_detect(index, "^1") |str_detect(catchwords, "mietenrecht|kraftfahrrecht") ~ "Infrastructure",
    str_detect(index, "^(22|27|14|40|1007|2011|2012)") | str_detect(applied, "(avg)") & str_detect(index, "^1") | str_detect(applied, "(eo|aussstrg|vwgg|geg|vwgvg|zpo|gog)") & str_detect(index, "^1") & is.na(applied_without_po) ~ "Judicature",
    TRUE ~ "Other"
  )) 
  

#counting the frequency of the policy areas
G_sprueche %>%
  count(policyarea, sort = TRUE) 

G_sprueche %>%
  filter(policyarea == "Other") %>%
  count(index, sort = TRUE) 

#In a few cases, I manually defined policyareas
policyareas <- read_excel("manual/policyareas_forimport.xlsx")

#In these cases I will overwrite the existing variable with the new data
G_sprueche <- G_sprueche %>%
  left_join(policyareas, by = "link") %>% 
  mutate(policyarea = case_when(
    is.na(policyarea2) ~ str_extract(policyarea, ".*"),
    TRUE ~ str_extract(policyarea2, ".*")
  )) %>%
  select(-policyarea2)

rm(policyareas)

saveRDS(G_sprueche, "data/G_sprueche4.rds")


#defining a dataframe for the appendix that shows
#which policy area was applied to each field of law
Law_Areas <- read_excel("manual/Law_Areas.xlsx") %>% 
  na.omit() %>%
  mutate(number = str_extract(Area, "\\d{2}\\/\\d{2}|\\d{2}|\\d{1}")) %>%
  mutate(area = str_remove(Area, "\\d{2}\\/\\d{2}|\\d{2}|\\d{1}")) %>%
  select(number, area) %>%
  mutate(number_tidy = str_remove(number, "\\/")) %>%
  mutate(policyarea = case_when(
    str_detect(number, "^(1003|1004|1006|1011|1012)") ~ "Political Rights",
    str_detect(number, "^(44|66|82|65|67|68|6903|6905|2003)")  ~ "Social",
    str_detect(number, "^(2002|61)") ~ "Family",
    str_detect(number, "^(4102|6904|2914)")  ~ "Migration",
    str_detect(number, "^(31|30|37|38|3901)")  ~ "Finances",
    str_detect(number, "^(32|35)") ~ "Taxation",
    str_detect(number, "^(21|23|26|36|34|97|5|2006|2008)") ~ "Economy",
    str_detect(number, "^(24|25|41|43)") ~ "Security",
    str_detect(number, "^(60|62|63|64|6901|6902)")  ~ "Employment",
    str_detect(number, "^16")  ~ "Media",
    str_detect(number, "^(70|71|72|73|75|76)") ~ "Education",
    str_detect(number, "^74") ~ "Religion",
    str_detect(number, "^(22|27|14|40|1007|1013|2011|2012)")  ~ "Judicature",
    str_detect(number, "^(80|8901)") ~ "Agriculture",
    str_detect(number, "^(81|83|86|89|8902|8907|8908|8909)") ~ "Environment and Animals",
    str_detect(number, "^(9901|9902|9903|9905|9906|90|93|94|96|92)")  ~ "Mobility",
    str_detect(number, "^(2005|98)")  ~ "Housing",
    TRUE ~ "Other"
  ))

