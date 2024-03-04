library(tidyverse)
library(readxl)
library(lubridate)
library(fuzzyjoin)

# data on laws under review ----

# import 
G_Land_2020 <- readRDS("data/data2020_land.rds") #2020

G_Land <- readRDS("data/G_sprueche_land.rds") %>% # 1980-2019
  bind_rows(G_Land_2020)

# basic cleaning
G_Land <- G_Land %>%
  mutate(date = case_when(
    str_detect(date, "\\d{2}\\.\\d{2}\\.") ~ dmy(date),
    str_detect(date, "^\\d{4}") ~ ymd(date)
   )
  ) %>%
  mutate(geschaeftszahl = str_remove(geschaeftszahl, ".*zahl")) %>%
  mutate(geschaeftszahl = str_squish(geschaeftszahl)) %>%
  mutate(applied = stringi::stri_replace_all_fixed(applied, c("ü","ä", "ö", "ß"), c("ue", "ae", "oe", "ss"), vectorize_all = FALSE))

# populist parties

populist_parties <- c("fpö", "bzö", "fpk", "ts")

# import data on provincial governments
landesregierungen <- read_excel("manual/landesregierungen.xlsx") %>%
  mutate_at(c("spö", "övp", "bzö", "fpö", "fpk", "grüne", "neos", "ts", "independent", "members"), as.numeric) %>%
  mutate_at(c("start_date", "end_date"), as_date) %>%
  mutate(populists_sum = bzö + fpö + fpk + ts) %>%
  mutate(populist_share = populists_sum/members*100) %>%
  mutate(landeshauptmann_n = ifelse(is.na(stllv_landeshauptmann2), 2, 3)) %>%
  mutate(populist_landeshauptmann_n = pmap_int(select(., contains("landeshauptmann")), ~sum(c(...) %in% populist_parties))) %>%
  mutate(populist_landeshauptmann_share = populist_landeshauptmann_n/landeshauptmann_n*100) %>%
  mutate(populist_landeshauptteam_dummy = ifelse(populist_landeshauptmann_n > 0, 1, 0)) %>%
  mutate(populist_landeshauptmann_dummy = ifelse(landeshauptmann %in% populist_parties, 1, 0)) %>%
  mutate(end_date = ifelse(is.na(end_date), today(), end_date)) %>%
  mutate(end_date = as_date(end_date))
  

# in which of the provinces did populists occupy more than 20% of government departments?
landesregierungen %>%
  filter(populist_share > 20) %>%
  distinct(land)

# defining populist governments ----
# done manually for each of the regions that had a FPÖ/BZÖ politician as (stllv.) Landeshauptmann

# BURGENDLAND ----

# from when on were populists in office?

landesregierungen %>%
  filter(populist_share > 20) %>%
  filter(land == "burgenland") %>%
  pull(start_date) %>% 
  min() ->
  min_burgenland

G_burgenland <- G_Land %>%
  filter(str_detect(entscheidung, "burgenland|bgld")) %>%
  filter(date > min_burgenland) %>%
  select(link, geschaeftszahl)

G_burgenland %>%
  write_tsv("manual/G_burgenland")

# open all decision containing Kärnten in browser
# lapply(G_burgenland$link, function(x) browseURL(as.character(x)))

# OBEROESTERREICH ----

# from when on were populists in office?

landesregierungen %>%
  filter(populist_share > 20) %>%
  filter(land == "oberösterreich") %>%
  pull(start_date) %>% 
  min() ->
  min_oberoesterreich

G_oberoesterreich <- G_Land %>%
  filter(date > min_oberoesterreich) %>%
  filter(str_detect(entscheidung, "(oberoesterreich|ooe)")) %>%
  select(link, geschaeftszahl) 

G_oberoesterreich %>%
  write_tsv("manual/G_oberoesterreich")

# open all decision containing OÖ in browser
# lapply(G_oberoesterreich$link, function(x) browseURL(as.character(x)))

# SALZUBURG ----

# from when on were populists in office?

landesregierungen %>%
  filter(populist_share > 20) %>%
  filter(land == "salzburg") %>%
  pull(start_date) %>% 
  min() ->
  min_salzburg

G_salzburg <- G_Land %>%
  filter(date > min_salzburg) %>%
  filter(str_detect(entscheidung, "(salzburg|sbg)")) %>%
  select(link, geschaeftszahl) 

G_salzburg %>%
  write_tsv("manual/G_salzburg")

# open all decision containing salzburg in browser
# lapply(G_salzburg$link, function(x) browseURL(as.character(x)))

# KÄRNTEN ----

# code which laws were passed by populists

# select only cases referring to Kärntner laws
landesregierungen %>%
  filter(landeshauptmann == "fpö") %>% 
  filter(land == "kärnten") %>% 
  pull(start_date) %>% 
  min() ->
  min_kaernten

G_kaernten <- G_Land %>%
  filter(str_detect(entscheidung, "kaernten")) %>%
  filter(date > min_kaernten) 

# save for manual coding
G_kaernten %>%
  select(link, geschaeftszahl) %>% 
  write_tsv("manual/G_kaernten")

# import coded data

# kärnten
G_kaernten_filled <- read_excel("manual/G_kaernten_filled.xlsx") %>%
  select(-geschaeftszahl)

# burgenland
G_burgenland_filled <- read_excel("manual/G_burgenland_filled.xlsx") %>%
  select(-geschaeftszahl)

# oberoesterreich
G_oberoesterreich_filled <- read_excel("manual/G_oberoesterreich_filled.xlsx") %>%
  select(-geschaeftszahl, -date_law2, -date_law3, -date_law4)

# salzburg
G_salzburg_filled <- read_excel("manual/G_salzburg_filled.xlsx") %>%
  select(-geschaeftszahl)

# bind manual data
G_land_manual <-
  bind_rows(G_burgenland_filled, G_salzburg_filled, G_oberoesterreich_filled, G_kaernten_filled)

# add variable on date_law to orig dataframe

G_land_manual %>%
  mutate(date_law = ymd(date_law)) %>%
  distinct(link, .keep_all = TRUE) %>%
  filter(!is.na(date_law)) %>%
  select(link, date_law) %>%
  right_join(G_Land, by = "link") ->
  G_Land

# add variable on which land

G_land_manual %>%
  filter(!is.na(land_manual)) %>%
  distinct(link, .keep_all = TRUE) %>%
  select(link, land_manual) %>%
  right_join(G_Land, by = "link") %>%
  mutate(land_manual = str_replace_all(land_manual, "kaernten", "krnt")) ->
  G_Land

# define land ---- 

# long code because of hierarchy -> first only check applied norms, then the shortened antrag then only entscheidung
G_Land %>%
  mutate(land = case_when(
    str_detect(applied, "sbg") | str_detect(spruch, "salzburg") ~ "sbg",
    str_detect(applied, "krnt|kag") | str_detect(spruch, "kaernten") ~ "krnt",
    str_detect(applied, "stmk|steiermark|graz") | str_detect(spruch, "steiermark") ~ "stmk",
    str_detect(applied, "wr|wien|wao") | str_detect(spruch, "wien") ~ "wien",
    str_detect(applied, "vlbg") | str_detect(spruch, "vorarlberg") ~ "vlbg",
    str_detect(applied, "bgld") | str_detect(spruch, "burgenland") ~ "bgld",
    str_detect(applied, "noe") | str_detect(spruch, "niederoesterreich|noe") ~ "noe",
    str_detect(applied, "tir|innsbruck") | str_detect(spruch, "tirol|tir") ~ "tirol",
    str_detect(applied, "ooe") | str_detect(spruch, "oberoesterreich") ~ "ooe",
    str_detect(applied, "stmk|steiermark|graz") | str_detect(spruch, "steiermark") ~ "stmk",
    str_detect(antrag, "sbg|salzburg") ~ "slbg",
    str_detect(antrag, "krnt|kag|kaernten") ~ "krnt",
    str_detect(antrag, "vlbg|vorarlberg") ~ "vlbg",
    str_detect(antrag, "bgld|burgenland") ~ "bgld",
    str_detect(antrag, "noe|niederoesterreich") ~ "noe",
    str_detect(antrag, "tir|innsbruck|tirol") ~ "tirol",
    str_detect(antrag, "ooe|oberoesterreich") ~ "ooe",
    str_detect(antrag, "stmk|steiermark|graz") ~ "stmk",
    str_detect(antrag, "wr|wien|wao") ~ "wien",
    str_detect(entscheidung, "sbg|salzburg") ~ "sbg",
    str_detect(entscheidung, "krnt|kag|kaernten") ~ "krnt",
    str_detect(entscheidung, "stmk|steiermark|graz") ~ "stmk",
    str_detect(entscheidung, "vlbg|vorarlberg") ~ "vlbg",
    str_detect(entscheidung, "bgld|burgenland") ~ "bgld",
    str_detect(entscheidung, "noe|niederoesterreich") ~ "noe",
    str_detect(entscheidung, "wr|wien|wao") ~ "wien",
    str_detect(entscheidung, "stmk|steiermark|graz") ~ "stmk",
    str_detect(entscheidung, "tir|innsbruck|tirol") ~ "tirol",
    str_detect(entscheidung, "ooe|oberoesterreich") ~ "ooe"
  )) ->
  G_Land

# adapt how the laender are written in landesregierung df

landesregierungen %>%
  mutate(land = case_when(
    land == "steiermark" ~ "stmk",
    land == "kärnten" ~ "krnt",
    land == "wien" ~ "wien",
    land == "vorarlberg" ~ "vlbg",
    land == "salzburg" ~ "sbg",
    land == "niederösterreich" ~ "noe",
    land == "oberösterreich" ~ "ooe",
    land == "burgenland" ~ "bgld",
    land == "tirol" ~ "tirol"
  )) ->
  landesregierungen

# check whether manual and automatized länder codes align

G_Land %>%
  filter(!is.na(land_manual)) %>%
  mutate(dummy = ifelse(land %in% land_manual, 1, 0)) %>% 
  count(dummy) # they do, yeah!

saveRDS(G_Land, "data/G_Land1.rds")

# extract decision & plaintiff

G_Land %>%
  mutate(decision = case_when(
    str_detect(spruch, "kundmachung|kundzumachen|verlautbaren|(wird|werden) als verfassungswidrig") ~ "sustained",
    str_detect(spruch, "zurueckgewies*|abgewies*|abgelehnt|nicht als verfassungswidrig|(war|waren) nicht verfassungswidrig|(keine|nicht) folge|eingestellt") ~ "overruled"
  )) %>%
  mutate(plaintiff = case_when(
    str_detect(catchwords, "parteiantrag") ~ "parteiantrag",
    str_detect(catchwords, "individualantrag") ~ "individualantrag",
    str_detect(catchwords, "pruefungsbeschluss") | str_detect(entscheidung, "(im verfassungsgerichtshof bedenken|sind beim verfassungsgerichtshof einerseits bedenken|verfassungsgerichtshof von amts wegen|von amts wegen die verfassungsmaessigkeit)") ~ "vfgh",
    str_detect(entscheidung, "(abgeordnete(|n)|mitglieder) (zum landtag|des landtages)") ~ "nationalrat",
    str_detect(entscheidung, "((stell(t|te|en)|beantrag(t|te|en)|begehr(t|te|en))\\sdie\\s\\w+(|\\.)\\sbundesregierung|die\\s\\w+(|\\.)\\sbundesregierung\\s(stell(t|te|en)|beantrag(t|te|en)|begehr(t|te|en)))") ~ "bundesregierung",
    str_detect(entscheidung, "((stell(t|te|en)|beantrag(t|te|en)|begehr(t|te|en))\\sdie\\s\\w+(|\\.)\\slandesregierung|die\\s\\w+(|\\.)\\slandesregierung\\s(stell(t|te|en)|beantrag(t|te|en)|begehr(t|te|en)))") ~ "landesregierung",
    str_detect(entscheidung, "beim (verfassungsgerichtshof|vfgh) (sind|ist)") ~ "vfgh", #not part of the first coding of vfgh since it is not as safe, safer code is used first
    str_detect(entscheidung, "((stell(t|te|en)|beantrag(t|te|en)|begehr(t|te|en)|beschlos(s|sen))\\s(das|der|dieser)(\\s\\w+\\s|\\s)((\\w+|)(gericht|senat)(|shof)|uvs|ogh|vwgh|olg)|(das|der)(\\s\\w+\\s|\\s)((\\w+|)(gericht|senat)(|shof)|uvs|vwgh|ogh|olg)(\\s\\w+\\s|\\s)(stell(t|te|en)|beantrag(t|te|en)|begehr(t|te|en))|antrag des \\w+gericht)") ~ "gericht",
    str_detect(entscheidung, "((der|die) antragstell|einschreiter)") ~ "privat",
    str_detect(entscheidung, "(beim (verfassungsgerichtshof|vfgh) (sind|ist)|der verfassungsgerichtshof leitete)") ~ "vfgh", #not part of the first coding of vfgh since it is not as safe, safer code is used first
   # str_detect(entscheidung, "anlassverfahren\\, antragsvorbringen und vorverfahren") ~ "gericht",
    str_detect(entscheidung, "(aus anlass (dieses verfahrens|(einer|der)bei ihm anhaengigen)|\\w+gericht(shof|) hat mit beschlu|(einschreitende|antragstellende)(\\s\\w+\\s|\\s)(\\w+|)(gericht|senat)|der unabhaengige verwaltungssenat)") ~ "gericht",
    str_detect(entscheidung, "((der|die) antragstell|einschreiter|beschwerdefuehrer)") ~ "privat",
    str_detect(entscheidung, "von amts wegen") ~ "vfgh")) %>%
  mutate(plaintiff_category = case_when(
    str_detect(plaintiff, "vfgh|gericht") ~ "Court",
    str_detect(plaintiff, "nationalrat|bundesrat|landesregierung") ~ "Politician",
    str_detect(plaintiff, "individualantrag|parteiantrag|privat") ~ "Person",
    TRUE ~ "Other"
  )) -> 
  G_Land

G_Land %>%
  count(decision)

G_Land %>%
  count(plaintiff)

G_Land %>%
  count(plaintiff_category)

# define populism 

landesregierungen <- landesregierungen %>%
  select(land, start_date, end_date, populists_sum, populist_share, populist_landeshauptmann_n, populist_landeshauptmann_share, populist_landeshauptmann_dummy, populist_landeshauptteam_dummy)

G_Land %>% 
  select(-land_manual) %>%
  fuzzy_left_join(
    landesregierungen,
    by = c(
      "land" = "land",
      "date_law" = "start_date",
      "date_law" = "end_date"
    ),
    match_fun = c(`==`, `>=`, `<=`)
  ) %>% 
  rename(land = land.x) %>%
  select(-land.y, start_date, end_date) %>%
  mutate(populist_share = if_else(is.na(populist_share), 0, populist_share)) -> 
  G_Land

saveRDS(G_Land, "data/G_Land2.rds")
