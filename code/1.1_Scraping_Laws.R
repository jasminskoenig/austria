library(rvest)
library(tidyverse)
library(stringi)

library(furrr)
plan(multisession)

# Dieser Code funktioniert als Grundlage, um populistische Ursprünge von Gesetzgebung zu codieren. Allerdings gibt es keine Auskunft darüber, welcher Art ein Gesetzgebungsantrag vor 2000 war, davor können auch Initiativanträge keinen Personen zugeordnet werden.

#Scraping all laws legislated in the relevant period ----
#2004-2021----

# info from RIS ----

#Hmtl ist eine Liste, eine zweite die Nummern am Ende
page_basic <- "https://www.ris.bka.gv.at/Ergebnis.wxe?Abfrage=BgblAuth&Titel=&Bgblnummer=&SucheNachGesetzen=True&SucheNachKundmachungen=False&SucheNachVerordnungen=False&SucheNachSonstiges=False&SucheNachTeil1=True&SucheNachTeil2=False&SucheNachTeil3=False&Einbringer=&VonDatum=01.01.2004&BisDatum=31.12.2021&ImRisSeitVonDatum=01.01.2004&ImRisSeitBisDatum=31.12.2021&ImRisSeit=Undefined&ResultPageSize=100&Suchworte=&Position="
page_nmbr <- c(1, 101, 201, 301, 401, 501, 601, 701, 801, 901, 1001, 1101, 1201, 1301, 1401, 1501, 1601, 1701, 1801, 1901, 2001, 2101, 2201, 2301)

#HTML und Nummern werden zusammengefuegt
pages_all <- map2_chr(page_basic, page_nmbr, paste0)

#Es wird eine Funktion gebaut, um den Link zum Dokument zu laden
get_info_laws <- function(x) {
  page <- read_html(x)
  
  tibble(
    link = page %>%
      html_nodes(".nonWrappingCell") %>%
      html_attr("href") %>%
      paste0("https://www.ris.bka.gv.at", .), 
    norm = page %>%
      html_nodes(".nonWrappingCell") %>%
      html_text(),
    date = page %>%
      html_nodes(".bocListDataCell:nth-child(4) .bocListCommandText") %>%
      html_text(),
    name = page %>%
      html_nodes(".bocListTextContent .bocListCommandText") %>%
      html_text())
}

#Die Funktion wird aufgefuehrt und die Info geladen
Laws_2004 <- future_map_dfr(pages_all, get_info_laws, .progress = TRUE)
saveRDS(Laws_2004, "data/laws/Laws_2004.rds")

# Einbringende Stelle
get_2004 <- function(x) {
  page <- read_html(x)
  
  tibble(
    link = x,
    law_type = page %>% 
      html_nodes(xpath = '//div[h3[contains(text(), "Typ")]]') %>%
      html_text(),
    law_id = page %>% 
      html_nodes(xpath = '//div[h3[contains(text(), "Dokumentnummer")]]') %>%
      html_text(),
    shorttitle = page %>% 
      html_nodes(xpath = '//div[h3[contains(text(), "Kurztitel")]]') %>%
      html_text(),
    department = page %>% 
      html_nodes(xpath = '//div[h3[contains(text(), "Einbringende")]]') %>%
      html_text(),
    content = page %>% 
      html_nodes('.documentContent') %>%
      html_text())
}

Laws_2004_es <- future_map_dfr(Laws_2004$link, get_2004, .progress = TRUE)

Laws_2004_es %>%
  left_join(Laws_2004, by = "link") %>%
  mutate(content = str_squish(content)) %>%
  mutate(department = str_to_lower(department)) %>%
  mutate(initiated = case_when(
    str_detect(content, "Regierungsvorlage") ~ "Regierungsvorlage",
    str_detect(content, "Initiativantrag") ~ "Initiativantrag",
    !str_detect(content, "Initiativantrag|Regierungsvorlage") & str_detect(department, "parlament") & str_detect(content, "Ausschussbericht") ~ "Selbstständiger Antrag"
  )) ->
  Laws_2004_es

Laws_2004_es %>%
group_by(department) %>%
summarise(n = n()) %>% View()

# PARLIAMENT DATA  ----

# links to parliament data 

#regierungsvorlagen
get_2004_reg <- function(x) {
  page <- read_html(x)
  
  tibble(
    link = x,
    parl_link = page %>% 
      html_nodes(xpath = '//div[h5[contains(text(), "Regierungsvorlage")]]') %>% 
      html_nodes("a") %>% 
      html_attr('href')
    )
}

Laws_2004_reg <- future_map_dfr(Laws_2004_es$link, get_2004_reg,  .progress = TRUE) %>%
  distinct(link, .keep_all = TRUE)

#initiativanträge
get_2004_ini <- function(x) {
  page <- read_html(x)
  
  tibble(
    link = x,
    parl_link = page %>% 
      html_nodes(xpath = '//div[h5[contains(text(), "Initiativantrag")]]') %>% 
      html_nodes("a") %>% 
      html_attr('href')
  )
}

Laws_2004_ini <- future_map_dfr(Laws_2004_es$link, get_2004_ini,  .progress = TRUE)

# im fall von initiativanträgen - link zu abgeorndeten?
get_2004_ini2 <- function(x) {
  page <- read_html(x)
  
  tibble(
    parl_link = x,
    wwer_link = page %>% 
      html_nodes(xpath = '//p[contains(text(), "Eingebracht")]') %>% 
      html_nodes('a') %>% 
      html_attr('href'),
    legislator_names = page %>%
      html_nodes(xpath = '//p[contains(text(), "Eingebracht")]') %>% 
      html_text()
  )
  
}

Laws_2004_ini2 <- future_map_dfr(Laws_2004_ini$parl_link, get_2004_ini2,  .progress = TRUE) %>%
  mutate(wwer_link = paste0("https://www.parlament.gv.at", wwer_link))
  
# und welcher partei sie angehören
get_2004_ini3 <- function(x) {
  page <- read_html(x)
  
  tibble(
    wwer_link = x,
    party = page %>% 
      html_nodes('.tabs-responsive__contentBlock') %>% 
      html_text()
  )
  
}

Laws_2004_ini3 <- future_map_dfr(Laws_2004_ini2$wwer_link, get_2004_ini3,  .progress = TRUE) %>%
  mutate(party = case_when(
    str_detect(party, "ÖVP|Volkspartei") ~ "OVP",
    str_detect(party, "SPÖ|Sozialdemokrat") ~ "SPO",
    str_detect(party, "Grün|GRÜN") ~ "G",
    str_detect(party, "NEOS") ~ "NEOS",
    str_detect(party, "FPÖ|Freheitlich|\\,\\sF\\b") ~ "FPO",
    str_detect(party, "Bündnis|BZÖ") ~ "BZO",
    str_detect(party, "Jetzt") ~ "LP",
    str_detect(party, "Stronach") ~ "TS",
    str_detect(party, "Forum") ~ "LF",
    str_detect(party, "Sobotka") ~ "OVP",
    str_detect(party, "Trutnov") ~ "OVP", # Sonderfall eines FPÖ Politikers
    TRUE ~ NA_character_
  )) %>%
  distinct(wwer_link, .keep_all = TRUE)


Laws_2004_ini2 %>% 
  left_join(Laws_2004_ini3, by = "wwer_link") %>% 
  group_by(parl_link) %>%
  select(-wwer_link) %>%
  mutate(group_id = row_number()) %>%
  ungroup() %>% 
  pivot_wider(names_from = group_id, values_from = c("party", "legislator_names"))->
  Laws_2004_ini2

Laws_2004_ini2 %>%
  left_join(Laws_2004_ini, by = "parl_link") ->
  Laws_2004_ini

#selbstständige antrage
get_2004_aus <- function(x) {
  page <- read_html(x)
  
  tibble(
    link = x,
    parl_link = page %>% 
      html_nodes(xpath = '//div[h4[contains(text(), "Nationalrat")]]') %>%
      html_nodes(xpath = '//div[h5[contains(text(), "Ausschussbericht")]]') %>% 
      html_nodes("a") %>% 
      html_attr('href')
  )
  
}

Laws_2004_aus <- future_map_dfr(Laws_2004_es$link, get_2004_aus,  .progress = TRUE)

Laws_2004_aus %>%
  distinct(link, .keep_all = TRUE) %>%
  anti_join(Laws_2004_ini, by = "link") %>%
  anti_join(Laws_2004_reg, by = "link") ->
  Laws_2004_aus

Laws_2004_parl <- Laws_2004_reg %>%
  bind_rows(Laws_2004_ini) %>%
  bind_rows(Laws_2004_aus)

# abstimmunsgverhalten
get_2004_votes <- function(x) {
  page <- read_html(x)
  
  tibble(
    parl_link = x,
    votes = page %>% 
      html_nodes('.status p') %>%
      html_text()
  )
  
}

Laws_2004_votes <- future_map_dfr(Laws_2004_parl$parl_link, get_2004_votes,  .progress = TRUE) %>%
  mutate(votes = str_extract(votes, "(D|d)afür.*$")) %>%
  distinct(parl_link, .keep_all = TRUE)

Laws_2004_parl %>%
  left_join(Laws_2004_votes, by = "parl_link") ->
  Laws_2004_parl

# combine PARLIAMENT AND RIS

Laws_2004_es %>%
  left_join(Laws_2004_parl, by = "link") ->
  Laws_2004_complete

saveRDS(Laws_2004_complete, "data/laws/laws_2004_complete.rds")

#1980-2004----
#Hmtl ist eine Liste, eine zweite die Nummern am Ende
page_basic <- "https://www.ris.bka.gv.at/Ergebnis.wxe?Abfrage=BgblPdf&Titel=&Bgblnummer=&SucheNachGesetzen=True&SucheNachKundmachungen=False&SucheNachVerordnungen=False&SucheNachSonstiges=False&SucheNachTeil1=False&SucheNachTeil2=False&SucheNachTeil3=False&SucheNachTeilAlt=False&VonDatum=01.01.1979&BisDatum=31.12.2003&ImRisSeitVonDatum=01.01.1979&ImRisSeitBisDatum=31.12.2003&ImRisSeit=Undefined&ResultPageSize=100&Suchworte=&Position="
page_nmbr <- c(1, 101, 201, 301, 401, 501, 601, 701, 801, 901, 1001, 1101, 1201, 1301, 1401, 1501, 1601, 1701, 1801, 1901, 2001, 2101, 2201, 2301, 2401, 2501, 2601, 2701, 2801, 2901, 3001, 3101, 3201)

#HTML und Nummern werden zusammengefuegt
pages_all <- map2_chr(page_basic, page_nmbr, paste0)

#Die Funktion wird mit den neuen Links aufgefuehrt und die Info geladen
Laws_1980 <- map_dfr(pages_all, get_info_laws)
saveRDS(Laws_1980, "data/laws/Laws_1980.rds")

#Funktion um den Text der Sprueche herunter zu laden
get_content <- function(x) {
  page <- read_html(x)
  
  tibble(
    link = x,
    law_type = page %>% 
      html_nodes(xpath = '//div[h3[contains(text(), "Typ")]]') %>%
      html_text(),
    law_id = page %>% 
      html_nodes(xpath = '//div[h3[contains(text(), "Dokumentnummer")]]') %>%
      html_text(),
    shorttitle = page %>% 
      html_nodes(xpath = '//div[h3[contains(text(), "Kurztitel")]]') %>%
      html_text())
}


#downloading info for 1980-2004----
Laws_1980_info <- future_map_dfr(Laws_1980$link, get_content, .progress = TRUE)

# combine RIS info 

Laws_1980 %>%
  left_join(Laws_1980_info, by = "link") -> 
  Laws_1980

saveRDS(Laws_1980, "data/laws/Laws_1980.rds")

rm(Laws_1980_info)

# retrieve parliament links

get_parllink <- function(x) {
  page <- read_html(x)
  
  tibble(
    link = x,
    parl_link = page %>% 
      html_nodes(xpath = '//a[contains(text(), "IA") or contains(text(), "RV") or contains(text(), "AB")]') %>%
      html_attr('href'),
    initiated = page %>% 
      html_nodes(xpath = '//a[contains(text(), "IA") or contains(text(), "RV") or contains(text(), "AB")]') %>%
      html_text())
}

Laws_1980_parllinks <- future_map_dfr(Laws_1980$link, get_parllink, .progress = TRUE) %>%
  mutate(initiated = case_when(
    str_detect(initiated, "RV") ~ "Regierungsvorlage", 
    str_detect(initiated, "IA") ~ "Initiativantrag", 
    str_detect(initiated, "AB") ~ "Selbstständiger Antrag")) %>%
  arrange(initiated) %>%
  distinct(link, .keep_all = TRUE) 

Laws_1980 %>%
  left_join(Laws_1980_parllinks, by = "link") ->
  Laws_1980

# PARLIAMENT DATA

# we only need the legislators starting 2000 (also there's not enough info before)
Laws_1980 %>%
  mutate(year = str_extract(date, "\\d{4}")) %>%
  filter(year > 1999) ->
  Laws_2000

Laws_1980 %>%
  mutate(year = str_extract(date, "\\d{4}")) %>%
  filter(year < 2000) %>%
  select(-year) ->
  Laws_1980

# links to parlamentarians - this works the same way as for the 2004 data
Laws_2000_ini <- Laws_2000 %>%
  filter(initiated == "Initiativantrag")

Laws_2000_ini2 <- future_map_dfr(Laws_2000_ini$parl_link, get_2004_ini2, .progress = TRUE) %>%
  mutate(wwer_link = paste0("https://www.parlament.gv.at", wwer_link))

# which party do they belong to
Laws_2000_ini3 <- future_map_dfr(Laws_2000_ini2$wwer_link, get_2004_ini3, .progress = TRUE) %>%
  mutate(party = case_when(
    str_detect(party, "ÖVP|Volkspartei") ~ "OVP",
    str_detect(party, "SPÖ|Sozialdemokrat") ~ "SPO",
    str_detect(party, "Grün|GRÜN") ~ "G",
    str_detect(party, "NEOS") ~ "NEOS",
    str_detect(party, "FPÖ|Freheitlich|\\,\\sF\\b") ~ "FPO",
    str_detect(party, "Bündnis|BZÖ") ~ "BZO",
    str_detect(party, "Jetzt") ~ "LP",
    str_detect(party, "Stronach") ~ "TS",
    str_detect(party, "Forum") ~ "LF",
    str_detect(party, "Sobotka") ~ "OVP",
    str_detect(party, "Trutnov") ~ "OVP", # Sonderfall eines FPÖ Politikers
    TRUE ~ NA_character_
  )) %>%
  distinct(wwer_link, .keep_all = TRUE)

Laws_2000_ini2 %>% 
  left_join(Laws_2000_ini3, by = "wwer_link") %>% 
  group_by(parl_link) %>%
  select(-wwer_link) %>%
  mutate(group_id = row_number()) %>%
  ungroup() %>% 
  pivot_wider(names_from = group_id, values_from = c("party", "legislator_names"))->
  Laws_2000_ini2

Laws_2000_ini %>%
  left_join(Laws_2000_ini2, by = "parl_link") ->
  Laws_2000_ini

# For 2003 Regierungsvorlagen have information on the department on the parl page

Laws_2000 %>%
  filter(year == 2003) ->
  Laws_2003

# Regierungsvorlagen


get_department_2003 <- function(x) {
  page <- read_html(x)
  
  tibble(
    parl_link = x,
    department = page %>% 
      html_nodes(xpath = '//p[contains(text(), "Ressort")]') %>%
      html_text())

}

Laws_2003_rv2 <- Laws_2003 %>%
  filter(initiated == "Regierungsvorlage")

Laws_2003_rv <- future_map_dfr(Laws_2003_rv2$parl_link, get_department_2003, .progress = TRUE) %>%
  mutate(department = str_extract(department, ".*(ministerium|kanzler).*")) %>%
  distinct(parl_link, .keep_all = TRUE)

# XXX to many NAs
Laws_2003_rv2 %>% 
  left_join(Laws_2003_rv, by = "parl_link") ->
  Laws_2003_rv

Laws_2003_rv %>%
  filter(is.na(department)) %>%
  select(-department) ->
  Laws_2003_rv_man

Laws_2003_rv_man$department <- c(" BMJ (Bundesministerium für Justiz)", "BMI (Bundesministerium für Inneres)", "", "", "BMWUA (Bundesministerium für Wirtschaft und Arbeit)", "BMLFU (Bundesministerium für Land- und Forstwirtschaft, Umwelt und Wasserwirtschaft)", "", "", "BMSG (Bundesministerium für soziale Sicherheit und Generationen)", "https://www.ris.bka.gv.at/Dokument.wxe?ResultFunctionToken=96c49692-4ca7-4428-953a-ceb243eef77b&Position=1&Abfrage=BgblPdf&Titel=&Bgblnummer=&SucheNachGesetzen=True&SucheNachKundmachungen=False&SucheNachVerordnungen=False&SucheNachSonstiges=False&SucheNachTeil1=False&SucheNachTeil2=False&SucheNachTeil3=False&SucheNachTeilAlt=False&VonDatum=01.01.1979&BisDatum=31.12.2003&ImRisSeitVonDatum=01.01.1979&ImRisSeitBisDatum=31.12.2003&ImRisSeit=Undefined&ResultPageSize=100&Suchworte=&Dokumentnummer=2003_67_1
", "BMVIT (Bundesministerium für Verkehr, Innovation und Technologie)", "BMSG (Bundesministerium für soziale Sicherheit und Generationen) ", "BMWUA (Bundesministerium für Wirtschaft und Arbeit)", "BMJ (Bundesministerium für Justiz)", "", "", "", "BMJ (Bundesministerium für Justiz) ")
  
Laws_2003_rv_man -> Laws_2003_rv

# For 2000-2002, we must go through the Ausschussname
Laws_2000 %>% 
  filter(year < 2003) %>% 
  filter(initiated == "Regierungsvorlage") ->
  Laws_2000_rv

get_parlverf_2000 <- function(x) {
  page <- read_html(x)
  
  tibble(
    parl_link = x,
    parl_verf = page %>% 
      html_nodes(xpath = '//a[contains(text(), "Parlamentarisches")]') %>%
      html_attr("href"))
  
}

Laws_2000_rv2 <- future_map_dfr(Laws_2000_rv$parl_link, get_parlverf_2000, .progress = TRUE)  %>%
  mutate(parl_verf = paste0(parl_link, parl_verf))

get_ausschuss_2000 <- function(x) {
  page <- read_html(x)
  
  tibble(
    parl_verf = x,
    ausschuss = page %>% 
      html_nodes('.text') %>%
      html_text())
  
}

Laws_2000_rv3 <- future_map_dfr(Laws_2000_rv2$parl_verf, get_ausschuss_2000, .progress = TRUE) 

Laws_2000_rv3 %>%
  filter(str_detect(ausschuss, "Bericht") & str_detect(ausschuss, "(A|a)usschuss")) %>% 
  group_by(parl_verf) %>%
  summarise(ausschuss = paste(ausschuss, collapse=' ')) ->
  Laws_2000_rv3

Laws_2000_rv2 %>%
  left_join(Laws_2000_rv3, by = "parl_verf") %>%
  distinct(parl_link, .keep_all = TRUE) ->
  Laws_2000_rv2

Laws_2000_rv %>%
  left_join(Laws_2000_rv2, by = "parl_link") ->
  Laws_2000_rv

rm(Laws_2000_rv2, Laws_2000_rv3)

Laws_2000 %>%
  anti_join(Laws_2000_ini, by = "link") %>%
  anti_join(Laws_2003_rv, by = "link") %>%
  anti_join(Laws_2000_rv, by = "link") %>%
  bind_rows(Laws_2000_ini) %>%
  bind_rows(Laws_2003_rv) %>%
  bind_rows(Laws_2000_rv) ->
  Laws_2000

Laws_2000 %>%
  filter(!is.na(parl_link)) ->
  Laws_2000_votes

Laws_2000_votes <- future_map_dfr(Laws_2000_votes$parl_link, get_2004_votes,  .progress = TRUE) %>%
  mutate(votes = str_extract(votes, "(D|d)afür.*$")) %>%
  distinct(parl_link, .keep_all = TRUE)

Laws_2000 %>%
  left_join(Laws_2000_votes, by = "parl_link") ->
  Laws_2000

#combining the datasets----
Laws_1980 %>%
  bind_rows(Laws_2000) %>%
  bind_rows(Laws_2004_complete) ->
  Laws_complete

saveRDS(Laws_complete, "data/laws/Laws_complete.rds")
