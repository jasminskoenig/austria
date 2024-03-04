Sys.setenv(LANG = "en")
library(rvest)
library(plyr)
library(tidyverse)
library(stringi)
library(xml2)

library(furrr)
plan(multiprocess)


#in lesbares Format wandeln 
page_basic <- "https://www.ris.bka.gv.at/Ergebnis.wxe?Abfrage=Vfgh&Entscheidungsart=Undefined&Sammlungsnummer=&Index=&SucheNachRechtssatz=False&SucheNachText=True&GZ=G*&VonDatum=05.12.2019&BisDatum=31.12.2020&Norm=&ImRisSeitVonDatum=&ImRisSeitBisDatum=&ImRisSeit=Undefined&ResultPageSize=100&Suchworte=&Position=1&SkipToDocumentPage=true"
page_basic_2021 <- "https://www.ris.bka.gv.at/Ergebnis.wxe?Abfrage=Vfgh&Entscheidungsart=Undefined&Sammlungsnummer=&Index=&SucheNachRechtssatz=False&SucheNachText=True&GZ=G*&VonDatum=01.01.2021&BisDatum=31.12.2021&Norm=&ImRisSeitVonDatum=&ImRisSeitBisDatum=&ImRisSeit=Undefined&ResultPageSize=100&Suchworte=&Position=1&SkipToDocumentPage=true"

#Es wird eine Funktion gebaut, um den Link zum Dokument zu laden
get_info <- function(x) {
  page <- read_html(x)
  
  tibble(
    link = page %>%
      html_nodes(".nonWrappingCell") %>%
      html_attr("href") %>%
      paste0("https://www.ris.bka.gv.at", .), 
    date = page %>%
      html_nodes(".bocListDataCell:nth-child(4) .bocListCommandText") %>%
      html_text(),
    type = page %>%
      html_nodes(".bocListDataCell:nth-child(5) .bocListContent") %>%
      html_text())
}

#Die Funktion wird aufgefuehrt und die Info geladen
data2020 <- map_dfr(page_basic, get_info)
saveRDS(data2020, "data/data2020.rds")

data2021 <- map_dfr(page_basic_2021, get_info)
saveRDS(data2021, "data/data2021.rds")

#Funktion um den Text der Sprueche herunter zu laden
get_sprueche <- function(x) {
  page <- read_html(x)
  
  tibble(
    link = x,
    spruch = page %>% 
      html_nodes(xpath = '//div[preceding-sibling::h3[contains(text(), "Spruch")]]') %>%
      html_text(),
    geschaeftszahl = page %>% 
      html_nodes(xpath = '//div[child::h3[contains(text(), "ftszahl")]]') %>%
      html_text())
}

#Ausfuehren der Funktion zum Sprueche laden
#genutzt wird der Link der im ersten Schritt herunter geladen wurde
data2020_sprueche <- future_map_dfr(data2020$link, get_sprueche, .progress = TRUE)
saveRDS(data2020_sprueche, "data/data2020_sprueche")

data2021_sprueche <- future_map_dfr(data2021$link, get_sprueche, .progress = TRUE)
saveRDS(data2021_sprueche, "data/data2021_sprueche")

#Funktion um den Text der EntscheidungsgrÃÂ¼nde herunter zu laden
get_entscheidung <- function(x) {
  page <- read_html(x)
  tibble(
    link = x,
    entscheidung = page %>% 
      html_nodes(xpath = '//div[preceding-sibling::h3[contains(text(), "Begr")]]') %>%
      html_nodes('p') %>%
      html_text() %>%
      paste(collapse = "\r\n"))
}

#Ausfuehren der Funktion zum Entscheidungstexte laden
#genutzt wird der Link der im ersten Schritt herunter geladen wurde
data2020_entscheidung <- future_map_dfr(data2020$link, get_entscheidung, .progress = TRUE) 
saveRDS(data2020_entscheidung, "data/data2020_entscheidung")

data2021_entscheidung <- future_map_dfr(data2021$link, get_entscheidung, .progress = TRUE) 
saveRDS(data2021_entscheidung, "data/data2021_entscheidung")

#---------------------------------------------------------------------------------------------------------
#Im Verlauf der Arbeit wurden einige Daten gebraucht die zuvor nicht geladen wurden, dies wird nun nachgeholt

#Funktion um Zeilenumbrüche beim scrapen zu behalten
preserv_linebreaks <- function(x) {
  xml_find_all(x, ".//br") %>% xml_add_sibling("p", "\r\n")
  #xml_find_all(x, ".//br") %>% xml_remove()
  x
}

#angewandte Normen laden
get_appliednorms <- function(x) {
  page <- read_html(x)
  
  tibble(
    link = x,
    applied = page %>% 
      html_nodes(xpath = '//div[child::h3[contains(text(), "Norm")]]') %>%
      preserv_linebreaks %>%
      html_text())
}

data2020_appliednorms <- future_map_dfr(data2020$link, get_appliednorms, .progress = TRUE)
saveRDS(data2020_appliednorms, "data/data2020_appliednorms.rds")

data2021_appliednorms <- future_map_dfr(data2021$link, get_appliednorms, .progress = TRUE)
saveRDS(data2021_appliednorms, "data/data2021_appliednorms.rds")

#Überprüfen ob Zeilenumbrüche geladen wurden
data2020_appliednorms %>%
  mutate(applied2 = str_count(applied, "\\r\\n")) %>%
  View()

data2020_appliednorms %>%
  mutate(applied2 = str_split(applied[1], "\\r\\n")) %>%
  View()

#--------------------------------------------------

#schlagworte wurden nachträglich geladen

#schlwagworte laden
get_catchwords <- function(x) {
  page <- read_html(x)
  
  tibble(
    link = x,
    catchwords = page %>% 
      html_nodes(xpath = '//div[child::h3[contains(text(), "Schlagworte")]]') %>%
      html_text())
}

data2020_catchwords <- future_map_dfr(data2020$link, get_catchwords, .progress = TRUE)
saveRDS(data2020_catchwords, "data/data2020_catchwords.rds")

data2021_catchwords <- future_map_dfr(data2021$link, get_catchwords, .progress = TRUE)
saveRDS(data2021_catchwords, "data/data2021_catchwords.rds")

#wird getrennt gespeichert und gesäubert, da dies bei den restlichen daten bereits passiert ist
G_catchwords <- G_catchwords %>%
  mutate(catchwords = str_to_lower(catchwords)) %>%
  mutate(catchwords = str_remove(catchwords, "schlagworte")) %>%
  mutate(catchwords = str_squish(catchwords))

#und zu den anderen daten hinzugefügt
G_sprueche <- G_sprueche %>%
  left_join(G_catchwords, by = "link")

#-----------------------------------------------
#index laden

get_index <- function(x) {
  page <- read_html(x)
  
  tibble(
    link = x,
    index = page %>% 
      html_nodes(xpath = '//div[child::h3[contains(text(), "Index")]]') %>%
      html_text())
}

#index speichern
data2020_index <- future_map_dfr(data2020$link, get_index, .progress = TRUE)
saveRDS(data2020_index, "data/data2020_index.rds")

data2021_index <- future_map_dfr(data2021$link, get_index, .progress = TRUE)
saveRDS(data2021_index, "data/data2021_index.rds")

#-----------------------------------------------------
plan(sequential)

#zu grossem Datensatz hinzufuegen + speichern
data2020 <- data2020 %>%
  left_join(data2020_sprueche, by = "link") %>%
  left_join(data2020_entscheidung, by = "link") %>%
  left_join(data2020_appliednorms, by = "link") %>%
  left_join(data2020_catchwords, by = "link") %>%
  left_join(data2020_index, by = "link")
saveRDS(data2020, "data/data2020raw.rds")

#zu grossem Datensatz hinzufuegen + speichern
data2021 <- data2021 %>%
  left_join(data2021_sprueche, by = "link") %>%
  left_join(data2021_entscheidung, by = "link") %>%
  left_join(data2021_appliednorms, by = "link") %>%
  left_join(data2021_catchwords, by = "link") %>%
  left_join(data2021_index, by = "link")
saveRDS(data2021, "data/data2021raw.rds")


data2020 <- data2020 %>%
  bind_rows(data2021)

saveRDS(data2020, "data/data20202021raw.rds")
