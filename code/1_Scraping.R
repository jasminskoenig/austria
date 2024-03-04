Sys.setenv(LANG = "en")
library(rvest)
library(plyr)
library(tidyverse)
library(stringi)

library(furrr)
plan(multiprocess)


#Laden der Daten aus dem Rechtsinformationssystem des Bundes
#HTML einlesen
page <- read_html("https://www.ris.bka.gv.at/Ergebnis.wxe?Abfrage=Vfgh&Entscheidungsart=Undefined&Sammlungsnummer=&Index=&SucheNachRechtssatz=False&SucheNachText=True&GZ=G*&VonDatum=01.01.1980&BisDatum=09.12.2019&Norm=&ImRisSeitVonDatum=&ImRisSeitBisDatum=&ImRisSeit=Undefined&ResultPageSize=100&Suchworte=&Position=1")

#Hmtl ist eine Liste, eine zweite die Nummern am Ende
page_basic <- "https://www.ris.bka.gv.at/Ergebnis.wxe?Abfrage=Vfgh&Entscheidungsart=Undefined&Sammlungsnummer=&Index=&SucheNachRechtssatz=False&SucheNachText=True&GZ=G*&VonDatum=01.01.1980&BisDatum=09.12.2019&Norm=&ImRisSeitVonDatum=&ImRisSeitBisDatum=&ImRisSeit=Undefined&ResultPageSize=100&Suchworte=&Position="
page_nmbr <- c(1, 101, 201, 301, 401, 501, 601, 701, 801, 901, 1001, 1101, 1201, 1301, 1401, 1501, 1601, 1701, 1801, 1901, 2001, 2101, 2201, 2301, 2401, 2501, 2601, 2701, 2801, 2901, 3001, 3101)

#HTML und Nummern werden zusammengefuegt
pages_all <- map2_chr(page_basic, page_nmbr, paste0)

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
G_data <- map_dfr(pages_all, get_info)
saveRDS(G_data, "data/raw/G_data.rds")

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
G_sprueche <- future_map_dfr(G_data$link, get_sprueche, .progress = TRUE)
saveRDS(G_sprueche, "data/raw/G_sprueche_data.rds")

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
G_entscheidung <- future_map_dfr(G_data$link, get_entscheidung, .progress = TRUE) 
saveRDS(G_entscheidung, "data/raw/G_entscheidung_data.rds")

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

G_applied_norms <- future_map_dfr(G_sprueche$link, get_appliednorms, .progress = TRUE)
saveRDS(G_applied_norms, "data/raw/G_applied_norms.rds")

#Überprüfen ob Zeilenumbrüche geladen wurden
G_applied_norms %>%
  mutate(applied2 = str_count(applied, "\\r\\n")) %>%
  View()

G_applied_norms %>%
  mutate(applied2 = str_split(applied[1], "\\r\\n")) %>%
  View()

#--------------------------------------------------

# schlagworte wurden nachträglich geladen

# schlagworte laden
get_catchwords <- function(x) {
  page <- read_html(x)
  
  tibble(
    link = x,
    catchwords = page %>% 
      html_nodes(xpath = '//div[child::h3[contains(text(), "Schlagworte")]]') %>%
      html_text())
}

G_catchwords <- future_map_dfr(G_sprueche$link, get_catchwords, .progress = TRUE)

saveRDS(G_catchwords, "data/raw/G_catchwords.rds")

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
G_index <- future_map_dfr(G_sprueche$link, get_index, .progress = TRUE)
saveRDS(G_index, "data/raw/G_index.rds")

#-----------------------------------------------------

#zu großem Datensatz hinzufügen + speichern
G_sprueche <- G_sprueche %>%
  select(-date) %>%
  left_join(G_data, by = "link") %>%
  left_join(G_applied_norms, by = "link") %>%
  left_join(G_index, by = "link") %>%
  left_join(G_catchwords, by = "link")

saveRDS(G_sprueche, "data/raw/G_sprueche.rds")

plan(sequential)

