library(rvest)
library(purrr)
library(furrr)
library(tidyverse)
plan(multisession)

# Downloading press releases 2005 - 2016 ----

# provided as pdf - download of pdfs

periods <- c(2001, 2003:2016) 

urls <- map_chr(periods, ~paste0("https://www.vfgh.gv.at/medien/news_",
                                 ., ".de.html"))

get_pdflinks <- function(url) {
  html_page <- read_html(url)
  
  tibble(
    link = html_page %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      str_subset("pdf") %>%
      paste0("https://www.vfgh.gv.at", .))
}

pdf_links <- map_dfr(urls, get_pdflinks) %>%
  rownames_to_column("number")

saveRDS(pdf_links, "data/pdf_links_early.RDS")

# download the pdfs
map(pdf_links$link,
       ~download.file(url = .x, 
                      destfile = paste0("prs/pdfs/", basename(.x)), 
                      mode = "wb"))

# Scrape info from table on website

download_table <- function(url) {
  html_page <- read_html(url)
  
  table <- html_page %>%
    html_nodes('table') %>%
    html_table()
}

tables <- map_dfr(urls, download_table) 

saveRDS(tables, "data/prs_early_table.rds")

# Downloading press releases 2017-2021 ----
# now everything available in html format - yeah

# creating a list of the links to the years
page_basic <- "https://www.vfgh.gv.at/medien/news_" #basic link
page_nmbr <- c(2017:2021) #years


pages_all <- map2_chr(page_basic, page_nmbr, paste0) # combining both

# we need to add .html at the end for it to work
url_all <- str_replace_all(pages_all, "$", ".de.html")


#Es wird eine Funktion gebaut, um den Link zum Dokument zu laden
scrape_links <- function(url) {
  page <- read_html(url)
  
  tibble(
    link = page %>%
      html_nodes(".black") %>%
      html_attr("href") %>%
      paste0("https://www.vfgh.gv.at", .))
}

links <- map_dfr(url_all, scrape_links) 

# scrape content
scrape_prs  <- function(link){
  page <- read_html(link)
  
  tibble(
    link = link,
    date = page %>%
      html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "date", " " ))]') %>%
      html_text(),
    header = page %>%
      html_nodes("h1") %>%
      html_text(),
    text = page %>%
      html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "text", " " ))]//p') %>%
      html_text() %>%
      paste(collapse = " "))
}

df_prs <- future_map_dfr(links$link, scrape_prs, .progress = TRUE)

# scrape whether they refer to a case 

scrape_case  <- function(link){
  page <- read_html(link)
  
  tibble(
    link = link,
    geschaeftszahl = page %>%
      html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "zahl", " " ))]') %>%
      html_text())
    
} 

df_case <- future_map_dfr(links$link, scrape_case, .progress = TRUE)  

# combine dfs on case and prs

df_prs <- df_prs %>%
  left_join(df_case, by = "link")

saveRDS(df_prs, "data/prs_2017on.RDS")

rm(df_case)

source("prs/2_pdf-to-df.R")