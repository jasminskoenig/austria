# INTERVIEWS

# Libraries ----
library(tesseract)
library(tidyverse)
library(googlesheets4)
library(patchwork)
library(ggimage)
library(camcorder)
library(ggrepel)
library(cropcircles)
library(ggtext)
library(showtext)
library(sysfonts)

# graphics
source("src/graphics.R")

# meta data

interviews_meta <- read_sheet("https://docs.google.com/spreadsheets/d/19wdsLpZO6KC48Diky4Ef8BnwhQeXh1WMYM5ewFOR-dA/edit?hl=de#gid=0")

interviews_meta |> 
  filter(!is.na(doc_name)) |> 
  mutate(date = ymd(date),
         year = year(date)) ->
  interviews_meta

# interview text 
# the commented out code imports the pdfs into a dataframe, now we can just load it

# pdfs to text column
# files <- list.files(path = "data/interviews",
#                     pattern = "pdf$")
# 
# files <- paste0("data/interviews/", files)
# 
# deu <- tesseract("deu")
# 
# interviews <- lapply(files, tesseract::ocr, engine = deu)
# 
# max_length <- max(sapply(interviews, length))
# 
# # Fill shorter sub-lists with NAs to make them all the same length
# interviews <- lapply(interviews, function(x) c(x, rep(NA, max_length - length(x))))
# 
# # Create a data frame from the list of lists
# df <- as.data.frame(do.call(rbind, interviews)) |> 
#   rowid_to_column() |> 
#   pivot_longer(cols = !rowid,
#                names_to = "page",
#                values_to = "text")

# saveRDS(df, "data/interviews/interviews.rds")

df_interviews <- readRDS("data/interviews/interviews.rds") |> 
  rename("docid" = "rowid") |> 
  mutate(page = str_remove(page, "V"))

df_interviews |> 
  mutate(text = str_remove(text, ".*"),
         text = str_remove(text, "^\\n"),
         page = as.numeric(page),
         date = if_else(page == 1, if_else(str_detect(text, "\\d{1,2}\\.\\d{1,2}\\.\\d{4}"),
                                                      str_extract(text, "\\d{1,2}\\.\\d{1,2}\\.\\d{4}"),
                                                      str_extract(text, "\\d{4}\\-\\d{1,2}\\-\\d{1,2}")), 
                                           str_extract(text, "\\d{1,2}\\.\\d{1,2}\\.\\d{4}", 
                                           NA)),
         date = if_else(str_detect(text, "\\d{1,2}\\.\\d{1,2}\\.\\d{4}"), dmy(date), ymd(date))) ->
  df_interviews_cleandate

# overview of newspapers

interviews_meta |> 
  mutate(Newspaper = str_remove_all(doc_name, "\\d"),
         Newspaper = case_when(
           Newspaper == "kz" ~ "Kleine Zeitung",
           Newspaper == "nvt" ~ "Neue Vorarlberger Tageszeitung",
           str_detect(Newspaper, "(o|)on$") ~ "Oberösterreiche Nachrichten",
           Newspaper == "sn" ~ "Salzburger Nachrichten",
           Newspaper == "tt" ~ "Tiroler Tageszeitung",
           Newspaper == "vn" ~ "Vorarlberger Nachrichten",
           Newspaper == "wz" ~ "Wiener Zeitung",
           TRUE ~ str_to_title(Newspaper)
         )) |> 
  group_by(Newspaper) |> 
  summarize(`Earliest Date`= min(year, na.rm = TRUE),
            `Latest Date` = max(year, na.rm = TRUE),
            `Total` = n()) |> 
  filter(Newspaper != "Apa") ->
  interviews_summary

saveRDS(interviews_summary, "data/interviews/interviews_summary.rds")

# descriptives

articles <- readRDS("C:/Users/Jasmin/OneDrive - Universität Hamburg/Desktop/Dissertation/standard/data/articles_gptclassified.rds")

pres <- read_sheet("https://docs.google.com/spreadsheets/d/1m8-y_UbyuTEJ0fr8Ooy7u-b79Ai3UscGPAwn-XqDgks/edit#gid=0") |> 
  mutate(lastname = str_extract(president, "\\b\\S*$$")) |> 
  mutate(path = paste0("data/presidents/", str_to_lower(lastname), ".jpg"),
         images_cropped = if_else(file.exists(path), circle_crop(path), NA))

articles |> 
  mutate(year = year(date)) |> 
  group_by(conflict, year) |> 
  count() |> 
  filter(conflict == 1) ->
  attacks

interviews_meta |> 
  group_by(year) |> 
  count() ->
  interviews_year


add_year <- tibble(
  year = c(2000, 2015, 2016, 2022),
  n = 0
)

interviews_year |>
  rbind(add_year) ->
  interviews_year

pres |> 
  mutate(reign = interval(ymd(start), ymd(end)),
         start = year(start),
         end = year(end)) |> 
  mutate(start = if_else(president == "Ludwig Adamovich", 2000, start),
         end = if_else(is.na(end), 2023, end),
         test =  lead(start) == end,
         xmax =  if_else(is.na(lead(start)) | lead(start) != end, end, end - 0.1),
         xmin = if_else(lag(end) == start, start + 0.1, start),
         middle = (end - start) / 2 + start,
         label_pos = case_when(
           president == "Brigitte Bierlein" ~ middle - 0.8,
           president == "Christoph Grabenwarter" ~ middle + 185,
           TRUE ~ middle
         ),
         justification = case_when(
           president == "Brigitte Bierlein" ~ 0.6,
           president == "Christoph Grabenwarter" ~ 0.4,
           TRUE ~ 0.5
         )) |> 
  filter(end > 2000) ->
  pres_manipulated

gg_record(
  dir = "manual/temp_plots",
  device = "png",
  width = 15,
  height = 10,
  units = "cm",
  dpi = 600
)

vfgh_col <- "#786AA0"
vfgh_col <- "#33256C"

upperend <- -0.8
lowerend <- -1.2

# PAPER VERSION!!!
# Possible Title "Interviewing the Guardian of the Constitution"
interviews_year |>
    ggplot() +
    geom_col(aes(x = year, 
                 y = n),
             fill = color_dark) +
    geom_segment(data = pres_manipulated, mapping = aes(
      x = xmin,
      xend = xmax,
      y = lowerend,
      yend = lowerend
    ),
    linewidth = 0.1) +
  geom_segment(data = pres_manipulated |> filter(president != "Christoph Grabenwarter"),
                 mapping = aes(x = xmax, 
                               xend =  xmax, 
                               y = lowerend, 
                               yend = upperend),
               linewidth = 0.1) +
  geom_segment(data = pres_manipulated |>  
                 filter(president != "Ludwig Adamovich"), 
               mapping = aes(x = xmin, 
                             xend =  xmin, 
                             y = lowerend, 
                             yend = upperend),
               linewidth = 0.1) +
  geom_segment(data = pres_manipulated, aes(x = middle,
                                            xend = middle,
                                            y = lowerend,
                                            yend = lowerend - 2),
               linewidth = 0.1) +
  geom_text(aes(x = year, 
                y = -0.35, 
                label = paste0("'", str_extract(year, "\\d\\d$"))),
            size = 12.5,
            color = color_darkgrey) +
  geom_image(data = pres_manipulated, 
             mapping = aes(y = lowerend - 1.5, 
                           x = middle, 
                           image = images_cropped),
             size = 0.13) +
  geom_text(data = pres_manipulated, 
                mapping = aes(y = lowerend - 3.15, 
                              x = middle, 
                              label = president,
                              hjust = justification),
             size = 11,
             fontface = "bold",
             color = color_darkgrey) +
  # geom_richtext(aes(x = 1999,
  #                   y = 15,
  #                   label = "Interviews with the <span style='color:#33256C'> Guardian of the Constitution</span>"),
  #               hjust = 0,
  #               fill = NA, label.color = NA,
  #               fontface = "bold",
  #               size = 8) +
  # geom_richtext(aes(x = 2002,
  #                   y = 13,
  #                   label = "Public Commnication is a key for the work of constitutional courts. 
  #                   Constitutional courts can not enforce their decisions, their power is based on the societal 
  #                   acceptance. 
  #                   With the increasing judicialization of politics, constitutional courts have to decide on more
  #                   polarizing issues for which they need to explain there decisions."),
  #               hjust = 0,
  #               fill = NA, label.color = NA,
  #               size = 6) +
  scale_y_continuous(limits = c(-4.5, 12), 
                     breaks = seq(from = 2, to = 12, by = 2),
                     expand = c(0,0)) +
  scale_x_continuous(expand = c (0,0),
                     limits = c(2000, 2024)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        panel.grid.major.y = element_line(linewidth = 0.1,
                                          color = "lightgrey"),
        axis.text.y = element_text(size = 35,
                                   color = "#636363")) 
ggsave("results/images/interview.pdf",
       device = cairo_pdf,
       height = 16,
       width = 30)

attacks |> 
  ggplot() +
  geom_col(aes(x = year, y = n)) +
  geom_line(data = interviews_year, aes(x = year, y = n), color = "red")


attacks |> 
  filter(year != 2000) |> 
  ggplot() +
  geom_col(aes(x = year, y = n)) +
  geom_text(aes(x = year, y = -10, label = year)) +
  scale_x_reverse() +
  scale_y_reverse() +
  coord_flip() +
  labs(y = "",
       x = "") +
  theme(axis.text.y.left = element_blank(),
        panel.border = element_blank()) ->
  attacks_plot

interviews_year |> 
  filter(year != 2023) |> 
  ggplot() +
  geom_col(aes(x = year, y = n)) +
  scale_x_reverse() +
  coord_flip()  +
  labs(y = "",
       x = "") +
  ylim(-3,20) +
  theme(axis.text.y.left = element_blank(),
        panel.border = element_blank()) ->
  interviews_plot

attacks_plot + interviews_plot

df_interviews_cleandate |> 
  mutate(year = year(date)) |> 
  ggplot() +
  geom_histogram(aes(x = year)) +
  geom_text(aes(x = year, label = year, y = 10)) +
  ylim(0,15)

library(quanteda)
library(seededlda)

corp_interviews <- corpus(df_interviews_cleandate)

toks_news <- tokens(corp_interviews, remove_punct = TRUE, remove_numbers = TRUE, remove_symbol = TRUE)
toks_news <- tokens_remove(toks_news, pattern = c(stopwords("de"), "*-time", "updated-*", "gmt", "bst"))
dfmat_news <- dfm(toks_news) %>% 
  dfm_trim(min_termfreq = 0.8, termfreq_type = "quantile",
           max_docfreq = 0.1, docfreq_type = "prop")
tmod_lda <- textmodel_lda(dfmat_news, k = 7)
terms(tmod_lda, 10)
