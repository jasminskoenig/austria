Sys.setenv(LANG = "en")

library(lubridate)
library(tidyverse)
library(readxl)
library(magrittr)
#library(quanteda)
#library(quanteda.textstats)
library(fuzzyjoin)
#library(spacyr)
#library(sophistication)
library(googlesheets4)

# judicial reviews data import ---- 
df_reviews_link <- readRDS("data/dataincl2020_final2.rds")

# for matching with manual hearings later
df_reviews_link |>  
  select(geschaeftszahl, link) |>
  rownames_to_column("id") |> 
  mutate(geschaeftszahl = if_else(geschaeftszahl == "g494/97 - g39; 40/98",
                                  "g494/97, g40/98",
                                  geschaeftszahl), # einzelfall
         geschaeftszahl = str_remove(geschaeftszahl, ".*zahl"),
         geschaeftszahl = if_else(
           str_detect(geschaeftszahl, "g208/2017-209/2017-8, g210/2017-211/2017-6, g222/2017-226/2017-4"),
           "g208/2017, 209/2017, g210/2017, 211/2017, g222/2017, 226/2017",
           geschaeftszahl)) |> 
  mutate(case = str_replace_all(geschaeftszahl, "\\;", "\\,"),
         case = str_replace_all(case, "\\(", "\\,"),
         case = str_replace_all(case, "\\)", "\\,"),
         case = str_remove_all(case, "ua"),
         case = str_remove_all(case, "\\-\\d{1,3}"),
         case = str_replace(case, "\\,\\s\\,", "\\,"),
         case = str_remove(case, "- b(1538/02|456/03)"), # einzelfall
         case = str_remove_all(case, "\\s")) |> 
  separate_rows(case, sep = ",") |> 
  mutate(case = str_replace(case, "\\/\\/", "\\/")) ->
  review_cases

# prs ----

df_allprs <- readRDS("prs/data/df_allprs.rds")

# cleaning 
df_allprs %>%
  mutate(date = dmy(date)) %>%
  mutate(year = year(date)) %>%
  mutate_if(is.character, str_squish) %>%
  # clean up how the geschaeftszahl is written, adapt to how review case numbers were cleaned
  mutate(case = str_remove_all(geschaeftszahl, "\\s")) %>% 
  mutate(case = str_remove_all(case, "((ua|u.a.|ua.|UA|U.A.|UA.)((?=g)|(?=$)))")) %>%
  mutate(case = str_replace_all(case, "(?<=\\d)(?=[a-zA-Z])", ", ")) %>%
  mutate(case = str_replace_all(case, c("[A-Za-zÀ-ÿ]*(G|g)|(G|g)[A-Za-zÀ-ÿ]*" = "g",
                                        "(?<=\\d)(?=[:alpha:])" = "\\,"))) %>% # replace all uag or ag combinations with g, add comma if two cases are written without sep
  # delete english press releases
  filter(!str_detect(header, "constitutional court")) %>%
  filter(!str_detect(text, "unconstitutional")) -> 
  df_allprs

handcoded_prs <- read_excel("manual/prs_2017on_filled.xlsx")

handcoded_prs |> 
  left_join(df_allprs |>  select(-geschaeftszahl), by = "link") |> 
  mutate(case = geschaeftszahl) -> 
  handcoded_prs_full

df_allprs |> 
  anti_join(handcoded_prs_full, by = "link") |> 
  rbind(handcoded_prs_full) -> 
  df_allprs_corrected


# # readability 
# 
# corpus_prs <- corpus(df_allprs_corrected)
# 
# # reading ease
# readability <- corpus_prs %>%
#   textstat_readability(measure = c("Flesch.Kincaid", "Flesch", "SMOG")) %>%
#   mutate(document = str_remove(document, "text"))
# 
# source("./prs/covars_make_baselines_CR.R")
# 
# corp_prs <- corpus_subset(corpus_prs, !is.na(year))
# 
# fam <- covars_make_baselines(corp_prs, baseline_data = "google", baseline_year = docvars(corp_prs, "year")) %>%
#   rownames_to_column(var = "document") %>%
#   mutate(document = str_remove(document, "text"))
# 
# pos <- covars_make_pos(corp_prs) %>%
#   rename(document = doc_id)

df_allprs_corrected %>%
  mutate(document = as.character(row_number())) %>%
 # left_join(readability, by = "document") %>% 
  #left_join(fam, by = "document") %>%
  #left_join(pos, by = "document") %>%
  mutate_if(is.character, str_to_lower) %>%
  dplyr::select(-document) -> # now we can str_to_lower, was needed before for flesch.kincaid
  df_allprs_corrected

# separate the case numbers (one case number per column)  
df_allprs_corrected %>%
  separate(case, into = c("case1", "case2", "case3", "case4", "case5"), sep = "\\,", remove = FALSE) %>% 
  mutate(case = str_remove(case, "(\\,$|[a-z]$)")) %>% 
  mutate(case5 = ifelse(str_detect(case4, "\\d(-|–)\\d"), str_extract(case4, ".*"), case5)) %>% # doing this in multiple lines because i want the hierarchy
  mutate(case5 = ifelse(str_detect(case3, "\\d(-|–)\\d"), str_extract(case3, ".*"), case5)) %>%  
  mutate(case5 = ifelse(str_detect(case2, "\\d(-|–)\\d"), str_extract(case2, ".*"), case5)) %>% # wenn bindestrich zwischen cases in case 1 oder 2 dann in case5 kopieren
  mutate(case5 = ifelse(str_detect(case1, "\\d(-|–)\\d") & str_detect(case1, "g"), str_extract(case1, ".*"), case5)) %>%
  mutate(across(c(case1, case2, case3, case4), ~ str_remove(., "(-|–)\\d+"))) %>% # case nach dem bindestrich wird in 1-4 entfernt
  mutate(case5 = str_remove(case5, "\\d+(-|–)"))  %>% #case vor dem bindestrich wird in case 5 entfernt
  mutate_at(vars(contains('case')), ~str_squish(.)) ->
  df_allprs_corrected


# add info on type of pr and length

df_allprs_corrected <- df_allprs_corrected %>%
  mutate(type_pr = case_when(
    str_detect(header, "öffentliche (mündliche |)verhandlung|interessiert*|session|vorschau") | str_detect(link, "session|vorschau") |  str_detect(text, "^die 14 verfassungsrichterinnen") ~ "Session",
    str_detect(header, "angelob|ruhestand|ausgeschrieb*|ausschreibung|verabschiedet|trauer|nachbesetzung|geburtstag|tritt zurück|verstorben|bundesregierung schlägt|ersatzmitglied|neue mediensprecherin")  | str_detect(link, "geburtstag|interessentinnen|angelob*|vorgeschlagen|ausschreibung|interim*") | str_detect(text, "trauert") ~ "Staff",
    str_detect(header, "verfassungstag|rede|vortrag|würdig|besuch|ausgezeich|tag der offenen|(150|100) jahre|neues buch|festschrift|auszeichnung|delegation|gespräch|konferenz|jubiläum|fachtagung|sechser-treffen|vfgh stellt sich|tätigkeitsbericht|macht schule|gemaelde") | str_detect(link, "rede|vfgh_auf_tour|besuch|vortra*|praesentation|verfassungstag|konferenz|taetigkeitsbericht|buchpraesentation|festsschrift") | str_detect(text, "delegation|konferenz|gedankenaustausch|festveranstaltung") ~ "Event",
    str_detect(header, "stellungnahme|arbeitsweise")  ~ "Other",
    str_detect(header, "erkenntnis|entscheidung|beschwerde|weist|antrag|gerechtfertigt|verfassungswidrig|verfassungskonform|gesetzwidrig|bestätigt|verfassungsgerichtshof hebt|prüft|zurückgewiesen|verletzt|verhältnismäßig") ~ "Decision",
    TRUE ~ "Decision"
  )) %>%
  mutate(pr_length = as.numeric(str_count(text)))

handcode_types <- read_excel("manual/prs_classification_filled.xlsx") 

handcode_types |> 
  mutate(link = str_squish(link)) |> 
  left_join(df_allprs_corrected |> select(-type_pr), by = "link") |> 
  # the sampling of the handcoding cases was done earlier - so some cases that were already filetered out are now back
  # these are removed
  filter(!is.na(text)) ->
  handcode_types_full


df_allprs_corrected |> 
  anti_join(handcode_types_full, by = "link")  |> 
  rbind(handcode_types_full) ->
  df_allprs_corrected

df_allprs_corrected |> 
  filter(date > ymd("2020-01-01") | type_pr == "Session") |> 
  filter(type_pr == "Session" | type_pr == "Decision") |> 
  select(link, starts_with("case")) |> 
  anti_join(handcoded_prs_full, by = "link") ->
  handcoding_cases

prs_handcoding_filled <- read_excel("manual/prs_handcoding_filled.xlsx") |> 
  mutate(across(starts_with("case"), ~str_squish(.x)))

prs_handcoding_filled |> 
  left_join(df_allprs_corrected |> select(-starts_with("case")), by = "link") ->
  prs_handcoding_complete

df_allprs_corrected |> 
  anti_join(prs_handcoding_complete, by = "link") |> 
  rbind(prs_handcoding_complete) |> 
  mutate(across(starts_with("case"), ~na_if(., "na")),
         across(starts_with("case"), ~na_if(., "n")),
         across(starts_with("case"), ~na_if(., "NA"))) ->
  df_allprs_correctcases

saveRDS(df_allprs_correctcases, "prs/data/df_allprs_clean.rds")

# hearings ----

# dataset for handlabelling
# df_allprs_correctcases |>  
#   filter(str_detect(header, "session")| str_detect(text, "mündlich")) |> 
#   select(link) -> 
#   muendlich1
# 
# df_allprs_correctcases |>
#   filter(type_pr == "Decision" | type_pr == "Session") |> 
#   filter(str_detect(header, "session")| str_detect(text, "mündlich|öffentlich|verkünd")) |> 
#   select(link) -> 
#   muendlich2
# 
# muendlich2 |> 
#   anti_join(muendlich1) ->
#   muendlich2

gs4_auth(cache = ".secrets", email = "j.s.koenig95@gmail.com")
gsheet <- "https://docs.google.com/spreadsheets/d/1q7rcMaoZ4YKKLAYLmqiWR_0QwYnQTmBjXY-5GKrRYE0/edit#gid=0"
gsheet2 <- "https://docs.google.com/spreadsheets/d/1ZOJiv8GSqeGOjKtKGvr_zDdNo-lT0I3kYQaHZHg3cgE/edit#gid=0"

#sheet_write(muendlich, gsheet)
#sheet_write(muendlich2, gsheet2)

# import handcoded data on hearings
hearings <- read_sheet(gsheet)
hearings2 <- read_sheet(gsheet2)

str_count(hearings2$case, ",")

# one row per case
hearings2 |> 
  separate_rows(case, sep = ",") ->
  hearings2_long

# same for other imported hearings dataset
hearings |> 
  pivot_longer(starts_with("case"), names_to = "number", values_to = "case") |> 
  filter(!is.na(case)) |> 
  select(-number) |> 
  mutate(case = str_squish(case)) ->
  hearings_long

#combine both handlabelled datasets into one with one row per hearings/case
hearings2_long |> 
  rbind(hearings_long) ->
  hearings_all

# only keep hearings for g cases
hearings_all |> 
  filter(!is.na(case)) |> 
  mutate(case = str_to_lower(case)) |> 
  filter(str_detect(case, "g")) |> 
  mutate(case = str_squish(str_remove_all(case, "[^\\d\\/\\-]"))) |> 
  mutate(case = paste0("g", case)) ->
  hearings_g

# diese variable sagt nicht so viel, aussagekräftiger in long format weil dort der genaue case angegeben wird
# add column to pr df whether a hearings is mentioned in pr
# issue: this is done on the pr level and not on the case level
df_allprs_correctcases |> 
  mutate(hearing_man = if_else(link %in% hearings_all$link, 1, 0)) ->
  df_allprs_correctcases

saveRDS(hearings_g, "prs/data/hearings-g.rds")
saveRDS(hearings_all, "prs/data/hearings-all.rds")

hearings_g |> 
  # replace nas with 0
  mutate(verkündung = replace_na(verkündung, 0),
         verhandlung = replace_na(verhandlung, 0)) |> 
  select(-link) |> 
  group_by(case) |> 
  # summarize which cases had a verkündung/verhandlung (this transforms everything from pr-case to case level)
  summarize(verkündung = sum(verkündung),
            verhandlung = sum(verhandlung)) |> 
  mutate(verkündung = if_else(verkündung > 0, 1, 0)) |> 
  mutate(verhandlung = if_else(verhandlung > 0, 1, 0)) ->
  hearings_g_formatching

# add information on hearings to review dataset
review_cases |> 
  left_join(hearings_g_formatching, by = "case") |> 
  mutate(verkündung = replace_na(verkündung, 0),
         verhandlung = replace_na(verhandlung, 0)) |> 
  select(-case) |> 
  group_by(id, geschaeftszahl, link) |> 
  summarize(verkündung = sum(verkündung),
            verhandlung = sum(verhandlung),
            .groups = "drop") |> 
  mutate(verkündung = if_else(verkündung > 0, 1, 0),
         verhandlung = if_else(verhandlung > 0, 1, 0)) ->
  review_hearing

df_reviews_link |> 
  left_join(review_hearing |>  select(link,
                                      verkündung,
                                      verhandlung),
            by = "link") ->
  df_reviews_hearing

# add PRs to review data ----
# wide to long for merge of prs 

# change prs into long format
df_allprs_correctcases %>%
  dplyr::select(-case) %>%
  pivot_longer(starts_with("case"), names_to = "case_number", values_to = "case") %>%
  filter(!is.na(case)) %>%
  mutate(case = str_remove_all(case, "-|–")) %>%
  rename(year_pr = year,
         date_pr = date,
         link_pr = link,
         geschaeftszahl_pr = geschaeftszahl,
         case_number_pr = case_number) %>%
  filter(type_pr == "Decision" | type_pr == "Session") %>% # we don't want to include announcements of public hearings
  filter(!str_detect(link_pr, "entscheidung.pdf")) %>%
  mutate(case = str_remove_all(str_squish(str_to_lower(case)), " ")) |> 
  distinct(case, text, .keep_all = TRUE) %>% # check whether some cases may be duplicated through pivot_longer
  group_by(case) %>%
  add_count() %>%
  filter(case_when(
    case == "g67/2019" & pr_length != 3258 ~ F,
    TRUE ~ T
  )) %>%
  slice_max(pr_length) -> # in some cases there is more than one pr - by choosing the longest one (except for case 67/2019) we choose the one which is published after the decision has been proclaimed
  df_allprs_long

#g67/2019 mittlere länge
#bei allen anderen die längste pr wählen

saveRDS(df_allprs_long, "data/prslong.RDS")

# how many cases are in the dataset multiple times?

df_allprs_long %>%
  group_by(case) %>%
  count() %>%
  ungroup() %>%
  group_by(n) %>%
  count()

df_allprs_formatching <- df_allprs_long |> 
  rename(header_pr = header,
         text_pr = text) |> 
  select(link_pr, date_pr, year_pr, type_pr, pr_length, case)

# reviews and prs are matched on a case level
# one case can have more than one pr
review_cases |> 
  left_join(df_allprs_formatching, 
            by = "case") |> 
  select(-type_pr, -case) |> 
  group_by(id) |> 
  mutate(count = row_number(id)) |> 
  filter(count < 5) |> 
  ungroup() |> 
  pivot_wider(values_from = c(link_pr, date_pr, year_pr, pr_length), 
              names_from = count)  |> 
  filter(!is.na(link_pr_1) | !is.na(link_pr_2) | !is.na(link_pr_3) | !is.na(link_pr_4)) |> 
  select(-geschaeftszahl) ->
  review_cases_prs

# match pr and hearing info on case level - this is now in the review dataset
df_reviews_hearing |> 
  left_join(review_cases_prs,
            by = "link") |> 
  mutate(pr_dummy_new = if_else(
    !is.na(link_pr_1) | !is.na(link_pr_2) | !is.na(link_pr_3) | !is.na(link_pr_4), 1, 0
    )) ->
  df_reviews_hearing_pr

saveRDS(df_reviews_hearing_pr, "data/dataincl2020_final_inclpr.rds")

