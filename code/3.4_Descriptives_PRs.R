library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(RColorBrewer)
library(zoo)
library(readxl)
library(lubridate)
library(ggthemes)
library(janitor)
library(ggtext)

# data ----

df_allprs <- readRDS("prs/data/df_allprs_clean.RDS")

# import data ----


## caselevel data ----

caselevel_dec <- readRDS("data/dataincl2020_final_attacks.rds") |> 
  mutate(threshold = as.integer(if_else(date_2 > ymd("2020-02-11"), 1, 0)),
         pr_dummy_new = as.integer(pr_dummy_new),
         covid = as.integer(covid),
         politician = as.integer(if_else(plaintiff_category == "Politician", 1, 0)),
         court = as.integer(if_else(plaintiff_category == "Court", 1, 0)),
         start_session = as.factor(start_session),
         year = year(date_2)) |> 
  filter(!is.na(share_attacks_break) & date_2 > ymd("2015-01-01")) |> 
  mutate(decision = if_else(str_detect(decision, "sustained"), 1, 0))

# select relevant variables
caselevel_dec <- caselevel_dec |> 
  dplyr::select(pr_dummy_new, date_2, decision, year, type, gov_brief, court, verhandlung, verkündung, politician, court_president, share_attacks_break, covid, threshold, politician, start_session) 

caselevel_dec <- na.omit(caselevel_dec)

## normlevel data ----

normlevel_dec <- readRDS("data/G_analysis_R.rds") |> 
  filter(!is.na(share_attacks_break) & date_2 > ymd("2015-01-01")) 

normlevel_dec |> 
  filter(date_2 > ymd("2016-01-01")) ->
  normlevel_dec_pa
# 
# df_reviews_prs_bund <- readRDS("data/G_analysis_R.RDS") %>%
#   mutate(court_president = as.factor(court_president)) |> 
#   mutate(date_review = date_2) |> 
#   filter(!is.na(norm)) %>%
#   mutate(year_factor = as.factor(year(date_review)))
# 
# 
# df_reviews_prs_bund_2000_e <- df_reviews_prs_bund %>%
#   filter(date_2 > 1999) %>%
#   filter(type == "erkenntnis")
# 
# df_reviews_prs_bund_2005_e <- df_reviews_prs_bund %>%
#   filter(date_2 > ymd("2005-12-31")) %>%
#   filter(type == "erkenntnis")
# 
# df_reviews_prs_bund_2000 <- df_reviews_prs_bund %>%
#   filter(date_2 > 1999) 
# 
 # df_reviews_prs_bund_2005 <- df_reviews_prs_bund %>%
 #  filter(date_2 > ymd("2005-12-31")) 
# 
# df_reviews_prs_bund_2010_e <- df_reviews_prs_bund %>%
#   filter(date_2 > 2008) %>%
#   filter(type == "erkenntnis")
# 
# df_reviews_prs_bund_2010 <- df_reviews_prs_bund %>%
#   filter(date_2 > 2008) 
# 
# df_reviews_prs_bund %>%
#   filter(!str_detect(government, "Kurz I")) %>%
#   filter(date_2 > 2004) ->
#   df_reviews_prs_bund_nok
# 
# df_reviews_prs_bund %>%
#   filter(!str_detect(government, "Schuessel")) %>%
#   filter(date_2 > 2004) ->
#   df_reviews_prs_bund_nos
# 
# df_reviews_prs_bund %>%
#   group_by(pop_origin, oralhearing) %>%
#   count()

# set theme----

source("src/graphics.R")

# Descriptives ----

## IN PAPER ----

###  histograms ----

hist_verkündung <- caselevel_dec |> 
  filter(verkündung == 1) |> 
  group_by(year) |> 
  arrange(share_attacks_break) |> 
  mutate(id = row_number(),
         pressactivity = "Announcement") 

hist_verhandlung <- caselevel_dec |> 
  filter(verhandlung == 1) |> 
  group_by(year) |> 
  arrange(share_attacks_break) |> 
  mutate(id = row_number(),
         pressactivity = "Hearing") 

hist_pr <- caselevel_dec |> 
  filter(pr_dummy_new == 1) |> 
  group_by(year) |> 
  arrange(share_attacks_break) |> 
  mutate(id = row_number(),
         pressactivity = "Press Release") 

data <- rbind(hist_pr, hist_verhandlung, hist_verkündung)

theme_update(panel.spacing = unit(4, "lines"),
             legend.spacing = grid::unit(c(0, 2, 2, 2), "mm"),
             legend.title.align = 0.5,
             legend.margin = margin(t = 0),
             axis.title.x = element_text(hjust = 0.5,
                                         margin = margin(t = 50, b = 0)),
             plot.margin = margin(b = 0)) 

histogram <- data |> 
  ggplot(aes(x = year, 
             y = id,
             color = share_attacks_break)) +
  geom_point(size = 2) +
  facet_wrap(~ pressactivity) +
  scale_x_continuous(breaks = c(2015, 2017, 2019, 2021),
                     limits = c(2015, 2021),
                     expand = c(0,0)) +
  scale_y_continuous(breaks = c(5,10,15,20,25),
                     expand = c(0,0),
                     limits = c(0,25)) +
  scale_color_gradientn(colors = c('#b2b9af', '#a0a89c', '#8e9889', '#7c8877', '#6b7865', '#5a6854', '#495944', '#394a33', '#2a3c24'),
                       limits = c(round(min(data$share_attacks_break), 1),
                                  round(max(data$share_attacks_break), 1)),
                       breaks = c(round(min(data$share_attacks_break), 1),
                                  round((max(data$share_attacks_break) -
                                    min(data$share_attacks_break))/2
                                    + min(data$share_attacks_break), 1),
                                  round(max(data$share_attacks_break), 1)),
                       guide = guide_colorbar(barwidth = 10)) +
  coord_cartesian(clip = "off") +
  labs(y = "N",
       x = "Average Number of Reports on Attacks Ahead of Session per Day",
       color = element_blank()) 

histogram

ggsave("results/images/histogram_communication.pdf",
       device = cairo_pdf,
       height = 6,
       width = 10)

saveRDS(histogram, "results/images/histogram_publicity.RDS")

### PR Types per Year ----

# type per year

df_allprs %>%
  mutate(type_pr = case_when(
    type_pr == "Decision" & str_detect(case, "g") ~ "Decision" , 
    type_pr == "Session" ~ "Decision",
    type_pr == "OH" ~ "Decision",
    is.na(type_pr) ~ "Other",
    TRUE ~ type_pr)) |> 
  group_by(year, type_pr) %>%
  count() %>%
  ungroup() %>%
  complete(year, type_pr,
           fill = list(n = 0)) ->
  plotdata

theme_update(plot.margin = margin(r = 30))

plotdata %>%  
  filter(type_pr != "Decision") |> 
  mutate(adjust = case_when(
    type_pr == "Decision" ~ 0.61,
    type_pr == "Event" ~ 0.8,
    type_pr == "Staff" ~ 0.9,
    type_pr == "Other" ~ 0.61
  )) |> 
  ggplot(aes(x = year, y = n, linetype = type_pr)) +
  #geom_line(color = color_dark_light) +
  scale_linetype_manual(values = c("dotted", "longdash", "solid", "dotdash", "dashed"),
                        labels = c("Event", "Oral Hearing", "Other", "Decision", "Staff")) +
  geomtextpath::geom_textline(data = plotdata %>%  filter(type_pr == "Decision"),
                              aes(x = year, y = n, linetype = "solid", label = type_pr),
                              color = "darkred",
                              hjust = 0.61,
                              show.legend = FALSE,
                              size = 5.5) +
  geomtextpath::geom_textline(aes(x = year, y = n, linetype = type_pr, label = type_pr, hjust = adjust),
                              color = color_dark_light, 
                              size = 5.5,
                              show.legend = FALSE) +
  # geom_text(data = plotdata |>  filter(year == 2021),
  #           aes(x = 2022, 
  #               y = n,
  #               label = type_pr)) +
  labs(linetype = "Content") +
  scale_x_continuous(limits = c(2015, 2021),
                     expand = c(0,0)) +
  labs(x = element_blank(),
       y = "Press Releases")

ggsave("prs/results/pr_type.png", device = "png")
ggsave("results/images/pr_type.pdf", width = 23, height = 13, units = "cm", device = cairo_pdf)

plotdata |> 
  ggplot(aes(x = year, y = n, fill = type_pr)) +
  geom_area()
  
### Shares of populist legislators, hearings and PRs

df_reviews_prs_bund %>%
  group_by(year(date_review)) %>%
  count() |> 
  clean_names() ->
  total

df_reviews_prs_bund %>%
  filter(verhandlung == 1) %>%
  group_by(year(date_review)) %>% 
  count() %>%
  rename(n_ph = n) |> 
  clean_names()->
  ph

df_reviews_prs_bund %>%
  filter(verkündung == 1) %>%
  group_by(year(date_review)) %>% 
  count() %>%
  rename(n_pro = n) |> 
  clean_names()->
  pro

df_reviews_prs_bund %>%
  filter(pr_dummy_new == 1) %>%
  group_by(year(date_review)) %>%
  count()%>%
  rename(n_pr = n)|> 
  clean_names() ->
  pr

df_reviews_prs_bund %>%
  filter(pop_origin == 1) %>%
  group_by(year(date_review)) %>%
  count() %>%
  rename(n_po = n) |> 
  clean_names()->
  po

total %>%
  left_join(ph, by = "year_date_review") %>%
  left_join(pr, by = "year_date_review") %>% 
  left_join(po, by = "year_date_review") %>% 
  left_join(pro, by = "year_date_review") %>% 
  mutate(n_pr = ifelse(is.na(n_pr), 0, n_pr)) %>%
  mutate(n_ph = ifelse(is.na(n_ph), 0, n_ph)) %>%
  mutate(n_po = ifelse(is.na(n_po), 0, n_po)) %>%
  mutate(n_pro = ifelse(is.na(n_pro), 0, n_pro)) %>%
  mutate(share_ph = n_ph/n*100) %>%
  mutate(share_pr = n_pr/n*100) %>% 
  mutate(share_po = n_po/n*100) %>% 
  mutate(share_pro = n_pro/n*100) %>% 
  dplyr::select(year_date_review, share_ph, share_pr, share_po, share_pro) |> 
  pivot_longer(starts_with("share"), names_to = "type", values_to = "share") %>%
  mutate(share = as.numeric(share))  ->
  total_shares

ggplot(total_shares) +
  geom_vline(xintercept = 2020, alpha = 0.2) +
  geom_line(aes(x = year_date_review, y = share, color = type, linetype = type)) +
  scale_color_tableau(direction = -1, labels = c("Public Hearing", "Populist Legislator", "Press Release", "Pronouncement"),
                      name = "Share of") +
  scale_linetype_manual(name = "Share of",
                        values = c("dotted", "dotdash", "solid", "dashed"),
                        labels = c("Public Hearing", "Populist Legislator", "Press Release", "Pronouncement")) +
  xlab("Year") +
  ylab("Share") +
  xlim(2015,2021) 

ggsave("prs/results/shares.pdf", width = 15, height = 8, units = "cm", device = cairo_pdf)
ggsave("prs/results/shares.png", width = 15, height = 8, units = "cm", device = "png")

total_shares |> 
  filter(type != "share_po") |> 
  ggplot() +
  geom_vline(xintercept = 2020, alpha = 0.2) +
  geom_line(aes(x = year_date_review, y = share, color = type, linetype = type)) +
  geom_curve(
    aes(x = 2020.2, y = 30, xend = 2020, yend = 25),
    arrow = arrow(length = unit(0.08, "inch")), linewidth = 0.1,
    color = "darkgrey", curvature = -0.3) +
  geom_text(aes(x = 2020.3, y = 31, label = "Treatment"),
            size = 8, family = fontname, color = "darkgrey") +
  scale_color_tableau(direction = -1, labels = c("Public Hearing", "Populist Legislator", "Press Release", "Pronouncement"),
                      name = "Share of") +
  scale_linetype_manual(name = "Share of",
                        values = c("dotted", "dotdash", "solid", "dashed"),
                        labels = c("Public Hearing", "Populist Legislator", "Press Release", "Pronouncement")) +
  xlab("Year") +
  ylab("Share") +
  xlim(2015,2021) 

ggsave("prs/results/shares_treatment.pdf", width = 33, height = 16, units = "cm", device = cairo_pdf)
ggsave("prs/results/shares_treatment.png", width = 15, height = 8, units = "cm", device = "png")

df_allprs_long |> 
  filter(str_detect(case, "g")) |> 
  mutate(year_pr = year(date_pr)) |> 
  group_by(year_pr) |> 
  count() |> 
  ggplot(aes(x = year_pr, y = n)) +
  geom_line() +
  xlim(2015, 2021)

### Trust in VfGH ----

trust <- read_excel("manual/Vertrauensindex_Institutionen_Zeitreihe_2011-2021.xlsx") %>%
  rename(Government = Regierung, Parliament = Parlament, Judiciary = Justiz) %>%
  pivot_longer(cols = c("Government", "Parliament", "Judiciary", "VfGH"), names_to = "institution", values_to = "saldo")

trust %>%
  ggplot() +
  geom_line(aes(x = year(Erhebungszeitraum), y = saldo, color = institution)) +
  ylab("Trust Saldo") +
  xlab("Year") +
  scale_color_tableau(name = "") +
  scale_x_continuous(breaks = c(2011,2012,2016, 2019, 2021)) +
  ylim(-50, 50)+
  theme(legend.position = "right")

ggsave("results/trust.pdf", width = 14, height = 5, units = "cm", device = cairo_pdf)




## NOT IN PAPER ----

### prs per year ----

# per year
df_allprs %>%
  group_by(year) %>%
  count() %>% 
  ggplot() +
  geom_line(aes(x = year, y = n)) +
  xlab("Year") +
  theme_ipsum_rc()

# only g cases

df_allprs %>%
  filter(str_detect(case, "g")) %>%
  group_by(year(date)) %>%
  count() %>%
  rename(year = 1) %>%
  ggplot() +
  geom_line(aes(x=year, y = n))

# type per month

df_allprs %>%
  filter(!is.na(date)) %>%
  mutate(month = month(date))%>%
  group_by(month, year, type_pr) %>%
  summarize(N = n()) %>%
  ungroup() %>% 
  mutate(date = paste(year, month, sep = "-")) %>%
  mutate(date = ym(date)) %>%
  complete(date = seq.Date(min(date), max(date), by="month"), type_pr,
           fill = list(N = 0)) %>% 
  ggplot(aes(x = date, y = N, fill = type_pr)) +
  geom_area() +
  scale_fill_brewer(palette = "PuBu", direction = -1) +
  scale_x_date(date_labels = "%m-%Y")+
  xlab("Year") 

ggsave("prs/results/pr_type_months.png", device = "png")

df_allprs %>%
  filter(!is.na(date)) %>%
  filter(date > "2015-01-01") %>%
  mutate(month = month(date))%>%
  group_by(month, year, type_pr) %>%
  summarize(N = n()) %>%
  ungroup() %>% 
  mutate(date = paste(year, month, sep = "-")) %>%
  mutate(date = ym(date)) %>%
  complete(date = seq.Date(min(date), max(date), by="month"), type_pr,
           fill = list(N = 0)) %>% 
  ggplot(aes(x = date, y = N, fill = type_pr)) +
  geom_area() +
  scale_fill_brewer(palette = "PuBu", direction = -1) +
  scale_x_date(date_labels = "%m-%Y")+
  xlab("Year") +
  theme_ipsum_rc() + 
  geom_vline(xintercept = ymd("2017-10-01"), 
             color = "red", size = 0.5)



ggsave("prs/results/pr_type_months_zoomedin.png", device = "png")


# type without staff
df_allprs %>%
  filter(type_pr != "Staff" & type_pr != "Event") %>%
  group_by(year, type_pr) %>%
  summarize(N = n()) %>%
  ungroup() %>%
  complete(year, type_pr,
           fill = list(N = 0)) %>% 
  ggplot(aes(x = year, y = N, fill = type_pr)) +
  geom_area() +
  scale_fill_brewer(palette = "PuBu", direction = -1) +
  xlab("Year")

# length of prs (2002 decision should be 0 XXXX)

df_allprs %>%
  filter(type_pr != "Other") %>%
  group_by(year, type_pr) %>%
  summarise(mean_length = mean(pr_length)) %>% 
  ggplot() +
  geom_line(aes(x = year, y = mean_length, linetype = type_pr)) +
  xlab("Year") +
  ylab("Mean Characters per PR") +
  geom_vline(xintercept = 2017, color = "red") +
  theme_ipsum_rc()


# mentions of rechtsstaat

df_allprs %>%
  mutate(rechtsstaat = ifelse(str_detect(text, "rechtsstaat"), "yes", "no")) %>%
  group_by(year, rechtsstaat) %>% 
  count() %>% 
  pivot_wider(year, names_from = rechtsstaat, values_from = n) %>% 
  replace_na(list(yes = 0, no = 0)) %>%
  mutate(n = yes + no) %>%
  mutate(share = yes/n*100) %>%
  ggplot() +
  geom_line(aes(x = year, y = share)) +
  theme_minimal()

df_allprs %>%
  filter(type_pr != "Decision") %>%
  mutate(rechtsstaat = ifelse(str_detect(text, "rechtsstaat"), "yes", "no")) %>%
  group_by(year, rechtsstaat) %>% 
  count() %>% 
  pivot_wider(year, names_from = rechtsstaat, values_from = n) %>% 
  replace_na(list(yes = 0, no = 0)) %>%
  mutate(n = yes + no) %>%
  mutate(share = yes/n*100) %>%
  ggplot() +
  geom_line(aes(x = year, y = share)) +
  theme_minimal()

# mentions of fpö

df_allprs %>%
  mutate(rechtsstaat = ifelse(str_detect(text, "fpö|fpo"), "yes", "no")) %>%
  group_by(year, rechtsstaat) %>% 
  count() %>% 
  pivot_wider(year, names_from = rechtsstaat, values_from = n) %>% 
  replace_na(list(yes = 0, no = 0)) %>%
  mutate(n = yes + no) %>%
  mutate(share = yes/n*100) %>%
  ggplot() +
  geom_line(aes(x = year, y = share)) +
  theme_minimal()

# how many prs have a case number attached?

df_allprs %>%
  mutate(case_dummy = ifelse(is.na(case), 0, 1)) %>% 
  {sum(.$case_dummy)}

### reviews with prs ----

# how many of the prs with a case number were matched 

df_reviews_prs_bund %>%
  {sum(.$pr_dummy_new)}

# length of invalidated text ----

df_reviews_prs_bund_2005 %>%
  group_by(year) %>%
  summarise(mean_length = mean(invalidated_length, na.rm = TRUE)) %>% 
  ggplot() +
  geom_line(aes(x = year, y = mean_length)) +
  xlab("Year") +
  ylab("Mean Characters of Invalidated Text") +
  geom_vline(xintercept = 2017, color = "red", linetype = "dotted") +
  geom_vline(xintercept = 2019, color = "red", linetype = "dotted") +
  theme_ipsum_rc()

ggsave("results/invalidated_text.png", device = "png")

# public hearings

df_reviews_prs_bund %>%
  filter(date > 1980)%>%
  group_by(date, verhandlung) %>% 
  count() %>%  
  ungroup() %>%
  complete(date, verhandlung,
           fill = list(n = 0)) %>%
  ggplot(aes(x = date, y = n, fill = as.factor(verhandlung))) +
  geom_area() +
  scale_fill_brewer(palette = "PuBu", direction = -1) +
  xlab("Year") +
  theme_ipsum_rc()


# inspect the 2017 cases

df_reviews_prs_bund_2005 %>%
  filter(date == 2017 & pop_origin == 1) %>% View()

# how many public hearings included
df_reviews_prs_bund %>%
  filter(date > 1999) %>%
  filter(verhandlung == 1) 

# how many press releases included
df_reviews_prs_bund %>%
  filter(date > 1999) %>%
  filter(pr_dummy_new == 1) 

# cases with populist litigator and public hearing

df_reviews_prs_bund %>%
  filter(pr_dummy_new == 1 & pop_origin == 1) 

# absolute number of reviews concerning populist legislators

po %>%
  mutate(n_po = ifelse(is.na(n_po), 0, n_po)) %>%
  ggplot() +
  geom_line(aes(x = year_date_review, y = n_po))

# length invalidated

df_reviews_prs_bund_2005 %>%
  ggplot(aes(x = invalidated_length)) +
  geom_histogram() 

# geom area for oh and pl

df_reviews_prs_bund %>%
  filter(type == "erkenntnis") %>%
  filter(date > 1999) %>%
  mutate(pophear = ifelse(pop_origin == 1, ifelse(verhandlung == 1, 2, 1), 0)) %>%
  group_by(date, pophear) %>%
  count() %>% 
  ungroup() %>%
  complete(date, pophear,
           fill = list(n = 0)) %>% 
  ggplot() +
  geom_area(aes(x=date, y=n, group= as.factor(pophear), fill= as.factor(pophear))) +
  scale_fill_manual(values=c("#999999", "#666666", "#333333"), 
                    name = NULL, 
                    labels = c("Reviews", "Populist Legislators", "Populist Legislator and Oral Hearing")) 

ggsave("results/small_n.pdf", width = 14, height = 9, units = "cm", device = cairo_pdf)

### CLARITY ------

# Currently these variables are not in the dataset to save space, can add this by decommenting code in cleaning prs script

# Flesch Reading Ease
df_allprs %>%
  ggplot() +
  geom_point(aes(x = year, y = Flesch, color = type_pr)) +
  geom_smooth(aes(x = year, y = Flesch))

# Flesch-Kincaid Reading Ease
df_allprs %>%
  ggplot() +
  geom_point(aes(x = year, y = Flesch.Kincaid, color = type_pr)) +
  geom_smooth(aes(x = year, y = Flesch.Kincaid))

# SMOG Reading Ease
df_allprs %>%
  ggplot() +
  geom_point(aes(x = year, y = SMOG, color = type_pr)) +
  geom_smooth(aes(x = year, y = SMOG))

# Flesch-Kincaid Reading Ease with populist legisaltors
df_reviews_prs_bund %>%
  filter(pr_dummy == 1) %>%
  ggplot() +
  geom_point(aes(x = year_review, y = Flesch.Kincaid, color = as.factor(pop_origin))) +
  geom_smooth(aes(x = year_review, y = Flesch.Kincaid))

# readability of prs in answer to populist and non-populist legislators
df_reviews_prs_bund %>%
  ggplot(aes(x = as.factor(pop_origin), y = Flesch.Kincaid)) +
  geom_boxplot() +
  geom_point(alpha = 0.2, color = "darkblue")+
  xlab("Populist") +
  ylab("Readability") +
  theme(panel.grid.minor.x = element_blank()) +
  theme_ipsum_rc()

df_reviews_prs_bund %>%
  filter(pop_origin == 1) %>%
  filter(pr_dummy == 1) %>%
  View()

ggsave("prs/results/pr_difficulty_populist.png", device = "png")

