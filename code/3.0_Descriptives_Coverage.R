Sys.setenv(LANG = "en")
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(hrbrthemes)
library(naniar)
library(gmodels)
library(xtable)
library(descr)
library(janitor)
library(readxl)

#-----------------------------------------------------------
#Dataset Coverage
#completeness of the dataset in the beginning (before exclusions)
G_cases_dataset <- readRDS("data/G_sprueche1.rds") 
G_cases_dataset2020 <- readRDS("data/data2020cleaned.rds") # adding the cases from 2020 that were scraped later

G_cases_dataset <- G_cases_dataset %>%
  bind_rows(G_cases_dataset2020) %>% # binding old cases and newly scraped cases
  select(link, spruch, geschaeftszahl, date, type, applied, entscheidung, antrag)

rm(G_cases_dataset2020) # delete unneccessary dataframe

#import cases according to Jahresbericht
cases <- read_excel("manual/Verfahren_seit_1991_export.xlsx") 

#cases in the final dataset (without excluded cases)
cases_finaldataset <- readRDS("data/dataincl2020_final2.rds") %>%
  mutate(date = str_extract(date, "\\d{4}")) %>%
  mutate(date = as.numeric(date))

#how many cases do not contain exact information which reference numbers are decided
#these include more lawsuits than stated
G_cases_dataset %>%
  mutate(referencenumber = str_count(geschaeftszahl, "ua")) %>%
  dplyr::count(referencenumber) 

#percentage of sustained cases per year
cases_finaldataset %>%
  filter(decision != "overruled") %>%
  group_by(date) %>% 
  mutate(number_sustained = str_count(geschaeftszahl, "g")) %>%
  summarize(number_sustained = sum(number_sustained)) %>% 
  full_join(cases, by = "date")

#number cases per year in the final dataset
cases_finaldataset <- cases_finaldataset %>% 
  mutate(number_cases_final = str_count(geschaeftszahl, "g"))%>%
  group_by(date, decision) %>% 
  summarize(number_cases_final = sum(number_cases_final)) %>% 
  spread(decision, number_cases_final) %>% 
  rename(partlysustained = "partly sustained") %>% 
  mutate(number_cases_final = overruled+partlysustained+sustained) %>% 
  mutate(number_cases_sustained = partlysustained+sustained) %>%
  select(date, number_cases_sustained, number_cases_final)

#number cases per year in the original dataset
G_cases_dataset <- G_cases_dataset %>%
  mutate(date = str_extract(date, "\\d{4}")) %>%
  mutate(date = as.numeric(date)) %>%
  mutate(number_cases_dataset = str_count(geschaeftszahl, "g")) %>%
  group_by(date) %>%
  summarize(number_cases_dataset = sum(number_cases_dataset)) 

#how many reference numbers are there in the complete dataset
G_cases_dataset %>%
  ungroup() %>%
  summarize(sum(number_cases_dataset))

G_cases_dataset <- G_cases_dataset %>%
  full_join(cases, by = "date") %>%
  full_join(cases_finaldataset, by = "date") 

#average percentage of sustained cases
G_cases_dataset %>%
  mutate(number_cases_sustained = number_cases_sustained / number_cases * 100) %>% 
  filter(number_cases_sustained > 0) %>%
  summarize(mean(number_cases_sustained))

#dataset coverage graph
G_cases_dataset %>%
  gather("dataset", "number", -date) %>% 
  filter(!is.na(date)) %>% 
  filter(dataset != "number_cases_sustained") %>%
  ggplot(aes(x = date, y = number, fill = dataset)) + 
  geom_area() +
  xlab('Year') +
  ylab('n') +
  theme_ipsum_tw(plot_title_size = 9, plot_title_margin = 2, 
                 axis_text_size = 12, 
                 axis_title_size = 12) +
  theme(plot.background = element_rect(fill = "white"),
        legend.position = "bottom", 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.5, "cm")) +
  scale_fill_manual(values=c("#999999", "#666666", "#333333"), 
                    name = NULL, 
                    labels = c("VfGH Reports", "Dataset", "Analysis")) 
  ggsave("results/dataset_coverage.jpg", height = 9, width = 11, units = "cm", device = "jpg")
  ggsave("C:/Users/Jasmin/OneDrive - Universität Hamburg/Desktop/Dissertation/Website/website/starter-hugo-academic-/content/project/vfgh/featured.jpg", height = 9, width = 11, units = "cm", device = "jpg")


#percentage of dataset coverage
G_cases_dataset %>%
  mutate(number_cases_dataset = number_cases_dataset / number_cases * 100) %>%
  mutate(number_cases_final = number_cases_final / number_cases * 100) %>%
  mutate(number_cases_sustained = number_cases_sustained / number_cases * 100) %>%
  select(date, number_cases_dataset, number_cases_final, number_cases_sustained) %>% 
  gather("dataset", "percentage", -date) %>%
  filter(date > 1989) %>% 
  ggplot(aes(x = date, y = percentage, fill = dataset, color = dataset, linetype = dataset)) + 
  geom_line() +
  xlab('Year') +
  ylab('%') +
  theme_ipsum_tw(plot_title_size = 9, plot_title_margin = 2, 
                 axis_text_size = 12, 
                 axis_title_size = 12) +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.5, "cm")) +
  scale_color_manual(name = "", 
                     labels = c("Dataset", "Analysis", "(Partly) Sustained"),
                     values = c("#999999", "#666666", "#333333")) +
  scale_linetype_manual(name = "",
                        labels = c("Dataset", "Analysis", "(Partly) Sustained"),                        
                        values=c("solid", "solid", "dotted")) +
  ggsave("results/dataset_coverage_percentage.pdf", height = 9, width = 11, units = "cm", device = cairo_pdf)

#percentage of dataset sustained 
#there is a time trend (probably also due to digitalization)
G_cases_dataset %>%
  mutate(number_cases_sustained = number_cases_sustained / number_cases_dataset * 100) %>%
  select(date, number_cases_sustained) %>% 
  gather("dataset", "percentage", -date) %>%
  filter(date > 1979) %>%
  ggplot(aes(x = date, y = percentage)) + 
  geom_line() +
  xlab('Year') +
  ylab('%') +
  theme_ipsum_tw(axis_text_size = 12, 
                 axis_title_size = 12) +
  ggsave("results/dataset_sustained_percentage.pdf", height = 8, width = 13, units = "cm", device = cairo_pdf)

