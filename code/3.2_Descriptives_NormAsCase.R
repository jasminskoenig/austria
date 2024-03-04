Sys.setenv(LANG = "en")

library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(naniar)
library(gmodels)
library(xtable)
library(descr)
library(janitor)
library(readxl)
library(knitr)
library(kableExtra)
library(lubridate)
library(Cairo) # to save as pdf 


#load data
G_analysis <- readRDS("data/G_analysis.rds")

#define themes
theme_gridYy <- theme_ipsum_tw(axis_text_size = 12, 
                               axis_title_size = 12,
                               grid="Yy") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.5, "cm"),
        plot.margin=grid::unit(c(1,0.5,0.5,0), "mm"))

theme_base<- theme_ipsum_tw(axis_text_size = 12, 
                              axis_title_size = 12,
                              strip_text_size = 12,
                              grid = "Y") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.5, "cm"),
        plot.margin=grid::unit(c(1,0.5,0.5,0), "mm"))



#set theme
theme_set(theme_gridY)

#
G_analysis %>%
  filter(year > 1980) %>%
  group_by(year, decision) %>% 
  filter(oralhearing == 1) %>%
  dplyr::count(oralhearing) %>% 
  ggplot(aes(x=year, y=n)) +
  geom_line(aes(linetype = decision)) +
  theme(legend.title= element_blank())

#decisions by date with different decisions
G_analysis %>% 
  mutate(date = str_extract(date, "\\d{4}")) %>%
  filter(!is.na(decision)) %>%
  mutate(date = as.numeric(date)) %>%
  group_by(decision, date) %>%
  dplyr::count() %>%
  ggplot(aes(x = date, y = n, group = decision, fill = decision)) + 
  geom_area() +
  xlab('Year') +
  ylab('Number of Laws Under Review') +
  scale_fill_manual(values=c("#999999", "#333333"), 
                    name = NULL, 
                    labels = c("Constitutional", "Invalidated")) 

ggsave("results/decisions.pdf", device = cairo_pdf, height = 10, width = 18, units = "cm") # save as pdf
ggsave("Presentation/decisions.jpg", device = "jpeg", height = 8, width = 14, units = "cm") #for presentation

#decisions by year with different decisions
G_analysis %>% 
  group_by(decision, year) %>%
  filter(year > 1979) %>%
  dplyr::count() %>%
  ungroup() %>%
  complete(year, decision,
           fill = list(n = 0)) %>%
  ggplot(aes(x = year, y = n, group = decision, fill = decision)) + 
  geom_area() +
  xlab('Year of Legislation') +
  ylab('Number of Laws Reviewed') +
  scale_fill_manual(values=c("#999999", "#333333"), 
                    name = NULL, 
                    labels = c("Constitutional", "Invalidated")) 
ggsave("results/decisions_yearleg.pdf", device = cairo_pdf, height = 8, width = 14) # save as pdf

# decisions by year with plaintiffs
G_analysis %>% 
  filter(!is.na(norm)) %>%
  mutate(date = year(date_2)) %>%
  group_by(plaintiff_category, date) %>%
  dplyr::count() %>%
  ungroup %>%
  complete(date, plaintiff_category,
           fill = list(n = 0)) %>%
  ggplot(aes(x = date, y = n, group = plaintiff_category, fill = plaintiff_category)) + 
  geom_area() +
  xlab('Year') +
  ylab('Number of Laws Under Review') +
  scale_fill_manual(values=c("#999999", "#333333", "#666666", "black"), 
                    name = NULL, 
                    labels = c("Concrete Review Initiated by Court", "NA ", "Constitutional Complaint by Individual", "Abstract Review")) +
  ggtitle("Review Types over Time", subtitle = "all cases")


ggsave("results/decisions_by_plaintiff.pdf", device = cairo_pdf, height = 10, width = 18, units = "cm") # save as pdf

G_analysis %>% 
  filter(!is.na(norm)) %>%
  filter(type == "erkenntnis") %>%
  mutate(date = year(date_2)) %>%
  group_by(plaintiff_category, date) %>%
  dplyr::count() %>%
  ungroup %>%
  complete(date, plaintiff_category,
           fill = list(n = 0)) %>%
  ggplot(aes(x = date, y = n, group = plaintiff_category, fill = plaintiff_category)) + 
  geom_area() +
  xlab('Year') +
  ylab('Number of Laws Under Review') +
  scale_fill_manual(values=c("#999999", "#333333", "#666666", "black"), 
                    name = NULL, 
                    labels = c("Concrete Review Initiated by Court", "NA ", "Constitutional Complaint by Individual", "Abstract Review")) +
  ggtitle("Review Types over Time", subtitle = "includes only decisions on the merits")


ggsave("results/decisions_by_plaintiff_erkenntnis.pdf", device = cairo_pdf, height = 10, width = 18, units = "cm") # save as pdf

G_analysis %>%
  filter(!is.na(norm)) %>%
  filter(type == "erkenntnis") %>%
  group_by(plaintiff_category, decision) %>%
  dplyr::count() %>%
  ungroup %>%
  complete(decision, plaintiff_category,
           fill = list(n = 0)) %>%
  group_by(plaintiff_category) %>%
  mutate(share = n/sum(n)*100) %>%
  ggplot() +
  geom_bar(aes(x = share, y = plaintiff_category, fill = decision), position="dodge", stat="identity") +
  ggtitle("Share of Reviews Sustained", subtitle = "includes only decisions on the merits")

ggsave("results/decisions_by_plaintiff_share1.pdf", device = cairo_pdf, height = 10, width = 18, units = "cm") # save as pdf

G_analysis %>%
  filter(!is.na(norm)) %>%
  filter(type == "erkenntnis") %>%
  filter(case_when(
    plaintiff_category == "Politician" & !(date_2 %within% inoffice) ~ FALSE,
    TRUE ~ TRUE
  )) %>%
  group_by(plaintiff_category, decision) %>%
  dplyr::count() %>%
  ungroup %>%
  complete(decision, plaintiff_category,
           fill = list(n = 0)) %>%
  group_by(plaintiff_category) %>%
  mutate(share = n/sum(n)*100) %>%
  ggplot() +
  geom_bar(aes(x = share, y = plaintiff_category, fill = decision), position="dodge", stat="identity") +
  ggtitle("Share of Reviews Sustained", subtitle = "includes only decisions on the merits, filtered abstract reviews to only those decided \n within the legislature in which the law was passed")

ggsave("results/decisions_by_plaintiff_share2.pdf", device = cairo_pdf, height = 10, width = 18, units = "cm") # save as pdf

G_analysis %>%
  filter(!is.na(norm)) %>%
  filter(type == "erkenntnis") %>%
  filter(case_when(
    plaintiff_category == "Politician" & !str_detect(plaintiff, "rat") ~ FALSE,
    TRUE ~ TRUE
  ))  %>%
  group_by(plaintiff_category, decision) %>%
  dplyr::count() %>%
  ungroup %>%
  complete(decision, plaintiff_category,
           fill = list(n = 0)) %>%
  group_by(plaintiff_category) %>%
  mutate(share = n/sum(n)*100) %>%
  ggplot() +
  geom_bar(aes(x = share, y = plaintiff_category, fill = decision), position="dodge", stat="identity") +
  ggtitle("Share of Reviews Sustained", subtitle = "includes only decisions on the merits, filtered abstract reviews to only those initiated \n by Bundesrat or Nationalrat")

ggsave("results/decisions_by_plaintiff_share3.pdf", device = cairo_pdf, height = 10, width = 18, units = "cm") # save as pdf

G_analysis %>%
  filter(!is.na(norm)) %>%
  filter(type == "erkenntnis") %>%
  group_by(plaintiff_category, decision) %>%
  dplyr::count() %>%
  ungroup %>%
  complete(decision, plaintiff_category,
           fill = list(n = 0)) %>%
  ggplot() +
  geom_bar(aes(x = n, y = plaintiff_category, fill = decision), position="dodge", stat="identity") +
  ggtitle("Absolut Number of Reviews", subtitle = "includes only decisions on the merits")


ggsave("results/decisions_by_plaintiff_n.pdf", device = cairo_pdf, height = 10, width = 18, units = "cm") # save as pdf


#government-------------------------------------------------------------------------------------------

#importing the dataset on governments
Governments <- readRDS("data/governments.rds")

#norms reviewed per government
G_analysis %>% 
  filter(year > 1978) %>%
  filter(!is.na(decision)) %>% 
  mutate(government = factor(government, levels = Governments$government)) %>% 
  filter(government != "NA")  %>%
  group_by(government, decision, lawstotal) %>%
  dplyr::count() %>% 
  mutate(percentage = n/lawstotal*100) %>% 
  ggplot(aes(fill = decision, y = percentage, x = government)) + 
  geom_bar(position="stack", stat="identity") +
  xlab('') +
  ylab('Share of Total Laws Reviewed') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  coord_flip() +
  scale_fill_manual(name = NULL, 
                    labels = c("Overruled", "Sustained"),
                    values = c("#999999", "#333333")) 

ggsave("results/norms_by_government.pdf", height = 13, width = 18, units = "cm", device = cairo_pdf)

#time between law and judgement
G_analysis %>%
  mutate(date = str_extract(date, "\\d{4}")) %>%
  mutate(date = as.numeric(date)) %>%
  mutate(timespan = date - year) %>% 
  filter(!is.na(year)) %>%
  group_by(date) %>% 
  summarize(mean_timespan = mean(timespan)) %>% 
  ggplot(aes(y = mean_timespan, x = date)) + 
  geom_line() +
  xlab('Date') +
  ylab('Years') 

ggsave("results/timespan_by_time.pdf", height = 8, width = 13, units = "cm", device = cairo_pdf)

#density plot of timespan
G_analysis %>%
  mutate(date = str_extract(date, "\\d{4}")) %>%
  mutate(date = as.numeric(date)) %>%
  filter(year > 1945) %>%
  mutate(timespan = date - year) %>%
  ggplot(aes(x = timespan)) +
  geom_density(fill = "#666666", color = "#666666") +
  xlab('Years') +
  ylab('Density') +
  scale_x_continuous(lim = c(0, 75)) 

ggsave("results/timespan_density.pdf", height = 8, width = 13, units = "cm", device = cairo_pdf)

#average of timespan (9 years)
#I have to check what dataset to use here, unit of analysis is wrong
#G_analysis %>%
# mutate(date = str_extract(date, "\\d{4}")) %>%
#mutate(date = as.numeric(date)) %>%
#filter(year > 1945) %>%
#mutate(timespan = date - year) %>%
#summarize(mean(timespan)) %>%
#head()

#populism---------------------------------------------------------------------------------------------------

#norms by populist
G_analysis %>% 
  filter(year > 1978) %>% 
  filter(populist != "NA")  %>%
  group_by(populist, decision) %>%
  dplyr::count() %>% 
  mutate(lawstotal = (ifelse(populist == "populist", 1409, 3746))) %>%
  mutate(percentage = n/lawstotal*100) %>%  head()

#policy areas by populism
G_analysis %>% 
  filter(year > 1978) %>% 
  filter(!is.na(decision)) %>% 
  filter(populist != "NA")  %>%
  group_by(populist, decision, policyarea) %>%
  dplyr::count() %>% head()

#when were populist governments in power
G_analysis %>% 
  filter(year > 1979) %>%
  filter(government != "NA") %>%
  ggplot(aes(x=year,y=populist)) + 
  geom_point() 

#percentage of unconstitutional norms per policyarea by populist
#result rather even in the theoretically interesting fields 
#higher percentage for populists in Economy and Taxation
G_analysis %>% 
  filter(year>1979) %>% 
  group_by(populist, policyarea, decision) %>%
  summarize(n=n()) %>% 
  mutate(freq = n / sum(n)) %>%
  mutate(sum = sum(n)) %>%
  filter(decision == "sustained") %>%
  head()

#plaintiffs--------------------------------------------- 

# share of plaintiffs over time

G_analysis %>%
  filter(!is.na(plaintiff_category)) %>%
  filter(!is.na(decision)) %>%
  filter(plaintiff_category != "Other") %>%
  mutate(plaintiff_category = ifelse(plaintiff_category == "Politician", "Abstract Review", plaintiff_category)) %>%
  mutate(plaintiff_category = ifelse(plaintiff_category == "Person", "Individual", plaintiff_category)) %>%
  mutate(date = year(date_2)) %>%
  group_by(date, plaintiff_category) %>%
  count() %>%
  ungroup() %>%
  complete(date, plaintiff_category,
           fill = list(n = 0)) %>%
  group_by(date) %>%
  mutate(n_date = sum(n))%>%
  mutate(share = n/n_date) %>% 
  ggplot() +
  geom_line(aes(x=date, y=share, linetype = plaintiff_category)) +
  scale_linetype_manual(values=c("dotted", "longdash", "solid")) +
  ylab("Share") +
  xlab("Decision Year") +
  theme(legend.title=element_blank()) +
    ylim(0,1)
  
  ggsave("results/plaintiff_time.pdf", height = 7, width = 17, units = "cm", device = cairo_pdf)


# general share  
G_analysis %>%
    filter(!is.na(plaintiff_category)) %>%
    filter(!is.na(decision)) %>% 
    group_by(plaintiff_category) %>%
    count() %>%
  mutate(share = n/2583)

# share of plaintiffs over time

G_analysis %>%
  filter(!is.na(plaintiff_category)) %>%
  filter(!is.na(decision)) %>%
  filter(type == "erkenntnis") %>%
  filter(plaintiff_category != "Other") %>%
  mutate(plaintiff_category = ifelse(plaintiff_category == "Politician", "Abstract Review", plaintiff_category)) %>%
  mutate(plaintiff_category = ifelse(plaintiff_category == "Person", "Individual", plaintiff_category)) %>%
  mutate(date = year(date_2)) %>%
  group_by(date, plaintiff_category) %>%
  count() %>%
  ungroup() %>%
  complete(date, plaintiff_category,
           fill = list(n = 0)) %>%
  group_by(date) %>%
  mutate(n_date = sum(n))%>%
  mutate(share = n/n_date) %>% 
ggplot() +
  geom_line(aes(x=date, y=share, linetype = plaintiff_category)) +
  scale_linetype_manual(values=c("dotted", "longdash", "solid")) +
  ylab("Share") +
  xlab("Decision Year") +
  theme(legend.title=element_blank())

ggsave("results/plaintiff_time_erkenntnis.pdf", height = 7, width = 17, units = "cm", device = cairo_pdf)


  

#barplot plaintiff by populist
G_analysis %>% 
  filter(!is.na(plaintiff_category)) %>%
  filter(plaintiff_category != "Other") %>%
  filter(!is.na(decision)) %>%
  group_by(plaintiff_category, pop_origin) %>%
  dplyr::count() %>% 
  ggplot(aes(fill = as.factor(pop_origin), y = n, x = plaintiff_category)) + 
  geom_bar(position="dodge", stat="identity") +
  xlab('Plaintiff') +
  ylab('n') +
  scale_fill_manual(name = NULL, 
                    labels = c("Non-populist", "Populist"),
                    values = c("#999999", "#333333")) 

ggsave("results/plaintiff_by_populist_barplot.pdf", height = 13, width = 13, units = "cm", device = cairo_pdf)

#Alternative dotchart plaintiff by populist
G_analysis %>% 
  filter(!is.na(plaintiff_category)) %>%
  filter(plaintiff_category != "Other") %>%
  filter(!is.na(decision)) %>%
  group_by(plaintiff_category, populist) %>%
  dplyr::count() %>% 
  ggplot(aes(x = plaintiff_category, y = n)) 


#crosstab plaintiff by decision
plaintiff_crosstab <- CrossTable(G_analysis$plaintiff_category, G_analysis$decision, prop.chisq = F, prop.t = F, dnn = c("Plaintiff", "Decision"))

print(xtable(plaintiff_crosstab, digits=3, caption = "Plaintiffs by decision", label="plaintiff_crosstab"), file = "results/plaintiff_crosstab.tex", include.rownames = FALSE)

#but looking only at the dataset is not helpful, we need to look at this in relation to the total amount of laws passed
#this is not correct yet
plaintiff_populist_crosstab_freq <- G_analysis %>%
  filter(year > 1979) %>%
  filter(!is.na(norm)) %>%
  group_by(populist, plaintiff_category) %>%
  dplyr::count() %>% 
  mutate(lawstotal = (ifelse(populist == "populist", 1409, 3746))) %>%
  mutate(percentage = as.numeric(n/lawstotal))%>% 
  select(-n, -lawstotal) %>%
  spread(plaintiff_category, percentage)

plaintiff_populist_crosstab_freq <- G_analysis %>%
  filter(year > 1979) %>%
  filter(!is.na(norm)) %>%
  group_by(government, plaintiff_category, lawstotal, populist) %>%
  dplyr::count() %>% 
  mutate(percentage = as.numeric(n/lawstotal*100)) %>%
  mutate(populist = as.factor(populist))%>%
  ungroup() %>%
  filter(government != "NA") %>%
  write_tsv("manual/plaintiff_populist.tsv") 

#update theme to gridYy
theme_set(theme_gridYy)

plaintiff_populist_crosstab_freq %>%
  filter(plaintiff_category != "Other") %>%
  ggplot(aes(x=populist, y=percentage))+
  geom_point(alpha = 0.4) +
  stat_summary(fun = mean, aes(shape = "mean"), geom = "point", size = 5, alpha = 0.5)+
  facet_wrap(~plaintiff_category)+
  theme(legend.title = element_blank())+
  labs(x="",
       y="Share of Total Laws Sued per Government")+
  ylim(0,60)+
  scale_x_discrete(breaks=c("nonpopulist","populist"),
                   labels=c("Non-populist", "Populist"))+
  labs(caption = "") 
  ggsave("results/plaintiffs.pdf", width = 19, height = 13, units = "cm",  device = cairo_pdf)

  plaintiff_populist_crosstab_freq %>%
    filter(plaintiff_category != "Other") %>%
    ggplot(aes(x=populist, y=percentage))+
    geom_boxplot(alpha = 0.4) +
    facet_wrap(~plaintiff_category)+
    theme(legend.title = element_blank())+
    labs(x="",
         y="Share of Total Laws Sued per Government")+
    ylim(0,60)+
    scale_x_discrete(breaks=c("nonpopulist","populist"),
                     labels=c("Non-populist", "Populist"))+
    labs(caption = "") 
    ggsave("results/plaintiffs.pdf", width = 19, height = 13, units = "cm",  device = cairo_pdf)
  

#only includes cases where a decision could be extracted
G_analysis %>%
  filter(!is.na(policyarea)) %>%
  filter(!is.na(decision)) %>%
  group_by(policyarea, populist) %>%
  dplyr::count() %>% 
  spread(populist, n) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(n = populist + nonpopulist) %>%
  mutate(percentage = round(populist/n*100, 2)) %>%
  select(-populist, -nonpopulist) %>% 
  mutate(n = as.numeric(n)) %>%
  arrange(desc(n)) %>%
  write.table(sep = ";", file = "results/policyarea_frequency.txt")

#theme back to gridY

theme_set(theme_gridY)

#populist governments and policyareas
G_analysis %>% 
  filter(year > 1978) %>%
  filter(!is.na(policyarea)) %>% 
  filter(government != "NA")  %>%
  filter(populist == "populist") %>% 
  group_by(government, decision, policyarea, lawstotal) %>%
  dplyr::count() %>% 
  ggplot(aes(y = n/lawstotal, x = policyarea, fill = decision)) + 
  facet_wrap(~ government) +
  geom_bar(position= "stack", stat="identity") +
  coord_flip() +
  xlab('') +
  ylab('Share') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_manual(name = NULL, 
                    labels = c("Overruled", "Sustained"),
                    values = c("#999999", "#333333")) 

ggsave("results/populistgovernments_policyarea.pdf", height = 24, width = 20, units = "cm", device = cairo_pdf)

#policyareas----
G_analysis %>% 
  filter(year > 1978) %>%
  filter(!is.na(decision)) %>% 
  filter(government != "NA")  %>%
  group_by(decision, policyarea) %>%
  dplyr::count() %>% 
  ggplot(aes(y = n, x = policyarea, fill = decision)) + 
  geom_bar(position= "stack", stat="identity") +
  coord_flip() +
  xlab('') +
  ylab('n') +
  scale_fill_manual(name = NULL, 
                    labels = c("Overruled", "Sustained"),
                    values = c("#999999", "#333333")) 
  ggsave("results/policyareas.pdf", height = 13, width = 15, units = "cm", device = cairo_pdf)

#populist governments and policyareas
#Need to rename populist and nonpopulist first
names_pop <- as_labeller(c(`nonpopulist` = "Non-populist", `populist` = "Populist")) 
#economy looks relevant, but we cannot differentiate whether the policies were driven by the populists or
#conservatives...
G_analysis %>% 
  filter(year > 1978) %>%
  filter(!is.na(policyarea)) %>% 
  filter(government != "NA")  %>%
  mutate(lawstotal = (ifelse(populist == "populist", 1409, 3746))) %>%
  group_by(populist, decision, policyarea, lawstotal) %>%
  dplyr::count() %>%
  ggplot(aes(y = policyarea, x = n/lawstotal, fill = decision)) + 
  facet_wrap(~ populist, labeller=names_pop) +
  geom_bar(position= "stack", stat="identity") +
  coord_flip() +
  xlab('Share') +
  ylab('') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_manual(name = NULL, 
                    labels = c("Overruled", "Sustained"),
                    values = c("#999999", "#333333")) 
  ggsave("results/populist_policyarea.pdf", height = 15, width = 20, units = "cm", device = cairo_pdf)

#only for relevant policyareas
G_analysis %>% 
  filter(year > 1978) %>%
  mutate(policyarea = case_when(
    str_detect(policyarea, "Social|Security|Migration") ~ str_extract(policyarea, ".*"),
    TRUE ~ "Other"
  )) %>% 
  mutate(policyarea = factor(policyarea, levels = c("Migration", "Security", "Social", "Other"), ordered = TRUE)) %>% 
  filter(government != "NA")  %>%
  mutate(populist = case_when(
    str_detect(populist, "nonpopulist") ~ "Non-populist",
    str_detect(populist, "populist") ~ "Populist",
  )) %>%
  mutate(lawstotal = (ifelse(populist == "Populist", 1409, 3746))) %>%
  group_by(populist, decision, policyarea, lawstotal) %>%
  dplyr::count() %>%
  ggplot(aes(y = populist, x = n/lawstotal, fill = policyarea, order = policyarea)) + 
  geom_bar(position= "stack", stat="identity", width= 0.5) +
  coord_flip() +
  xlab('Share') +
  ylab('') +
  scale_fill_manual(name = NULL, 
                    labels = c("Migration", "Security", "Social", "Other"),
                    values = c("#333333", "#666666", "#999999", "#CCCCCC")) 
  ggsave("results/populist_fewpolicyarea2.pdf", height = 10, width = 10, units = "cm", device = cairo_pdf)

# same thing for presentation as jpg
G_analysis %>% 
  filter(year > 1978) %>%
  mutate(policyarea = case_when(
    str_detect(policyarea, "Social|Security|Migration") ~ str_extract(policyarea, ".*"),
    TRUE ~ "Other"
  )) %>% 
  mutate(policyarea = factor(policyarea, levels = c("Migration", "Security", "Social", "Other"), ordered = TRUE)) %>% 
  filter(government != "NA")  %>%
  mutate(populist = case_when(
    str_detect(populist, "nonpopulist") ~ "Non-populist",
    str_detect(populist, "populist") ~ "Populist",
  )) %>%
  mutate(lawstotal = (ifelse(populist == "Populist", 1409, 3746))) %>%
  group_by(populist, decision, policyarea, lawstotal) %>%
  dplyr::count() %>%
  ggplot(aes(y = populist, x = n/lawstotal, fill = policyarea, order = policyarea)) + 
  geom_bar(position= "stack", stat="identity", width= 0.5) +
  coord_flip() +
  xlab('Share') +
  ylab('') +
  scale_fill_manual(name = NULL, 
                    labels = c("Migration", "Security", "Social", "Other"),
                    values = c("#333333", "#666666", "#999999", "#CCCCCC")) 
  ggsave("presentation/populist_fewpolicyarea2.jpg", height = 10, width = 10, units = "cm", device = "jpg")


#t-test 
#testing whether populists are sued significantly more often
#frequency of lawsuits
T_Test_sued_policyarea <- G_analysis %>% 
  filter(year > 1978) %>%
  filter(government != "NA")  %>%
  mutate(populist = case_when(
    str_detect(populist, "nonpopulist") ~ "Non-populist",
    str_detect(populist, "populist") ~ "Populist",
  )) %>%
  group_by(government, populist, lawstotal) %>%
  dplyr::count() %>%
  mutate(share = n/lawstotal) %>%
  ungroup() %>%
  select(populist, share) %>%
  mutate(populist = as.factor(populist))

lm1 <- lm(share~populist, data=T_Test_sued_policyarea) #linear regression as t-test

summary(lm1)


#df only including a norm once
G_analysis_distinct <- G_analysis %>%
  filter(government != "NA")  %>% 
  distinct(normnumber, year, .keep_all = TRUE) 

# Share of laws sued and reviewed by populists
G_analysis %>% 
  filter(government != "NA")  %>%
  bind_rows(G_analysis_distinct, .id = "id") %>% 
  filter(year > 1979) %>%
  mutate(populist = if_else(pop_origin == 1, "Populist", "Non-populist")) %>%
  mutate(lawstotal = ifelse(populist == "Populist", 196, 5316)) %>%
  mutate(id = case_when(
    id == 1 ~ "Initiation of Review",
    id == 2 ~ "Share of Total Laws Reviewed"
  )) %>%
  group_by(populist, lawstotal, id) %>%
  dplyr::count() %>% 
  mutate(x = as.numeric(n/lawstotal)) %>% 
  ggplot(aes(y = populist, x = x)) + 
  facet_wrap(~ id) +
  geom_bar(position= "dodge", stat="identity", width= 0.5) +
  coord_flip() +
  xlab('Share of Total Laws Under Review') +
  ylab('') +
  xlim(0,0.5)

ggsave("results/shareofreview_pop.pdf", height = 13, width = 20, units = "cm", device = cairo_pdf)

#share of laws sued and reviewed in relation to legislative activity
G_analysis %>% 
  filter(government != "NA")  %>%
  bind_rows(G_analysis_distinct, .id = "id") %>% 
  filter(year > 1978) %>%
  mutate(policyarea = case_when(
    str_detect(policyarea, "Social|Security|Migration") ~ str_extract(policyarea, ".*"),
    TRUE ~ "Other"
  )) %>% 
  mutate(policyarea = factor(policyarea, levels = c("Migration", "Security", "Social", "Other"), ordered = TRUE)) %>% 
  mutate(populist = case_when(
    str_detect(populist, "nonpopulist") ~ "Non-populist",
    str_detect(populist, "populist") ~ "Populist",
  )) %>%
  mutate(lawstotal = (ifelse(populist == "Populist", 1409, 3746))) %>%
  mutate(id = case_when(
    id == 1 ~ "Frequency of Lawsuits",
    id == 2 ~ "Share of Total Laws Reviewed"
  )) %>%
  group_by(populist, policyarea, lawstotal, id) %>%
  dplyr::count() %>%
  mutate(x = as.numeric(n/lawstotal)) %>% 
  ggplot(aes(y = populist, x = x, fill = policyarea, order = policyarea)) + 
  facet_wrap(~ id) +
  geom_bar(position= "dodge", stat="identity", width= 0.5) +
  coord_flip() +
  xlab('Share of Total Laws Under Review') +
  ylab('') +
  xlim(0,0.3)+
  scale_fill_manual(name = NULL, 
                    labels = c("Migration", "Security", "Social", "Other"),
                    values = c("#333333", "#666666", "#999999", "#CCCCCC")) 
  ggsave("results/populist_fewpolicyarea2.pdf", height = 13, width = 20, units = "cm", device = cairo_pdf)

# same thing for pres as jpg
  G_analysis %>% 
    filter(government != "NA")  %>%
    bind_rows(G_analysis_distinct, .id = "id") %>% 
    filter(year > 1978) %>%
    mutate(policyarea = case_when(
      str_detect(policyarea, "Social|Security|Migration") ~ str_extract(policyarea, ".*"),
      TRUE ~ "Other"
    )) %>% 
    mutate(policyarea = factor(policyarea, levels = c("Migration", "Security", "Social", "Other"), ordered = TRUE)) %>% 
    mutate(populist = case_when(
      str_detect(populist, "nonpopulist") ~ "Non-populist",
      str_detect(populist, "populist") ~ "Populist",
    )) %>%
    mutate(lawstotal = (ifelse(populist == "Populist", 1409, 3746))) %>%
    mutate(id = case_when(
      id == 1 ~ "Frequency of Lawsuits",
      id == 2 ~ "Share of Total Laws Reviewed"
    )) %>%
    group_by(populist, policyarea, lawstotal, id) %>%
    dplyr::count() %>%
    mutate(x = as.numeric(n/lawstotal)) %>% 
    ggplot(aes(y = populist, x = x, fill = policyarea, order = policyarea)) + 
    facet_wrap(~ id) +
    geom_bar(position= "dodge", stat="identity", width= 0.5) +
    coord_flip() +
    xlab('Share of Total Laws Under Review') +
    ylab('') +
    xlim(0,0.3)+
    scale_fill_manual(name = NULL, 
                      labels = c("Migration", "Security", "Social", "Other"),
                      values = c("#333333", "#666666", "#999999", "#CCCCCC")) 
  ggsave("presentation/populist_fewpolicyarea2.jpg", height = 13, width = 20, units = "cm", device = "jpg")

#t-test
#share of laws reviewed
T_Test_sued_policyarea <- G_analysis_distinct %>% 
  filter(year > 1978) %>%
  filter(government != "NA")  %>%
  mutate(populist = case_when(
    str_detect(populist, "nonpopulist") ~ "Non-populist",
    str_detect(populist, "populist") ~ "Populist",
  )) %>%
  group_by(government, populist, lawstotal) %>%
  dplyr::count() %>%
  mutate(share = n/lawstotal) %>%
  ungroup() %>%
  select(populist, share) %>%
  mutate(populist = as.factor(populist))

lm1 <- lm(share~populist, data=T_Test_sued_policyarea) #linear regression as t-test

summary(lm1)

#which policy was sued most often
G_analysis %>%
  filter(!is.na(normnumber)) %>%
  add_count(norm, sort = TRUE) %>% 
  select(norm, government, policyarea, pop_origin, n) %>%
  distinct(norm, .keep_all = TRUE) %>%
  slice(1:20) %>%
  kable("latex", booktabs = T) #write table

G_analysis %>%
  filter(!is.na(normnumber)) %>%
  add_count(norm, sort = TRUE) %>% 
  select(norm, government, policyarea, populist, n) %>%
  distinct(norm, .keep_all = TRUE) %>%
  slice(1:20) %>% 
  unite(norm_new, government, norm, sep = " - ") %>% 
ggplot(aes(x= n, y=norm_new, group = populist, color = populist)) +
  geom_segment(aes(x=0, xend=n, y=norm_new, yend=norm_new)) +
  geom_point(stat='identity') +
  xlab('Number of Complaints') +
  ylab('Norm Under Review')+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 31))+
  scale_color_manual(name = NULL, 
                     labels = c("Non-Populist", "Populist"),
                     values = c("#999999", "darkred")) 


#plaintiffs and policyareas
G_analysis %>% 
  filter(year > 1978) %>%
  filter(!is.na(policyarea)) %>% 
  filter(plaintiff_category != "Other") %>%
  filter(plaintiff_category != "NA")  %>%
  group_by(plaintiff_category, decision, policyarea) %>%
  dplyr::count() %>%
  ggplot(aes(y = n, x = policyarea, fill = decision)) + 
  facet_wrap(~ plaintiff_category) +
  geom_bar(position= "stack", stat="identity") +
  coord_flip() +
  xlab('') +
  ylab('n') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_manual(name = NULL, 
                    labels = c("Overruled", "Sustained"),
                    values = c("#999999", "#333333")) 
  ggsave("results/plaintiff_policyarea.pdf", height = 17, width = 22, units = "cm", device = cairo_pdf)

#govbrief----

#who wrote the govbrief?
#actually I don't think this tells us much, should rather look at ratio how often other governments
#file a gov brief for populist govs
G_analysis %>% 
  filter(year > 1978) %>%
  filter(!is.na(policyarea)) %>% 
  filter(government != "NA")  %>%
  group_by(gov_brief, gov_brief_by, populist) %>%
  dplyr::count() %>%
  filter(gov_brief == 1) %>% 
  ggplot() + 
  geom_bar(aes(y = n, x = populist, fill = as.factor(gov_brief_by)), position= "stack", stat="identity") +
  xlab('') +
  ylab('') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_manual(name = NULL, 
                    labels = c("Own", "Other"),
                    values = c("#999999", "#333333")) 
  ggsave("results/govbrief.pdf", height = 24, width = 20, units = "cm", device = cairo_pdf)


#Do govbriefs go down after a populist government leaves office?
#There doesn't seem to be a relationshio
G_analysis %>%
  filter(year > 1978) %>%
  filter(government != "NA")  %>%
  filter(coalition != 0) %>%
  filter(type == "erkenntnis") %>%
  mutate(year = as.numeric(year)) %>% 
  mutate(date = str_extract(date, "\\d{4}")) %>%
  mutate(date = as.numeric(date)) %>% 
  mutate(diff = date - year) %>% 
  filter(diff < 10) %>% 
  group_by(coalition, date) %>%
  mutate(n_total = n()) %>% 
  group_by(gov_brief, coalition, date, n_total) %>% 
  dplyr::count() %>% 
  filter(gov_brief == 1) %>% 
  ggplot(aes(x = date, y = n/n_total, color = coalition, group = coalition)) + 
  geom_line() +
  xlab('Year') +
  ylab('%') +
  geom_vline(xintercept = c(1987,2000,2007,2018))
  ggsave("results/govbrief2.pdf", height = 9, width = 11, units = "cm", device = cairo_pdf)

#oralhearing----

#share of laws that have to go through an oral hearing
G_analysis %>% 
  filter(year > 1978) %>%
  filter(!is.na(oralhearing)) %>% 
  filter(government != "NA")  %>%
  mutate(lawstotal = (ifelse(populist == "populist", 1409, 3746))) %>%
  group_by(oralhearing, populist, lawstotal, type) %>% 
  dplyr::count() %>% 
  filter(oralhearing == 1) %>%
  ggplot(aes(y = n/lawstotal*100, x = populist, fill=type)) + 
  geom_bar(position= "stack", stat="identity") +
  xlab('') +
  ylab('') +
  scale_fill_manual(name = NULL, 
                    labels = c("Beschluss", "Erkenntnis"),
                    values = c("#999999", "#333333")) 
  ggsave("results/oralhearing.pdf", height = 24, width = 20, units = "cm", device = cairo_pdf)

#Share of total laws that ended up in an oral hearing by government
G_analysis %>% 
  filter(year > 1978) %>%
  filter(!is.na(oralhearing)) %>% 
  filter(government != "NA")  %>%
  group_by(government, oralhearing, lawstotal) %>%
  dplyr::count() %>% 
  ggplot(aes(y = n/lawstotal, x = government)) + 
  geom_bar(position= "stack", stat="identity") +
  coord_flip() +
  xlab('') +
  ylab('Share') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_manual(name = NULL, 
                    labels = c("Overruled", "Sustained"),
                    values = c("#999999", "#333333")) 
  ggsave("results/oralhearing_government.pdf", height = 24, width = 20, units = "cm", device = cairo_pdf)

#easy stats-----

#average norms under review per year
G_analysis %>%
  mutate(date = str_extract(date, "\\d{4}")) %>%
  mutate(date = as.numeric(date)) %>% 
  count(date) %>% 
  summarise(mean(n))

#share of laws sustained overall
G_analysis %>%
  group_by(decision) %>%
  count() %>%
  mutate(share = n/2603)

# share of reviews initiated per plaintiff type

G_analysis_distinct %>%
  filter(plaintiff_category != "Other") %>%
  filter(!is.na(norm)) %>%
  mutate(plaintiff_category = if_else(plaintiff_category == "Politician", "Abstract Review", plaintiff_category)) %>%
  group_by(pop_origin) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  group_by(pop_origin, plaintiff_category, count) %>%
  count() %>%
  mutate(share = n/count) %>%
  ggplot(aes(x = plaintiff_category, y = share, fill = as.factor(pop_origin))) +
  geom_bar(position = "dodge", stat = "identity", width = 0.5) +
  scale_fill_manual(values=c("#999999", "#333333"), 
                    name = NULL, 
                    labels = c("Non-Populist", "Populist")) +
  ylab("Share of Reviews Per Plaintiff Type") +
  xlab("")

ggsave("results/shareofreview_plain.pdf", height = 13, width = 20, units = "cm", device = cairo_pdf)

chisq.test(table(G_analysis$plaintiff_category, G_analysis$pop_origin))


