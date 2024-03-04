library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(Cairo)

laws <- readRDS("data/laws/laws.rds")

# add info on review and invalidation

readRDS("data/G_analysis.rds") %>%
  pull(norm) ->
  review

readRDS("data/G_analysis.rds") %>%
  filter(decision == "sustained") %>%
  pull(norm) ->
  invalidated

laws %>%
  mutate(review_matched = ifelse(norm %in% review, 1, 0)) %>%
  mutate(invalidation_matched = ifelse(norm %in% invalidated, 1, 0)) ->
  laws

saveRDS(laws, "data/laws/laws1.rds")

#theme
theme_base<- theme_ipsum_tw(axis_text_size = 10, 
                              axis_title_size = 10,
                              strip_text_size = 10,
                              grid = "Y") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.5, "cm"),
        plot.margin=grid::unit(c(1,0.5,0.5,0), "mm"),
        plot.background = element_rect("white"))

#set theme
theme_set(theme_gridY)

# number of laws by populist and non-populist politicians

laws %>%
  filter(year > 1979) %>%
  group_by(pop_origin) %>%
  count()


# with new detailed measures (by exact legislator)
laws %>%
  filter(year > 1990) %>%
  group_by(year, pop_origin) %>% 
  count() %>% 
  ungroup() %>%
  complete(year = full_seq(year, period = 1), pop_origin, fill = list(n = 0)) %>% 
  mutate(pop_origin = ifelse(pop_origin == 1, "Populist", "Non-Populist")) %>%
  ggplot(aes(x=year, y=n, color=pop_origin)) +
  geom_line()+
  xlab("Year")+
  ylab("Norms Passed")+
  scale_color_tableau(name = "") +
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020)) 

ggsave("results/legislative_origins_area.pdf", height = 7, width = 12, units = "cm", device = cairo_pdf)

laws %>%
  filter(year > 2000) %>%
  filter(!is.na(initiated)) %>%
  mutate(initiated = case_when(
    initiated == "initiativantrag" ~ "MP(s)",
    initiated == "regierungsvorlage" ~ "Government",
    initiated == "selbstständiger antrag" ~ "Committee"
  )) %>%
  mutate(initiated = factor(initiated, levels = c("Government", "MP(s)", "Committee"))) %>%
  group_by(year, initiated) %>%
  count() %>% 
  ggplot(aes(x=year, y=n, fill=initiated)) +
  geom_area(position = "stack")+
  xlab("Year")+
  ylab("Norms Passed")+
  scale_fill_brewer(palette = "PuBu", direction = -1, name = "")


 ggsave("results/legislative_initiators_area.pdf", height = 5, width = 8, units = "cm", device = cairo_pdf)

# with old rough measures (by government)

#legislative activity over time as geom_area
laws %>%
  group_by(year, populism_exact) %>%
  count() %>% 
  ungroup() %>%
  complete(year = full_seq(year, period = 1), populism_exact, fill = list(n = 0)) %>% 
  ggplot(aes(x=year, y=n, fill=populism_exact))+
  geom_area(position = "stack")+
  xlab("Year")+
  ylab("n")+
  scale_fill_manual(values=c("#999999", "#666666"), 
                    name = NULL, 
                    labels = c("Non-Populist", "Populist")) +
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020)) 
  
ggsave("results/legislative_activity.pdf", height = 8, width = 13, units = "cm", device = 'png')

#legislative activity over time as geom_line
#this graph differentiates exactly which law was legislated by which government (not just roughly by year)
laws %>%
  group_by(year, populism_exact) %>%
  count() %>% 
  ungroup() %>%
  mutate(position = case_when(
    populism_exact == "Populist" & str_detect(year, "1983|2000|2018")  ~ 2,
    populism_exact == "Non-Populist" & str_detect(year, "1983|2000|2018") ~ 1,
    populism_exact == "Non-Populist" & str_detect(year, "1986|2007|2019") ~ 2,
    populism_exact == "Populist" & str_detect(year, "1986|2007|2019") ~ 1
  )) %>% 
  arrange(year, position) %>%
  ggplot(aes(year, n)) +
  geom_segment(aes(xend=lead(year),
                   yend=lead(n),
                   lty=lead(populism_exact)))+
  labs(linetype = "") +
  scale_linetype_discrete(na.translate=FALSE)+
  xlab("Year") 
  #geom_line(data=~subset(.,year==1983| year>1983 & year<1986|year==1986|year==2000|year>2000&year<2007|year==2007|year==2018|year==2019), linetype="dashed")+
  #geom_line(data=~subset(.,year<1982|year==1983|year==1986|year>1986&year<2000|year==2000|year==2007|year>2007&year<2018|year==2019|year==2020))+
ggsave("results/legislative_activity_line.pdf",device = cairo_pdf)

# one with invalidated and reviewed numbers as well

reviews <- readRDS("data/G_analysis.rds") %>%
  filter(year > 1979) %>%
  group_by(year, decision) %>%
  count() %>%
  ungroup() %>%
  complete(year, decision, fill = list(n= 0)) %>%
  group_by(year) %>%
  mutate(reviewed = sum(n)) %>%
  filter(decision == "sustained")

laws %>%
  group_by(year, populism_exact) %>%
  count() %>% 
  ungroup() %>%
  mutate(position = case_when(
    populism_exact == "Populist" & str_detect(year, "1983|2000|2018")  ~ 2,
    populism_exact == "Non-Populist" & str_detect(year, "1983|2000|2018") ~ 1,
    populism_exact == "Non-Populist" & str_detect(year, "1986|2007|2019") ~ 2,
    populism_exact == "Populist" & str_detect(year, "1986|2007|2019") ~ 1
  )) %>% 
  arrange(year, position) %>% 
  ggplot(aes(year, n)) +
  geom_segment(aes(xend=lead(year),
                   yend=lead(n),
                   lty=lead(populism_exact),
                   color = "Legislated"))+
    geom_segment(aes(xend=lead(year),
                     yend=lead(n),
                     lty=lead(populism_exact),
                     color = "Invalidated"))+
  labs(linetype = "") +
  scale_linetype_discrete(na.translate=FALSE)+
  xlab("Year") 
#geom_line(data=~subset(.,year==1983| year>1983 & year<1986|year==1986|year==2000|year>2000&year<2007|year==2007|year==2018|year==2019), linetype="dashed")+
#geom_line(data=~subset(.,year<1982|year==1983|year==1986|year>1986&year<2000|year==2000|year==2007|year>2007&year<2018|year==2019|year==2020))+
ggsave("results/legislative_activity_line.pdf",device = cairo_pdf)

#legislative activity over time as geom_line
#this graph defines populism by year 
laws %>%
  filter(year > 1979) %>%
  group_by(year, populism_rough) %>%
  count() %>% 
  ungroup() %>% 
  left_join(reviews, by = "year") %>%
  rename(legislated = n.x, invalidated = n.y) %>% 
  pivot_longer(c(legislated, reviewed, invalidated), names_to = "laws", values_to = "n") ->
  laws_aggregated
  
laws_aggregated %>%
  filter(laws == "legislated") -> 
  legislated

laws_aggregated %>% 
  filter(laws == "invalidated") ->
  invalidated

laws_aggregated %>% 
  filter(laws == "reviewed") ->
  reviewed

ggplot() +
  geom_segment(aes(x=year, y = n, xend=lead(year),
                   yend=lead(n),
                   lty=lead(populism_rough), color = "black"), data = legislated)+
  geom_segment(aes(x=year, y = n, xend=lead(year),
                   yend=lead(n),
                   lty=lead(populism_rough), color = "#FF6666"), data = invalidated)+
  geom_segment(aes(x=year, y = n, xend=lead(year),
                   yend=lead(n),
                   lty=lead(populism_rough), color = "#3399FF"), data = reviewed)+
  labs(linetype = "") +
  scale_linetype_discrete(na.translate=FALSE)+
  scale_color_manual(values = c("black", "#FF6666", "#3399FF"),
                     labels = c("Reviewed", "Invalidated", "Legislated"),
                     name = "")+
  xlab("Year") +
  ylab("Laws Passed")
#geom_line(data=~subset(.,year==1983| year>1983 & year<1986|year==1986|year==2000|year>2000&year<2007|year==2007|year==2018|year==2019), linetype="dashed")+
#geom_line(data=~subset(.,year<1982|year==1983|year==1986|year>1986&year<2000|year==2000|year==2007|year>2007&year<2018|year==2019|year==2020))+
ggsave("results/legislative_activity_line_rough.pdf", width = 14, height = 8, units = "cm", device = cairo_pdf)
ggsave("C:/Users/Jasmin/OneDrive - Universität Hamburg/Desktop/Dissertation/Website/website/starter-hugo-academic-/content/project/austrianlaws/featured.jpg", width = 14, height = 8, units = "cm", device = "jpg")

laws %>%
  group_by(year, populism_rough) %>%
  count() %>% 
  ungroup() %>% 
  ggplot(aes(year, n)) +
  geom_segment(aes(xend=lead(year),
                   yend=lead(n),
                   lty=lead(populism_rough)))+
  labs(linetype = "") +
  scale_linetype_discrete(na.translate=FALSE)+
  xlab("Year") +
  ylab("Laws Passed")
  #geom_line(data=~subset(.,year==1983| year>1983 & year<1986|year==1986|year==2000|year>2000&year<2007|year==2007|year==2018|year==2019), linetype="dashed")+
  #geom_line(data=~subset(.,year<1982|year==1983|year==1986|year>1986&year<2000|year==2000|year==2007|year>2007&year<2018|year==2019|year==2020))+
  ggsave("presentation/legislative_activity_line_rough.jpg", width = 14, height = 8, units = "cm", device = "jpg")

  
  
