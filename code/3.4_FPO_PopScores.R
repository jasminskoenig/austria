library(tidyverse)
library(ggplot2)
library(hrbrthemes)


#theme
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

#load data
VParty <- readRDS("data/V-Dem-CPD-Party-V1.rds")

VParty %>%
  filter(v2pashname == "FPO") %>%
  filter(year > 1945) %>% 
  ggplot() +
  geom_line(aes(x=year, y=v2xpa_popul)) +
  geom_ribbon(aes(x=year, ymin=v2xpa_popul_codelow, ymax=v2xpa_popul_codehigh), alpha=0.5)+
  xlim(1970, 2020)+
  ylim(0,1)+
  xlab("Year") +
  ylab("VParty Populism Indice FPÖ") +
  theme_ipsum_rc() 
  ggsave("results/FPO_pop_score.pdf", width= 10, height=7, units = "cm", device = cairo_pdf)


VParty %>%
  filter(v2pashname == "BZO") %>%
  filter(year > 1978) %>% View()
  ggplot() +
  geom_line(aes(x=year, y=v2xpa_popul)) +
  geom_ribbon(aes(x=year, ymin=v2xpa_popul_codelow, ymax=v2xpa_popul_codehigh), alpha=0.5)+
  xlim(1978, 2020)+
  ylim(0,1)+
  xlab("Year") +
  ylab("VParty Populism Indice BZÖ") +
  theme_ipsum_rc()+
  ggsave("results/BZO_pop_score.pdf", width= 10, height=7, units = "cm", device = cairo_pdf)

  
#with pop indicator
VParty_Austria_Mainstream <- VParty %>%
  filter(country_name == "Austria") %>%
  mutate(populist = if_else(v2pashname == "FPO", 1,0)) %>%
  filter(year > 1978) %>% 
  mutate(government = if_else(v2pagovsup == 1, "Government", "Opposition")) %>% 
  mutate(government = as.factor(government)) %>%
  filter(!str_detect(v2pashname, "BZO|LIF|TS|FPO"))  

VParty_Austria_FPO <- VParty %>%
  mutate(populist = if_else(v2pashname == "FPO", 1,0)) %>%
  filter(v2pashname == "FPO") %>%
  filter(year > 1978) %>% 
  mutate(government = if_else(v2pagovsup == 1, "Government", "Opposition")) %>% 
  mutate(government = as.factor(government))

VParty_Austria_FPO %>%
  ggplot()+
  geom_ribbon(aes(x=year, ymin=v2xpa_popul_codelow, ymax=v2xpa_popul_codehigh), alpha=0.2)+
  geom_segment(aes(x = year,
                   y = v2xpa_popul,
                   xend=lead(year),
                     yend=lead(v2xpa_popul),
                     lty=lead(government)))+
    labs(linetype = "") +
    scale_linetype_discrete(na.translate=FALSE)+
    xlab("Year") +
    ylab("VParty Populism Indice FPÖ")+
  ylim(0,1) #+
    #geom_line(data=~subset(.,year_legislated==1983| year_legislated>1983 & year_legislated<1986|year_legislated==1986|year_legislated==2000|year_legislated>2000&year_legislated<2007|year_legislated==2007|year_legislated==2018|year_legislated==2019), linetype="dashed")+
    #geom_line(data=~subset(.,year_legislated<1982|year_legislated==1983|year_legislated==1986|year_legislated>1986&year_legislated<2000|year_legislated==2000|year_legislated==2007|year_legislated>2007&year_legislated<2018|year_legislated==2019|year_legislated==2020))+
    ggsave("Presentation/FPO_pop_score_government.jpg", width = 14, height = 8, units = "cm", device = "jpeg")

VParty_Austria_Mainstream %>%
  ggplot()+
  geom_line(aes(x=year, y=v2xpa_popul, group = v2pashname, linetype = v2pashname)) +
  geom_ribbon(aes(x=year, ymin=v2xpa_popul_codelow, ymax=v2xpa_popul_codehigh, group=v2pashname), alpha=0.1)+
  geom_line(data = VParty_Austria_FPO, aes(x=year, y=v2xpa_popul, group = v2pashname, color = v2pashname)) +
  geom_ribbon(data = VParty_Austria_FPO, aes(x=year, ymin=v2xpa_popul_codelow, ymax=v2xpa_popul_codehigh, group=v2pashname), alpha=0.1)+
  #geom_segment(aes(x = year,
                  # y = v2xpa_popul,
                  # xend=lead(year),
                  # yend=lead(v2xpa_popul),
                  # lty=lead(government)))+
  labs(linetype = "") +
  scale_linetype_discrete(na.translate=FALSE)+
  xlab("Year") +
  ylab("VParty Populism Indice")+
  ylim(0,1)+
  labs(linetype='Party') +
  labs(color="")+
  #geom_line(data=~subset(.,year_legislated==1983| year_legislated>1983 & year_legislated<1986|year_legislated==1986|year_legislated==2000|year_legislated>2000&year_legislated<2007|year_legislated==2007|year_legislated==2018|year_legislated==2019), linetype="dashed")+
  #geom_line(data=~subset(.,year_legislated<1982|year_legislated==1983|year_legislated==1986|year_legislated>1986&year_legislated<2000|year_legislated==2000|year_legislated==2007|year_legislated>2007&year_legislated<2018|year_legislated==2019|year_legislated==2020))+
  ggsave("Presentation/Austria_pop_score_government.jpg", width = 14, height = 8, units = "cm", device = "jpeg")

# import manual data
fpo_gov <- read_excel("manual/fpo_pop_score.xls.xlsx") %>%
  mutate(across(matches("popul"), as.numeric))

fpo_gov %>%
  ggplot()+
  geom_ribbon(aes(x=year, ymin=popul_low, ymax=popul_high), alpha=0.2)+
  geom_segment(aes(x = year,
                   y = popul,
                   xend=lead(year),
                   yend=lead(popul),
                   lty=gov)) +
  labs(linetype = "") +
  scale_linetype_discrete(na.translate=FALSE)+
  xlab("Year") +
  ylab("VParty Populism Indice FPÖ")+
  ylim(0,1) 
#geom_line(data=~subset(.,year_legislated==1983| year_legislated>1983 & year_legislated<1986|year_legislated==1986|year_legislated==2000|year_legislated>2000&year_legislated<2007|year_legislated==2007|year_legislated==2018|year_legislated==2019), linetype="dashed")+
#geom_line(data=~subset(.,year_legislated<1982|year_legislated==1983|year_legislated==1986|year_legislated>1986&year_legislated<2000|year_legislated==2000|year_legislated==2007|year_legislated>2007&year_legislated<2018|year_legislated==2019|year_legislated==2020))+
ggsave("Presentation/FPO_pop_score_government.jpg", width = 14, height = 8, units = "cm", device = "jpeg")


fpo_gov2 <- VParty_Austria_FPO %>%
  select(v2xpa_popul, year)

fpo_gov <- fpo_gov %>%
  left_join(fpo_gov2, by = "year")


fpo_gov %>%
  ggplot()+
  geom_ribbon(data = VParty_Austria_FPO, aes(x=year, ymin=v2xpa_popul_codelow, ymax=v2xpa_popul_codehigh), alpha=0.2)+
  geom_segment(aes(x = year,
                   y = v2xpa_popul,
                   xend=lead(year),
                   yend=lead(v2xpa_popul),
                   lty=lead(gov)))+
  labs(linetype = "") +
  scale_linetype_discrete(na.translate=FALSE)+
  xlab("Year") +
  ylab("VParty Populism Indice FPÖ")+
  ylim(0,1)+
  #geom_line(data=~subset(.,year_legislated==1983| year_legislated>1983 & year_legislated<1986|year_legislated==1986|year_legislated==2000|year_legislated>2000&year_legislated<2007|year_legislated==2007|year_legislated==2018|year_legislated==2019), linetype="dashed")+
  #geom_line(data=~subset(.,year_legislated<1982|year_legislated==1983|year_legislated==1986|year_legislated>1986&year_legislated<2000|year_legislated==2000|year_legislated==2007|year_legislated>2007&year_legislated<2018|year_legislated==2019|year_legislated==2020))+
  ggsave("Presentation/FPO_pop_score_government.jpg", width = 14, height = 8, units = "cm", device = "jpeg")
