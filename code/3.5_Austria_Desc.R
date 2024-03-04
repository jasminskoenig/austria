# 3.5 Austria

library(readxl)
library(lubridate)
library(vdemdata)
library(Cairo)
library(scales)

source("src/graphics.R")

vdem <- vdemdata::vdem

vdem %>%
  filter(country_name == "Austria") %>%
  select(v2juhcind_osp, year, v2juhcind_osp_codehigh, v2juhcind_osp_codelow) %>% 
  filter(year > 1969) %>%
  ggplot() +
  geom_line(aes(x = year, y = v2juhcind_osp)) +
  geom_ribbon(aes(x = year, ymin = v2juhcind_osp_codelow, ymax = v2juhcind_osp_codehigh), alpha=0.5) +
  xlim(1970, 2020) +
  ylim(0,4)
    
trust <- read_excel("manual/Vertrauensindex_Institutionen_Zeitreihe_2011-2021.xlsx")

trust %>%
  mutate(year = year(Erhebungszeitraum)) %>%
  pivot_longer(cols = c(Regierung, Parlament, Justiz, VfGH), names_to = "Institution", values_to = "saldo") %>%
  ggplot() +
  geom_line(aes(x = year, y = saldo, linetype = Institution)) +
  ylim(-50, 50) +
  ylab("Trust Saldo") +
  xlab("Year") +
  theme(legend.position = "right") +
  scale_x_continuous(breaks= c(2011, 2012, 2016, 2019, 2021))

ggsave("results/images/trust.pdf", height = 13, width = 20, units = "cm", device = cairo_pdf)

eurobarometer <- readRDS("C:/Users/Jasmin/OneDrive - Universität Hamburg/Desktop/Dissertation/barometer/data/eurobarometer_complete.rds")

plotting_trust <- function(countryname){

  eurobarometer |> 
    filter(country == countryname) |> 
    group_by(year) |> 
    mutate(trust_judiciary = if_else(trust_judiciary == 2, 0, trust_judiciary)) |> 
    summarize(mean = mean(trust_judiciary, na.rm = TRUE),
              share_trust = sum(trust_judiciary == 1, na.rm = TRUE) / n(),
              share_notrust = sum(trust_judiciary == 0, na.rm = TRUE) / n(),
              share_NA = sum(is.na(trust_judiciary) / n())) |> 
    mutate(year = as.integer(year)) ->
    eurobarometer_trust_agg
  
  missing <- tibble(year = c(2011, 2012, 2013))
  
  eurobarometer_trust_agg <- bind_rows(eurobarometer_trust_agg,
                                   missing) |> 
    pivot_longer(cols = starts_with("share"), names_to = "answer", values_to = "share")
  
  theme_update(panel.grid.major.y = element_line(color = "lightgrey"))
  
  eurobarometer_trust_agg |> 
    ggplot(aes(x = year,
               y = share,
               color = answer)) +
    geom_point() +
    geom_line() +
    labs(x = element_blank(),
         y = "Trust Judiciary") +
    scale_x_continuous(breaks = c(2000, 2003, 2006, 2009,  2012, 2015, 2018, 2021),
                       expand = c(0,0),
                       limits = c(2000, 2022)) +
    scale_y_continuous(breaks = seq(0.25, 1, 0.25),
                       expand = c(0,0),
                       limits = c(0,1),
                       labels = label_percent()) +
    scale_color_manual(values = c("share_trust" = color_dark, 
                                  "share_notrust" = color_colorful, 
                                  "share_NA" = color_dark_light),
                       name = element_blank(),
                       labels = c("NA", "Do not trust", "Trust")) +
    coord_cartesian(clip = "off")
  
}

plotting_trust("Austria")

ggsave("results/images/eurobarometer_trust.pdf",
       device = cairo_pdf,
       width = 9,
       height = 5)

plotting_trust("Germany")

min(eurobarometer_trust_agg$share, na.rm = TRUE)
max(eurobarometer_trust_agg$share, na.rm = TRUE)

media <- readRDS("C:/Users/Jasmin/OneDrive - Universität Hamburg/Desktop/Dissertation/Survey/acpp/data/media_party.RDS")

theme_update(axis.line.x = element_blank(),
             axis.ticks.x = element_blank(),
             strip.text = element_text(textsize - 3))

media |> 
  filter(party %in% c("FPÖ", "ÖVP", "SPÖ", "Grüne", "Neos")) |> 
  ggplot(aes(x = party, 
             y = share*100,
             fill = party)) +
  geom_col(position = "dodge",
           show.legend = FALSE) +
  facet_wrap(~newspaper, 
             ncol = 2) +
  scale_y_continuous(limits = c(0,80),
                     breaks = seq(0, 80, by = 20),
                     name = "Share of voters who are readers") +
  scale_fill_manual(values = c("FPÖ" = "#4D4DAE",
                               "ÖVP" = "#C6E4EE", 
                               "Grüne" = "#6A7766", 
                               "Neos" = "#E4BAB7", 
                               "SPÖ" = "#D7887A")) +
  labs(x = element_blank())

ggsave("results/images/media.pdf",
        cairo_pdf,
        width = 21,
        height = 29,
        unit = "cm")  
