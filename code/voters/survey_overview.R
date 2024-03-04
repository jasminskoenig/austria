library(tidyverse)
library(readxl)
library(ggplot2)
library(hrbrthemes)
library(ggalt)


# Data Import
overview <- read_excel("voters/overview.xlsx")

overview |> 
  separate(Start, sep = "–", into = c("Start", "end")) |> 
  mutate(End = if_else(is.na(End), end, End)) |>  
  select(-end) |> 
  mutate_at(c("Start", "End"), ~str_replace_all(.,
                                                c("October" = ".10.",
                                                "Jun" = ".6",
                                                "Oct" = ".10",
                                                "Feb" = ".2",
                                                "Sept" = ".9",
                                                "Sep" = ".9",
                                                "Mar" = ".3",
                                                "December" = ".12.",
                                                "Dec" = ".12",
                                                "Aug" = ".8",
                                                "Jan" = ".1"))) |> 
  mutate_at(c("Start", "End"), ~str_remove_all(., "\\s")) |> 
  mutate_at(c("Start", "End"), ~ str_replace(., "(\\.\\.)", ".")) |> 
  mutate_at(c("Start", "End"), ~ dmy(.)) |> 
  mutate(Study = case_when(
    str_detect(Study, "AUTNES") ~ "Austrian \n Election Study",
    str_detect(Study, "Eurobarometer") ~ "Special \n Eurobarometer",
    str_detect(Study, "FlashEB") ~ "Flash \n Eurobarometer",
    str_detect(Study, "EBS") ~ "Standard \n Eurobarometer",
    str_detect(Study, "EB") ~ "Standard \n Eurobarometer",
    str_detect(Study, "Radar") ~ "Democracy \n Radar",
    TRUE ~ Study)) |> 
  rowid_to_column()  |> 
  filter(!is.na(Study)) ->
  overview_clean

decisions <- readRDS("data/G_analysis.rds")

decisions |> 
  filter(government == "Kurz I" & decision == "sustained") ->
  decisions_filtered

add <- tibble(
  review_attack = c(1, 1),
  date_2 = c(ymd("2021-03-03"), ymd("2021-05-12"))
)

decisions_filtered |> 
  mutate(review_attack = 0)  |>
  select(review_attack, date_2) |> 
  rbind(add) ->
  rev_att

plot <- overview_clean |> 
  filter(Start > ymd("2016-01-01")) |>  
  ggplot() +
  geom_segment(aes(x = ymd("2017-12-17"),
                   xend = ymd("2019-05-28"),
                   y = Study,
                   yend = Study),
               linewidth = 100,
               color = "#EBF6F6", 
               alpha = 0.4) +
  geom_segment(aes(x = Start, 
                xend = End,
                y = Study,
                yend = Study,
                color = Questions),
               linewidth = 3,
               alpha = 0.9) + 
  geom_vline(data = rev_att,
             aes(xintercept = date_2,
                  linetype = as.factor(review_attack)),
             linewidth = 0.2) +
  labs(y = "",
       x = "",
       linetype = "Event",
       color = "Questions",
       title = "Survey, Review & Government Timing",
       caption = "Light blue background is the FPÖ-ÖVP Government.") +
  xlim(ymd("2017-01-01"), ymd("2022-12-31")) +
  theme_ipsum(grid = "Y") +
  theme(strip.text = element_blank(),
        legend.position = "bottom",
        legend.box = "vertical",
        plot.caption = element_text(
          hjust = 0.5,
          face = "plain"
        )) +
  scale_linetype_manual(values = c("dashed", "solid"),
                     label = c("Judicial Review", "Attacks on Judiciary")) +
  scale_color_manual(values = c("#B47978", "#F2E86D", "#381D2A", "#6B9080"),
                        label = c("Battery on Liberal Democracy", "Strong Leader", "Perceived Independence", "Trust")) 

plot
ggsave("voters/overview.pdf", device = cairo_pdf, width = 10, height = 7)

