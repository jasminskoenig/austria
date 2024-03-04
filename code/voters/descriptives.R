library(tidyverse)
library(ggdist)

textsize <- 30
source("src/graphics.R")
source("src/basic_vars.R")

ausnet <- 
  readRDS("data/voters/ausnet/ausnet_clean.rds")

ausnet |> 
  group_by(wave) |> 
  summarise(across(starts_with("trust"), ~ mean(., na.rm = TRUE))) |> 
  pivot_longer(cols = -wave,
               names_to = "institution",
               values_to = "trust") |> 
  ggplot(aes(x = as.numeric(wave), 
             y = trust,
             group = institution,
             color = institution)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(1,10))

ausnet |> 
  filter(party %in% parties & !is.na(trust_constitutional_court)) |> 
  group_by(party, wave) |> 
  ggplot(aes(x = party, 
             y = trust_constitutional_court,
             color = party,
             fill = party)) +
  geom_boxplot(alpha = 0.5) +
  facet_wrap(~wave) +
  scale_color_manual(values = c(color_oevp, color_spoe, color_fpoe, color_neos, color_greens)) +
  scale_fill_manual(values = c(color_oevp, color_spoe, color_fpoe, color_neos, color_greens))

theme_update(axis.line.x = element_blank(),
             axis.ticks.x = element_blank(),
             axis.text.x = element_text(hjust = 0.5),
             panel.grid.major.y = element_line(color = "lightgrey"))

ausnet |> 
  filter(party %in% parties & !is.na(trust_constitutional_court)) |> 
  group_by(party, wave) |> 
  ggplot(aes(x = as.factor(wave), 
             y = trust_constitutional_court,
             color = party,
             fill = party)) +
  geom_boxplot(alpha = 0.3) +
  facet_wrap(~party) +
  scale_color_manual(values = c(color_oevp, color_spoe, color_fpoe, color_neos, color_greens)) +
  scale_fill_manual(values = c(color_oevp, color_spoe, color_fpoe, color_neos, color_greens)) +
  scale_x_discrete(expand = c(0,0)) +
  labs(y = "Trust in the Constitutional Court",
       color = element_blank(),
       fill = element_blank(),
       x = element_blank()) +
  coord_cartesian(clip = "off")

ggsave("slides/slides_survey/images/trust.png",
       width = 8,
       height = 4,
       device = "png")

source("src/graphics.R")

ausnet |> 
  group_by(wave, pop_mean) |> 
  #  summarize(mean = mean(trust_constitutional_court, na.rm = TRUE)) |> 
  filter(party %in% c("OEVP", "SPOE", "FPOE", "NEOS", "Greens")) |> 
  ggplot(aes(x = as.factor(as.numeric(wave)), 
             y = pop_mean)) +
  geom_boxplot() +
  facet_wrap(~ party) +
  scale_y_continuous(limits = c(1,5)) 


# Distribution of Populism Scores in Wave 7
ausnet |> 
  filter(wave == 7) |> 
  mutate(pop_min = as.integer(pop_min)) |> 
  filter(party %in% c("OEVP", "SPOE", "FPOE", "NEOS", "Greens")) |> 
  group_by(party) |> 
  ggplot(aes(x = pop_min,
             color = party,
             fill = party)) +
  geom_density(
    alpha = 0.4,
    bw = 0.4
  ) +
  scale_x_discrete(expand = c(0,0),
                   breaks = seq(from = 0, to = 4, by = 1)) +
  scale_color_manual(values = c(color_oevp,
                                color_spoe, 
                                color_fpoe,
                                color_neos,
                                color_greens)) +
  scale_fill_manual(values = c(color_oevp,
                               color_spoe, 
                               color_fpoe,
                               color_neos,
                               color_greens)) +
  labs(y = "Populism Score",
       x = element_blank()) +
  facet_wrap(~ party) +
  guides(color = FALSE,
         fill = FALSE) +
  coord_cartesian(clip = "off")

theme_update(axis.line.x = element_blank(),
             axis.ticks.x = element_blank(),
             axis.text.x = element_text(hjust = 0),
             panel.grid.major.y = element_line(color = "lightgrey"))

ausnet |> 
  filter(wave == 7) |> 
  filter(party %in% c("OEVP", "SPOE", "FPOE", "NEOS", "Greens")) |> 
  group_by(party) |> 
  ggplot(aes(x = fct_reorder(party, pop_mean), 
             y = pop_mean,
             color = party,
             fill = party)) +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    justification = -.2, 
    .width = 0, 
    point_colour = NA,
    alpha = 0.4
  ) + 
  geom_boxplot(
    width = .15, 
    alpha = 0.4,
    outlier.color =  NA
  ) +
  scale_y_continuous(limits = c(0,4)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_color_manual(values = c(color_oevp,
                                color_spoe, 
                                color_fpoe,
                                color_neos,
                                color_greens)) +
  scale_fill_manual(values = c(color_oevp,
                                color_spoe, 
                                color_fpoe,
                                color_neos,
                                color_greens)) +
  labs(y = "Populism Score",
       x = element_blank()) +
  guides(color = FALSE,
         fill = FALSE) +
  coord_cartesian(clip = "off")

ggsave("slides/slides_survey/images/populismscores.png",
       width = 8,
       height = 4,
       device = "png")


ausnet |> 
  filter(wave == "8" & party %in% parties &!is.na(w8_q82x2)) |>
  mutate(review = to_factor(w8_q82x2)) |> 
  group_by(wave) |> 
  mutate(n = n()) |>
  group_by(wave, party) |> 
  mutate(n_party = n()) |> 
  group_by(wave, party, review) |> 
  mutate(n_answer = n(), 
         share = n_answer / n_party * 100) |> 
  ungroup()|>  
  select(party, share, review)  |> 
  distinct(party,review, .keep_all = TRUE) ->
  opinion_shares

opinion_shares$party <- factor(opinion_shares$party, levels = c("Greens", "SPOE", "NEOS", "OEVP", "FPOE"))

opinion_shares |> 
  ggplot() +
  geom_bar(aes(y = party, 
               x = share, 
               fill = review), 
           position="stack", 
           stat="identity",
           width = 0.6,
           alpha = 0.7)+
  labs(title = "Support Judicial Review - Wave 8",
       y = "",
       x = "Share",
       fill = "OPINION: THE COURTS SHOULD BE ABLE TO STOP THE GOVERNMENT ACTING BEYOND ITS AUTHORITY") +
  scale_fill_manual(values = c("#2A3C24", "#7D8C78", "lightslategrey", "#D09D6A", "#C16200")) +
  guides(fill = guide_legend(title.position="top", 
                             title.hjust = 0.5,
                             reverse = TRUE)) +
  labs(title = element_blank(),
       x = element_blank()) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = c(0,0)) +
  guides(fill = guide_legend(nrow = 2,
                             title.position = "top",
                             byrow = TRUE)) 

ggsave("results/images/judicial_review.pdf", 
       device = cairo_pdf, 
       width = 13, 
       height = 8)

ggsave("slides/slides_survey/images/judicial_review.png", 
       device = "png", 
       width = 7, 
       height = 4)

