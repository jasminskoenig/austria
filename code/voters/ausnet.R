library(tidyverse)
library(plotrix)
library(ggplot2)
library(hrbrthemes)
library(ggdist)
library(scales)
library(patchwork)

# functions

share_calculation <- function(df, variable, grouping_var){
  
  df |> 
    filter(!is.na({{variable}}) & party %in% parties) |>
    left_join(wave_participation, by = "id") |> 
    unite(col = "waves_needed", w1_panelist, w3_panelist, w6_panelist, w7_panelist, w11_panelist, w12_panelist, sep = ",") |> 
    filter(!str_detect(waves_needed, "0|NA")) ->
    df2
  
    df2 |>
      group_by(wave) |>
      mutate(n_wave = n()) |>
      group_by(wave, {{grouping_var}}) |>
      mutate(n_group = n(),
             share_group = n_group / n_wave) |>
      group_by(wave, {{grouping_var}}, {{variable}}) |>
      mutate(n_variable = n(),
             share = n_variable / n_group * 100) |>
      ungroup()|>
      select(wave, {{grouping_var}}, {{variable}}, share, n_group, share_group)  |>
      distinct(wave, {{grouping_var}}, {{variable}}, .keep_all = TRUE) ->
      shares
    
    shares |> 
      mutate(reordered = case_when(
        wave == "w7_" ~ 4,
        wave == "w6_" ~ 3,
        wave == "w3_" ~ 2,
        wave == "w13_" ~ 6,
        wave == "w11_" ~ 5,
        wave == "w1_" ~ 1
      )) ->
      shares

  return(shares)
}



# data 
load("voters/AUSNET.RData")

ausnet <- table

load("voters/ausnet_variables.RData")

ausnet_var <- x

ausnet_var |> 
  mutate(Label =  str_to_lower(Label)) |> 
  filter(str_detect(Label, "(party choice: prospective|vote intention|trust: the constitutional court|opinion: the courts should|date of interview)")) |>
  pull(variable) ->
  variables_keep
variables_keep <- c(variables_keep, "w6_q57")
variables_keep <- variables_keep[! variables_keep %in% c("w8_q65", "w9_q5")]

ausnet_var |> 
  mutate(Label =  str_to_lower(Label)) |> 
  filter(str_detect(Label, "(party choice: prospective|vote intention)")) |>
  pull(variable) ->
  variables_party

variables_party <- variables_party[! variables_party %in% c("w8_q65", "w9_q5")]
variables_party <- c(variables_party, "w6_q57")

ausnet_var |> 
  mutate(Label =  str_to_lower(Label)) |> 
  filter(str_detect(Label, "trust: the constitutional court")) |>
  pull(variable) ->
  variables_trust

ausnet |> 
  select(id, ends_with("_panelist")) |> 
  mutate_all(~haven::as_factor(.)) ->
  wave_participation

ausnet |> 
  select(id, sd7) |> 
  mutate_all(~haven::as_factor(.)) ->
  education

ausnet |> 
  select(id, starts_with("w7_q8")) |> 
  filter(if_any(everything(), ~ !is.na(.))) ->
  ausnet_populism

ausnet |> 
  select(all_of(variables_keep), ends_with("_panelist"), contains("w8_q82"), id) ->
  ausnet_small

# warning does not matter, NAs are "refused" for example
ausnet_small |> 
  mutate_at(vars(-contains("date")), ~haven::as_factor(.)) |> 
  mutate_at(vars(contains("date")), ~factor(.)) |>
  pivot_longer(cols = !id, names_to = "variable", values_to = "value") |>
  mutate(wave = str_extract(variable, "w.*_")) |> 
  pivot_wider(id_cols = c(wave, id), names_from = variable, values_from = value) |>  
  unite(col = "date", contains("date"), sep = "") |> 
  mutate(date = str_remove_all(date, "NA")) |> 
  filter(date != "") |> 
  select(-contains("dte"), -contains("_date"), -contains("_panelist")) |> 
  unite(col = "party", all_of(variables_party), sep = "") |> 
  unite(col = "trust", all_of(variables_trust), sep = "") |> 
  mutate_at(c("party", "trust"), ~str_remove_all(., "NA")) |> 
  mutate(trust = case_when(
    trust == "completely trust" ~ "10",
    trust == "don't trust at all" ~ "0",
    trust == "do not trust at all" ~ "0",
    TRUE ~ trust 
    ),
    trust = as.numeric(trust),
    party = case_when(
    party == "The Greens" ~ "Greens",
    str_detect(party, "Sebastian Kurz") ~ "OEVP",
    TRUE ~ party
    ))  ->
  ausnet_clean

ausnet_clean |> 
  filter(!is.na(trust)) |> 
  group_by(wave) |> 
  mutate(n = n()) |> 
  group_by(wave, party) |> 
  mutate(n_party = n())

parties <- c("NEOS", "FPOE", "Greens", "OEVP", "SPOE")

# descriptives ----

theme_base<- theme_ipsum_rc(grid = "X") +
  theme(
    axis.text = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.text.y.left = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y.left = element_text(size = 20),
    strip.text = element_text(size = 22),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.key.size = unit(0.5, "cm"),
    plot.margin = grid::unit(c(1, 0.5, 0.5, 0), "mm"),
    panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1)
  )

theme_set(theme_gridY)

decisions <- readRDS("data/G_analysis.rds")

decisions |> 
  filter(government == "Kurz I" & decision == "sustained") ->
  decisions_filtered

ausnet_clean |>
  filter(!is.na(trust) & party %in% parties) |> 
  mutate(wave = case_when(
    wave == "w1_" ~ "Wave 1",
    wave == "w3_" ~ "Wave 3",
    wave == "w6_" ~ "Wave 6 \nPopulist Government got into Power ahead of Wave",
    wave == "w7_" ~ "Wave 7",
    wave == "w11_" ~ "Wave 11",
    wave == "w13_" ~ "Wave 13 \nJudicial Review of Populist Laws Ahead of Wave"
  )) |> 
  ggplot(aes(x = trust, y = party, group = party, color = party)) +
  ggdist::stat_halfeye(aes(fill = party), 
                       alpha = 0.2, 
                       adjust = .5, 
                       width = .3, 
                       .width = c(0.5, 1)) + 
  geom_boxplot(alpha = 0.4, width = 0.3) +
  facet_wrap(~ fct_reorder(wave, ymd(date)), ncol = 1) +
  labs(title = "Trust in the Austrian Constitutional Court Among Parties",
       y = "",
       x = "Trust",
       color = "Party",
       fill = "Party") +
  scale_color_manual(values = c("darkblue", "#2A3C24", "#D89C98", "lightblue", "#C65441")) +
  scale_fill_manual(values = c("darkblue", "#2A3C24", "#D89C98", "lightblue", "#C65441"))

ggsave("voters/trust.pdf", device = cairo_pdf, width = 16, height = 20)


ausnet_clean |> 
  filter(!is.na(trust) & party == "FPOE") |> 
  mutate(month = floor_date(ymd(date), "month")) |>  
  ggplot(aes(x = month, y = trust, group = month)) +
  geom_rect(aes(xmin = ymd("2017-12-01"),
                xmax = ymd("2019-05-28"),
                ymin = 1,
                ymax = 10),
            color = "#EBF6F6", 
            fill = "#EBF6F6", 
            alpha = 0.4) +
  ggdist::stat_halfeye(aes(fill = party), 
                       alpha = 0.2, 
                       adjust = .5, 
                       width = 60, 
                       .width = c(0.5, 1),
                       fill = "darkblue") + 
  geom_vline(data = decisions_filtered, aes(xintercept = date_2), linetype = "dotted") +
  labs(title = "Trust in the Austrian Constitutional Court Among FPÖ Voters",
       y = "Trust",
       x = "") +
  ylim(0,10) +
  geom_boxplot(alpha = 0.4, width = 15, color = "darkblue") 

ggsave("voters/fpoe_trust.pdf", device = cairo_pdf, width = 10, height = 5)

ausnet_clean |> 
  filter(wave == "w8_" & party %in% parties) |> 
  group_by(wave) |> 
  mutate(n = n()) |> 
  group_by(wave, party) |> 
  mutate(n_party = n()) |> 
  group_by(wave, party, w8_q82x2) |> 
  mutate(n_answer = n(), 
         share = n_answer / n_party * 100) |> 
  ungroup()|>  
  select(party, share, w8_q82x2)  |> 
  distinct(party,w8_q82x2, .keep_all = TRUE) ->
  opinion_shares

opinion_shares$party <- factor(opinion_shares$party, levels = c("Greens", "SPOE", "NEOS", "OEVP", "FPOE"))

opinion_shares |> 
  ggplot() +
  geom_bar(aes(y = party, x = share, fill = w8_q82x2), 
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
                             reverse = TRUE)) ->
  opinion_shares_plot

saveRDS(opinion_shares_plot, "voters/judicial_review.rds")

opinion_shares_plot
ggsave("voters/judicial_review.pdf", device = cairo_pdf, width = 13, height = 8)

trust_shares <- share_calculation(ausnet_clean, trust, party)

trust_fpoe <- trust_shares |> 
  mutate(trust = as.factor(trust)) |> 
  mutate(wave = case_when(
    wave == "w1_" ~ "Wave 1",
    wave == "w3_" ~ "Wave 3",
    wave == "w6_" ~ "Populist Government in Office\nAhead of Wave 6",
    wave == "w7_" ~ "Populist Government Left\n Ahead of Wave 7",
    wave == "w11_" ~ "Wave 11",
    wave == "w13_" ~ "Judicial Review Ahead of\nWave 13"
  )) |> 
  filter(party == "FPOE") |> 
  ggplot() +
  geom_bar(aes(y = fct_rev(fct_reorder(wave, reordered)), x = share, fill = trust), 
           position="stack", 
           stat="identity") +
  labs(title = "Trust in the Constitutional Court",
       y = "",
       x = "Share",
       fill = "Trust in the Constitutional Court") +
  scale_fill_manual(values = c("#B63907", "#BA4B1F", "#B36344", "#B66E51", "#D09D6A", "lightslategrey", "#CAD593", "#99A26B", "#636947", "#484C32", "#2A3C24"),
                    labels = c("0 - Do not trust at all", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10 - Completely trust")) +
  guides(fill = guide_legend(title.position="top", 
                             title.hjust = 0.5))

theme_update(axis.text.x = element_text(size = 20),
             axis.text.y.left = element_blank(),
             axis.title.y.left = element_blank(),
             axis.title.x = element_blank(),
             panel.border = element_blank())

n <- trust_shares |> 
  filter(party == "FPOE") |> 
  distinct(wave, .keep_all =  TRUE) |> 
  ggplot(aes(x = n_group, y = wave)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste(round(share_group, 2), "%", sep = ""), 
                x = n_group + 150)) +
  scale_x_continuous(labels = comma,
                     breaks = c(0, 400, 800))

trust_fpoe + n +
  plot_layout(widths = c(2, 0.7))

ggsave("voters/fpoe_trust_likert.pdf", device = cairo_pdf, width = 13, height = 8)

# only all waves

ausnet_clean |> 
  left_join(wave_participation, by = "id") |> 
  filter(wave %in% c("w1_", "w3_", "w6_", "w7_", "w11_", "w13_")) |> 
  unite(col = "waves_needed", w1_panelist, w3_panelist, w6_panelist, w7_panelist, w11_panelist, w12_panelist, sep = ",") |> 
  filter(!str_detect(waves_needed, "0|NA")) |> 
  group_by(id) |> 
  arrange(id, date) |>  
  mutate(trust_change = trust - lag(trust)) |> 
  ungroup() ->
  ausnet_clean_change


ausnet_clean_change |>
  group_by(id) |> 
  filter(any(party == "FPOE")) |> 
  mutate(is_all_FPOE = if_else(all(party == "FPOE"), 1, 0)) |> 
  ggplot(aes(x = date, y = trust)) +
  geom_line(aes(group = id,
                color = as.factor(is_all_FPOE)), 
            alpha = 0.6) +
  theme_void() +
  theme(panel.grid.major.y = element_blank()) +
  scale_color_manual(values = c("#BCD8C1", "#E9D985"))
  guides(color = "none")

ausnet_clean_change |> 
  group_by(id) |> 
  mutate(ever_fpoe = if_else(any(party == "FPOE"), 1, 0)) |> 
  mutate(is_all_FPOE = if_else(all(party == "FPOE"), 1, 0)) |> 
  mutate(fpoe_refined = case_when(
    ever_fpoe == 1 & is_all_FPOE == 1 ~ "always",
    ever_fpoe == 1 & is_all_FPOE != 1 ~ "sometimes",
    ever_fpoe == 0 ~ "never",
    TRUE ~ NA_character_
  )) |> 
  mutate(month = floor_date(ymd(date), "month")) |> 
  ungroup() ->
  ausnet_clean_fpoetypes

ausnet_clean_fpoetypes |> 
  group_by(fpoe_refined) |> 
  count() 

theme_update(axis.text.x = element_text(size = 20),
             axis.text.y.left = element_text(size = 20),
             axis.title.y.left = element_text(size = 20),
             axis.title.x = element_blank(),
             panel.border = element_blank(),
             panel.grid.major.x = element_blank(),
             panel.grid.major.y = element_line(color = "lightgrey"))

# Change in Trust

ausnet_clean_fpoetypes |> 
  group_by(fpoe_refined, month) |> 
  mutate(month = if_else(wave == "w7_", ymd("2019-02-01"), month)) |> 
  mutate(mean = mean(trust_change, na.rm = TRUE),
            sd = std.error(trust_change, na.rm = TRUE)) |> 
  mutate(wave = case_when(
    wave == "w7_" ~ 4,
    wave == "w6_" ~ 3,
    wave == "w3_" ~ 2,
    wave == "w13_" ~ 6,
    wave == "w11_" ~ 5,
    wave == "w1_" ~ 1
  )) |> 
  arrange(wave) |> 
  mutate(month = if_else(fpoe_refined == "sometimes", month + 5, if_else(fpoe_refined == "always", month + 10, month))) |> 
  ggplot(aes(x = month)) +
  geom_rect(aes(xmin = ymd("2017-12-01"),
                xmax = ymd("2019-05-28"),
                ymin = -1.5,
                ymax = 1.5),
            color = "#EBF6F6", 
            fill = "#EBF6F6", 
            alpha = 0.4) +
  geom_vline(data = decisions_filtered, aes(xintercept = date_2), linetype = "dotted") +
  geom_hline(aes(yintercept = 0)) +
  geom_pointrange(aes(y = mean,
                      ymin = mean - sd, 
                      ymax = mean + sd, 
                      color = as.factor(fpoe_refined))) +
  scale_color_manual(values = c("#aa8036", "lightblue", "darkblue"),
                     labels = c("never", "sometimes", "always")) +
  labs(y = "Mean & Standard Error (Change of Trust)",
       color = "Vote Intention FPÖ") +
  coord_cartesian(ylim=c(-1.5, 1.5)) 


# Absolute Trust
ausnet_clean_fpoetypes |> 
  group_by(fpoe_refined, month) |> 
  mutate(month = if_else(wave == "w7_", ymd("2019-02-01"), month)) |> 
  mutate(mean = mean(trust, na.rm = TRUE),
         sd = sd(trust, na.rm = TRUE)) |> 
  mutate(wave = case_when(
    wave == "w7_" ~ 4,
    wave == "w6_" ~ 3,
    wave == "w3_" ~ 2,
    wave == "w13_" ~ 6,
    wave == "w11_" ~ 5,
    wave == "w1_" ~ 1
  )) |> 
  arrange(wave) |> 
  mutate(month = if_else(fpoe_refined == 1, month + 5, if_else(fpoe_refined == 2, month + 10, month))) |> 
  ggplot(aes(x = month)) +
  geom_rect(aes(xmin = ymd("2017-12-01"),
                xmax = ymd("2019-05-28"),
                ymin = 0,
                ymax = 10),
            color = "#EBF6F6", 
            fill = "#EBF6F6", 
            alpha = 0.4) +
  geom_vline(data = decisions_filtered, aes(xintercept = date_2), linetype = "dotted") +
  geom_hline(aes(yintercept = 0)) +
  geom_pointrange(aes(y = mean,
                      ymin = mean - sd, 
                      ymax = mean + sd, 
                      color = as.factor(fpoe_refined))) +
  scale_color_manual(values = c("#aa8036", "lightblue", "darkblue"),
                     labels = c("never", "sometimes", "always")) +
  labs(y = "Mean & Standard Deviation (Trust)",
       color = "Vot Intention FPÖ") +
coord_cartesian(ylim=c(0, 10)) 

trust_shares_types <- share_calculation(ausnet_clean_fpoetypes, trust, fpoe_refined)

trust_shares_types$fpoe_refined <- factor(trust_shares_types$fpoe_refined, 
                                          levels = c("never", "sometimes", "always"))

trust_shares_types |> 
  mutate(trust = as.factor(trust)) |> 
  mutate(wave = case_when(
    wave == "w1_" ~ "Wave 1",
    wave == "w3_" ~ "Wave 3",
    wave == "w6_" ~ "Populist Government in Office\nAhead of Wave 6",
    wave == "w7_" ~ "Populist Government Left\n Ahead of Wave 7",
    wave == "w11_" ~ "Wave 11",
    wave == "w13_" ~ "Judicial Review Ahead of\nWave 13"
  )) |> 
  ggplot() +
  geom_bar(aes(y = fct_rev(fct_reorder(wave, reordered)), x = share, fill = trust), 
           position="stack", 
           stat="identity") +
  facet_grid(~ fpoe_refined) +
  labs(title = "Trust in the Constitutional Court",
       subtitle = "Vote Intention FPÖ...",
       y = "",
       x = "Share",
       fill = "Trust in the Constitutional Court") +
  scale_fill_manual(values = c("#B63907", "#BA4B1F", "#B36344", "#B66E51", "#D09D6A", "lightslategrey", "#CAD593", "#99A26B", "#636947", "#484C32", "#2A3C24"),
                    labels = c("0 - Do not trust at all", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10 - Completely trust")) +
  guides(fill = guide_legend(title.position="top", 
                             title.hjust = 0.5))

# See boxplot per wave

own_boxplot <- function(wavefill) {
  
  ausnet_clean_change |> 
    filter(wave == wavefill) |> 
    group_by(id) |> 
    mutate(ever_fpoe = if_else(any(party == "FPOE"), 1, 0)) |> 
    mutate(is_all_FPOE = if_else(all(party == "FPOE"), 1, 0)) |> 
    ggplot() +
    geom_boxplot(aes(x = wave, y = trust, color = as.factor(is_all_FPOE))) +
    coord_cartesian(ylim=c(0, 10)) ->
    plotted
  
  return(plotted)
  
}

waves <- c("w7_", "w6_", "w3_", "w13_", "w11_", "w1_")

plots_perwave = lapply(waves, own_boxplot)

plots_perwave[[1]] + plots_perwave[[2]] + plots_perwave[[3]] + plots_perwave[[4]] + plots_perwave[[5]] + plots_perwave[[6]]


# EDUCATION

ausnet_clean_fpoetypes |> 
  left_join(education, by = "id") |> 
  mutate(month = if_else(wave == "w7_", ymd("2019-02-01"), month)) |> 
  group_by(sd7, month) |> 
  mutate(mean = mean(trust_change, na.rm = TRUE),
         sd = std.error(trust_change, na.rm = TRUE)) |> 
  mutate(wave = case_when(
    wave == "w7_" ~ 4,
    wave == "w6_" ~ 3,
    wave == "w3_" ~ 2,
    wave == "w13_" ~ 6,
    wave == "w11_" ~ 5,
    wave == "w1_" ~ 1
  )) |> 
  arrange(wave) |> 
  ggplot(aes(x = month)) +
  geom_rect(aes(xmin = ymd("2017-12-01"),
                xmax = ymd("2019-05-28"),
                ymin = -1.5,
                ymax = 1.5),
            color = "#EBF6F6", 
            fill = "#EBF6F6", 
            alpha = 0.4) +
  geom_vline(data = decisions_filtered, aes(xintercept = date_2), linetype = "dotted") +
  geom_hline(aes(yintercept = 0)) +
  geom_pointrange(aes(y = mean,
                      ymin = mean - sd, 
                      ymax = mean + sd, 
                      color = as.factor(sd7))) +
  labs(y = "Mean & Standard Error (Change of Trust)",
       color = "Vote Intention FPÖ") +
  coord_cartesian(ylim=c(-1.5, 1.5)) 

# POPULISM

ausnet_populism %>%
  mutate(across(starts_with("w7_q8"), ~ na_if(., 88))) ->
  ausnet_populism_fil
  
ausnet_populism_f <- ausnet_populism_fil[complete.cases(ausnet_populism_fil), ]

matrix_populism <- as.matrix(ausnet_populism_f[, -1])
rownames(matrix_populism) <- ausnet_populism_f$id

factor_analysis_result <- factanal(x = matrix_populism, 
         factors = 1)

factor_loadings <- factor_analysis_result$loadings
factor_scores <- factor_analysis_result$scores
respondent_scores <- as.data.frame(as.matrix(ausnet_populism_f[, -1]) %*% factor_loadings)

respondent_scores |> 
  cbind(ausnet_populism_f) ->
  ausnet_populism_index

ausnet_populism_index$Factor1_scale <- scale(ausnet_populism_index$Factor1) 
ausnet_populism_index$Factor1_robscale <- scale(ausnet_populism_index$Factor1, 
                                                center = FALSE,
                                                scale = FALSE) 

ausnet_populism_index |> 
  as.data.frame() |> 
  mutate(id = as.factor(id)) ->
  ausnet_populism_index

ausnet_clean |> 
  filter(party %in% parties) |> 
  left_join(ausnet_populism_index, by = "id") |> 
  ggplot(aes(x = party, y = Factor1_robscale)) +
  ggdist::stat_halfeye(aes(fill = party), alpha = 0.7) +
  labs(fill = "",
       y = "Populism Score (robust scale)") +
  scale_fill_manual(values = c("darkblue", "#2A3C24", "#D89C98", "lightblue", "#C65441"))


ausnet_clean |> 
  left_join(ausnet_populism_index, by = "id") |> 
  mutate(populist = if_else(Factor1_robscale < 3, "High", if_else(Factor1_robscale < 6, "Medium", "Low"))) |> 
  filter(party %in% parties & !is.na(populist)) ->
  ausnet_clean_populistvoters

trust_shares_populists <- share_calculation(ausnet_clean_populistvoters, trust, populist)


trust_shares_populists |> 
  mutate(trust = as.factor(trust)) |> 
  mutate(wave = case_when(
    wave == "w1_" ~ "Wave 1",
    wave == "w3_" ~ "Wave 3",
    wave == "w6_" ~ "Populist Government in Office\nAhead of Wave 6",
    wave == "w7_" ~ "Populist Government Left\n Ahead of Wave 7",
    wave == "w11_" ~ "Wave 11",
    wave == "w13_" ~ "Judicial Review Ahead of\nWave 13"
  )) |> 
  ggplot() +
  geom_bar(aes(y = fct_rev(fct_reorder(wave, reordered)), x = share, fill = trust), 
           position="stack", 
           stat="identity") +
  facet_grid(~ fct_relevel(populist, "High", "Medium", "Low")) +
  labs(title = "Trust in the Constitutional Court",
       subtitle = "Populism Index...",
       y = "",
       x = "Share",
       fill = "Trust in the Constitutional Court") +
  scale_fill_manual(values = c("#B63907", "#BA4B1F", "#B36344", "#B66E51", "#D09D6A", "lightslategrey", "#CAD593", "#99A26B", "#636947", "#484C32", "#2A3C24"),
                    labels = c("0 - Do not trust at all", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10 - Completely trust")) +
  guides(fill = guide_legend(title.position="top", 
                             title.hjust = 0.5))

# alternative way of defining populism

ausnet_populism_fil |> 
  pivot_longer(cols = !id, 
               names_to = "question",
               values_to = "answer") |> 
  group_by(id) |> 
  filter(!is.na(answer)) |> 
  summarize(pop_mean = mean(answer),
            pop_min = min(answer)) |> 
  mutate(id = as.factor(id)) ->
  ausnet_populism_mean

ausnet_clean |> 
  filter(party %in% parties) |> 
  left_join(ausnet_populism_mean, by = "id") |> 
  ggplot(aes(x = party, y = pop_mean, group = party)) +
  ggdist::stat_halfeye(aes(fill = party), alpha = 0.7) +
  labs(fill = "",
       y = "Mean Populism Score") +
  scale_fill_manual(values = c("darkblue", "#2A3C24", "#D89C98", "lightblue", "#C65441"))


ausnet_clean |> 
  filter(party %in% parties) |> 
  left_join(ausnet_populism_mean, by = "id") |> 
  ggplot(aes(x = party, y = pop_min, group = party)) +
  ggdist::stat_halfeye(aes(fill = party), alpha = 0.7) +
  labs(fill = "",
       y = "Minimum Populism Score") +
  scale_fill_manual(values = c("darkblue", "#2A3C24", "#D89C98", "lightblue", "#C65441"))



         
