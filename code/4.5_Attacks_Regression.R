# library ----
library(stargazer)
library(tidyverse)
library(DAMisc)
library(margins)
library(effects)
library(lubridate)
library(ggeffects)
library(hrbrthemes)
library(ggplot2)
library(lme4)
library(see)
library(ggtext)
library(ggthemes)
library(patchwork)
library(ggpubr)
library(ggtext)
library(miceadds)
library(showtext)
library(texreg)

# functions ----

source("src/attacks_analysis_functions.R")
source("src/graphics.R")

theme_set(theme_regression)

# import data ----

## caselevel data ----

caselevel_dec <- readRDS("data/dataincl2020_final_attacks.rds") |> 
  mutate(threshold = as.integer(if_else(date_2 > ymd("2020-02-11"), 1, 0)),
         pr_dummy_new = as.integer(pr_dummy_new),
         covid = as.integer(covid),
         politician = as.integer(if_else(plaintiff_category == "Politician", 1, 0)),
         court = as.integer(if_else(plaintiff_category == "Court", 1, 0)),
         start_session = as.factor(start_session),
         year = as.factor(year(date_2)),
         session_id = as.integer(if_else(date_2 > ymd("2020-02-11"), session_id, 0))) |> 
  filter(!is.na(share_attacks_break) & date_2 > ymd("2015-01-01")) |> 
  mutate(decision = if_else(str_detect(decision, "sustained"), 1, 0),
         session_id = as.factor(session_id))

# select relevant variables
caselevel_dec <- caselevel_dec |> 
  dplyr::select(pr_dummy_new, session_id, date_2, decision, year, type, 
                gov_brief, court, verhandlung, verkündung, politician, 
                court_president, share_attacks_break, covid, threshold, 
                politician, start_session) 

caselevel_dec_pa <- caselevel_dec |> 
  filter(date_2 > ymd("2016-01-01"))

caselevel_dec <- na.omit(caselevel_dec)

## normlevel data ----

normlevel_dec <- readRDS("data/G_analysis_R.rds") |> 
  filter(!is.na(share_attacks_break) & date_2 > ymd("2015-01-01")) 

normlevel_dec |> 
  filter(date_2 > ymd("2016-01-01")) ->
  normlevel_dec_pa


# MAIN RESULTS  ----

controls <- c("gov_brief", "court", "politician", "covid", "type", "verhandlung")
controls_small <- c("court", "politician", "covid", "type")
interaction_var <- "threshold"
x_var <- "share_attacks_break"

## FE ----

# releases
reg_pr_models_fe <- linear_regression_fe(x = x_var,
                                         y = caselevel_dec$pr_dummy_new,
                                         controls = controls,
                                         dataset = caselevel_dec,
                                         fe = "session_id")
reg_pr_fe <- reg_pr_models_fe$m_pr2
summary(reg_pr_fe$lm_res)


# hearings
reg_ph_models_fe <- linear_regression_fe(x = x_var,
                                         y = caselevel_dec$verhandlung,
                                         controls = controls_small,
                                         dataset = caselevel_dec,
                                         fe = "session_id")
reg_ph_fe <- reg_ph_models_fe$m_pr2
summary(reg_ph_fe$lm_res)


# verkündung
reg_pa_models_fe <- linear_regression_fe(x = x_var,
                                         y = caselevel_dec_pa$verkündung,
                                         controls = controls,
                                         dataset = caselevel_dec_pa,
                                         fe = "session_id")
reg_pa_fe <- reg_pa_models_fe$m_pr2
summary(reg_pa_fe$lm_res)


# ROBUSTNESS ---- 

## DECISION ----

### DECISION CSE ----

# Does the decision differ after attacks?

reg_dec_models <- linear_regression(x = x_var,
                                    y = caselevel_dec$decision,
                                    interaction = interaction_var,
                                    controls = controls,
                                    dataset = caselevel_dec)
reg_dec <- reg_dec_models$m_pr4
summary(reg_dec)



#### Plotting ----


levels <-  seq(
  min(caselevel_dec$share_attacks_break, na.rm = TRUE),
  max(caselevel_dec$share_attacks_break, na.rm = TRUE),
  0.05)

dataset <- caselevel_dec |> 
  mutate(x = share_attacks_break)


plot_dec <- plotter_cse(reg_dec,
                        data = caselevel_dec) 

plot_dec

ggsave("results/images/interaction_dec.pdf", 
       device = cairo_pdf, 
       height = 10, 
       width = 20)

### FE Decision ----

# releases
reg_dec_models_fe <- linear_regression_fe(x = x_var,
                                                  y = caselevel_dec$decision,
                                                  controls = controls,
                                                  dataset = caselevel_dec,
                                                  fe = "session_id")
reg_dec_fe <- reg_dec_models_fe$m_pr2
summary(reg_dec_fe)

### FE & CSE Decision ----

# releases
reg_dec_models_fe_cse <- linear_regression_fe_cse(x = x_var,
                                         y = caselevel_dec$decision,
                                         controls = controls,
                                         dataset = caselevel_dec,
                                         fe = "session_id")
reg_dec_fe_cse <- reg_dec_models_fe_cse$m_pr2
summary(reg_dec_fe_cse$lm_res)

## WITHOUT CSE AND FE ----

# releases
reg_pr_models <- linear_regression(x = x_var,
                                   y = caselevel_dec$pr_dummy_new,
                                   interaction = interaction_var,
                                   controls = controls,
                                   dataset = caselevel_dec)
reg_pr <- reg_pr_models$m_pr4

# hearings
reg_ph_models <- linear_regression(x = x_var,
                                   y = caselevel_dec$verhandlung,
                                   interaction = interaction_var,
                                   controls = controls_small,
                                   dataset = caselevel_dec)
reg_ph <- reg_ph_models$m_pr4

# verkündung
reg_pa_models <- linear_regression(x = x_var,
                                   y = caselevel_dec_pa$verkündung,
                                   interaction = interaction_var,
                                   controls = controls,
                                   dataset = caselevel_dec_pa)
reg_pa <- reg_pa_models$m_pr4



### Plotting ----


levels <-  seq(
  min(caselevel_dec$share_attacks_break, na.rm = TRUE),
  max(caselevel_dec$share_attacks_break, na.rm = TRUE),
  0.05)

dataset <- caselevel_dec |> 
  mutate(x = share_attacks_break)

theme_update(legend.margin = margin(t = 6))


plot_pr <- plotter_cse(reg_pr,
                       caselevel_dec) 
plot_ph <- plotter_cse(reg_ph,
                   caselevel_dec)
plot_pa <- plotter_cse(reg_pa,
                   caselevel_dec_pa)

plot_pr + plot_ph + plot_pa +
  plot_layout(guides = "collect")

ggsave("results/images/interaction_press.pdf", 
       device = cairo_pdf, 
       height = 10, 
       width = 20)

## FE & CSE ----


# releases
reg_pr_models_fe_cse <- linear_regression_fe_cse(x = x_var,
                                         y = caselevel_dec$pr_dummy_new,
                                         controls = controls,
                                         dataset = caselevel_dec,
                                         fe = "session_id")
reg_pr_fe_cse <- reg_pr_models_fe_cse$m_pr2
summary(reg_pr_fe_cse$lm_res)


# hearings
reg_ph_models_fe_cse <- linear_regression_fe_cse(x = x_var,
                                         y = caselevel_dec$verhandlung,
                                         controls = controls_small,
                                         dataset = caselevel_dec,
                                         fe = "session_id")
reg_ph_fe_cse <- reg_ph_models_fe_cse$m_pr2
summary(reg_ph_fe_cse$lm_res)


# verkündung
reg_pa_models_fe_cse <- linear_regression_fe(x = x_var,
                                         y = caselevel_dec_pa$verkündung,
                                         controls = controls,
                                         dataset = caselevel_dec_pa,
                                         fe = "session_id")
reg_pa_fe_cse <- reg_pa_models_fe_cse$m_pr2
summary(reg_pa_fe_cse$lm_res)


## CSE & Interaction ----

# releases
reg_pr_models_cse <- linear_regression_cse(x = x_var,
                                   y = caselevel_dec$pr_dummy_new,
                                   interaction = interaction_var,
                                   controls = controls,
                                   dataset = caselevel_dec)
reg_pr_cse <- reg_pr_models_cse$m_pr4
summary(reg_pr_cse$lm_res)


# hearings
reg_ph_models_cse <- linear_regression_cse(x = x_var,
                                   y = caselevel_dec$verhandlung,
                                   interaction = interaction_var,
                                   controls = controls_small,
                                   dataset = caselevel_dec)
reg_ph_cse <- reg_ph_models_cse$m_pr4
summary(reg_ph_cse$lm_res)


# verkündung
reg_pa_models_cse <- linear_regression_cse(x = x_var,
                                   y = caselevel_dec_pa$verkündung,
                                   interaction = interaction_var,
                                   controls = controls,
                                   dataset = caselevel_dec_pa)
reg_pa_cse <- reg_pa_models_cse$m_pr4
summary(reg_pa_cse$lm_res)


## PROBIT ----

### Probit CSE & FE ----

# releases
reg_pr_probmodels <- probit_regression(x = x_var,
                                   y = caselevel_dec$pr_dummy_new,
                                   interaction = interaction_var,
                                   controls = controls,
                                   dataset = caselevel_dec)
reg_pr_prob <- reg_pr_probmodels$m_pr4
summary(reg_pr_prob$glm_res)

# hearings
reg_ph_probmodels <- probit_regression(x = x_var,
                                   y = caselevel_dec$verhandlung,
                                   interaction = interaction_var,
                                   controls = controls_small,
                                   dataset = caselevel_dec)
reg_ph_prob <- reg_ph_probmodels$m_pr4
summary(reg_ph_prob$glm_res)


# verkündung
reg_pa_probmodels <- probit_regression(x = x_var,
                                   y = caselevel_dec_pa$verkündung,
                                   interaction = interaction_var,
                                   controls = controls,
                                   dataset = caselevel_dec_pa)
reg_pa_prob <- reg_pa_probmodels$m_pr4
summary(reg_pa_prob$glm_res)

### Probit FE ----

# releases
reg_pr_probmodels_fe <- probit_regression_fe(x = x_var,
                                         y = caselevel_dec$pr_dummy_new,
                                         controls = controls,
                                         dataset = caselevel_dec,
                                         fe = "session_id")
reg_pr_probit_fe <- reg_pr_probmodels_fe$m_pr2
summary(reg_pr_probit_fe$glm_res)



# hearings
reg_ph_probmodels_fe <- probit_regression_fe(x = x_var,
                                         y = caselevel_dec$verhandlung,
                                         controls = controls_small,
                                         dataset = caselevel_dec,
                                         fe = "session_id")
reg_ph_probit_fe <- reg_ph_probmodels_fe$m_pr2
summary(reg_ph_probit_fe$glm_res)


# verkündung
reg_pa_probmodels_fe <- probit_regression_fe(x = x_var,
                                         y = caselevel_dec_pa$verkündung,
                                         controls = controls,
                                         dataset = caselevel_dec_pa,
                                         fe = "session_id")
reg_pa_probit_fe <- reg_pa_probmodels_fe$m_pr2
summary(reg_pa_probit_fe$glm_res)

## KURZ ----


reg_pr_kurz <- lm(pr_dummy_new ~ share_attacks_break  + decision + gov_brief + court + politician + kurz_dummy*share_attacks_break,
                data = normlevel_dec)
summary(reg_pr_kurz)

reg_ph_kurz <- lm(verhandlung ~ share_attacks_break  + decision + gov_brief + court + politician + kurz_dummy*share_attacks_break,
                  data = normlevel_dec)
summary(reg_ph_kurz)

reg_pa_kurz <- lm(verkündung ~ share_attacks_break  + decision + gov_brief + court + politician + kurz_dummy*share_attacks_break,
                  data = normlevel_dec_pa)
summary(reg_pa_kurz)


## INTERACTION WITH DECISION ----


interaction_var = "decision"

### with threshold triple interaction -----

# releases
reg_dec_pr <- linear_regression_cse(x = x_var,
                             y = caselevel_dec$pr_dummy_new,
                             interaction = interaction_var,
                             interaction2 = "threshold",
                             controls = controls,
                             dataset = caselevel_dec)
summary(reg_dec_pr$m_pr4$lm_res)



# hearings
reg_dec_ph <- linear_regression_cse(x = x_var,
                             y = caselevel_dec$verhandlung,
                             interaction = interaction_var,
                             interaction2 = "threshold",
                             controls = controls_small,
                             dataset = caselevel_dec)
summary(reg_dec_ph$m_pr4$lm_res)


# verkündung
reg_dec_pa <- linear_regression_cse(x = x_var,
                             y = caselevel_dec_pa$verkündung,
                             interaction = interaction_var,
                             interaction2 = "threshold",
                             controls = controls,
                             dataset = caselevel_dec_pa)
summary(reg_dec_pa$m_pr4$lm_res)

#### without threshold triple interaction----

# releases
reg_dec_pr <- linear_regression_cse(x = x_var,
                                y = caselevel_dec$pr_dummy_new,
                                interaction = interaction_var,
                                controls = controls,
                                dataset = caselevel_dec)
summary(reg_dec_pr$m_pr4$lm_res)




# hearings
reg_dec_ph <- linear_regression_cse(x = x_var,
                                y = caselevel_dec$verhandlung,
                                interaction = interaction_var,
                                controls = controls,
                                dataset = caselevel_dec)
summary(reg_dec_ph$m_pr4$lm_res)



# verkündung
reg_dec_pa <- linear_regression_cse(x = x_var,
                                y = caselevel_dec_pa$verkündung,
                                interaction = interaction_var,
                                controls = controls,
                                dataset = caselevel_dec_pa)
summary(reg_dec_pa$m_pr4$lm_res)



## By Week ----

# weeks_prs_attacks_session |> 
#   mutate(session = if_else(lag(session) == 1, 1, session)) ->
#   weeks_prs_attacks_session
# 
# m_week <- lm(weekly_prs ~ lag(weekly_sum, 2) + session, data = weeks_prs_attacks_session)
# summary(m_week)

# ## MERITS ----
# 
# interaction_var = "threshold"
# 
# # releases
# caselevel_dec |> 
#   filter(type == "erkenntnis") ->
#   caselevel_dec
# 
# caselevel_dec_pa |> 
#   filter(type == "erkenntnis") ->
#   caselevel_dec_pa
# 
# reg_pr_merits_models <- linear_regression_cse(x = x_var,
#                             y = caselevel_dec$pr_dummy_new,
#                             interaction = interaction_var,
#                             controls = controls,
#                             dataset = caselevel_dec)
# reg_pr_merits <- reg_pr_merits_models$m_pr4
# 
# 
# # hearings
# reg_ph_merits_models <- linear_regression_cse(x = x_var,
#                             y = caselevel_dec$verhandlung,
#                             interaction = interaction_var,
#                             controls = controls,
#                             dataset = caselevel_dec)
# reg_ph_merits <- reg_ph_merits_models$m_pr4
# 
# 
# # verkündung
# reg_pa_merits_models <- linear_regression_cse(x = x_var,
#                             y = caselevel_dec_pa$verkündung,
#                             interaction = interaction_var,
#                             controls = controls,
#                             dataset = caselevel_dec_pa)
# reg_pa_merits <- reg_pa_merits_models$m_pr4
# 
# 
# plot_pr <- plotter_cse(reg_pr_merits$lm_res,
#                    caselevel_dec) 
# plot_ph <- plotter_cse(reg_ph_merits$lm_res,
#                    caselevel_dec)
# plot_pa <- plotter_cse(reg_pa_merits$lm_res,
#                    caselevel_dec_pa)
# 
# hist <- dataset |> 
#   ggplot() +
#   geom_histogram(aes(x = share_attacks_break),
#                  binwidth = 0.1) +
#   labs(x = "Average Reports about Attacks Per Day Ahead of Session",
#        y = "") +
#   scale_y_reverse(breaks = c(0, 30, 60))  +
#   theme(panel.border = element_blank()) 
# 
# plot_pr + plot_ph + plot_pa + hist +
#   plot_layout(ncol = 1,
#               heights = c(1,1,1,0.3))
# 
# ggsave("prs/results/images/interaction_pr_merits.pdf", device = cairo_pdf, height = 10, width = 15)
# 
# ##   MERITS FE ----
# 
# # releases
# reg_pr_models_merits_fe <- linear_regression_fe(x = x_var,
#                                          y = caselevel_dec$pr_dummy_new,
#                                          controls = controls,
#                                          dataset = caselevel_dec,
#                                          fe = "session_id")
# reg_pr_merits_fe <- reg_pr_models_merits_fe$m_pr2
# summary(reg_pr_merits_fe$lm_res)
# 
# 
# # hearings
# reg_ph_models_merits_fe <- linear_regression_fe(x = x_var,
#                                          y = caselevel_dec$verhandlung,
#                                          controls = controls,
#                                          dataset = caselevel_dec,
#                                          fe = "session_id")
# reg_ph_merits_fe <- reg_ph_models_merits_fe$m_pr2
# summary(reg_ph_merits_fe$lm_res)
# 
# 
# # verkündung
# reg_pa_models_merits_fe <- linear_regression_fe(x = x_var,
#                                          y = caselevel_dec_pa$verkündung,
#                                          controls = controls,
#                                          dataset = caselevel_dec_pa,
#                                          fe = "session_id")
# reg_pa_merits_fe <- reg_pa_models_merits_fe$m_pr2
# summary(reg_pa_merits_fe$lm_res)
