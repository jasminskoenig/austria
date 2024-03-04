library(tidyverse)
library(haven)
library(MASS)
library(ggeffects)
library(ggplot2)

source("src/basic_vars.R")

ausnet <- 
  readRDS("data/voters/ausnet/ausnet_clean.rds")

ausnet |> 
  filter(party %in% parties) ->
  ausnet

ausnet |> 
  group_by(id) |> 
  mutate(pop_mean = as.numeric(if_else(wave == 8, lag(pop_mean), NA)),
         pop_max = if_else(wave == 8, lag(pop_max), NA)) |> 
  filter(wave == 8 & party %in% parties) |> 
  filter(!is.na(pop_mean)) |> 
  ungroup(id) ->
  ausnet_w8

ausnet_w8$review <- as_factor(ausnet_w8$w8_q82x2)
ausnet_w8$party <- factor(ausnet_w8$party,
                          levels = c("OEVP", 
                                     "SPOE",
                                     "FPOE",
                                     "NEOS", 
                                     "Greens"))

# OLM ----

ausnet_w8 |> 
  dplyr::select(pop_mean, party, review, age, gender, birthaus, residential, education) |> 
  mutate(across(c(gender, residential, education), ~haven::as_factor(.)),
         age = as.numeric(age)) |> 
  na.omit() ->
  ausnet_w8

table(ausnet_w8$review, ausnet_w8$party)

OLRmodel <- polr(review ~ pop_mean +  relevel(party, ref = "NEOS") + age + gender + birthaus + residential + education, 
                 data = ausnet_w8, 
                 method = "logistic")
summary(OLRmodel)


OIM <- polr(review ~ 1, 
            data = ausnet_w8)
summary(OIM)

anova(OIM,OLRmodel)

(OLRestimates <- coef(summary(OLRmodel)))
p <- pnorm(abs(OLRestimates[, "t value"]), lower.tail = FALSE) * 2

# Coefficients
coef_summary <- summary(OLRmodel)$coefficients

# Odds ratios
odds_ratios <- exp(coef_summary[, 1])

# Confidence intervals
conf_int <- exp(confint(OLRmodel)) |> 
  as.matrix()

library(stargazer)
stargazer(OLRmodel, 
          title = "Results of Ordered Logistic Regression",
          ci = TRUE,
          omit = "gender|birthaus|residential|education",
          covariate.labels = c(
            "Populism Score",
            "ÖVP",
            "SPÖ",
            "FPÖ",
            "Greens"
          ),
          type = "html",
          out = "slides/slides_survey/tables/regression.html")

library(gt)
library(gtsummary)
tbl <-
  ausnet_w8 %>%
  tbl_uvregression(
    y = review, 
    method = polr,
    method.args = list(method = "logistic"),
    exponentiate = TRUE
  ) %>%
  # add significance stars to sig estimates
  add_significance_stars() %>%
  # additioanlly bolding significant estimates
  modify_table_styling(
    columns = estimate,
    rows = p.value < 0.05,
    text_format = "bold"
  )

tbl

table <- tbl_regression(OLRmodel, exponentiate = TRUE) %>%
  add_significance_stars(
    hide_se = FALSE,
    hide_ci = FALSE
    ) 
  modify_header(label = "**Variable**") %>%
  add_p() %>%
  add_significance_stars() %>%
  as_gt()

table

ggpredict(OLRmodel, terms="party") |> 
  #mutate(response.level = ordered(as_factor(response.level))) |> 
  ggplot(aes(x=x, 
             y = predicted,
             color = response.level,
             group = response.level)) +
  geom_point()

m1 <- lm(w8_q82x2 ~  relevel(party, ref = "NEOS") + age + gender + birthaus + residential + education,
         data = ausnet_w8)
summary(m1)

m11 <- lm(w8_q82x2 ~  relevel(party, ref = "Greens") + age + gender + birthaus + residential + education,
         data = ausnet_w8)
summary(m11)

m2 <- lm(trust_constitutional_court ~ relevel(party, ref = "NEOS") + + age + gender + birthaus + residential + education + as.factor(wave),
         data = ausnet)
summary(m2)

m3 <- lm(trust_constitutional_court ~ pop_mean + relevel(party, ref = "NEOS") + age + gender + birthaus + residential + education + in_power + as.factor(wave),
         data = ausnet)
summary(m3)

m31 <- lm(trust_constitutional_court ~ pop_mean + relevel(party, ref = "NEOS") + age + gender + birthaus + residential + education + in_power,
         data = ausnet)
summary(m31)

m33 <- lm(trust_constitutional_court ~ pop_max + relevel(party, ref = "NEOS") + age + gender + birthaus + residential + education + in_power + as.factor(wave),
         data = ausnet)
summary(m33)

m4 <- lm(w8_q82x2 ~ pop_mean + 
           relevel(party, ref = "NEOS") + 
           age + gender + birthaus + 
           residential + education,
         data = ausnet_w8)
summary(m4)
saveRDS(m4, "slides/slides_survey/tables/regressionw8.rds")
saveRDS(m3, "slides/slides_survey/tables/regressiontrust.rds")


