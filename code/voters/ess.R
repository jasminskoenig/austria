library(readr)
library(tidyverse)

source("src/graphics.R")

ESS10 <- read_csv("data/voters/ess/ESS10-ESS10SC.csv")
ESS_select <- read_csv("data/voters/ess/ESS10-subset.csv")

# ANALYSIS DE ----

ESS10 |> 
  filter(cntry == "DE") |> 
  select(party = prtvfde2, 
         courts = cttresac) |>
  group_by(party, courts) |> 
  summarize(n_answer = n(),
            .groups = "drop") |> 
  group_by(party) |> 
  mutate(party = na_if(party, 88),
         party = na_if(party, 66),
         party = na_if(party, 99),
         courts = na_if(courts, 99),
         n_party = sum(n_answer),
         share = n_answer/n_party,
         party = as.factor(party),
         party = fct_recode(party,
                            "CDU/CSU" = "1",
                            "SPD" = "2",
                            "Linke" = "3",
                            "Greens" = "4",
                            "FDP" = "5",
                            "AfD" = "6",
                            "Other" = "7"),
         courts_rel = case_when(
           courts %in% c(0,1,2) ~ "Disagree",
           courts %in% c(3,4) ~ "Mostly Disagree",
           courts == 5 ~ "Partly Agree / Partly Disagree",
           courts %in% c(6,7) ~ "Mostly Agree",
           courts %in% c(8,9,10) ~ "Agree"
         ),
         courts_rel = fct_relevel(courts_rel, 
                                  c("Disagree", 
                                    "Mostly Disagree", 
                                    "Partly Agree / Partly Disagree",
                                    "Mostly Agree",
                                    "Agree"))) |> 
  ungroup() |>
  filter(!is.na(party)) |> 
  ggplot(aes(y = party,
             x = share,
             fill = as.factor(courts_rel))) + 
  geom_col() 

## ANALYSIS AUS

ESS10 |> 
  filter(cntry == "AT") |> 
  select(party = prtvtcat, 
         courts = cttresac) |> 
  group_by(party, courts) |> 
  summarize(n_answer = n(),
            .groups = "drop") |> 
  group_by(party) |> 
  mutate(party = na_if(party, 88),
         party = na_if(party, 66),
         party = na_if(party, 77),
         party = na_if(party, 77),
         courts = na_if(courts, 99),
         n_party = sum(n_answer),
         share = n_answer/n_party,
         party = as.factor(party),
         party = if_else(party %in% c(4,6,8,9), "Other", party),
         party = fct_recode(party,
                            "SPÖ" = "1",
                            "ÖVP" = "2",
                            "FPÖ" = "3",
                            "Greens" = "5",
                            "NEOS" = "7"),
         courts_rel = case_when(
           courts %in% c(0,1,2) ~ "Disagree",
           courts %in% c(3,4) ~ "Mostly Disagree",
           courts == 5 ~ "Partly Agree / Partly Disagree",
           courts %in% c(6,7) ~ "Mostly Agree",
           courts %in% c(8,9,10) ~ "Agree"
         ),
         courts_rel = fct_relevel(courts_rel, 
                                  c("Disagree", 
                                    "Mostly Disagree", 
                                    "Partly Agree / Partly Disagree",
                                    "Mostly Agree",
                                    "Agree"))) |> 
  ungroup() |>
  filter(!is.na(party), party != "Other") |> 
  ggplot(aes(y = party,
             x = share,
             fill = as.factor(courts_rel))) + 
  geom_col()
