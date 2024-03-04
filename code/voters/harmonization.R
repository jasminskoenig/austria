library(tidyverse)
library(codebook)
library(labelled)

source("src/voters/survey_cleaning.R")

# Import Data and Choose Variables to keep ----

## Digitize Data ----
# digitize <- read.csv("data/voters/digitize/digitize_v5.csv")
# digitize_variables <- read.csv("data/voters/digitize/variables.csv")
# 
# digitize_variables |> 
#   filter(str_detect(Label, str_to_upper("gericht|sonntagsfrage|alter |geschlecht|gruppenidentifikation")) | 
#            # populism items
#            str_detect(variable, "Q40")|
#            variable == "ID") |> 
#   filter(str_to_upper(Label) == Label | variable == "ID") ->
#   variables_keep_digitize


## AUSNET Data 2017 ----
# data 
load("data/voters/ausnet/AUSNET.RData")
ausnet <- table
rm(table)

load("data/voters/ausnet/ausnet_variables.RData")
ausnet_var <- x
rm(x)

## Create a long version with column for wave and questions - so far IDS are duplicated XXX
# 
# digitize |> 
#   mutate(ID = as.factor(ID)) |> 
#   filter(!is.na(ID), ID != "") |> 
#   select(-age, -SD1) |> 
#   pivot_longer(cols = -c(ID),
#                names_to = c("question"),
#                values_to = "answer") |> select(ID, question) 
#   # pivot_wider(id_cols = c(ID, wave),
#   #             names_from = question,
#   #             values_from = answer) 
  
### Ausnet ----

# new idea 

ausnet_var |> 
  filter(str_detect(Label, "^TRUST:")) |> 
  distinct(Label) |> 
  pull(Label) ->
  trust_labels

populism_variablenamesshort <- get_variablenames("OPINION: 'COMPROMISES' IN POLITICS MEANS BETRAYING ONE'S PRINCIPLES",
                                            removex = TRUE,
                                            add_id = FALSE)
ausnet_var |> 
  filter(reduce(map(populism_variablenamesshort, ~ str_starts(variable, .x)), `|`)) |>  
  distinct(variable) |> 
  pull(variable) ->
  populism_variablenames

populism_labels <- map_chr(populism_variablenames, ~ attr(ausnet[[.x]], "label"))

populism_labels_lookup <- populism_labels |>
  as.data.frame() |> 
  group_by(across(everything())) |> 
  # a couple were added only in wave 13, we don't want those but we want checks even if only occuring once
  filter(n() > 1 | str_detect(populism_labels, "CHECK")) |> 
  distinct() |> 
  ungroup() |> 
  arrange(populism_labels) |> 
  mutate(columnnamesold = str_remove_all(str_squish(str_to_lower(populism_labels)), ":|the\\s|[:punct:]|\\s"),
         populismitem = if_else(!str_detect(columnnamesold, "check"), paste0("popitem_", row_number() -1), "pop_check")) 

# df with all trust variables
ausnet_trust <- get_multiplevars(trust_labels, 
                                 "label") 

# populism items
ausnet_populism <- get_multiplevars(populism_labels_lookup$populism_labels,
                                    type = "label")
rename_vector <- setNames(populism_labels_lookup$columnnamesold, 
                          populism_labels_lookup$populismitem)
ausnet_populism_renamed <- ausnet_populism %>%
  relocate(contains("check"), .after = wave) |> 
  rename_with(~ str_remove_all(.x, "[:punct:]"), everything())
ausnet_populism_renamed <- rename(ausnet_populism_renamed, any_of(rename_vector)) 

# populism value per respondent and wave

ausnet_populism_renamed |> 
  mutate(popitem_4 = case_when(
    popitem_4 == 1 ~ 5,
    popitem_4 == 2 ~ 4,
    popitem_4 == 4 ~ 2,
    popitem_4 == 5 ~ 1,
    TRUE ~ popitem_4
  ),
  across(c(-id, -wave), ~ na_if(., 88)),
  across(c(-id, -wave), ~ na_if(., 99)))  |> 
  group_by(id, wave) |> 
  pivot_longer(cols = starts_with("popitem"),
               names_to = "item",
               values_to = "answer") |> 
  group_by(id, wave, pop_check) |> 
  summarize(pop_mean = mean(answer, na.rm = TRUE),
            pop_min = min(answer, na.rm = TRUE),
            pop_max = max(answer, na.rm = TRUE),
            .groups = "drop") |> 
  mutate(
    pop_mean = if_else(
      wave %in% c(1,2) & pop_check == 4 | wave > 2, 
      pop_mean,
      NA
      ),
    pop_min = if_else(
      wave %in% c(1,2) & pop_check == 4 | wave > 2, 
      pop_min,
      NA
      ),
    pop_mean = if_else(
      pop_mean == "NaN",
      NA,
      pop_mean
    ),
    pop_min = if_else(
      is.infinite(pop_min),
      NA,
      pop_min
    ),
    pop_max = if_else(
      wave %in% c(1,2) & pop_check == 4 | wave > 2, 
      pop_max,
      NA
    ),
    pop_max = if_else(
      is.infinite(pop_max),
      NA,
      pop_max
    ),
    ) |> 
  ungroup() ->
  ausnet_populism_summarized
  

# df courts should stop
ausnet |> 
  select(starts_with("w8_q82"), id) |> 
  mutate(wave = "8") ->
  ausnet_libdem

# df with vote choice 2019
ausnet_voting <- get_dfvars("VOTE CHOICE: NATIONAL ELECTION 2019$", FALSE) |> 
  select(-ends_with("t")) |> 
  mutate(across(1:3, ~ na_if(., 88)),
         across(1:3, ~ na_if(., 99)),
         throw = case_when(
           w11_q18 != w12_q12 & !is.na(w11_q18) & !is.na(w12_q12) ~ 1,
           w11_q18 != w13f_q30 & !is.na(w11_q18) & !is.na(w13f_q30) ~ 1,
           w12_q12 != w13f_q30 & !is.na(w12_q12) & !is.na(w13f_q30) ~ 1,   
           TRUE ~ 0
            ),
         vote2019 = case_when(
           throw == 1 ~ NA,
           !is.na(w11_q18) ~ w11_q18,
           !is.na(w12_q12) ~ w12_q12,
           !is.na(w13f_q30) ~ w13f_q30
         ))  |> 
  select(id, vote2019)

# df with demographic variables
ausnet |> 
  select(education = sd7, 
         sd18, 
         gender = sd3, 
         age, 
         residential = sd24, 
         id) ->
  ausnet_sd


ausnet_trust |> 
  left_join(ausnet_voting, by = "id") |> 
  left_join(ausnet_sd, by = "id") |> 
  left_join(ausnet_libdem, by = c("id", "wave")) |> 
  left_join(ausnet_populism_summarized, by = c("id", "wave")) |> 
  mutate(across(starts_with("trust"), ~ na_if(., 88))) |> 
  mutate(wave = as.integer(wave),
         party = to_factor(vote2019),
         birthaus = if_else(sd18 == 1, 1, 0),
         pop_mean = 5 - pop_mean,
         pop_min = 5 - pop_min,
         in_power = case_when(
           wave < 7 & party %in% c("OEVP", "SPOE") ~ 1,
           wave > 7 & wave < 10 & party %in% c("OEVP", "FPOE") ~ 1,
           wave == 13 & party %in% c("OEVP", "Greens") ~ 1,
           TRUE ~ 0
         ),
         in_power_el = case_when(
           wave < 5 & party %in% c("OEVP", "SPOE") ~ 1,
           wave > 5 & wave < 10 & party %in% c("OEVP", "FPOE") ~ 1,
           wave > 11  & party %in% c("OEVP", "Greens") ~ 1,
           TRUE ~ 0
         )) ->
  ausnet_clean

saveRDS(ausnet_clean, "data/voters/ausnet/ausnet_clean.rds")

#' 
#' # old idea
#' 
#' wave = "w1_"
#' 
#' waves = c("w1_", "w2_", "w3_")
#' 
#' get_values <- function(column){
#'   
#'   #' Returns the relevant lines of information about the chosen column from the variable overview
#'   
#'   columnname <- deparse(substitute(column))
#'   
#'   ausnet_var |> 
#'     filter(variable == columnname) ->
#'     var_labels
#'   
#'   column |> 
#'     as.data.frame() |> 
#'     left_join(var_labels |>  
#'                 select(variable, value), 
#'               by = join_by("column" == "value")) |> 
#'     pull(value) ->
#'     out
#'   
#'   return(out)
#' }
#' 
#' 
#' create_longtable <- function(wave){
#'   
#'   #' Creates long
#'   #'
#'   #' All of this text goes
#'   #' in the Description section
#'   #'
#'   #' This part goes in the Details!
#'   
#'   ausnet_small |> 
#'     select(starts_with(wave), id) |> 
#'     mutate(wave = wave) |> 
#'     to_factor() |>  
#'     label_to_colnames() |> 
#'     rename_with(~ gsub(wave, "", .), everything()) ->
#'     out
#'   
#'   return(out)
#' }
#' 
#' map_dfr(waves, create_longtable) |>  View()
#' 
#' ausnet_small |> 
#'   mutate(id = as.factor(id)) |> 
#'   filter(!is.na(id), id != "") |> 
#'   select(-age, -starts_with("sd")) |> 
#'   pivot_longer(cols = contains("date"),
#'                names_to = c("wave", "question"),
#'                names_sep = "_",
#'                values_to = "interview_date") |> 
#'   select(-question) |>  
#'     pivot_longer(cols = -c(id, interview_date, wave),
#'                  names_to = c("wave2", "question"),
#'                  names_sep = "_",
#'                  values_to = "answer") 
#'   pivot_wider(id_cols = c(ID, wave),
#'               names_from = question,
#'               values_from = answer) |> View()

## ACPP DATA works but not needed right now ----
# acpp_variables <- read.csv("data/voters/acpp/acpp_variables.csv")
# 
# acpp_variables |> 
#   filter(str_detect(Label, str_to_upper("sonntagsfrage|geschlecht|populismus|demokratieverstaendnis"))) ->
#   variables_keep_acpp

## GLES DATA not working yet ----
#  TBC
# gles <- read_dta("data/voters/gles/allwavesdta/ZA6838_w1to9_sA_v6-0-0.dta")
# 
# function(file) {
#   file |> 
#     select(study, field_start, field_end, p_participation, access_panel, starts_with("kpX_160"), kpX_201b, kpX_2280, kpX_192b, kpX_2290, kpX_3103, kpX_2320)
# }
# 
# gles |> 
#   select(study, field_start, field_end, p_participation, access_panel, starts_with("kpX_160"), kpX_201b, kpX_2280, kpX_192b, kpX_2290, kpX_3103, kpX_2320)
# 
# dictionary <- labelled::generate_dictionary(gles)
