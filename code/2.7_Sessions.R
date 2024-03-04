# libraries
library(tidyverse)
library(ggplot2)
library(zoo)
library(fuzzyjoin)
library(hrbrthemes)
library(ggtext)
library(sysfonts)
library(ISOweek)
library(Cairo)
library(showtext)
library(ggthemes)

source("src/graphics.R")

# data import ----

decisions <- readRDS("data/dataincl2020_final_inclpr.rds")

decisions |> 
  filter(!is.na(date_2) & year(date_2) > 1999) |> 
  arrange(date_2) |> 
  distinct(date_2) ->
  decision_dates

decision_dates |> 
  mutate(date_before = lag(date_2),
         date_after = lead(date_2),
         days_before = as.numeric(difftime(date_2, date_before, units="days")),
         days_after = as.numeric(difftime(date_after, date_2, units="days")),
         begin_session = if_else(days_before > 10, 1, 0),
         end_session = if_else(days_after > 10, 1, 0)) |> 
  filter(begin_session == 1 | end_session == 1) |> 
  mutate(session_interval = case_when(
    begin_session == 1 & end_session == 1 ~ interval(date_2, date_2),
    begin_session == 1 ~ interval(date_2, lead(date_2)),
    end_session == 1 ~ interval (lag(date_2), date_2)
  ),
  date_end = case_when(
    begin_session == 1 & end_session == 1 ~ date_2,
    begin_session == 1 ~ lead(date_2)
  )) |> 
  arrange(session_interval, date_end) |>  
  distinct(session_interval, .keep_all = TRUE) |> 
  select(start = date_2, end = date_end, intervaldate = session_interval) |> 
  mutate(type = "session") |> 
  rowid_to_column("session_id") -> 
  sessions

sessions |> 
  mutate(season = quarter(start),
         year = year(start)) |>
  filter(year == 2019) |> 
  ggplot(aes(y = year)) +
  geom_linerange(aes(xmin = start, xmax = end)) +
  scale_x_date(limits = c(ymd("2019-01-01"), ymd("2020-01-01")),
               expand = c(0,0)) +
  scale_y_continuous(limits = c(2019, 2019),
                     expand = c(0,0),
                     name = element_blank()) 
  

sessions |> 
  mutate(sessionbreak_end = start - days(1),
         sessionbreak_start = lag(end) + days(1),
         sessionbreak_intervaldate = interval(sessionbreak_start, sessionbreak_end)) |> 
  select(starts_with("sessionbreak"), session_id) |> 
  rename_with(~stringr::str_remove(.x, "sessionbreak_"), starts_with("sessionbreak_")) |> 
  mutate(type = "break") ->
  sessionbreaks

sessions_breaks <- rbind(sessions, sessionbreaks)

articles <- readRDS("C:/Users/Jasmin/OneDrive - Universität Hamburg/Desktop/Dissertation/standard_clean/data/articles_final.rds")

articles |> 
  count(conflict)

articles |> 
  mutate(month = zoo::as.yearmon(date),
         weekISO = date2ISOweek(date),
         weekISO = str_remove(weekISO, "\\-\\d$"),
         week = week(date),
         year = year (date)) |> 
  group_by(month) |> 
  mutate(monthly_sum = sum(conflict),
         monthly_sum = if_else(is.na(monthly_sum) | monthly_sum <= 5, 0, 1)) |> 
  group_by(weekISO) |> 
  mutate(weekly_sum = sum(conflict)) |> 
  ungroup() ->
  articles_month

articles_month |> 
  arrange(year, week) |> 
  complete(year, week) |> 
  mutate(weekly_sum = if_else(is.na(weekly_sum), 0, weekly_sum),
         weekISO = if_else(is.na(weekISO), paste(year, week, sep = "-W"), weekISO)) |> 
  select(year, week, weekly_sum, weekISO) ->
  articles_week

articles_week |> 
  pull(weekISO) ->
  week_year_ordered

articles_month |> 
  distinct(weekISO, .keep_all = TRUE) |> 
  filter(weekly_sum > 4) 

articles_month |> 
  distinct(weekISO, .keep_all = TRUE) |> 
  filter(weekly_sum > 4) |> 
  select(strong_attack = date) ->
  date_of_attacks

xmin <- ymd("2015-01-01")
xmax <- ymd("2022-01-01")
  
articles_month |>
  distinct(weekISO, weekly_sum) |>  
  mutate(extreme = if_else(weekly_sum > 5, 1, 0),
         weekISO = str_remove(weekISO, "W"),
         weekISO = paste0(weekISO, "-1"),
         weekISOdate = parse_date_time(weekISO, "YWw"),
         weekISOdate = ymd(weekISOdate)) |>
  filter(!is.na(weekly_sum) & !is.na(weekISOdate)) |> 
  ggplot(aes(x = weekISOdate, y = weekly_sum, fill = as.factor(extreme))) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "#EBF6F6",
            alpha = 0.2) +
  geom_col() +
  scale_fill_manual(values = c("darkslategrey", "darkred"),
                     labels = c("Usual Coverage", "High Coverage")) +
  labs(y = "Reports on Attacks per Week",
       x = "",
       fill = "") +
  ylim(0,20)

ggsave("results/images/attacks.pdf", width = 15, height = 10, device = cairo_pdf)

curve <- tibble(
  curve_start = ymd(c("2000-12-01", "2006-01-01", "2017-02-01")),
  y_start = c(42, 21, 15),
  curvature = c(-0.2, -0.2, 0.2),
  nudge_x = c(1, 1.5, -4.5),
  label =  c("Attacks on VfGH by Jörg Haider", 
             "Non-Compliance with VfGH by Haider",
             "Kurz talks of \nRed Networks in Judiciary")) |> 
  mutate(
    curve_end = curve_start %m+% years(3),
    y_end = if_else(curve_start > ymd("2015-01-01"), y_start + 3, y_start -3))

articles_month |>
  group_by(month) |> 
  summarize(n_attack = sum(conflict, na.rm = TRUE)) |> 
  ggplot() +
  geom_rect(aes(xmin = as.yearmon(ymd("2015-01-01")), 
                xmax = as.yearmon(ymd("2022-01-01")),
                ymin = -Inf, 
                ymax = Inf),
            fill = "#EBF6F6",
            alpha = 0.2) +
  geom_col(aes(x = month, y = n_attack), fill = color_dark) +
  geom_curve(aes(x = as.yearmon(curve_start), 
                 xend = as.yearmon(curve_end), 
                 y = y_start, 
                 yend = y_end),
             data = curve,
             curvature = -0.2,
             linewidth = 0.1) +
  geom_richtext(aes(x = as.yearmon(curve_end) + nudge_x, 
                    y = if_else(y_start < y_end, y_start, y_end) - 1,
                    label = paste0("<span style = 'font-size:20pt; font-family:", fontname, ";'>", label, "</span>")),
                data = curve,
                label.color = NA,
                fill = NA) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(2000,2023),
                     breaks = c(2000,2005,2010,2015,2020,2023)) +
  scale_y_continuous(expand = c(0.004,0),
                     breaks = c(10,20,30,40,50),
                     limits = c(0,50)) +
  labs(y = "Mentions of Attacks per Month",
       x = "",
       fill = "") 

ggsave("results/images/attacks.pdf", width = 15, height = 8, device = cairo_pdf)


articles_month |> 
  group_by(year) |> 
  summarize(attacks = sum(conflict, na.rm = TRUE)) ->
  attacks_yearly

decisions |>  
  mutate(year = year(date_2)) |> 
  group_by(year) |> 
  summarize(prs = sum(pr_dummy_new, na.rm = TRUE)) |> 
  ggplot() +
  geom_area(aes(y = attacks,
                x = year),
            fill = color_colorful,
            alpha = 0.3,
            data = attacks_yearly) +
  geom_line(aes(y = prs,
                x = year)) +
  scale_x_continuous(limits = c(2015, 2021))

articles_sessions <- fuzzy_left_join(articles, sessions_breaks,
                 by = c("date" = "start", "date" = "end"),
                 match_fun = list(`>=`, `<=`)
)

articles_sessions |> 
  group_by(start, end, intervaldate, type, session_id) |> 
  summarize(attacks = n(),
            .groups = "drop") |> 
  mutate(interval_length = int_length(intervaldate)/86400,
         share_attacks = attacks / interval_length) ->
  session_attacks

session_attacks_complete <- fuzzy_left_join(session_attacks, date_of_attacks,
                by = c("start" = "strong_attack", "end" = "strong_attack"),
                match_fun = list(`<=`, `>=`)
) |> 
  distinct(intervaldate, .keep_all = TRUE) |> 
  mutate(strong_attack = if_else(is.na(strong_attack), 0, 1))

session_attacks_complete |> 
  select(-interval_length) |> 
  pivot_wider(id_cols = session_id, 
              names_from = type, 
              values_from = c(start, 
                              end, 
                              intervaldate, 
                              attacks, 
                              share_attacks, 
                              strong_attack)) |> 
  select(-ends_with("NA")) ->
  sessions_attacks_wide

decisions_sessions <- fuzzy_left_join(decisions, sessions_attacks_wide,
                                      by = c("date_2" = "start_session", "date_2" = "end_session"),
                                      match_fun = list(`>=`, `<=`)
) |> 
  mutate(after_2020attack = if_else(date_2 > ymd("2020-02-03"), 1, 0))

write_rds(decisions_sessions, "data/dataincl2020_final_attacks.rds")

theme_update(legend.text = element_text(size = textsize - 4),
             axis.text.y = element_text(size = textsize - 4))
decisions_sessions |> 
  left_join(session_attacks |>  select(intervaldate, attacks, interval_length),
            by = join_by("intervaldate_break" == "intervaldate")) |> 
  distinct(intervaldate_session, .keep_all = TRUE) |> 
  select(start_session, end_session, start_break, end_break, interval_length, attacks, share_attacks_break, date_2, intervaldate_break) |> 
  mutate(year = year(date_2),
         share_attacks = round(share_attacks_break, 2),
         length = as.integer(interval_length / 2),
         middledate = start_session - days(length),
         color = "color",
         start_break = if_else(start_break < ymd("2019-01-01"), 
                               ymd("2019-01-01"),
                               start_break),
         end_break = if_else(end_break > ymd("2019-12-31"), 
                               ymd("2019-12-31"),
                               end_break)) |> 
  filter(year(start_session) == 2019 | year(start_break) == 2019) |> 
  ggplot(aes(y = year)) +
  geom_linerange(aes(xmin = start_session, 
                     xmax = end_session,
                     color = color)) +
  geom_text(aes(x = middledate,
                y = 2019.09,
                label = paste(interval_length, " days /", attacks, "attacks =\n", share_attacks, "average", sep = " ")),
            family = fontname,
            size = 5.5,
            color = "#3b3b3b") +
  geom_linerange(aes(xmin = start_break,
                     xmax = end_break,
                     y = 2019.03),
                 color = "darkgrey") +
  geom_linerange(aes(ymin = 2019.03,
                     ymax = 2019.04,
                     x = middledate),
                 color = "darkgrey") +
  geom_linerange(aes(ymin = 2019.01,
                     ymax = 2019.03,
                     x = start_break,
                     color = ifelse(start_break == ymd("2019-01-01"), "white", "darkgrey"))) +
  geom_linerange(aes(ymin = 2019.01,
                     ymax = 2019.03,
                     x = end_break,
                     color = ifelse(end_break == ymd("2019-12-31"), "white", "darkgrey"))) +
  scale_x_date(limits = c(ymd("2019-01-01"), ymd("2020-01-01")),
               expand = c(0,0),
               name = element_blank()) +
  scale_y_continuous(limits = c(2018.90, 2019.15),
                     expand = c(0,0),
                     name = element_blank(), 
                     breaks = c(2019)) +
  scale_color_manual(values = c("black", "darkgrey", "white"),
                     labels = c("Session", "Break", ""),
                     name = element_blank()) +
  coord_cartesian(clip = "off") 
ggsave("results/images/attacks_calulcation.pdf", 
     width = 13, 
     height = 3, 
     device = cairo_pdf)

# Match with PRs

prs <- readRDS("data/prslong.RDS")
# 
# prs |> 
#   pull(date_pr) ->
#   dates_pr
# 
# average_dates <- vector("list", 370)
# 
# average_attacks <- function(date){
#   
#   for(i in 1:7){
#     average_dates[i] = date-i
#   }
#   
#   # for(i in 1:14){
#   #   average_dates_14[i] = date-i
#   # }
#   
#   articles |> 
#     filter(date %in% average_dates & conflict == 1) |> 
#     count() ->
#     average_attacks_7
#   # 
#   # articles |> 
#   #   filter(date %in% average_dates & conflict == 1) |> 
#   #   count() ->
#   #   average_attacks_7
#   
#   return(average_attacks_7)
# }
# 
# map(prs$date_pr, average_attacks)
# 
# map_int

dates <- as.Date(ymd("2021-12-31"):ymd("2000-01-01")) 

dates_df <- data.frame(date = dates, row.names = NULL)


dates_df |> 
  mutate(weekISO = date2ISOweek(date),
         weekISO = str_remove(weekISO, "\\-\\d$")) ->
  weeks
  

prs |> 
  filter(type_pr == "Session" | type_pr == "Decision") |> 
  mutate(weekISO = date2ISOweek(date_pr),
         weekISO = str_remove(weekISO, "\\-\\d$")) |>
  group_by(weekISO) |> 
  summarize(weekly_prs = n()) ->
  prs_week

articles_week |> 
  unique() ->
  articles_week_unique

decisions |> 
  filter(verkündung == 1 | verhandlung == 1) |> 
  select(date_2, verkündung, verhandlung) |> 
  mutate(weekISO = date2ISOweek(date_2),
         weekISO = str_remove(weekISO, "\\-\\d$")) |> 
  group_by(weekISO) |> 
  summarize(weekly_verhandlung = sum(verhandlung), 
            weekly_verkündung = sum (verkündung)) ->
  public_weeks
  

weeks |> 
  distinct(weekISO, .keep_all = TRUE) |> 
  left_join(prs_week, by = "weekISO") |> 
  left_join(articles_week_unique, by = "weekISO") |> 
  left_join(public_weeks, by = "weekISO") |>
  mutate(weekly_prs = if_else(is.na(weekly_prs), 0, weekly_prs),
         weekly_sum = if_else(is.na(weekly_sum), 0, weekly_sum),
         weekly_verkündung = if_else(is.na(weekly_verkündung), 0, weekly_verkündung),
         weekly_verhandlung = if_else(is.na(weekly_verhandlung), 0, weekly_verhandlung)) ->
  weeks_prs_attacks

weeks_prs_attacks |> 
  pivot_longer(cols = c(weekly_prs, weekly_verhandlung, weekly_verkündung),
               values_to = "value",
               names_to = "name") |>
  ggplot(aes(x = date)) +
  geom_area(aes(y = weekly_sum), alpha = 0.4, fill = "red") +
  geom_line(aes(y = value, color = name, group = name), alpha = 0.7, linewidth = 0.5) +
  xlim(ymd("2019-01-01"), ymd("2021-12-31")) +
  labs(x = "",
       y = "N Per Week")

weeks_prs_attacks |> 
  mutate(year = year(date)) |> 
  group_by(year) |> 
  summarize(yearly_pr = sum(weekly_prs),
            yearly_sum = sum(weekly_sum),
            yearly_verhandlung = sum(weekly_verhandlung),
            yearly_verkündung = sum(weekly_verkündung),
            .groups = "drop") |> 
  ungroup() |> 
  mutate(yearly_verhandlung = if_else(year < 2015, NA, yearly_verhandlung),
         yearly_verkündung = if_else(year < 2016, NA, yearly_verkündung)) |> 
  ggplot(aes(x = year)) +
  geom_line(aes(y = yearly_pr), alpha = 0.7) +
  geom_area(aes(y = yearly_sum), alpha = 0.1, fill = "red") +
  scale_color_manual(values = c("#1C3144"),
                     labels = c("PRs"))  +
  labs(x = "",
       y = "",
       color = "",
       linetype = "")

ggsave("results/images/yearly_data.pdf", device = cairo_pdf, width = 15, height = 10)

weeks_prs_attacks |> 
  mutate(year = year(date)) |> 
  group_by(year) |> 
  summarize(yearly_pr = sum(weekly_prs),
            yearly_sum = sum(weekly_sum),
            yearly_verhandlung = sum(weekly_verhandlung),
            yearly_verkündung = sum(weekly_verkündung),
            .groups = "drop") |> 
  ungroup() |> 
  mutate(yearly_verhandlung = if_else(year < 2015, NA, yearly_verhandlung),
         yearly_verkündung = if_else(year < 2016, NA, yearly_verkündung)) |> 
  pivot_longer(cols = c(yearly_pr, yearly_verhandlung, yearly_verkündung),
               names_to = "names", 
               values_to = "values") |> 
  ggplot(aes(x = year)) +
  geom_line(aes(y = values, group = names, color = names, linetype = names)) +
  geom_area(aes(y = yearly_sum), alpha = 0.1, fill = "red") +
  scale_color_manual(values = c("#1C3144", "#758E4F", "#F6AE2D"),
                     labels = c("PRs", "Hearings", "Announcements"))  +
  scale_linetype_manual(values = c("longdash", "dotdash", "solid"),
                        labels = c("PRs", "Hearings", "Announcements")) +
  labs(x = "",
       y = "",
       color = "",
       linetype = "") +
  xlim(2015, 2021)

ggsave("results/images/yearly_data_window.pdf", device = cairo_pdf, width = 15, height = 10)

weeks_prs_attacks_session <- fuzzy_left_join(weeks_prs_attacks, sessions,
                                              by = c("date" = "start", "date" = "end"),
                                              match_fun = list(`>=`, `<=`)) |> 
  mutate(session = if_else(is.na(session_id), 0, 1)) |> 
  select(-session_id, - start, -end, -type)

saveRDS(weeks_prs_attacks_session, "data/weekly_attack_response.rds")

sessions_breaks |> 
  mutate(week_year_start = paste(week(start), year(start), sep = "-"),
         week_year_end = paste(week(end), year(end), sep = "-")) 

articles_week |> 
  unique() |> 
  full_join(prs_week, by = "weekISO") |> 
  mutate(weekISO = paste0(weekISO, "-1")) |> 
  filter(!is.na(weekISO)) |> 
  mutate(weekISO = str_remove(weekISO, "W"),
         weekISOdate = parse_date_time(weekISO, "YWw"),
         weekly_prs = if_else(is.na(weekly_prs) & !is.na(weekISOdate), 0, weekly_prs)) ->
  articles_prs_week

articles_prs_week_sessions <- fuzzy_left_join(articles_prs_week, sessions,
                                      by = c("weekISOdate" = "start", "weekISOdate" = "end"),
                                      match_fun = list(`>=`, `<=`)
) |> 
  mutate(in_session = if_else(is.na(session_id), 0, 1)) 


articles_prs_week_sessions |>
  pivot_longer(cols = c(weekly_prs, weekly_sum)) |>  
  mutate(weekISOdate = ymd(weekISOdate)) |> 
  rename("start_date" = "start", "end_date" = "end") |>
  filter(weekISOdate > ymd("2010-01-01")) |> 
  ggplot(aes(x=weekISOdate, y = value, color = name, group = name)) +
  geom_rect(aes(xmin = start_date,
                xmax = end_date,
                ymin = 0,
                ymax = 20),
            color = "#EBF6F6", 
            alpha = 0.4) +
  geom_line() +
  theme_minimal() 

theme_update(panel.grid.major.y = element_line(color = "lightgrey",
                                       size = 0.25))

decisions_sessions |> 
  filter(year(date_2) > 2014) |> 
  ggplot(aes(x = share_attacks_break)) +
  geom_histogram(binwidth = 0.1,
                 fill = color_dark,
                 color = "white") +
  scale_y_continuous(limits = c(0,60),
                     expand = c(0.005,0),
                     breaks = c(10,20,30,40,50,60)) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,2.5)) +
  labs(x = "Average Reports on Attacks Per Day",
       y = "") +
  coord_cartesian(clip = "off")

ggsave("results/images/reports_per_day.pdf", width = 12, height = 6, device = cairo_pdf)

decisions_sessions |> 
  filter(year(date_2) > 2014) |> 
  ggplot(aes(x = share_attacks_break,
             fill = type)) +
  geom_histogram(binwidth = 0.1,
                 color = "white") +
  scale_y_continuous(limits = c(0,60),
                     expand = c(0.005,0),
                     breaks = c(10,20,30,40,50,60)) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,2.5)) +
  scale_fill_manual(values = c(color_dark_light, 
                                color_dark),
                    labels = c("No", "Yes")) +
  labs(x = "Average Reports on Attacks Per Day",
       y = "",
       fill = "On the Merits") +
  coord_cartesian(clip = "off")

ggsave("results/images/reports_per_day_merits.pdf", width = 12, height = 6, device = cairo_pdf)

decisions_sessions |> 
  filter(year(date_2) > 2014) |>
  mutate(threshold = as.factor(if_else(date_2 > ymd("2020-02-11"), 1, 0))) |> 
  ggplot(aes(x = share_attacks_break,
             fill = threshold)) +
  geom_histogram(binwidth = 0.1,
                 color = "white") +
  scale_y_continuous(limits = c(0,60),
                     expand = c(0.005,0),
                     breaks = c(10,20,30,40,50,60)) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,2.5)) +
  scale_fill_manual(values = c(color_dark_light, 
                               color_dark),
                    labels = c("No", "Yes")) +
  labs(x = "Average Reports on Attacks Per Day",
       y = "",
       fill = "After Threshold") +
  coord_cartesian(clip = "off")

ggsave("results/images/reports_per_day_threshold.pdf", width = 12, height = 6, device = cairo_pdf)
         
