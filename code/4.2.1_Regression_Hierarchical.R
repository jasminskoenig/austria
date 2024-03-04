rm(list = ls())

# library
library(tidyverse)
library(broom.mixed)
library(stargazer)
library(lme4)
library(sjstats)
library(optimx)

# import data (only cases that were decided on the merits, should run this without the filter for robustness though)

G_analysis_R <- readRDS("data/G_analysis_R.rds") %>%
  filter(erkenntnis == 1)

# center continuous variable lawstotal
G_analysis_R %>% mutate(lawstotal_cent = (lawstotal - mean(lawstotal,na.rm=T))/100) -> G_analysis_R
glimpse(G_analysis_R)

# descriptives
table(G_analysis_R$decision,G_analysis_R$populistdummy)
table(G_analysis_R$decision,G_analysis_R$pop_origin)

# Pop_origin models

# only key IV
m1 <- glm(decision ~ pop_origin, data = G_analysis_R,family = binomial)
summary(m1)

# only key IV + FE
m2 <- glm(decision ~ pop_origin + as.factor(date) + as.factor(government), data = G_analysis_R,family = binomial)
summary(m2)

# + controls
m3 <- glm(decision ~ pop_origin + gov_brief+twothirds+lawstotal_cent + security+migration+social+court + as.factor(date) + as.factor(government), data = G_analysis_R,family = binomial)
summary(m3) #

# multilevel
m4 <- glmer(formula=decision ~ 1 + (1|date) + (1|government) + (1|id_decision) +
pop_origin + lawstotal_cent + gov_brief+twothirds+security+migration+social+court,
                family=binomial(link="logit"),
                data=G_analysis_R,
                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(m4)

# Pop_government models
m5 <- glm(decision ~ populistdummy, data = G_analysis_R,family = binomial)
summary(m5)

# only key IV + FE
m6 <- glm(decision ~ populistdummy  + as.factor(date), data = G_analysis_R,family = binomial)
summary(m6)

# + controls
m7 <- glm(decision ~ populistdummy + gov_brief+twothirds+lawstotal_cent + security+migration+social+court + as.factor(date), data = G_analysis_R,family = binomial)
summary(m7) #

# multilevel
m8 <- glmer(formula=decision ~ 1 + (1|date) + (1|government) + (1|id_decision) +
populistdummy + lawstotal_cent + gov_brief+twothirds+security+migration+social+court,
                family=binomial(link="logit"),
                data=G_analysis_R,
                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(m8)

# make table
tidym1 <- broom::tidy(m1)
tidym2 <- broom::tidy(m2)[1:2,]
tidym3 <- broom::tidy(m3)[1:9,] # omit fixed effects
tidym4 <- broom::tidy(m4) %>% filter(effect=="fixed")

# counts of random effects
num_date <- nrow(ranef(m4)$date)
num_gov <- nrow(ranef(m4)$government)
num_decision <- nrow(ranef(m4)$id_decision)

# standard deviation of random effects
sd_date <- round(attributes(VarCorr(m4)$"date")$stddev, 3)
sd_gov <- round(attributes(VarCorr(m4)$"government")$stddev, 3)
sd_decision <- round(attributes(VarCorr(m4)$"id_decision")$stddev, 3)


tribble(~stat, ~m1, ~m2, ~m3, ~m4,
        "Year FE", NA, 1, 1, NA,
        "Government FE", NA, 1, 1, NA,
        "Number of Years", NA, num_date, num_date, num_date,
        "Number of Governments", NA, num_gov, num_gov, num_gov,
        "Number of Decisions", NA, NA, NA, num_decision,
        "sd(Year)", NA, NA, NA, sd_date,
        "sd(Government)", NA, NA, NA, sd_gov,
        "sd(Decision)", NA, NA, NA, sd_decision,
        "", NA, NA, NA, NA,
        "N", nobs(m1), nobs(m2), nobs(m3),nobs(m4)) -> mod_stats


# create table
stargazer(m1, m2, m3, m4, type="latex", 
          # Below: manually supply tidied coefficients and standard errors
          coef = list(tidym1$estimate, tidym2$estimate, tidym3$estimate, tidym4$estimate),
          se = list(tidym1$std.error, tidym2$std.error, tidym3$std.error, tidym4$std.error),
          omit=c("date","government"),
          # Omit model statistics by default...
          omit.table.layout = "s",
          # ...but supply your own that you created (with random effects)
          add.lines = lapply(1:nrow(mod_stats), function(i) unlist(mod_stats[i, ])),
          covariate.labels = c("Populist","Gov Brief","2/3 Majority", "Total Laws", "Security", "Migration","Social","Plaintiff (Court)"),
          #notes="<small>Data: ESS, Round 9 (United Kingdom)</small>",
          dep.var.labels="Decision",
          model.names = FALSE,
          column.labels = c("Pooled", "Fixed Effects", "Fixed Effects + Controls", "Multilevel + Controls")
          )


# calculate ICC
## empty model, random effects for date, year, government
m1.h <- glmer(formula=decision ~ 1 + (1|date) + (1|government) + (1|id_decision),
                family=binomial(link="logit"),
                data=G_analysis_R)

# assess clustering structure of the data
performance::icc(m1.h)
# 0.621 means that 62.1% of the variation in the outcome variable can be accounted for by 
# the clustering structure of the data, multilevel model may make a difference

m2.h <- glmer(formula=decision ~ 1 + (1|date) + (1|government),
                family=binomial(link="logit"),
                data=G_analysis_R)

# assess clustering structure of the data
performance::icc(m2.h)
# 0.136 means that 13.6% of the variation in the outcome variable can be accounted for by 
# the clustering structure of the data, multilevel model may make a difference
# mostly from the decision clustering


# create table for populist government --> appendix


# make table
tidym5 <- broom::tidy(m5)
tidym6 <- broom::tidy(m6)[1:2,]
tidym7 <- broom::tidy(m7)[1:9,] # omit fixed effects
tidym8 <- broom::tidy(m8) %>% filter(effect=="fixed")

# counts of random effects
num_date <- nrow(ranef(m8)$date)
num_gov <- nrow(ranef(m8)$government)
num_decision <- nrow(ranef(m8)$id_decision)

# standard deviation of random effects
sd_date <- round(attributes(VarCorr(m8)$"date")$stddev, 3)
sd_gov <- round(attributes(VarCorr(m8)$"government")$stddev, 3)
sd_decision <- round(attributes(VarCorr(m8)$"id_decision")$stddev, 3)


tribble(~stat, ~m5, ~m6, ~m7, ~m8,
        "Year FE", NA, 1, 1, NA,
        "Number of Years", NA, num_date, num_date, num_date,
        "Number of Governments", NA, num_gov, num_gov, num_gov,
        "Number of Decisions", NA, NA, NA, num_decision,
        "sd(Year)", NA, NA, NA, sd_date,
        "sd(Government)", NA, NA, NA, sd_gov,
        "sd(Decision)", NA, NA, NA, sd_decision,
        "", NA, NA, NA, NA,
        "N", nobs(m5), nobs(m6), nobs(m7),nobs(m8)) -> mod_stats


# create table
stargazer(m5, m6, m7, m8, type="latex", 
          # Below: manually supply tidied coefficients and standard errors
          coef = list(tidym5$estimate, tidym6$estimate, tidym7$estimate, tidym8$estimate),
          se = list(tidym5$std.error, tidym6$std.error, tidym7$std.error, tidym8$std.error),
          omit=c("date","government"),
          # Omit model statistics by default...
          omit.table.layout = "s",
          # ...but supply your own that you created (with random effects)
          add.lines = lapply(1:nrow(mod_stats), function(i) unlist(mod_stats[i, ])),
          covariate.labels = c("Populist Gov","Gov Brief","2/3 Majority", "Total Laws", "Security", "Migration","Social","Plaintiff (Court)"),
          #notes="<small>Data: ESS, Round 9 (United Kingdom)</small>",
          dep.var.labels="Decision",
          model.names = FALSE,
          column.labels = c("Pooled", "Fixed Effects", "Fixed Effects + Controls", "Multilevel + Controls")
          )





##### APPENDIX
## repeat not filter by Erkenntnis
######

G_analysis_R <- readRDS("data/G_analysis_R.rds")

# center continuous variable lawstotal
G_analysis_R %>% mutate(lawstotal_cent = (lawstotal - mean(lawstotal,na.rm=T))/100) -> G_analysis_R
glimpse(G_analysis_R)

# descriptives
table(G_analysis_R$decision,G_analysis_R$populistdummy)
table(G_analysis_R$decision,G_analysis_R$pop_origin) # a few more cases

# Pop_origin models

# only key IV
m1all <- glm(decision ~ pop_origin, data = G_analysis_R,family = binomial)
summary(m1all)

# only key IV + FE
m2all <- glm(decision ~ pop_origin + as.factor(date) + as.factor(government), data = G_analysis_R,family = binomial)
summary(m2all)

# + controls
m3all <- glm(decision ~ pop_origin + gov_brief+twothirds+lawstotal_cent + security+migration+social+court + as.factor(date) + as.factor(government), data = G_analysis_R,family = binomial)
summary(m3) #

# multilevel
m4all <- glmer(formula=decision ~ 1 + (1|date) + (1|government) + (1|id_decision) +
pop_origin + lawstotal_cent + gov_brief+twothirds+security+migration+social+court,
                family=binomial(link="logit"),
                data=G_analysis_R,
                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(m4all)

# Pop_government models
m5all <- glm(decision ~ populistdummy, data = G_analysis_R,family = binomial)
summary(m5all)

# only key IV + FE
m6all <- glm(decision ~ populistdummy  + as.factor(date), data = G_analysis_R,family = binomial)
summary(m6all)

# + controls
m7all <- glm(decision ~ populistdummy + gov_brief+twothirds+lawstotal_cent + security+migration+social+court + as.factor(date), data = G_analysis_R,family = binomial)
summary(m7all) #

# multilevel
m8all <- glmer(formula=decision ~ 1 + (1|date) + (1|id_decision) +
populistdummy + lawstotal_cent + gov_brief+twothirds+security+migration+social+court,
                family=binomial(link="logit"),
                data=G_analysis_R,
                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(m8all)

# make table
tidym1 <- broom::tidy(m1all)
tidym2 <- broom::tidy(m2all)[1:2,]
tidym3 <- broom::tidy(m3all)[1:9,] # omit fixed effects
tidym4 <- broom::tidy(m4all) %>% filter(effect=="fixed")

# counts of random effects
num_date <- nrow(ranef(m4all)$date)
num_gov <- nrow(ranef(m4all)$government)
num_decision <- nrow(ranef(m4all)$id_decision)

# standard deviation of random effects
sd_date <- round(attributes(VarCorr(m4all)$"date")$stddev, 3)
sd_gov <- round(attributes(VarCorr(m4all)$"government")$stddev, 3)
sd_decision <- round(attributes(VarCorr(m4all)$"id_decision")$stddev, 3)


tribble(~stat, ~m1all, ~m2all, ~m3all, ~m4all,
        "Year FE", NA, 1, 1, NA,
        "Government FE", NA, 1, 1, NA,
        "Number of Years", NA, num_date, num_date, num_date,
        "Number of Governments", NA, num_gov, num_gov, num_gov,
        "Number of Decisions", NA, NA, NA, num_decision,
        "sd(Year)", NA, NA, NA, sd_date,
        "sd(Government)", NA, NA, NA, sd_gov,
        "sd(Decision)", NA, NA, NA, sd_decision,
        "", NA, NA, NA, NA,
        "N", nobs(m1all), nobs(m2all), nobs(m3all),nobs(m4all)) -> mod_stats


# create table
stargazer(m1all, m2all, m3all, m4all, type="latex", 
          # Below: manually supply tidied coefficients and standard errors
          coef = list(tidym1$estimate, tidym2$estimate, tidym3$estimate, tidym4$estimate),
          se = list(tidym1$std.error, tidym2$std.error, tidym3$std.error, tidym4$std.error),
          omit=c("date","government"),
          # Omit model statistics by default...
          omit.table.layout = "s",
          # ...but supply your own that you created (with random effects)
          add.lines = lapply(1:nrow(mod_stats), function(i) unlist(mod_stats[i, ])),
          covariate.labels = c("Populist","Gov Brief","2/3 Majority", "Total Laws", "Security", "Migration","Social","Plaintiff (Court)"),
          #notes="<small>Data: ESS, Round 9 (United Kingdom)</small>",
          dep.var.labels="Decision",
          model.names = FALSE,
          column.labels = c("Pooled", "Fixed Effects", "Fixed Effects + Controls", "Multilevel + Controls")
          )


# calculate ICC
## empty model, random effects for date, year
m1.h <- glmer(formula=decision ~ 1 + (1|date) + (1|id_decision),
                family=binomial(link="logit"),
                data=G_analysis_R)

# assess clustering structure of the data
performance::icc(m1.h)
# 0.621 means that 62.1% of the variation in the outcome variable can be accounted for by 
# the clustering structure of the data, multilevel model may make a difference

m2.h <- glmer(formula=decision ~ 1 + (1|date) + (1|government),
                family=binomial(link="logit"),
                data=G_analysis_R)

# assess clustering structure of the data
performance::icc(m2.h)
# 0.136 means that 13.6% of the variation in the outcome variable can be accounted for by 
# the clustering structure of the data, multilevel model may make a difference
# mostly from the decision clustering


# create table for populist government --> appendix


# make table
tidym5 <- broom::tidy(m5all)
tidym6 <- broom::tidy(m6all)[1:2,]
tidym7 <- broom::tidy(m7all)[1:9,] # omit fixed effects
tidym8 <- broom::tidy(m8all) %>% filter(effect=="fixed")

# counts of random effects
num_date <- nrow(ranef(m8all)$date)
num_decision <- nrow(ranef(m8all)$id_decision)

# standard deviation of random effects
sd_date <- round(attributes(VarCorr(m8all)$"date")$stddev, 3)
sd_decision <- round(attributes(VarCorr(m8all)$"id_decision")$stddev, 3)


tribble(~stat, ~m5all, ~m6all, ~m7all, ~m8all,
        "Year FE", NA, 1, 1, NA,
        "Number of Years", NA, num_date, num_date, num_date,
        "Number of Decisions", NA, NA, NA, num_decision,
        "sd(Year)", NA, NA, NA, sd_date,
        "sd(Government)", NA, NA, NA, sd_gov,
        "sd(Decision)", NA, NA, NA, sd_decision,
        "", NA, NA, NA, NA,
        "N", nobs(m5all), nobs(m6all), nobs(m7all),nobs(m8all)) -> mod_stats


# create table
stargazer(m5all, m6all, m7all, m8all, type="latex", 
          # Below: manually supply tidied coefficients and standard errors
          coef = list(tidym5$estimate, tidym6$estimate, tidym7$estimate, tidym8$estimate),
          se = list(tidym5$std.error, tidym6$std.error, tidym7$std.error, tidym8$std.error),
          omit=c("date","government"),
          # Omit model statistics by default...
          omit.table.layout = "s",
          # ...but supply your own that you created (with random effects)
          add.lines = lapply(1:nrow(mod_stats), function(i) unlist(mod_stats[i, ])),
          covariate.labels = c("Populist Gov","Gov Brief","2/3 Majority", "Total Laws", "Security", "Migration","Social","Plaintiff (Court)"),
          #notes="<small>Data: ESS, Round 9 (United Kingdom)</small>",
          dep.var.labels="Decision",
          model.names = FALSE,
          column.labels = c("Pooled", "Fixed Effects", "Fixed Effects + Controls", "Multilevel + Controls")
          )




















# fixed effects year and date and cabinet
m4 <- glm(decision ~ pop_origin + as.factor(year) + as.factor(date) + as.factor(government), data = G_analysis_R,family = binomial)
summary(m4) # 

# fixed effects year and date and cabinet
m5 <- glm(decision ~ pop_origin  + as.factor(date) +
 gov_brief+twothirds+lawstotal, data = G_analysis_R,family = binomial)
summary(m5) # 

# fixed effects year and date and cabinet + controls and policy areas
m6 <- glm(decision ~ pop_origin  + as.factor(date) + as.factor(year) +
 gov_brief+twothirds+lawstotal + security+migration+social+court, data = G_analysis_R,family = binomial)
summary(m6)

# mu


#### multilevel models



## empty model, random effects for date, year, government
m1.h <- glmer(formula=decision ~ 1 + (1|date) + (1|government) + (1|id_decision),
                family=binomial(link="logit"),
                data=G_analysis_R)

# assess clustering structure of the data
performance::icc(m1.h)

# 0.621 means that 62.1% of the variation in the outcome variable can be accounted for by 
# the clustering structure of the data, multilevel model may make a difference

# check which random intercept does the work
m1.hdate <- glmer(formula=decision ~ 1 + (1|date),
                family=binomial(link="logit"),
                data=G_analysis_R)
performance::icc(m1.hdate) # 0.123

m1.hdecision <- glmer(formula=decision ~ 1 + (1|id_decision),
                family=binomial(link="logit"),
                data=G_analysis_R)
performance::icc(m1.hdecision) # 0.620

m1.hgov <- glmer(formula=decision ~ 1 + (1|government),
                family=binomial(link="logit"),
                data=G_analysis_R)
performance::icc(m1.hgov) # 0.097

# full model







# note: have to increase tolerance
summary(m6.h)
library(jtools)
summ(m6.h)

# also decision as random intercept
m6.h2 <- glmer(formula=decision ~ 1 + (1|date) + pop_origin + (pop_origin|government) +
 gov_brief+twothirds+security+migration+social+court,
                family=binomial(link="logit"),
                data=G_analysis_R)

summary(m6.h)


anova(m6.h,m6.h2) # no improvement addign random slopes
