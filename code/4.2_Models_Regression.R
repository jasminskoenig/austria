library(tidyverse)
library(broom)
library(car)
library(stargazer)
library(rcompanion)
extrafont::loadfonts(device="win")
library(ggplot2)
library(ggeffects)
library(ggpubr) #ggarrange
library(hrbrthemes)
library(interplot)
library(margins) 
library(ggplotify)
library(Cairo)

# model in paper ----

G_analysis_R <- readRDS("data/G_analysis_R.rds") %>%
  filter(erkenntnis == 1)

model_erkenntnis_only1 <- glm(decision ~ pop_origin, data = G_analysis_R, 
                             family = binomial)
summary(model_erkenntnis_only1)
BIC(model_erkenntnis_only1)

model_erkenntnis_only2 <- glm(decision ~ pop_origin+as.factor(date), data = G_analysis_R, 
                             family = binomial)
summary(model_erkenntnis_only2)
BIC(model_erkenntnis_only2)

model_erkenntnis_only3 <- glm(decision ~ pop_origin+gov_brief+twothirds+lawstotal+as.factor(date), 
                              data = G_analysis_R, 
                              family = binomial)
summary(model_erkenntnis_only3)
BIC(model_erkenntnis_only3)

#model including policy areas
model_erkenntnis_only4 <- glm(decision ~ pop_origin+gov_brief+twothirds+lawstotal+security+migration+social+court+as.factor(date), 
                              data = G_analysis_R, 
                              family = binomial)
summary(model_erkenntnis_only4)
BIC(model_erkenntnis_only4)

#model including interactions
model_erkenntnis_only5<- glm(decision ~ pop_origin+gov_brief+twothirds+lawstotal+security+migration+social+court+pop_origin*security+pop_origin*migration+pop_origin*social+as.factor(date), 
                              data = G_analysis_R, 
                              family = binomial)
summary(model_erkenntnis_only5)
BIC(model_erkenntnis_only5)

stargazer(model_erkenntnis_only1, model_erkenntnis_only2, model_erkenntnis_only3, model_erkenntnis_only4, model_erkenntnis_only5, title="Results", align=TRUE, out = "results/models_erkenntnis.txt")

#-----------------------------------------------------------------------
#Pseudo R2
nagelkerke(model_erkenntnis_only1)
nagelkerke(model_erkenntnis_only2)
nagelkerke(model_erkenntnis_only3)
nagelkerke(model_erkenntnis_only4)
nagelkerke(model_erkenntnis_only5)

#----------------------------------------------------------------------------------------

#MARGINAL EFFECT PLOTS
#histogram to be added -> uneven distribution of populism
histogram_pop_origin <- G_analysis_R %>%
  ggplot()+
  geom_histogram(aes(x=pop_origin), binwidth = 1) +
  xlim(-0.5,46)+
  theme_void()

#saving it as.grob so we can insert it into another graph
histogram_pop_origin <- as.grob(histogram_pop_origin)

margeffect_populism <- cplot(model_erkenntnis_only4, "pop_origin", what = "effect") %>%
  ggplot(aes(x = xvals, y = yvals)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.2)+
  xlab('Share of Populist Ministries') +
  ylab('Marginal Effect on Probability of Repeal') +
  ylim(-0.01,0.01) +
  theme_ipsum_tw(axis_text_size = 12, 
                                   axis_title_size = 12,
                                   strip_text_size = 12) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.5, "cm"),
        plot.margin=grid::unit(c(1,0.5,0.5,0), "mm"))

saveRDS(margeffect_populism, "results/marginaleffect_populism.RDS")

margeffect_populism +
  ggsave("results/marginaleffect_populism.pdf", device = cairo_pdf, height = 3, width = 5)

margeffect_populism +
  ggsave("Presentation/marginaleffect_populism.jpg", device = "jpeg", height = 3, width = 5)


#added histogram if needed
#margeffect_populism +
  #annotation_custom(grob = histogram_pop_origin, xmin = -Inf, xmax = Inf, ymin = -0.0101, ymax = -0.008)+
  #ggsave("results/marginaleffect_populism.pdf", device = cairo_pdf, height = 3, width = 5)

#prediction
cplot(model_erkenntnis_only5, "pop_origin", what = "prediction") %>%
  ggplot(aes(x = xvals, y = yvals)) +
  geom_line()+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha = 0.4)+
  xlab('Policy Area Populism') +
  ylab('Predicted Probability of Repeal') +
  theme_ipsum_tw(axis_text_size = 12, 
                 axis_title_size = 12, 
                 caption_size = 12) 

#security
cplot(model_erkenntnis_only5, "security", what = "prediction") %>%
  filter(xvals == 0|xvals == 1) %>% 
  mutate(xvals = as.factor(xvals)) %>%
  ggplot(aes(x = xvals, y = yvals)) +
  geom_pointrange(aes(ymin=lower, ymax=upper))+
  xlab('Policy Area Security') +
  ylab('Predicted Probability of Repeal') +
  ylim(0,0.5)+
  theme_ipsum_tw(axis_text_size = 12, 
                 axis_title_size = 12, 
                 caption_size = 12) +
  ggsave("results/marginaleffect_security.png", device = 'png', height = 5, width = 5)

#migration
cplot(model_erkenntnis_only5, "migration", what = "prediction") %>%
  filter(xvals == 0|xvals == 1) %>% 
  mutate(xvals = as.factor(xvals)) %>%
  ggplot(aes(x = xvals, y = yvals)) +
  geom_pointrange(aes(ymin=lower, ymax=upper))+
  xlab('Policy Area Migration') +
  ylab('Predicted Probability of Repeal') +
  ylim(0,0.5)+
  theme_ipsum_tw(axis_text_size = 12, 
                 axis_title_size = 12, 
                 caption_size = 12) +
  ggsave("results/marginaleffect_migration.png", device = 'png', height = 5, width = 5)

#social
cplot(model_erkenntnis_only5, "social", what = "prediction") %>%
  filter(xvals == 0|xvals == 1) %>% 
  mutate(xvals = as.factor(xvals)) %>%
  ggplot(aes(x = xvals, y = yvals)) +
  geom_pointrange(aes(ymin=lower, ymax=upper))+
  xlab('Policy Area social') +
  ylab('Predicted Probability of Repeal') +
  ylim(0,0.5)+
  theme_ipsum_tw(axis_text_size = 12, 
                 axis_title_size = 12, 
                 caption_size = 12) +
  ggsave("results/marginaleffect_social.png", device = 'png', height = 5, width = 5)

#court
cplot(model_erkenntnis_only5, "court", what = "prediction") %>%
  filter(xvals == 0|xvals == 1) %>% 
  mutate(xvals = as.factor(xvals)) %>%
  ggplot(aes(x = xvals, y = yvals)) +
  geom_pointrange(aes(ymin=lower, ymax=upper))+
  xlab('Court as Plaintiff') +
  ylab('Predicted Probability of Repeal') +
  ylim(0,0.5)+
  theme_ipsum_tw(axis_text_size = 12, 
                 axis_title_size = 12, 
                 caption_size = 12) +
  ggsave("results/marginaleffect_court.png", device = 'png', height = 5, width = 5)

#private
cplot(model_erkenntnis_only5, "private", what = "prediction") %>% 
  filter(xvals == 0|xvals == 1) %>% 
  mutate(xvals = as.factor(xvals)) %>%
  ggplot(aes(x = xvals, y = yvals)) +
  geom_pointrange(aes(ymin=lower, ymax=upper))+
  xlab('Person as Plaintiff') +
  ylab('Predicted Probability of Repeal') +
  ylim(0,0.5)+
  theme_ipsum_tw(axis_text_size = 12, 
                 axis_title_size = 12, 
                 caption_size = 12) +
  ggsave("results/marginaleffect_person.png", device = 'png', height = 5, width = 5)

#------------------------------------------------------------------------------------------------
#INTERACTION EFFECTS as included in paper

#migration
pred_inter_migration <- interplot(model_erkenntnis_only5, "pop_origin", "migration") +
  theme_ipsum_tw(axis_text_size = 14, 
                 axis_title_size = 14, 
                 caption_size = 10,
                 grid = "Y") +
  theme(legend.position = "right", 
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.3, "cm")) +
  ylim(-0.09, 0.03) +
  labs(caption = "") +
  ylab("Coefficient Populism")+
  xlab("Migration")

pred_inter_migration2 <- interplot(model_erkenntnis_only5, "migration", "pop_origin") +
  theme_ipsum_tw(axis_text_size = 14, 
                 axis_title_size = 14, 
                 caption_size = 10) +
  theme(legend.position = "right", 
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.3, "cm")) +
  xlim(-0.25,46)+
  ylim(-4, 2) +
  labs(caption = "") +
  ylab("Coefficient Migration")

#security
pred_inter_security <- interplot(model_erkenntnis_only5, "pop_origin", "security")+
  theme_ipsum_tw(axis_text_size = 14, 
                 axis_title_size = 14, 
                 caption_size = 10,
                 grid = "Y") +
  theme(legend.position = "right", 
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.3, "cm")) +
  ylim(-0.09, 0.03) +
  labs(caption = "")+
  xlab("Security") +
  ylab("Coefficient Populism")


pred_inter_security2 <- interplot(model_erkenntnis_only5, "security", "pop_origin", hist = TRUE)+
  theme_ipsum_tw(axis_text_size = 14, 
                 axis_title_size = 14, 
                 caption_size = 10) +
  theme(legend.position = "right", 
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.3, "cm")) +
  xlim(-0.25, 45.6)+
  ylim(-4, 2) +
  labs(caption = "") +
  xlab("Share of Populist Ministers")+
  ylab("Coefficient Security")

#security is on the bottom -> adding the histogram
pred_inter_security2 <- pred_inter_security2 +
  annotation_custom(grob = histogram_pop_origin, xmin = -Inf, xmax = Inf, ymin = -4.05, ymax = -3)


#social
pred_inter_social <- interplot(model_erkenntnis_only5, "pop_origin", "social")+
  theme_ipsum_tw(axis_text_size = 14, 
                 axis_title_size = 14, 
                 caption_size = 10,
                 grid = "Y") +
  theme(legend.position = "right", 
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.3, "cm")) +
  ylim(-0.09, 0.03) +
  labs(caption = "")+
  ylab("Coefficient Populism")+
  xlab("social")

pred_inter_social2 <- interplot(model_erkenntnis_only5, "social", "pop_origin") +
  theme_ipsum_tw(axis_text_size = 14, 
                 axis_title_size = 14, 
                 caption_size = 10) +
  theme(legend.position = "right", 
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.3, "cm")) +
  xlim(-0.25, 46)+
  ylim(-4, 2) +
  labs(caption = "") +
  ylab("Coefficient social")


#arranging the graphs
#policy areas
ggarrange(pred_inter_social2, pred_inter_social,
          pred_inter_migration2, pred_inter_migration,
          pred_inter_security2, pred_inter_security,
          labels = c("","", "","", "",""),
          ncol = 2, nrow = 3,
          font.label = list(size = 12, color = "black", face = "plain"))  +
  labs(caption = "") +
  ggsave("results/interaction_condition_policyarea.png", device = 'png', height = 15, width = 10)


