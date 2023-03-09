# package ####
survival_data
library(dplyr)
library(survival)
library(survminer)
# Wrangling ####
survival_data <-
  survival_data_1

survival_data$histology <-
  survival_data_1$histology %>% 
  as.factor()

levels(survival_data$histology) <-
  list(adenocarcinoma = "adenocarcinoma", 
       squamous_cell_carcinoma = "squamous_cell_carcinoma", 
       other_histology_types = "other_histology_types")

# KM ####
sfit <- survfit(
  Surv(os_days, status_last_news) ~ histology, 
  data=survival_data)
summary(sfit, times=seq(0, 2000, 100))

survminer::ggsurvplot(sfit, 
                      conf.int=TRUE, pval=TRUE, 
                      risk.table=TRUE, 
                      legend.labs=c("adenocarcinoma", "squamous_cell_carcinoma", "other_histology_types"), legend.title="Histology",  
                      palette=c("#a80006", "#3900bf", "#cfc504"), 
                      #title="Kaplan-Meier Curve for Lung Cancer Survival", 
                      risk.table.height=.15)

# Cox ####

fit <- coxph(Surv(time, status)~sex, data=lung)
fit <- coxph(Surv(time, status)~sex+age+ph.ecog+ph.karno+pat.karno+meal.cal+wt.loss, data=lung)


# survfit ####
survdiff(Surv(time, status)~sex, data=lung)

coxph(Surv(time, status) ~ age, data=lung)

lung <- lung %>% 
  mutate(agecat=cut(age, breaks=c(0, 62, Inf), 
                    labels=c("young", "old")))

ggsurvplot(survfit(Surv(time, status)~agecat, data=lung), pval=TRUE)
