#Load the packages ####
library(pacman)
pacman::p_load(
  rio,          # File import
  here,         # File locator
  tidyverse,    # data management + ggplot2 graphics, 
  stringr,      # manipulate text strings 
  purrr,        # loop over objects in a tidy way
  gtsummary,    # summary statistics and tests 
  broom,        # tidy up results from regressions
  lmtest,       # likelihood-ratio tests
  parameters,   # alternative to tidy up results from regressions
  see          # alternative to visualise forest plots
)

source("./Script/Packages.R")

## Load data ####
data_s <-
  wrangling_clinical_data_def

data_s2 <-
  data_s

data_s2$status_last_news <-
  ifelse(data_s$status_last_news == "deceased",
         yes = 1,
         no = 0)
#censoring status 0=censored, 1=dead

survival_data <-
  data_s2 %>%
  mutate(
    os_days = as.duration(operation_date %--% last_news_date) / ddays(1)
  )


survival_data$histology <-
  survival_data$histology %>% 
  as.factor()

levels(survival_data$histology) <-
  list(adenocarcinoma = "adenocarcinoma", 
       squamous_cell_carcinoma = "squamous_cell_carcinoma", 
       other_histological_types = "other_histological_types")

# Kaplan-Meyer overall ####
km <- with(survival_data, Surv(os_days, status_last_news))

fit <-
  survfit2(Surv(os_days, status_last_news) ~ 1, data = survival_data) 

fit %>%
  ggsurvfit(theme = theme_ggsurvfit_KMunicate(),
  ) +
  add_censor_mark() +
  add_confidence_interval() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) +
  scale_colour_manual(values = histology_color) +
  scale_fill_manual(values = histology_color) +
  add_risktable(risktable_stats = "n.risk") +
  my_theme +
  theme(legend.position = "top")

#ggsave("./Result/Kaplan-Meyer_overall.png")

# Kaplan-Meyer histology ####
fit_histology <-
  survfit2(Surv(os_days, status_last_news) ~ histology, data = survival_data) 

fit_histology %>%
  ggsurvfit(theme = theme_ggsurvfit_KMunicate(),
  ) +
  add_censor_mark() +
  ggsurvfit::add_pvalue(location = "annotation") +
  #add_confidence_interval() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) +
  scale_colour_manual(values = histology_color) +
  scale_fill_manual(values = histology_color) +
  add_risktable(risktable_stats = "n.risk") +
  my_theme +
  theme(legend.position = "top")

#ggsave("./Result/Kaplan-Meyer_histology.png")

# Kaplan-Meyer sex ####
fit_sex <-
  survfit2(Surv(os_days, status_last_news) ~ sex, data = survival_data) 


fit_sex %>%
  ggsurvfit(theme = theme_ggsurvfit_KMunicate(),
  ) +
  add_censor_mark() +
  ggsurvfit::add_pvalue(location = "annotation") +
  #add_confidence_interval() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) +
  #scale_colour_manual(values = sex_color) +
  #scale_fill_manual(values = sex_color) +
  add_risktable(risktable_stats = "n.risk") +
  my_theme +
  theme(legend.position = "top")


# Cox survival ####
# Covariate to choose 
cox <- 
  coxph(Surv(os_days, status_last_news) ~ age + 
          sex + histology + smoking,
        data = survival_data)
cox_fit <- survfit(cox)

ggplot2::autoplot(cox_fit) +
  my_theme


# # Random Forest ####
r_fit <- 
  ranger(Surv(os_days, status_last_news) ~ age + sex + 
           histology + length_of_stay,
         data = survival_data,
         mtry = 4,
         importance = "permutation",
         splitrule = "extratrees",
         verbose = TRUE)

# Average the survival model
death_times <- r_fit$unique.death.times 
surv_prob <- data.frame(r_fit$survival)
avg_prob <- sapply(surv_prob,mean)


plot(r_fit$unique.death.times,r_fit$survival[1,], 
     type = "l", 
     ylim = c(0,1),
     col = "red",
     xlab = "Days",
     ylab = "survival",
     main = "Patient Survival Curves (Random Forest)")

cols <- colors()
for (n in sample(c(2:dim(survival_data)[1]), 20)){
  lines(r_fit$unique.death.times, r_fit$survival[n,], 
        type = "l", 
        col = cols[n])
}
lines(death_times, avg_prob, lwd = 3)
legend(500, 0.7, legend = c('Average = black'))

vi <- 
  data.frame(sort(round(r_fit$variable.importance, 4), decreasing = TRUE))
names(vi) <- "importance"
head(vi)

#cat("Prediction Error = 1 - Harrell's c-index = ", 
#    r_fit$prediction.error) #Prediction

# Define variables of interest ####


explanatory_vars <- explanatory_vars # from the Function R script


# OR univariable table ####
## drop rows with missing information for variables of interest 
# survival_data <- survival_data %>% 
#   drop_na(any_of(c("status_last_news", explanatory_vars)))
OR_univariate_table <-
  univariate_glm_analysis(glm_type = binomial, 
                        data = survival_data, 
                        explanatory_vars = explanatory_vars)

glm(status_last_news ~ prealbuminemia, family = binomial)
# OR multivariate analysis ####
OR_adjusted_table <-
  multivariate_glm_analysis(glm_type = binomial, 
                          data = survival_data, 
                          explanatory_vars = explanatory_vars)

# OR tbl merge ####
tbl_merge(list(test, test_2),
          tab_spanner = 
            c("**OR non-adjusted table**", "**OR adjusted table**"))


# RR univariate table #### IL Y A DES COVARIANCES ETC J'AI L'IMPRESSION

