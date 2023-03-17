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
  survival_data_1$histology %>% 
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
  coxph(Surv(os_days, status_last_news) ~ age + sex + 
          histology + length_of_stay,
        data = survival_data)
cox_fit <- survfit(cox)

autoplot(cox_fit) +
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


# OR univariate table ####
## drop rows with missing information for variables of interest 
#survival_data <- survival_data %>% 
  #drop_na(any_of(c("status_last_news", explanatory_vars)))
OR_models <- 
  univariate_analysis(glm_type = binomial, 
                      analyse_type = "OR", 
                      data = survival_data,
                      explanatory_vars = explanatory_vars) 

var_id <- NULL
for (var_id in explanatory_vars) {
  OR_models <- OR_models %>%
    add_row(
      Characteristic = paste0(var_id, "_cat"), 
      OR = NA, 
      `CI 95%` = NA, 
      p.value = NA,
      .before = min(which(str_detect(OR_models$Characteristic, var_id)) - 1)
    )
}

OR_models_2 <-
  OR_models

var_id <- NULL
for (var_id in explanatory_vars) {
  OR_models_2 <- 
    OR_models_2 %>%
  filter(!row_number() 
         %in%  
           max(
             which(OR_models_2$Characteristic == var_id) 
             - 1))
}

var_id <- NULL
for(var_id in explanatory_vars){
  if(
    (TRUE %in% (str_detect(OR_models_2$Characteristic, var_id) == T &
                str_detect(OR_models_2$Characteristic, "_cat") == F & 
                OR_models_2$Characteristic != var_id)) == T){
    numbers <-
      which(str_detect(OR_models_2$Characteristic, var_id) == T &
              str_detect(OR_models_2$Characteristic, "_cat") == F & 
              OR_models_2$Characteristic != var_id)
    
    OR_models_2[numbers,]$Characteristic <-
     ifelse(str_detect(string = OR_models_2[numbers,]$Characteristic, 
                          pattern = paste0(var_id, "_")) == F,
               yes = str_remove(OR_models_2[numbers,]$Characteristic, var_id),
               no = OR_models_2[numbers,]$Characteristic)
      }
}

OR_models_3 <-
  OR_models_2

OR_models_3$Characteristic <-
  str_remove(string = OR_models_2$Characteristic, 
             pattern = "_cat")

OR_models_2$OR <- as.character(OR_models_3$OR)
OR_models_2$p.value<- as.character(OR_models_3$p.value)
OR_models_3$OR <- as.character(OR_models_3$OR)
OR_models_3$p.value<- as.character(OR_models_3$p.value)

#add the intercept
OR_models_3[4,] <-
  removing_intercept_value(
    data = OR_models_2, 
    row_number = 4, 
    name = "female")

OR_models_3[7,] <-
  removing_intercept_value(
    data = OR_models_2, 
    row_number = 7, 
    name = "adenocarcinoma")
  
OR_models_4 <-
  OR_models_3

OR_univ_tab <-
  OR_models_4 %>%
  flextable::flextable()

# RR univariate table #### IL Y A DES COVARIANCES ETC J'AI L'IMPRESSION

RR_models <- 
  univariate_analysis(glm_type = binomial(link = log), 
                      analyse_type = "RR", 
                      data = survival_data,
                      explanatory_vars = explanatory_vars) 



var_id <- NULL
for (var_id in explanatory_vars) {
  RR_models <- RR_models %>%
    add_row(
      Characteristic = paste0(var_id, "_cat"), 
      RR = NA, 
      `CI 95%` = NA, 
      p.value = NA,
      .before = min(which(str_detect(RR_models$Characteristic, var_id)) - 1)
    )
}

RR_models_2 <-
  RR_models

var_id <- NULL
for (var_id in explanatory_vars) {
  RR_models_2 <- 
    RR_models_2 %>%
    filter(!row_number() 
           %in%  
             max(
               which(RR_models_2$Characteristic == var_id) 
               - 1))
}

var_id <- NULL
for(var_id in explanatory_vars){
  if(
    (TRUE %in% (str_detect(RR_models_2$Characteristic, var_id) == T &
                str_detect(RR_models_2$Characteristic, "_cat") == F & 
                RR_models_2$Characteristic != var_id)) == T){
    numbers <-
      which(str_detect(RR_models_2$Characteristic, var_id) == T &
              str_detect(RR_models_2$Characteristic, "_cat") == F & 
              RR_models_2$Characteristic != var_id)
    
    RR_models_2[numbers,]$Characteristic <-
      ifelse(str_detect(string = RR_models_2[numbers,]$Characteristic, 
                        pattern = paste0(var_id, "_")) == F,
             yes = str_remove(RR_models_2[numbers,]$Characteristic, var_id),
             no = RR_models_2[numbers,]$Characteristic)
  }
}

RR_models_3 <-
  RR_models_2

RR_models_3$Characteristic <-
  str_remove(string = RR_models_2$Characteristic, 
             pattern = "_cat")

RR_models_2$RR <- as.character(RR_models_3$RR)
RR_models_2$p.value<- as.character(RR_models_3$p.value)
RR_models_3$RR <- as.character(RR_models_3$RR)
RR_models_3$p.value<- as.character(RR_models_3$p.value)

#add the intercept
RR_models_3[4,] <-
  removing_intercept_value(
    data = RR_models_2, 
    row_number = 4, 
    name = "female")

RR_models_3[7,] <-
  removing_intercept_value(
    data = RR_models_2, 
    row_number = 7, 
    name = "adenocarcinoma")

RR_models_4 <-
  RR_models_3

RR_univ_tab <-
  RR_models_4 %>%
  flextable::flextable()
