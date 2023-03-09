# Load package ####
source("./Script/Packages.R")
source("./Script/Function.R")
set.seed(123)
library(survival)
library(ggplot2)
library(ggfortify)
library(ranger)

# Load data ####
data_s <-
  readxl::read_excel("./Preprocessed_data/Database_recode_V2.xlsx")

data_s2 <-
  data_s #%>%
  #select(c("operation_date", "last_news_date", "status_last_news", "histology")) 

data_s3 <-
  data_s2

data_s3$status_last_news <-
  ifelse(data_s2$status_last_news == "deceased",
       yes = 1,
       no = 0)
#censoring status 0=censored, 1=dead

survival_data_1 <-
  data_s3 %>%
  mutate(
    os_days = as.duration(operation_date %--% last_news_date) / ddays(1)
    )

survival_data <-
  survival_data_1

survival_data$histology <-
  survival_data_1$histology %>% 
  as.factor()

levels(survival_data$histology) <-
  list(adenocarcinoma = "adenocarcinoma", 
       squamous_cell_carcinoma = "squamous_cell_carcinoma", 
       other_histology_types = "other_histology_types")

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

#ggsave("./Result/Kaplan-Meyer_histology.png")

# Kaplan-Meyer sex ####
fit_sex <-
  survfit2(Surv(os_days, status_last_news) ~ sex, data = survival_data) 

fit_sex %>%
  ggsurvfit(theme = theme_ggsurvfit_KMunicate(),
  ) +
  add_censor_mark() +
  ggsurvfit::add_pvalue(location = "annotation") +
  add_confidence_interval() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) +
  #scale_colour_manual(values = sex_color) +
  #scale_fill_manual(values = sex_color) +
  add_risktable(risktable_stats = "n.risk") +
  my_theme +
  theme(legend.position = "top")

# Cox regression --> choose some covariates (2 or 3): Table + plot  HR ####
cox <- 
  coxph(Surv(os_days, status_last_news) ~ age + sex + 
          histology + length_of_stay,
        data = survival_data)
#summary(cox)
cox %>%
  tbl_regression(
    exp = TRUE
    )

cox_fit <- survfit(cox)

autoplot(cox_fit) +
  my_theme




# Random Forest ####
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

# Surement entrainer sur un training set
