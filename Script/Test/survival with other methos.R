# package ####
survival_data
library(dplyr)
library(survival)
library(ggplot2)
library(ggfortify)
library(ranger)
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

km <- with(veteran, Surv(time, status))

km_trt_fit <- survfit(Surv(time, status) ~ trt, data=veteran)

autoplot(km_trt_fit)
# Cox ####
cox <- 
  coxph(Surv(time, status) ~ trt + celltype + karno+ 
          diagtime + age + prior , data = veteran)
summary(cox)
cox %>%
  tbl_regression()

cox_fit <- survfit(cox)
autoplot(cox_fit)

#Random Forest ####
r_fit <- ranger(Surv(time, status) ~ trt + celltype + 
                  karno + diagtime + age + prior,
                data = veteran,
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
     main = "Patient Survival Curves")

cols <- colors()
for (n in sample(c(2:dim(veteran)[1]), 20)){
  lines(r_fit$unique.death.times, r_fit$survival[n,], type = "l", col = cols[n])
}
lines(death_times, avg_prob, lwd = 2)
legend(500, 0.7, legend = c('Average = black'))

vi <- 
  data.frame(sort(round(r_fit$variable.importance, 4), decreasing = TRUE))
names(vi) <- "importance"
head(vi)
#Prediction
#cat("Prediction Error = 1 - Harrell's c-index = ", r_fit$prediction.error)

# All ####
# Set up for ggplot
kmi <- rep("KM",length(km_fit$time))
km_df <- data.frame(km_fit$time,km_fit$surv,kmi)
names(km_df) <- c("Time","Surv","Model")

coxi <- rep("Cox",length(cox_fit$time))
cox_df <- data.frame(cox_fit$time,cox_fit$surv,coxi)
names(cox_df) <- c("Time","Surv","Model")

rfi <- rep("RF",length(r_fit$unique.death.times))
rf_df <- data.frame(r_fit$unique.death.times,avg_prob,rfi)
names(rf_df) <- c("Time","Surv","Model")

plot_df <- rbind(km_df,cox_df,rf_df)

p <- ggplot(plot_df, aes(x = Time, y = Surv, color = Model))
p + geom_line()

