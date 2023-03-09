#Load the packages ####
library(caTools)
set.seed(123)

#Split the dataset ####
split = sample.split(survival_data$status_last_news, SplitRatio = 0.8)
training_set = subset(survival_data, split == TRUE)
test_set = subset(dataset, split == FALSE)

#glm ####

test <- 
  glm(status_last_news ~ histology + sex + age, data = survival_data, family = binomial(link = "log"))
summary(test)
test %>% tbl_regression(exp = TRUE)
