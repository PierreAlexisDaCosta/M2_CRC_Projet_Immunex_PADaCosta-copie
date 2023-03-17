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


# Load data ####
survival_data
explanatory

#OR plot univariate analysis ###
OR_univariate_plot <-
  survival_data %>% 
  select(-c("samples_ID", "operation_date", 
            "last_news_date", "os_days", "Diamic")) %>% 
  select(-c(EGFR:p40)) %>% 
  #select(c(status_last_news, age)) %>%
  tbl_uvregression(
    method = glm,
    y = status_last_news,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
  add_global_p() %>% # add global p-value
  add_q() %>% # adjusts global p-values for multiple testing
  bold_p() %>% # bold p-values under a given threshold (default 0.05)
  bold_p(t = 0.10, q = TRUE) %>% # now bold q-values under the threshold of 0.10
  bold_labels()

# RR plot univariate analysis #### LA CA BUG

survival_data %>% 
  select(-c("samples_ID", "operation_date", 
            "last_news_date", "os_days", "Diamic")) %>% 
  select(-c(EGFR:p40)) %>% 
  #select(c(status_last_news, age)) %>%
  tbl_uvregression(
    method = glm,
    y = status_last_news,
    method.args = list(family = poisson),
    exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
  add_global_p() %>% # add global p-value
  add_q() %>% # adjusts global p-values for multiple testing
  bold_p() %>% # bold p-values under a given threshold (default 0.05)
  bold_p(t = 0.10, q = TRUE) %>% # now bold q-values under the threshold of 0.10
  bold_labels()

glm(status_last_news ~ age, data = survival_data, 
    family = binomial) %>% tbl_regression(exponentiate = TRUE)

#NEW TEST ####
# download a specific file into a folder on your computer
#pacman::p_load(epirhandbook)
linelist <- import("linelist_cleaned.rds")

## define variables of interest 
explanatory_vars <- c("sex", "histology")

## convert dichotomous variables to 0/1 
linelist <- linelist %>%  
  mutate(across(                                      
    .cols = all_of(c("outcome")),  ## for each column listed and "outcome"
    .fns = ~case_when(                              
      . %in% c("Death")   ~ 1,           ## recode male, yes and death to 1
      . %in% c("Recover") ~ 0          ## female, no and recover to 0    ## otherwise set to missing
  ))
  )

## drop rows with missing information for variables of interest 
survival_data <- survival_data %>% 
 drop_na(any_of(c("outcome", explanatory_vars)))

?map
models <- explanatory_vars %>%       # begin with variables of interest
  str_c("outcome ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = binomial(link = "log"),           # specify type of glm (logistic)
      data = linelist)) %>%          # dataset
  
  # tidy up each of the glm regression outputs from above
  map(
    .f = ~tidy(
      .x, 
      exponentiate = TRUE,           # exponentiate 
      conf.int = TRUE)) %>%          # return confidence intervals
  
  # collapse the list of regression outputs in to one data frame
  bind_rows() %>% 
  
  # round all numeric columns
  mutate(across(where(is.numeric), round, digits = 2))


## for each explanatory variable
univ_tab_base <- explanatory_vars %>% 
  map(.f = 
        ~{linelist %>%                ## begin with linelist
            group_by(outcome) %>%     ## group data set by outcome
            count(.data[[.x]]) %>%    ## produce counts for variable of interest
            pivot_wider(              ## spread to wide format (as in cross-tabulation)
              names_from = outcome,
              values_from = n) %>% 
            drop_na(.data[[.x]]) %>%         ## drop rows with missings
            rename("variable" = .x) %>%      ## change variable of interest column to "variable"
            mutate(variable = as.character(variable))} ## convert to character, else non-dichotomous (categorical) variables come out as factor and cant be merged
  ) %>% 
  
  ## collapse the list of count outputs in to one data frame
  bind_rows() %>% 
  
  ## merge with the outputs of the regression 
  bind_cols(., models) %>% 
  
  ## only keep columns interested in 
  select(term, 2:3, estimate, conf.low, conf.high, p.value) %>% 
  
  ## round decimal places
  mutate(across(where(is.numeric), round, digits = 2))


univ_tab_base %>% flextable::flextable()

glm(outcome ~ gender, data = linelist, family = binomial(link = "log")) %>% 
  tbl_regression(exponentiate = TRUE)
