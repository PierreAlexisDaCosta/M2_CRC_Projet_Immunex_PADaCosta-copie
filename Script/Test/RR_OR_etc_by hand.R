# Function to make OR, RR etc by hand

univariate_analysis <-
  function(glm_type, data, analyse_type, explanatory_vars){
    explanatory_vars %>%       # begin with variables of interest
      str_c("status_last_news ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
      # iterate through each univariate formula
      map(                               
        .f = ~glm(                       # pass the formulas one-by-one to glm()
          formula = as.formula(.x),      # within glm(), the string formula is .x
          family = glm_type,           # specify type of glm (logistic)
          data = data)) %>%          # dataset
      # tidy up each of the glm regression outputs from above
      map(
        .f = ~tidy(
          .x, 
          exponentiate = TRUE,           # exponentiate 
          conf.int = TRUE)) %>%          # return confidence intervals
      # collapse the list of regression outputs in to one data frame
      bind_rows(.id = "id") %>% 
      # round all numeric columns
      mutate(across(where(is.numeric), round, digits = 4)) %>%
      mutate(`CI 95%` = paste0(conf.low, ", ", conf.high)) %>%
      select(term, estimate, `CI 95%`, p.value) %>%
      rename(Characteristic = term) %>%
      rename(!!sym(analyse_type) := estimate)
  }                    

removing_intercept_value <-
  function(data, name, row_number){
    data[row_number,]$Characteristic <- paste0(name, " (Intercept)")
    data[row_number,]$OR <- "――"
    data[row_number,]$`CI 95%` <- "————"
    data[row_number,]$p.value <- "——"
    print(data[row_number,])
  }



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