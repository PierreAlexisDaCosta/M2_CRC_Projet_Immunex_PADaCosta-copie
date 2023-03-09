
#meth[c("Cholesterol")]="norm" 

imputed <-
  wrangling_clinical_data2 %>% 
  select(-last_news_date) %>% # remove the non numerical value
  select(-cancer_history_details) %>%
  select(-operation_date) %>%
  select(-second_stay) %>% 
  select(-c("EGFR":"p40")) %>%
  select(-Diamic) %>% 
  mice(method= "pmm", m=13)

imputed <- complete(imputed)

#sapply(imputed, function(x) sum(is.na(x))) #no NAs
imputed %>%
  tbl_summary
