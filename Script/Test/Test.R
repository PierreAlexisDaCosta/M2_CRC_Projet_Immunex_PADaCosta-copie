# package ####
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
library(gtsummary)  # formatted model output
library(rmarkdown) 
#install.packages('tinytex')
#tinytex::install_tinytex()
#tinytex:::is_tinytex()

#'# Example dataset
dataset = as.data.frame(cbind(Y1 = c(rep(0,10), rep(1,10)),
                              Y2 = c(rep(0,5), rep(1,10), rep(0,5)),
                              X = rnorm(20, 0, 1)))

#'# Loop with formatted output
#+ Table1, results = 'asis'
for(i in c("Y1", "Y2")){
  fit = glm( paste("get(i) ~", "X") ,
             family = binomial,
             data = dataset)
  
  print(length(fit$residuals))
  
     <- fit %>%  
    tbl_regression(
      exponentiate = TRUE, 
      conf.level = 0.999,
      pvalue_fun = ~style_pvalue(.x, digits = 3),
    ) %>% 
    bold_p(t = 0.01) %>%
    bold_labels() %>%
    italicize_levels() 
  
  tbl_stack()
  
  fit2
  
  cat(knitr::knit_print(fit2))
  
}

#'# Print to word document
knitr::opts_chunk$set(
  echo=FALSE, warning=FALSE, message=FALSE)
getwd()
dir.create("./Preresult")
rmarkdown::render("./Preresult/Loop.Rmd",
                  output_format = "pdf_document",
                  output_file = "/Users/pierre/Documents/my_projects/M2_CRC_Immunex_Lung_cancer/Preresult/myfile.pdf",  
                  quiet = TRUE)
