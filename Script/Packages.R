# Load packages ####
library(tidyverse)
library(dplyr)
library(tibble)
library(ggplot2)
library(readxl)
library(writexl)

library(ggprism)
library(patchwork)
library(magrittr)

library(ggpubr)
library(patchwork)
library(pheatmap)

library(mice)
library(impute)
library(VIM)
#library(impute)
#library(mice)

library(gtsummary)

# github configuration ####
library(usethis)
library(gitcreds)
usethis::edit_git_config() #Veryfy username and mail
usethis::use_git()
usethis::create_github_token()
#This will take you to the appropriate page on the GitHub website, 
#where you’ll give your token a name and copy it (don’t lose it because 
#it will never appear again!).
# Token_name : M2_CRC_Immunex_master_project_I2P 
# Token : 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
gitcreds::gitcreds_set()

usethis::use_git()
usethis::use_github()


