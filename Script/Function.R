# Load packages ####
source("./Script/Packages.R")

# set.seed ####
set.seed(123)

# Theme ####
my_theme <- theme( 
  panel.background = element_rect(fill = "white"),
  axis.line = element_line(color = "grey90"),
  strip.background = element_rect(fill = "grey30"),
  strip.text = element_text(colour = "white"),
  text = element_text(face = "bold")
)

# Colour ####

histology_color <- 
  c("adenocarcinoma" = "#c2be4a","squamous_cell_carcinoma" = "#de9a33", "other_histological_types" = "#94160d" )

MCA_color <- 
  c("histology" = "#de9a33", "significant_variables" = "#94160d")

# Variable ####
explanatory_vars <- c("age", "sex", #	"histology",
                     "performance_status",	"height",	"weight",	"bmi",
                     "body_surface", "weight_loss",	"smoking",	"pack_years",
                     "weaning",	"significant_comorbidities", "ischemic_heart_disease_history",	
                     "COPD_history",	"cancer_history",	"symptoms",	"dyspnea_mMRC",
                     "TDM",	"main_lesion_size_cm",	"atelectasia",	"main_lesion_topography",
                     "main_lesion_side",	"main_lesion_location",	"contact_adjacent_lobe",
                     "adjacent_structure_infiltration", "chest_wall_contact",	
                     "mediastinal_contact", "intrapleural_adenopathies",	"mediastinal_adenopathies",	
                     "extrathoracic_metastasis",	
                     "bronchial_fibroscopy",	"main_lesion_PET_fixation",	
                     "main_lesion_SUV",	"total_number_lung_fixations",	
                     "intrapleural_lymphnode_fixation",	
                     "mediastinal_lymphnode_fixation",	"FEV1",	"FVC",
                     "Tiffeneau",	"TLC",	"CRP",	"albuminemia",	"prealbuminemia",
                     "hemoglobin",	"platelets",	"leukocytes",	"lymphocytes",	
                     "neutrophils",	"Thoracoscore",	"surgery_indication",
                     "CRP_D1",	"hemoglobin_D1",	"platelets_D1",	
                     "leukocytes_D1",	"lymphocytes_D1",	"neutrophils_D1",
                     "CRP_D3",	"hemoglobin_D3",	"platelets_D3",
                     "leukocytes_D3",	"lymphocytes_D3",	"neutrophils_D3",
                     "ASA",	"surgery_approach",	"surgery_performed",	"post_operational",
                     "adenocarcinoma_subtypes",	"TTF1",	"second_lesion",
                     "PDL1",	"pT_size",	"pT",	"p_pleura",	"embolism",
                     "stroma_lymphocyte_population",	"pN")


# Regression function ####

explanatory_vars <- c("age", "sex")

multivariate_glm_analysis <-
  function(glm_type, data, explanatory_vars){
  explanatory_vars %>%       # begin with variables of interest
  str_c("status_last_news ~ histology +", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  # iterate through each univariate formula
  map(                               
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = glm_type,           # specify type of glm (logistic)
      data = data)) %>%       # dataset
  # tidy up each of the glm regression outputs from above
  map(
    .f = ~tbl_regression(
      .x, 
      include = !'histology',
      exponentiate = TRUE)) %>%
  tbl_stack()

}

multivariate_glm_analysis(glm_type = binomial, 
                        data = survival_data, 
                        explanatory_vars = explanatory_vars)

univariate_glm_analysis <-
  function(glm_type, data, explanatory_vars){
    explanatory_vars %>%       # begin with variables of interest
      str_c("status_last_news ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
      # iterate through each univariate formula
      map(                               
        .f = ~glm(                       # pass the formulas one-by-one to glm()
          formula = as.formula(.x),      # within glm(), the string formula is .x
          family = glm_type,           # specify type of glm (logistic)
          data = data)) %>%       # dataset
      # tidy up each of the glm regression outputs from above
      map(
        .f = ~tbl_regression(
          .x, 
          exponentiate = TRUE)) %>%
      tbl_stack()
  }
