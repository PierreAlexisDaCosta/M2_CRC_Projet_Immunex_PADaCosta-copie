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
  c("adenocarcinoma" = "#c2be4a","squamous_cell_carcinoma" = "#de9a33", "other_histology_types" = "#94160d" )


MCA_color <- 
  c("histology" = "#de9a33", "significant_variables" = "#94160d")
