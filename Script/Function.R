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

group_color <- 
  c("X" = "#c2be4a","Y" = "#de9a33", "Z" = "#94160d" )



