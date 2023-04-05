# Creating age_group
wrangling_clinical_data_def <-
  wrangling_clinical_data_def %>%
  mutate(
  # Create categories
  age_group = dplyr::case_when(
    age <= 40            ~ "< 40",
    age > 40 & age <= 45 ~ "40-45",
    age > 44 & age <= 50 ~ "45-50",
    age > 50 & age <= 55 ~ "50-55",
    age > 55 & age <= 60 ~ "55-60",
    age > 60 & age <= 65 ~ "60-65",
    age > 65 & age <= 70 ~ "65-70",
    age > 70 & age <= 75 ~ "70-75",
    age > 75 & age <= 80 ~ "75-80",
    age > 80 & age <= 85 ~ "80-85",
    age > 85 & age <= 90 ~ "80-85"))


  



  # Convert to factor
wrangling_clinical_data_def$age_group <-
  factor(wrangling_clinical_data_def$age_group)

levels(wrangling_clinical_data_def$age_group) <-
  list(`< 40` = "1", `40-45` = "2",`45-50` = "3", `50-55` = "4",
       `55-60` = "5", `60-65` = "6", `65-70` = "7", `70-75` = "8", 
       `75-80` = "9", `80-85` = "10", `80-85` = "11")

# Pyramid ####
apyramid::age_pyramid(data = wrangling_clinical_data_def,
                      age_group = age_group,
                      split_by = sex) +
  my_theme +
  scale_fill_manual(values = c("female" = "pink",   
               "male" = "lightblue")) +
  theme(axis.line.y = element_line(color = "grey90"))


# Histology ####

wrangling_clinical_data_def %>%
  dplyr::select(histology, smoking) %>% 
  group_by(histology) %>%
  summarise(n = n()) %>% 
  ggplot(aes(x = n,
             y = histology,
             fill = histology, 
             label = n)) +
  coord_flip() +
  geom_col() + 
  scale_fill_manual(values = histology_color) + 
  geom_text(color = "white",
            position = position_stack(vjust = 0.5),
            fontface = "bold") +
  my_theme

