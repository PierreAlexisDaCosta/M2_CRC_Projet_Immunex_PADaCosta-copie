# set.seed ####
set.seed(123)

# Data wrangling ####
df_1 <-
  readxl::read_excel("./Preprocessed_data/Database_recode_V2.xlsx")

df <-
  df_1 %>%
  select(-Diamic) %>%
  select(-age) %>% #Ou la mettre par tranche de 10 ans
  select(-samples_ID) %>%
  select(-operation_date) %>%
  select(-length_of_stay) %>%
  select(- preoperative_stay) %>%
  select(-postoperative_stay) %>%
  select(-performance_status)  %>% #pas obligé
  select(-c(height:weight_loss)) %>%
  select(- pack_years) %>%
  select(-significant_comorbidities) %>% #pas obligé
  select(-dyspnea_mMRC) %>%
  select(-main_lesion_SUV) %>% #pas obligé
  select(-c(FEV1:Thoracoscore)) %>%
  select(-c(CRP_D1:neutrophils_D5)) %>%
  select(-ASA) %>%
  select(- malignancy) %>%
  select(-pT_size) %>%
  select(-last_news_date) %>%
  select(-main_lesion_size_cm) %>%
  select(-total_number_lung_fixations) %>% #pas obligé
  select(- mediastinal_contact_details) %>%
  select(-second_extrathoracic_PET_fixation) %>%
  select(-pT3_details)%>%
  select(-pT4_details) %>%
  select(-pN2_details_site) %>%
  select(-pN2_details_location) %>%
  select(-cancer_history_details) %>%
  select(-c(TTF1:p40)) %>%
  select(-c(logFC_CRP_CRPD1_r0:logFC_neutrophilsD3_neutrophilsD5_r5)) %>%
  select(-second_stay) %>% #variables with almost one value at 100%
  select(-immunosuppression_history) %>%
  select(-postoperative_death) %>%
  select(-sample_origin) %>%
  select(- adenocarcinome_subtypes) %>% #changer par adenocarcinoma #concerne que les adk
  select(-death_cause) %>% #ou status_last_news en fonction de comment ce sera coder après
  select(-ischemic_heart_disease_history) %>% # j'ai enlevé ce qui était pas signif 
  select(-cancer_history) %>%
  select(-symptoms) %>%
  select(-chest_wall_contact) %>%
  select(-rachis_contact) %>%
  select(-mediastinal_contact) %>%
  select(-extrathoracic_PET_fixation) %>%
  select(-initial_treatment_decision) %>%
  select(-surgery_indication) %>%
  select(-surgery_approach) %>%
  select(-iterative_surgery) %>%
  select(-lymphnode_dissection) %>%
  select(-intervention_of_necessity) %>%
  select(-post_operational) %>%
  select(-main_lesion_topography) %>%
  select(-main_lesion_side) %>%
  select(-stroma_lymphocyte_population) %>%
  select(-mediastinal_structure_enlargement) %>%
  select(-parietal_structure_enlargement) %>%
  select(-lung_enlargement) %>%
  select(-mediastinal_lymphnode_fixation) %>%
  select(-intrapleural_lymphnode_fixation) %>%
  select(-extrathoracic_metastasis) %>%
  select(-extrathoracic_extension) %>% 
  select(-mediastinal_adenopathies) %>%
  select(-intrapleural_adenopathies) %>%
  select(-contact_adjacent_lobe)
 

df$embolism <-
  ifelse(df$embolism == "no_embolism",
         yes = "no_embolism",
         no = "embolism")

df$pT <-
  ifelse(df$pT == "pT1A"  | df$pT == "pT1B" | df$pT == "pT1C",
         yes = "pT1",
         no = ifelse(df$pT == "pT2A"  | df$pT == "pT2B",
                     yes = "pT2",
                     no = df$pT))

#df %>%
#  tbl_summary(by = "histology")

# MCA ####
res <- MCA(df, graph = FALSE) 
variance <- res$eig %>% t() %>% as.data.frame()

mca_first_output <- 
  as.data.frame(res$var$coord) %>% 
  mutate(category = c("Var_category"))

mca_second_output <-
  res$ind$coord %>%
  as.data.frame() %>%
  mutate(category = "Individual")

mca_pre_output <-
  rbind(mca_second_output, mca_first_output)


# MCA representation of variance explained, cos2, contrib ####
#Variance explained by dimensions 
fviz_eig(res)
#Quality of representation on Dim.1,  on Dim.2 and on dim1&2
fviz_cos2(res, choice = "var", axes = 1)
fviz_cos2(res, choice = "var", axes = 2)
fviz_cos2(res, choice = "var", axes = 1:2)
#Contribution of variable categories on Dim.1,  on Dim.2 and on dim1&2
fviz_contrib(res, choice = "var", axes = 1, top = 15)
fviz_contrib(res, choice = "var", axes = 2, top = 15)
fviz_contrib(res, choice = "var", axes = 1:2, top = 15)

# MCA (performed by factoextra, below by ggplot)
# contrib
fviz_mca_ind(res) 
fviz_mca_var(res)
fviz_mca_biplot(res)
fviz_mca_var(res, 
             col.var = "contrib",
             alpha.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping 
             ggtheme = theme_minimal())
#cos2/quality of representation
fviz_mca_var(res, col.var = "cos2", 
             alpha.var ="cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping 
             ggtheme = theme_minimal())


# MCA merging data ####

#Contribution
mca_var_contrib <-
  res$var$contrib %>%
  as.data.frame() %>%
  mutate(category = c("Var_category")) %>%
  rename("Contrib_Dim1" = `Dim 1`) %>%
  rename("Contrib_Dim2" = `Dim 2`) %>%
  rename("Contrib_Dim3" = `Dim 3`)%>%
  rename("Contrib_Dim4" = `Dim 4`)%>%
  rename("Contrib_Dim5" = `Dim 5`)

mca_ind_contrib <-
  res$ind$contrib %>%
  as.data.frame() %>%
  mutate(category = c("Individual")) %>%
  rename("Contrib_Dim1" = `Dim 1`) %>%
  rename("Contrib_Dim2" = `Dim 2`) %>%
  rename("Contrib_Dim3" = `Dim 3`)%>%
  rename("Contrib_Dim4" = `Dim 4`)%>%
  rename("Contrib_Dim5" = `Dim 5`)


mca_contrib <-
  rbind(mca_ind_contrib, mca_var_contrib) %>%
  mutate(
    contrib_dim1_2 = 
      ( ((Contrib_Dim1 * variance$`dim 1`[2]) + (Contrib_Dim2*variance$`dim 2`[2])) / (variance$`dim 1`[2] + variance$`dim 2`[2]))
  )

# Cos2/Quality
mca_var_reprez <-
  res$var$cos2 %>%
  as.data.frame() %>%
  mutate(category = c("Var_category")) %>%
  rename("Reprez_Dim1" = `Dim 1`) %>%
  rename("Reprez_Dim2" = `Dim 2`) %>%
  rename("Reprez_Dim3" = `Dim 3`)%>%
  rename("Reprez_Dim4" = `Dim 4`)%>%
  rename("Reprez_Dim5" = `Dim 5`)

mca_ind_reprez <-
  res$ind$cos2 %>%
  as.data.frame() %>%
  mutate(category = c("Individual")) %>%
  rename("Reprez_Dim1" = `Dim 1`) %>%
  rename("Reprez_Dim2" = `Dim 2`) %>%
  rename("Reprez_Dim3" = `Dim 3`)%>%
  rename("Reprez_Dim4" = `Dim 4`)%>%
  rename("Reprez_Dim5" = `Dim 5`)

mca_reprez <-
  rbind(mca_ind_reprez, mca_var_reprez) %>%
  mutate(
    cos2_dim1_2 = 
      ( Reprez_Dim1  + Reprez_Dim2)
  )

#MCA Ouput
mca_output <- 
  cbind(mca_pre_output, mca_contrib, mca_reprez) %>%  
  janitor::clean_names() #Check the variables and individuals

#IL FAUT ENSUITE METTRE LES CATEGORIES par rapport au Bar_category (A,B,C = Var1 ; ..=Var2 etc)
#mca_output %>% 
#  filter(category == "Var_category") %>%
#  mutate(name_category = c("Var1", "Var1", "Var1", "Var2", "Var2", "Var2", "Var3", "Var3"))
view(mca_output)
mca_output2 <-
  mca_output %>%
  mutate(categories = ifelse(rownames(mca_output) == "adenocarcinoma" | rownames(mca_output) == "squamous_cell_carcinoma"| rownames(mca_output) == "other_histology_types",
                       yes = "histology",
                       no = "significant_variables"))
# MCA 2D plot ####
MCA_plot <-
  mca_output2 %>%
  rename(contrib = contrib_dim1_2) %>%
  rename(cos2 = cos2_dim1_2) %>%
  mutate(label_name = ifelse(mca_pre_output$category == "Var_category", 
                             row.names(mca_pre_output),
                             NA)) %>% 
  filter(category == "Var_category") %>% #on peut enlever ça si on veut les individus avec
  ggplot(aes(x = dim_1,
             y = dim_2,
             colour = categories,
             fill = contrib
  )) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  geom_vline(xintercept = 0, alpha = 0.5) +
  geom_point(aes(colour = category, 
                 size = cos2   
                 ),
             shape = 21, 
             show.legend = T,
             alpha = 0.8,
             stroke = 1) + 
  ggrepel::geom_text_repel(aes(label = label_name), 
                           max.overlaps = 5000, 
                           size = 2.5, 
                           show.legend = F) +
  theme(
    axis.line = element_line(color = "grey80"),
    text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(x = "Dim 1 (11%)", y = "Dim 2 (7,1%)") +
  my_theme +
  scale_fill_gradientn(colours = c("darkblue", "#f0ed5d", "#ff6961"),
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill") +
  scale_colour_manual(values = category_color)

ggplot2::ggsave("./Preresult/MCA_plot.pdf", width = 17, height = 13)
# MCA 3D plot ####

mca_output %>%
  mutate(label_name = ifelse(mca_output$category == "Var_category", 
                             row.names(mca_pre_output),
                             NA)) %>% 
  plotly::plot_ly( 
    x = ~dim_1, 
    y = ~dim_2, 
    z = ~dim_3, 
    color = ~categories,
    text = ~label_name#, 
    #colors = group_color
  ) %>%
  add_markers() %>%
  add_text() %>%
  layout(scene = list(
    xaxis = list(title = 'Dim 1 (XX%)'),
    yaxis = list(title = 'Dim 2 (XX%)'),
    zaxis = list(title = 'Dim 3 (XX%)')))

