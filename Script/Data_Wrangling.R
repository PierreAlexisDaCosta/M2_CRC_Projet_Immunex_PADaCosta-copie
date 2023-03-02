# Load packages ####
source("./Script/Packages.R")

# set.seed ####
set.seed(123)

# Load the data ####

pre_wrangling_clinical_data <- 
  readxl::read_excel(
    "./Raw_data/pre_clinical_raw_data.xlsx") %>%
  filter(Diamic != "18H04264") %>%
  filter(Diamic != "18H05783") %>%
  filter(Diamic != "18H03170") %>%
  filter(Diamic != "18H03144") %>%
  filter(Diamic != "18H03355") %>%
  filter(Diamic != "18H23655") %>%
  filter(Diamic != "18H21509") %>%
  filter(Diamic != "18H23859") %>%
  filter(Diamic != "18H22316") %>%
  filter(Diamic != "18H17818") %>%
  filter(Diamic != "18H24175") %>%
  filter(Diamic != "18H22956")%>%
  filter(Diamic != "18H14844")%>%
  filter(Diamic != "18H16286")%>%
  filter(Diamic != "18H25707") %>%
  select(-NIP ) %>%
  select(-`...3`) %>%
  select(-`date naissance`) %>%
  select(-`date consultation`) %>%
  select(-`date hospitalisation 1`) %>%
  select(-`date sortie 1`) %>%
  select(-`date hospitalisation 2`) %>%
  select(-`date opératoire 2`) %>%
  select(-`date sortie 2`) %>%
  select(-`Bloc intérêt T`)%>%
  select(-`Bloc intérêt N`) %>%
  select(-`TRT RECIDIVE`)

# NA count by column ####

NA_col_clinical <-
  colSums(is.na(pre_wrangling_clinical_data)) %>%
  as.data.frame()
#view(NA_col_clinical)
wrangling_clinical_data <-
  pre_wrangling_clinical_data %>%
  select(-c(VO2max:`% desaturation TM6M`)) %>%
  select(-c(`PAP systolique`:`Surface psoas niveau L3 (TPA)`)) %>%
  select(-c(`% tumeur viable post-induction`:`stroma riche en lymphocytes  2ème lésion`)) %>%
  select(-c(`chimiothérapie...176`:`radiochimiothérapie concommittante`)) %>%
  select(-c(`type histologique 2ème lésion`:`TTF1 2ème lésion`)) %>%
  select(-c(`p40 2ème lésion`:`EGFR 2ème lésion`)) %>%
  select(-c(`probabilité infiltration`)) %>%
  select(-c(`taille adénopathies`)) %>%
  select(-c(`station adénopathie médiastinale la plus suspecte`:`allure adénopathie la plus suspecte`)) %>%
  select(-c(`topographie bronchique`)) %>%
  select(-c(`SUV  adénopathie médiastinale la plus suspecte`:`SUV   2 adénopathie médiastinale la plus suspecte`)) %>%
  select(-`durée préopératoire 2`) %>%
  select(-`durée postoperatoire 2`) %>%
  select(-`date opération/début traitements`)

wrangling_clinical_data$`duréé sejour 2` <-
  ifelse(test = is.na(pre_wrangling_clinical_data$`duréé sejour 2`) == T, 
         yes = "no_second_stay",
         no = "second_stay")

wrangling_clinical_data$`détail antécédent autre cancer` <-
  ifelse(wrangling_clinical_data$`antécédents autre cancer`== 2 & is.na(wrangling_clinical_data$`détail antécédent autre cancer`) == T,
                 9, 
         wrangling_clinical_data$`détail antécédent autre cancer`)

wrangling_clinical_data$`détail contact médiastin` <-
  ifelse(wrangling_clinical_data$`contact médiastin` == 2,
       yes = 0,
       no = wrangling_clinical_data$`détail contact médiastin`)

wrangling_clinical_data$`si pT3 lésion principale` <-
  ifelse(wrangling_clinical_data$`pT lésion principale` != 6,
         yes = 0,
         no = wrangling_clinical_data$`si pT3 lésion principale` )

wrangling_clinical_data$`si pT4 lésion principale` <-
  ifelse(wrangling_clinical_data$`pT lésion principale` != 7,
         yes = 0,
         no = wrangling_clinical_data$`si pT4 lésion principale` )

wrangling_clinical_data$`si pT4 lésion principale` <-
  ifelse(wrangling_clinical_data$`pT lésion principale` != 7,
         yes = 0,
         no = wrangling_clinical_data$`si pT4 lésion principale` )

wrangling_clinical_data$`si pN2_site` <-
  ifelse(wrangling_clinical_data$pN != 3,
         yes = 0,
         no = wrangling_clinical_data$`si pN2_site`)

wrangling_clinical_data$`si pN2_localisation`<-
  ifelse(wrangling_clinical_data$pN != 3,
         yes = 0,
         no = wrangling_clinical_data$`si pN2_localisation`)

wrangling_clinical_data$`cause du décès` <-
  ifelse(wrangling_clinical_data$`état à la date des dernières nouvelle` == 4 
         & is.na(wrangling_clinical_data$`cause du décès`) == T,
       yes = 3,
       no = wrangling_clinical_data$`cause du décès`)

wrangling_clinical_data$`cause du décès` <-
  ifelse(wrangling_clinical_data$`état à la date des dernières nouvelle` == 1 | 
         wrangling_clinical_data$`état à la date des dernières nouvelle` ==  2 |
         wrangling_clinical_data$`état à la date des dernières nouvelle` == 3 &
         is.na(wrangling_clinical_data$`cause du décès`) == T,
       yes = 0,
       no = wrangling_clinical_data$`cause du décès`)

wrangling_clinical_data$`dyspnée MRCS` <-
  as.numeric(wrangling_clinical_data$`dyspnée MRCS`)


wrangling_clinical_data$`si pT3 lésion principale` <-
  as.numeric(wrangling_clinical_data$`si pT3 lésion principale`)

wrangling_clinical_data$`sct14` <-
  ifelse(wrangling_clinical_data$`sct14` == "due", 
       yes = 2,
       ifelse(wrangling_clinical_data$`sct14` == "unoqu",
              yes = 1,
              ifelse(wrangling_clinical_data$`sct14` == "zero",
                     yes = 0,
                     no= wrangling_clinical_data$`sct14`
                     )))

wrangling_clinical_data$`sct14` <-
  as.numeric(wrangling_clinical_data$`sct14`)

wrangling_clinical_data$`détail antécédent autre cancer` <-
  wrangling_clinical_data$`détail antécédent autre cancer` %>% as.character()

wrangling_clinical_data$`élargissement structure pariétale` <-
  wrangling_clinical_data$`élargissement structure pariétale` %>% as.numeric()

# Rename the column data frame ####
wrangling_clinical_data2 <-
  wrangling_clinical_data %>%
  rename(age = âge) %>%
  rename(sex = sexe) %>%
  rename(length_of_stay = `duréé sejour 1`) %>%
  rename(operation_date = `date opératoire 1`) %>%
  rename(preoperative_stay = `durée préopératoire 1`) %>%
  rename(postoperative_stay = `durée postoperatoire 1`) %>%
  rename(second_stay = `duréé sejour 2`) %>%
  rename(performance_status = `PERFORMANCE STATUS`) %>%
  rename(height = `taille (m)`) %>%
  rename(weight = `poids (Kg)`) %>%
  rename(bmi = imc) %>%
  rename(body_surface = `Surface corporelle`) %>% 
  rename(usual_weight = `poids habituel` ) %>%
  rename(weight_loss = `perte ponderale` ) %>%
  rename(smoking = `tabagisme` ) %>%
  rename(pack_years = `paquets/année` ) %>%
  rename(weaning = `sevrage` ) %>%
  rename(significant_comorbidities = `nombre comorbidités significatives` ) %>%
  rename(ischemic_heart_disease_history = `antécédent cardiopathie ischémique` ) %>%
  rename(COPD_history = `antécédents BPCO` ) %>%
  rename(cancer_history = `antécédents autre cancer` ) %>%
  rename(cancer_history_details = `détail antécédent autre cancer` ) %>%
  rename(immunosuppression_history = `antécédents immunosuppression` ) %>%
  rename(symptoms = `Symptômes` ) %>%
  rename(dyspnea_MRCS = `dyspnée MRCS` ) %>%
  rename(TDM = TDM ) %>%
  rename(main_lesion_size_cm = `taille lésion principale (cm)` ) %>%
  rename(atelectasia = atélectasie ) %>%
  rename(main_lesion_topography = `topographie lésion principale` ) %>%
  rename(main_lesion_side = `coté lésion principale` ) %>%
  rename(main_lesion_location = `lésion centrale/périphérique` ) %>%
  rename(contact_adjacent_lobe = `contact lobe adjacent` ) %>%
  rename(adjacent_structure_infiltration = `infiltration structures adjacentes` ) %>%
  rename(chest_wall_contact = `contact paroi thoracique` ) %>%
  rename(rachis_contact = `contact rachis` ) %>%
  rename(mediastinal_contact = `contact médiastin` ) %>%
  rename(mediastinal_contact_details = `détail contact médiastin` ) %>%
  rename(intrapleural_adenopathies = `adénopathies intra pleurales` ) %>%
  rename(mediastinal_adenopathies = `adénopathies médiastinales` ) %>%
  rename(extrathoracic_extension = `extension extra thoracique` ) %>%
  rename(extrathoracic_metastasis = `méta extra thoracique` ) %>%
  rename(bronchial_fibroscopy = `fibro bronchique` ) %>% # fibroscopie ou fibrose
  rename(main_lesion_PET_fixation = `Fixation PET lésion principale` ) %>%
  rename(main_lesion_SUV = `SUV lésion principale` ) %>%
  rename(total_number_lung_fixations = `nombre total fixations pulmonaires` ) %>%
  rename(intrapleural_lymphnode_fixation = `fixation ganglions intra-pleuraux` ) %>%
  rename(mediastinal_lymphnode_fixation = `fixation ganglions médiastinaux` ) %>%
  rename(extrathoracic_PET_fixation = `fixation PET extra thoracique` ) %>%
  rename(second_extrathoracic_PET_fixation = `2eme fixation PET extra thoracique` ) %>%
  rename(FEV1 = `VEMS (%th)` ) %>%
  rename(FVC = `CVF (%th)` ) %>%
  rename(Tiffeneau = Tiffeneau ) %>%
  rename(TLC = CPT ) %>%
  rename(RV = VR ) %>%
  rename(DLCO = DLCO ) %>%
  rename(pulmonary_artery_axialdiameter = `Diamètre AP axial` ) %>%
  rename(pulmonary_artery_saggitaldiameter = `Diamètre AP sagittal` ) %>%
  rename(aortic_diameter = `Diamètre Ao` ) %>%
  rename(LVEF = FEVG ) %>%
  rename(CRP = CRP ) %>%
  rename(albuminemia = Albuminémie ) %>%
  rename(prealbuminemia = `Pré albuminémie` ) %>%
  rename(hemoglobin = hémoglobine ) %>%
  rename(platelets = `plaquettes (10*9)` ) %>%
  rename(leukocytes = `leucocytes totaux (10*9)` ) %>%
  rename(lymphocytes = `lymphocytes (10*9)` ) %>%
  rename(neutrophils = `neutrophiles (10*9)` ) %>%
  rename(Thoracoscore = Thoracoscore) %>%
  rename(initial_treatment_decision = `décision traitement initial` ) %>%
  rename(surgery_indication = `indication à la chirurgie` ) %>%
  rename(CRP_D1 = CRPJ1 ) %>%
  rename(hemoglobin_D1 = HemoglobineJ1 ) %>%
  rename(platelets_D1 = plaquetteJ1 ) %>%
  rename(leukocytes_D1 = leucocyteJ1 ) %>%
  rename(lymphocytes_D1 = lymphocyteJ1 ) %>%
  rename(neutrophils_D1 = neutrophileJ1 ) %>%
  rename(CRP_D3 = CRPJ3 ) %>%
  rename(hemoglobin_D3 = HemoglobineJ3 ) %>%
  rename(platelets_D3 = plaquetteJ3 ) %>%
  rename(leukocytes_D3 = leucocyteJ3 ) %>%
  rename(lymphocytes_D3 = lymphocyteJ3 ) %>%
  rename(neutrophils_D3 = neutrophileJ3 ) %>%
  rename(CRP_D5 = CRPJ5 ) %>%
  rename(hemoglobin_D5 = HEMOGLOJ5 ) %>%
  rename(platelets_D5 = PLAQUETTEJ5 ) %>%
  rename(leukocytes_D5 = LEUCOJ5 ) %>%   
  rename(lymphocytes_D5 = LYMPHOJ5 ) %>%    
  rename(neutrophils_D5 = NEUTROJ5 ) %>%    
  rename(ASA = ASA ) %>%    
  rename(emergency = URGENCE ) %>%    
  rename(surgical_contamination_class = `Classe contamination chirurgie` ) %>%    
  rename(surgery_approach = `chirurgie: voie d'abord` ) %>%    
  rename(iterative_surgery = `chirurgie itérative` ) %>%    
  rename(surgery_performed = `geste principal réalisé` ) %>%    
  rename(surgery_performed_extent = `geste principal réalisé: etendue` ) %>%    
  rename(lung_enlargement = `élargissement poumon` ) %>%    
  rename(parietal_structure_enlargement = `élargissement structure pariétale` ) %>%    
  rename(mediastinal_structure_enlargement = `élargissement structure médiastinale` ) %>%    
  rename(lymphnode_dissection = `curage ganglionnaire` ) %>%    
  rename(intervention_of_necessity = `Intervention de nécessité` ) %>%    
  rename(postoperative_death = `décès post-opératoire` ) %>%    
  rename(post_operational = `suites opératoires` ) %>%    
  rename(sample_origin = `pathologie: origine du prélèvement` ) %>%    
  rename(histology = `type histologique lésion principale` ) %>%    
  rename(malignancy = `pathologie benigne ou maligne` ) %>%    
  rename(adenocarcinome_subtypes = `sous-type  adk lésion principale` ) %>%    
  rename(TTF1 = `TTF1 lésion principale` ) %>%    
  rename(second_lesion = `2ème lésion` ) %>%    
  rename(EGFR = `EGFR lésion principale` ) %>%    
  rename(KRAS = `KRAS lésion principale` ) %>%    
  rename(ALK = `ALK lésion principale` ) %>%    
  rename(HER2 = `HER2 lésion principale` ) %>%    
  rename(BRAF = `BRAF lésion principale` ) %>%    
  rename(ROS = `ROS lésion principale` ) %>%    
  rename(PDL1 = `PDL-1 T` ) %>%    
  rename(p40 = `p40` ) %>%    
  rename(pT_size = `pT en cm lésion principale` ) %>%    
  rename(pT = `pT lésion principale` ) %>%    
  rename(pT3_details = `si pT3 lésion principale` ) %>%    
  rename(pT4_details = `si pT4 lésion principale` ) %>%    
  rename(embolism = `embols  lésion principale` ) %>%    
  rename(stroma_lymphocyte_population = `stroma riche en lymphocytes  lésion principale` ) %>%    
  rename(pN = `pN` ) %>%    
  rename(pN2_details_site = `si pN2_site` ) %>%    
  rename(pN2_details_location = `si pN2_localisation` ) %>%    
  rename(last_news_date = `date des dernières nouvelle` ) %>%  
  rename(status_last_news = `état à la date des dernières nouvelle` ) %>%  
  rename(cancer_recurrence_date = `date récidive` ) %>%    
  rename(death_cause = `cause du décès` ) %>% 
  rename(TO20T = TO20T ) %>%    
  rename(MCT1T = MCT1T ) %>%    
  rename(MCT4T = MCT4T ) %>%    
  rename(sctne = sctne ) %>%    
  rename(sct14 = sct14 ) %>%    
  rename(TO20I = TO20I ) %>%    
  rename(MCT1I = MCT1I ) %>%    
  rename(MCT1I = MCT1I ) %>%    
  rename(scoim = scoim ) %>%    
  rename(sci14 = sci14 ) %>%
  rename(p_pleura = `p plèvre lésion principale`)

#view(wrangling_clinical_data2)

#str(wrangling_clinical_data2, list.len=ncol(wrangling_clinical_data2))


# K-nearest neighbor algorithm for imputing NA/missing values ####
#view(knn_data)
knn_data <-
  wrangling_clinical_data2 %>%
  select(-cancer_recurrence_date) %>% # remove the non numerical value
  select(-last_news_date) %>%
  select(-cancer_history_details) %>%
  select(-operation_date) %>%
  select(-second_stay) %>% 
  select(-c("TO20T":"sci14")) %>%
  select(-Diamic) %>%
  VIM::kNN(k = 5)

knn_data_output <-
  knn_data %>%  
  select(-c(age_imp:death_cause_imp)) %>%
  mutate(samples_ID = paste0( "S_", row_number() ) ) %>%
  #column_to_rownames("samples_ID") %>% #pas obligatoirement
  mutate(Diamic = wrangling_clinical_data2$Diamic ) %>% #Identifiant histo
  mutate(cancer_recurrence_date = wrangling_clinical_data2$cancer_recurrence_date) %>%
  mutate(last_news_date = wrangling_clinical_data2$last_news_date) %>%
  mutate(cancer_history_details = wrangling_clinical_data2$cancer_history_details) %>%
  mutate(operation_date = wrangling_clinical_data2$operation_date) %>%
  mutate(second_stay = wrangling_clinical_data2$second_stay) %>%
  mutate(TO20T = wrangling_clinical_data2$TO20T) %>%
  mutate(MCT1T = wrangling_clinical_data2$MCT1T) %>%
  mutate(MCT4T = wrangling_clinical_data2$MCT4T) %>%
  mutate(sctne = wrangling_clinical_data2$sctne) %>%
  mutate(sct14 = wrangling_clinical_data2$sct14) %>%
  mutate(TO20I = wrangling_clinical_data2$TO20I) %>%
  mutate(MCT1I = wrangling_clinical_data2$MCT1I) %>%
  mutate(MCT4I = wrangling_clinical_data2$MCT4I) %>%
  mutate(scoim = wrangling_clinical_data2$scoim) %>%
  mutate(sci14 = wrangling_clinical_data2$sci14)

knn_data_output <-
  knn_data_output %>%
    select(c(Diamic, samples_ID,histology, age, sex, operation_date, second_stay, everything()))


# Uncode the dataframe ####
str(knn_data_output, list.len = ncol(knn_data_output) )

wrangling_clinical_data3 <-
  knn_data_output

#sex 
#wrangling_clinical_data3$sex <- 
# ifelse(test = knn_data_output$sex == 1, 
#   yes = "Male", 
#  no = "Female")
#Autre méthode

wrangling_clinical_data3$sex <- 
  factor(knn_data_output$sex)

levels(wrangling_clinical_data3$sex) <- 
  list(male ="1", female ="2")


# second_stay
wrangling_clinical_data3$second_stay <- 
  factor(knn_data_output$second_stay)

levels(wrangling_clinical_data3$second_stay) <- 
  list(second_stay ="1", no_second_stay ="2")

#Smoking
wrangling_clinical_data3$smoking <- 
  factor(knn_data_output$smoking)

levels(wrangling_clinical_data3$smoking) <- 
  list(smoker = "1", ancient_smoker = "3", non_smoker = "2")

# weaning (sevrage)
wrangling_clinical_data3$weaning <- 
  factor(knn_data_output$weaning)

levels(wrangling_clinical_data3$weaning) <- 
  list(weaned = "1", weaned_under3months = "2", not_weaned = "3", non_smoker = "4")

# Ischemic heart disease history 
wrangling_clinical_data3$ischemic_heart_disease_history <- 
  factor(knn_data_output$ischemic_heart_disease_history)

levels(wrangling_clinical_data3$ischemic_heart_disease_history) <- 
  list(ischemic_heart_disease_history = "1", 
       no_ischemic_heart_disease_history = "2")

# COPD history
wrangling_clinical_data3$COPD_history <- 
  factor(knn_data_output$COPD_history)

levels(wrangling_clinical_data3$COPD_history) <- 
  list(COPD_history = "1", 
       no_COPD_history = "2")
  
# Cancer history
wrangling_clinical_data3$cancer_history <- 
  factor(knn_data_output$cancer_history)

levels(wrangling_clinical_data3$cancer_history) <- 
  list(cancer_history = "1", 
       no_cancer_history = "2")

# Immunosuppression
wrangling_clinical_data3$immunosuppression_history <- 
  factor(knn_data_output$immunosuppression_history)

levels(wrangling_clinical_data3$immunosuppression_history) <- 
  list(HIV = "1", corticoids = "2", 
       immunosuppressants = "3",  other = "4", 
       no_immunosuppression = "5")


# Symptoms
wrangling_clinical_data3$symptoms <- 
  factor(knn_data_output$symptoms)

levels(wrangling_clinical_data3$symptoms) <- 
  list(symptoms = "1", 
       no_symptom = "2")

# Dyspnea 
# A FAIRE QUAND J AURAI LA REPONSE DE MATHILDE SUR LE MRCS

# TDM
wrangling_clinical_data3$TDM <- 
  factor(knn_data_output$TDM)

levels(wrangling_clinical_data3$TDM) <- 
  list(nodule = "1", mass = "2", 
       condensation = "3", 
       infiltrate = "4", 
       ground_glass_opacity = "5",
       ground_glass_opacity_with_condensation = "6")

# Atelectasia 
wrangling_clinical_data3$atelectasia <- 
  factor(knn_data_output$atelectasia)

levels(wrangling_clinical_data3$atelectasia) <- 
  list(segmental_atelectasia = "1", lobar_atelectasia = "2", 
       #pulmonary_atelectasia = "3", 
       no_atelectasia = "4")


# main_lesion_topography
wrangling_clinical_data3$main_lesion_topography <- 
  factor(knn_data_output$main_lesion_topography)

levels(wrangling_clinical_data3$main_lesion_topography) <- 
  list(upper_lobe = "1", middle_lobe = "2", 
       lower_lobe = "3",  lung_massing = "4")

#  main_lesion_side
wrangling_clinical_data3$main_lesion_side <-
  ifelse(knn_data_output$main_lesion_side == 1,
       yes = "right",
       no = "left")

# main_lesion_location
wrangling_clinical_data3$main_lesion_location <-
  ifelse(knn_data_output$main_lesion_location == 1,
         yes = "central",
         no = "peripheral")

# contact_adjacent_lobe	
wrangling_clinical_data3$contact_adjacent_lobe <- 
  factor(knn_data_output$contact_adjacent_lobe)

levels(wrangling_clinical_data3$contact_adjacent_lobe) <- 
  list(contact_adjacent_lobe = "1", 
       contact_adjacent_lobe_obvious_infiltration = "2", 
       no_contact_adjacent_lobe = "3")

# adjacent_structure_infiltration	
wrangling_clinical_data3$adjacent_structure_infiltration <- 
  factor(knn_data_output$adjacent_structure_infiltration)

levels(wrangling_clinical_data3$adjacent_structure_infiltration) <- 
  list(adjacent_structure_infiltration = "1", 
       no_adjacent_structure_infiltration = "2")

# chest_wall_contact	
wrangling_clinical_data3$chest_wall_contact <- 
  factor(knn_data_output$chest_wall_contact)

levels(wrangling_clinical_data3$chest_wall_contact) <- 
  list(chest_wall_contact = "1", 
       no_chest_wall_contact = "2")

# rachis_contact
wrangling_clinical_data3$rachis_contact <- 
  factor(knn_data_output$rachis_contact)

levels(wrangling_clinical_data3$rachis_contact) <- 
  list(rachis_contact = "1", 
       no_rachis_contact = "2")

# mediastinal_contact
wrangling_clinical_data3$mediastinal_contact <- 
  factor(knn_data_output$mediastinal_contact)

levels(wrangling_clinical_data3$mediastinal_contact) <- 
  list(mediastinal_contact = "1", 
       no_mediastinal_contact = "2")

# mediastinal_contact_details
wrangling_clinical_data3$mediastinal_contact_details <- 
  factor(knn_data_output$mediastinal_contact_details)

levels(wrangling_clinical_data3$mediastinal_contact_details) <- 
  list( SVC = "1", 
       tracheal_carina = "2",  pulmonary_artery = "3",
       left_auricle = "4", other_mediastinal_contact = "5",
       no_mediastinal_contact = "0")

# intrapleural_adenopathies 
wrangling_clinical_data3$intrapleural_adenopathies <- 
  factor(knn_data_output$intrapleural_adenopathies)

levels(wrangling_clinical_data3$intrapleural_adenopathies) <- 
  list(hilar_intrapleural_adenopathies = "1", 
       scissural_intrapleural_adenopathies = "2", 
       no_intrapleural_adenopathies = "3")

# mediastinal_adenopathies
wrangling_clinical_data3$mediastinal_adenopathies <- 
  factor(knn_data_output$mediastinal_adenopathies)

levels(wrangling_clinical_data3$mediastinal_adenopathies) <- 
  list(mediastinal_adenopathies = "1", 
       no_mediastinal_adenopathies = "2")

# extrathoracic_extension
wrangling_clinical_data3$extrathoracic_extension <- 
  factor(knn_data_output$extrathoracic_extension)

levels(wrangling_clinical_data3$extrathoracic_extension) <- 
  list(no_extrathoracic_extension = "1", 
       extrathoracic_extension = "2", 
       suspicious_extrathoracic_extension = "3")

levels(wrangling_clinical_data3$extrathoracic_extension) <- 
  list(extrathoracic_extension = "1",
       suspicious_extrathoracic_extension = "2",
       no_extrathoracic_extension = "3")

# extrathoracic_metastasis
wrangling_clinical_data3$extrathoracic_metastasis <- 
  factor(knn_data_output$extrathoracic_metastasis)

levels(wrangling_clinical_data3$extrathoracic_metastasis) <- 
  list(#adrenal_extrathoracic_metastasis = "2", 
       liver_extrathoracic_metastasis = "3", 
       bone_extrathoracic_metastasis = "4", 
       other_extrathoracic_metastasis = "5",
       no_extrathoracic_metastasis = "1")

# bronchial_fibroscopy
wrangling_clinical_data3$bronchial_fibroscopy <- 
  factor(knn_data_output$bronchial_fibroscopy)

levels(wrangling_clinical_data3$bronchial_fibroscopy) <- 
  list(no_extrathoracic_metastasis = "1", 
       adrenal_extrathoracic_metastasis = "2", 
       liver_extrathoracic_metastasis = "3", 
       bone_extrathoracic_metastasis = "4")


# main_lesion_PET_fixation
wrangling_clinical_data3$main_lesion_PET_fixation <- 
  factor(knn_data_output$main_lesion_PET_fixation)

levels(wrangling_clinical_data3$main_lesion_PET_fixation) <- 
  list(hyperfixing_PET_fixation = "1", 
       no_hyperfixing__PET_fixation = "2")

# total_number_lung_fixations
wrangling_clinical_data3$total_number_lung_fixations <- 
  factor(knn_data_output$total_number_lung_fixations)

levels(wrangling_clinical_data3$total_number_lung_fixations) <- 
  list("0" = "0", "1"  = "1",
       "2" = "2", ">2" = "3")

# intrapleural_lymphnode_fixation
wrangling_clinical_data3$intrapleural_lymphnode_fixation <- 
  factor(knn_data_output$intrapleural_lymphnode_fixation)

levels(wrangling_clinical_data3$intrapleural_lymphnode_fixation) <- 
  list(intrapleural_lymphnode_fixation = "1", 
       no_intrapleural_lymphnode_fixation  = "2")

# mediastinal_lymphnode_fixation
wrangling_clinical_data3$mediastinal_lymphnode_fixation <- 
  factor(knn_data_output$mediastinal_lymphnode_fixation)

levels(wrangling_clinical_data3$mediastinal_lymphnode_fixation) <- 
  list(mediastinal_lymphnode_fixation = "1", 
       no_mediastinal_lymphnode_fixation = "2")


# extrathoracic_PET_fixation
wrangling_clinical_data3$extrathoracic_PET_fixation <- 
  factor(knn_data_output$extrathoracic_PET_fixation)

levels(wrangling_clinical_data3$extrathoracic_PET_fixation) <- 
  list(adrenal_extrathoracic_PET_fixation  = "2",
       #liver_extrathoracic_PET_fixation = "3", 
       bone_extrathoracic_PET_fixation = "4",
       other_extrathoracic_PET_fixation = "5",
       no_extrathoracic_PET_fixation = "1")

# second_extrathoracic_PET_fixation
wrangling_clinical_data3$second_extrathoracic_PET_fixation <- 
  factor(knn_data_output$second_extrathoracic_PET_fixation)

levels(wrangling_clinical_data3$second_extrathoracic_PET_fixation) <- 
  list(adrenal_extrathoracic_PET_fixation  = "2",
       #liver_extrathoracic_PET_fixation = "3", 
       #bone_extrathoracic_PET_fixation = "4",
       other_extrathoracic_PET_fixation = "5",
       no_extrathoracic_PET_fixation = "1")

# initial_treatment_decision                                                                                     no = NA)))))))))
wrangling_clinical_data3$initial_treatment_decision <- 
  factor(knn_data_output$initial_treatment_decision)

levels(wrangling_clinical_data3$initial_treatment_decision) <- 
  list(primary_surgery = "1", 
       induction_chemotherapy  = "2",
       #exclusive_chemotherapy = "3", 
       #exclusive_radiotherapy = "4",
       #induction_radiochemotherapy = "5",
       exclusive_radiochemotherapy = "6"#,
       #radiofrequency = "7",
       #targeted_therapy = "8",
       #exclusive_supportive_care = "9"
       )

# surgery_indication	
wrangling_clinical_data3$surgery_indication <- 
  factor(knn_data_output$surgery_indication)

levels(wrangling_clinical_data3$surgery_indication) <- 
  list(#staging_surgery = "1", 
       #etiological_diagnosis_surgery  = "2",
       #etiological_diagnosis_staging_surgery = "3", 
       etiological_diagnosis_excision_surgery = "4",
       excision_surgery = "5")

# emergency
wrangling_clinical_data3$emergency <- 
  factor(knn_data_output$emergency)

levels(wrangling_clinical_data3$emergency) <- 
  list(#emergency = "1", 
       no_emergency  = "2")

# surgical_contamination_class
wrangling_clinical_data3$surgical_contamination_class <- 
  factor(knn_data_output$surgical_contamination_class)

levels(wrangling_clinical_data3$surgical_contamination_class) <- 
  list(clean_contaminated = "2")

# surgery_approach
wrangling_clinical_data3$surgery_approach <- 
  factor(knn_data_output$surgery_approach)

levels(wrangling_clinical_data3$surgery_approach) <- 
  list(thoracotomy = "1", 
       VATS  = "2"#,
       #VATS_robot = "3"
       )

# iterative_surgery
wrangling_clinical_data3$iterative_surgery <- 
  factor(knn_data_output$iterative_surgery)

levels(wrangling_clinical_data3$iterative_surgery) <- 
  list(no_iterative_surgery = "1", 
       iterative_surgery  = "2")

# surgery_performed
wrangling_clinical_data3$surgery_performed <- 
  factor(knn_data_output$surgery_performed)

levels(wrangling_clinical_data3$surgery_performed) <- 
  list(#exploratory_surgery_carcinosis = "1", 
       #exploratory_surgery_unresectable_disease  = "2",
       lobectomy = "3", 
       bilobectomy = "4",
       pneumonectomy = "5",
       wedge = "6",
       segmentectomy = "7")

# surgery_performed_extent
wrangling_clinical_data3$surgery_performed_extent <- 
  factor(knn_data_output$surgery_performed_extent)

levels(wrangling_clinical_data3$surgery_performed_extent) <- 
  list(pneumonectomy = "1", 
       no_pneumonectomy  = "2")

# lung_enlargement
wrangling_clinical_data3$lung_enlargement <- 
  factor(knn_data_output$lung_enlargement)

levels(wrangling_clinical_data3$lung_enlargement) <- 
  list(parenchyma_adjacent_lobe = "1", 
       bronchial  = "2",
       #arterial = "3", 
       #bronchovascular = "4",
       wedge_or_segmentectomy_adjacent_lobe = "5",
       no_lung_enlargement = "6")

# parietal_structure_enlargement
wrangling_clinical_data3$parietal_structure_enlargement <- 
  factor(knn_data_output$parietal_structure_enlargement)

levels(wrangling_clinical_data3$parietal_structure_enlargement) <- 
  list(parietal_pleura = "1", 
       costal_wall  = "2",
       vertebral_body = "3", 
       #diaphragm = "4",
       no_parietal_structure_enlargement = "5")

# mediastinal_structure_enlargement
wrangling_clinical_data3$mediastinal_structure_enlargement <- 
  factor(knn_data_output$mediastinal_structure_enlargement)

levels(wrangling_clinical_data3$mediastinal_structure_enlargement) <- 
  list(#tracheal_carina = "1", 
       #intrapericardial_vessel  = "2",
       #SVC = "3", 
       left_auricle = "4",
       #esophagus = "5",
       nerve = "6",
       other_mediastinal_structure_enlargement = "7",
       no_mediastinal_structure_enlargement= "8")

# lymphnode_dissection
wrangling_clinical_data3$lymphnode_dissection <- 
  factor(knn_data_output$lymphnode_dissection)

levels(wrangling_clinical_data3$lymphnode_dissection) <- 
  list(radical_lymphnode_dissection = "1", 
       sampling_lymphnode_dissection  = "2",
       no_lymphnode_dissection = "3")

# intervention_of_necessity
wrangling_clinical_data3$intervention_of_necessity <- 
  factor(knn_data_output$intervention_of_necessity)

levels(wrangling_clinical_data3$intervention_of_necessity) <- 
  list(#cleanliness_intervention_of_necessity = "1", 
       bleeding_intervention_of_necessity  = "2",
       no_intervention_of_necessity = "3")

# postoperative_death	
wrangling_clinical_data3$postoperative_death <- 
  factor(knn_data_output$postoperative_death)

levels(wrangling_clinical_data3$postoperative_death) <- 
  list(postop_death = "1", 
       postop_alive  = "2")

# post_operational
wrangling_clinical_data3$post_operational <- 
  factor(knn_data_output$post_operational )

levels(wrangling_clinical_data3$post_operational) <- 
  list(simple_complication ="1", 
       minor_complications ="2", 
       #major_complications_bleeding = "3", 
       #major_complications_fistula = "4",
       major_complications_pneumonia = "5", 
       major_complications_respiratory_failure = "6",
       major_complications_multiorgan_failure = "7", major_complications_other = "8")

# sample_origin
wrangling_clinical_data3$sample_origin <- 
  factor(knn_data_output$sample_origin )

levels(wrangling_clinical_data3$sample_origin) <- 
  list(excised_tissues ="1", 
       biopsy ="2")

# malignancy
wrangling_clinical_data3$malignancy <- 
  factor(knn_data_output$malignancy )

levels(wrangling_clinical_data3$malignancy) <- 
  list(#benign ="1", 
       malignant ="2")


# adenocarcinome_subtypes
wrangling_clinical_data3$adenocarcinome_subtypes <- 
  factor(knn_data_output$adenocarcinome_subtypes )

levels(wrangling_clinical_data3$adenocarcinome_subtypes) <- 
  list(low_grade ="1", intermediate_grade ="2",
       high_grade = "3", other_histology_type = "4")

# TTF1
wrangling_clinical_data3$TTF1 <- 
  factor(knn_data_output$TTF1 )

levels(wrangling_clinical_data3$TTF1) <- 
  list(TTF1_pos ="1", TTF1_neg ="2")

#pT
wrangling_clinical_data3$pT <- 
  factor(knn_data_output$pT )

levels(wrangling_clinical_data3$pT) <- 
  list(pT1A ="1", pT1B ="2",
       pT1C = "3", pT2A = "4",
       pT2B = "5", pT3 = "6",
       pT4 = "7", #pT0 = "8",
       #pTX = "9", 
       pTis = "10")

# p_pleura
wrangling_clinical_data3$p_pleura <- 
  factor(knn_data_output$p_pleura )

levels(wrangling_clinical_data3$p_pleura) <- 
  list(pl0 ="1", pl1 ="2",
       pl2 = "3", pl3 = "4")

# second_lesion	
wrangling_clinical_data3$second_lesion <- 
  factor(knn_data_output$second_lesion )

levels(wrangling_clinical_data3$second_lesion) <- 
  list( yes_synchrone ="2", no ="1" #,
       #yes_metachrone = "3"
       )

# pT3_details
wrangling_clinical_data3$pT3_details <- 
  factor(knn_data_output$pT3_details)

levels(wrangling_clinical_data3$pT3_details) <- 
  list( pt3_size ="1", 
        pt3_multiple_nodules_in_the_same_lobe = "2",
        pt3_wall ="3",
        #pt3_phrenic_nerve = "4",
        #pt3_parietal_pericardium = "5", 
        not_pT3 = "0")


# pT4_details
wrangling_clinical_data3$pT4_details <- 
  factor(knn_data_output$pT4_details)

levels(wrangling_clinical_data3$pT4_details) <- 
  list(pt4_size ="1", 
      pt4_multiple_nodules_in_another_ipsilateral_lobe = "2",
      #pt4_diaphragm ="3",
      pt4_mediastinum = "4",
      pt4_heart = "5", 
      #pt4_great_vessels = "6",
      #pt4_trachea = "7",
      #pt4_carina = "8",
      #pt4_recurrent_nerve = "9",
      #pt4_esophagus = "10",
      #pt4_vertebra = "11",
        not_pT4 = "0")

# embolism
wrangling_clinical_data3$embolism <- 
  factor(knn_data_output$embolism)

levels(wrangling_clinical_data3$embolism) <- 
  list( vascular_embolism ="1", 
        perineural_embolism  = "2",
        vascular_and_perineural_embolism ="3",
        no_embolism = "4")

# stroma_lymphocyte_population
wrangling_clinical_data3$stroma_lymphocyte_population <- 
  factor(knn_data_output$stroma_lymphocyte_population)

levels(wrangling_clinical_data3$stroma_lymphocyte_population) <- 
  list( rich_lymphocyte_stroma ="1", 
        not_rich_lymphocyte_stroma  = "2")

# pN
wrangling_clinical_data3$pN <- 
  factor(knn_data_output$pN)

levels(wrangling_clinical_data3$pN) <- 
  list(pN0 ="1", pN1 ="2",
       pN2 = "3", pN2 = "4")

# pN2_details_site
wrangling_clinical_data3$pN2_details_site <- 
  factor(knn_data_output$pN2_details_site)

levels(wrangling_clinical_data3$pN2_details_site) <- 
  list(pN2_unisite ="1", pN2_multisite ="2",
       not_pN2 = "0")

# pN2_details_location
wrangling_clinical_data3$pN2_details_location <- 
  factor(knn_data_output$pN2_details_location)

levels(wrangling_clinical_data3$pN2_details_location) <- 
  list(pN2_intrasinus ="1", pN2_massive ="2",
        pN2_extracapsular = "3", not_pN2 = "0")

# status_last_news
wrangling_clinical_data3$status_last_news <- 
  factor(knn_data_output$status_last_news)

levels(wrangling_clinical_data3$status_last_news) <- 
  list(alive_without_recurrence ="1", alive_with_recurrence ="2",
       alive_no_information = "3", deceased = "4")

# death_cause
wrangling_clinical_data3$death_cause <- 
  factor(knn_data_output$death_cause)

levels(wrangling_clinical_data3$death_cause) <- 
  list(cancer ="1", intercurrent ="2",
       unknown = "3", alive = "0")

# histology                                                                                                                         no = NA)))))))))))))
wrangling_clinical_data3$histology <- 
  factor(knn_data_output$histology )

levels(wrangling_clinical_data3$histology) <- 
  list(#small_cell_lungcancer ="1", 
       adenocarcinoma = "9", 
       squamous_cell_carcinoma = "6",
       adenosquamous_carcinoma = "11", 
       large_cell_undifferentiated_carcinoma = "8",
       large_cell_neuroendocrine_carcinoma ="2",
       non_small_cell_carcinoma = "10",
       #typical_carcinoid = "3", 
       #atypical_carcinoid = "4",
       sarcomatoid_carcinoma = "12",
       carcinoid = "5", 
       basaloid_squamous_cell_carcinoma = "7"#, 
       #metastasis_other_cancer = "13"
       )

# EGFR
wrangling_clinical_data3$EGFR <- 
  factor(knn_data_output$EGFR)

levels(wrangling_clinical_data3$EGFR) <- 
  list(EGFR_WT ="1", EGFR_activating_mutation ="2",
       EGFR_resistance_mutation = "3")

# KRAS
wrangling_clinical_data3$KRAS <- 
  factor(knn_data_output$KRAS)

levels(wrangling_clinical_data3$KRAS) <- 
  list(KRAS_WT ="1", KRAS_mutation ="2")


# ALK
wrangling_clinical_data3$ALK <- 
  factor(knn_data_output$ALK)

levels(wrangling_clinical_data3$ALK) <- 
  list(ALK_WT ="1", ALK_IHC_pos ="2",
       ALK_transfected_FISH = "3")

# HER2
wrangling_clinical_data3$HER2 <- 
  factor(knn_data_output$HER2)

levels(wrangling_clinical_data3$HER2) <- 
  list(HER2_WT ="1", HER2_mutation ="2")

# BRAF
wrangling_clinical_data3$BRAF <- 
  factor(knn_data_output$BRAF)

levels(wrangling_clinical_data3$BRAF) <- 
  list(BRAF_WT ="1", BRAF_mutation ="2")

# ROS
wrangling_clinical_data3$ROS <- 
  factor(knn_data_output$ROS)

levels(wrangling_clinical_data3$ROS) <- 
  list(ROS_WT ="1", ROS_mutation ="2")

# PDL1
wrangling_clinical_data3$PDL1 <- 
  factor(knn_data_output$PDL1)

levels(wrangling_clinical_data3$PDL1) <- 
  list(PDL1_neg ="1", PDL1_pos ="2")

# p40
wrangling_clinical_data3$p40 <- 
  factor(knn_data_output$p40)

levels(wrangling_clinical_data3$p40) <- 
  list(p40_neg ="1", p40_pos ="2")

# cancer_history_details 
wrangling_clinical_data3$cancer_history_details <-
  knn_data_output$cancer_history_details %>%
  str_remove(" ") %>%
  str_remove(" ") %>%
  str_remove(" ") %>%
  str_replace(",", "_and_") %>%
  str_replace("et", "_and_") %>%
  str_replace("1", "lung_cancer_history") %>%
  str_replace("2", "ENT_cancer_history") %>%
  str_replace("3", "digestive_cancer_history") %>%
  str_replace("4", "kidney_cancer_history") %>%
  str_replace("5", "breast_cancer_history") %>%
  str_replace("6", "prostate_cancer_history") %>%
  str_replace("7", "haemopathy_cancer_history") %>%
  str_replace("8", "other_cancer_history") %>%
  str_replace("9", "no_cancer_history")

wrangling_clinical_data4 <-
  wrangling_clinical_data3

wrangling_clinical_data4 %>%
  writexl::write_xlsx("./Preprocessed_data/Database_recode_V1.xlsx")

# Table summary ####
wrangling_clinical_data4  %>%
  select(-Diamic) %>%
  select(-samples_ID) %>%
  select(-c(TO20T:sci14)) %>%
  tbl_summary() 

#by phenotype
wrangling_clinical_data4  %>%
  select(-Diamic) %>%
  select(-samples_ID) %>%
  select(-c(TO20T:sci14)) %>%
  tbl_summary(by = "histology") %>%
  add_p() %>%
  add_q() 

