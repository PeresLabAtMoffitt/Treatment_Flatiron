Frontline <- read_rds("Frontline.rds")
analysis_data <- Frontline %>% 
  distinct(patientid, chemotherapy_type, drugname_type, .keep_all = TRUE) %>% 
  filter(exclude != "1" | thirty_days_exclusion == "Exclude")
#################################################################################################### I ### Clinical Mining
analysis_data %>% distinct(patientid, .keep_all = TRUE) %>% 
  filter(exclude != "1") %>% 
  select(c(race, raceeth, ethnicity, "state", "cens_division", "cens_region")) %>% 
  tbl_summary(by = raceeth) %>% bold_labels() %>% add_overall()

analysis_data %>% distinct(patientid, .keep_all = TRUE) %>% 
  filter(exclude != "1") %>% 
  select(c(race, ethnicity,
           "racecat", "ethcat", "raceeth", raceeth1)) %>% 
  tbl_summary(by = raceeth) %>% bold_labels() %>% add_p() %>% add_overall()

analysis_data %>% distinct(patientid, .keep_all = TRUE) %>% 
  filter(exclude != "1") %>% 
  select(c(race, raceeth, ethnicity, "vital",
           agecat, "ageatdx",
            
           "bmi", bmi_cat, "BSA",
           
           "histology", "groupstage", "tstage", "stagecat",
           
           "brca1_germline", "brca2_germline", "brca_germline", "brca1_tumor",
           "brca2_tumor", "brca_tumor", "brca1_unknown", "brca2_unknown", "brca_unknown", 
           
           "hrd_tissue", "gis_tissue")) %>% 
  tbl_summary(by = raceeth) %>% bold_labels() %>% add_overall() %>% add_p()


#################################################################################################### II ### Treatment Mining
# 1.	Determining frontline treatment regimen

analysis_data %>% distinct(patientid, .keep_all = TRUE) %>% 
  ggplot(aes(x= therapy))+
  geom_bar()+
  theme_minimal()+
  labs(title = "Frontline treatment")+
  coord_flip()

analysis_data %>% arrange(episodedate) %>% distinct(patientid, .keep_all = TRUE) %>% 
  group_by(chemotherapy_type) %>% 
  summarize(count = n()) %>% mutate(percent=(count/sum(count)*100)) %>%
  ggplot(aes(x= chemotherapy_type, y= percent, fill = chemotherapy_type))+
  geom_bar(stat="identity")+
  labs(x = "", title = "The first chemotherapy received by a patient Adjuvant for ~70% of the time") +
  theme_minimal()+
  guides(fill = FALSE) +
  coord_flip()

analysis_data %>% arrange(episodedate) %>% distinct(patientid, .keep_all = TRUE) %>% 
  group_by(treatment_sequence) %>% 
  summarize(count = n()) %>% mutate(percent=(count/sum(count)*100)) %>%
  ggplot(aes(x= treatment_sequence, y= percent, fill = treatment_sequence))+
  geom_bar(stat="identity")+
  labs(x = "", title = "The treatment_sequence") +
  theme_minimal()+
  guides(fill = FALSE) +
  coord_flip()

# analysis_data %>% distinct(patientid, .keep_all = TRUE) %>% 
#   ggplot(aes(x= chemo))+
#   geom_bar()+
#   theme_minimal()+
#   coord_flip()

# What are the RDI distribution for each type of chemo
summary(analysis_data$relative_dose_intensity)
analysis_data %>% filter(!is.na(relative_dose_intensity)) %>%
  ggplot(aes(x= chemotherapy_type, y= relative_dose_intensity))+
  geom_boxplot()+
  geom_jitter(shape=16)+
  labs(x = "", title = "Warning 1 dot = 1 RDI for 1 drugname per patient") +
  theme_minimal()

analysis_data %>% filter(!is.na(relative_dose_intensity)) %>%
  ggplot(aes(x= chemotherapy_type, y= relative_dose_intensity))+
  # geom_boxplot()+
  # ylim(c(-.5, 3.5))+
  geom_jitter(shape=16, aes(color=relative_dose_intensity<0.85))+
  labs(x = "", title = "Warning 1 dot = RDI for 1 drugname per patient") +
  theme_minimal() +
  guides(color=FALSE)+
  facet_wrap( ~ drugname_type)

analysis_data %>% filter(!is.na(relative_dose_intensity)) %>%
  ggplot(aes(x= chemotherapy_type, y= relative_dose_intensity))+
  geom_violin()+
  ylim(c(-.5, 3.5))+
  # geom_jitter(shape=16, aes(color=relative_dose_intensity<0.85))+
  theme_minimal()

# What are the RDI distribution for each drug
analysis_data %>% filter(!is.na(relative_dose_intensity)) %>%
  ggplot(aes(x= drugname, y= relative_dose_intensity))+
  geom_violin(aes(color=relative_dose_intensity<0.85))+
  ylim(c(-.5, 5.5))+
  # geom_jitter(shape=16, aes(color=relative_dose_intensity<0.85))+
  theme_minimal()+
  coord_flip()

analysis_data %>% filter(!is.na(relative_dose_intensity)) %>%
  ggplot(aes(x= RDI_grp, fill = drugname))+
  geom_bar()+
  theme_minimal()+
  coord_flip()

analysis_data %>% filter(!is.na(relative_dose_intensity)) %>%
  ggplot(aes(x= RDI_grp, fill = drugname))+
  geom_bar(position = "fill")+
  theme_minimal()+
  coord_flip()

# Treament summary table
tbl1 <- analysis_data %>% distinct(patientid, .keep_all = TRUE) %>% 
  select(c(raceeth, delay_incare_dx_to_treat, mean_skipped_cycle_indays,
           "issurgery", "extentofdebulking", "resdz", "debulking", "diff_surg_dx",
           "chemotherapy_type", "therapy", "relative_dose_intensity", "RDI_grp")) %>% 
  tbl_summary(by = raceeth) %>% bold_labels() %>% add_overall() %>% add_p()
tbl2 <- analysis_data %>% distinct(patientid, .keep_all = TRUE) %>% 
  select(c(raceeth, delay_incare_dx_to_treat, mean_skipped_cycle_indays,
           "issurgery", "extentofdebulking", "resdz", "debulking", "diff_surg_dx",
           "chemotherapy_type", "therapy", "relative_dose_intensity", "RDI_grp")) %>% 
  tbl_summary(by = raceeth, missing = "no") %>% bold_labels() %>% add_overall() %>% add_p()
tbl_merge(list(tbl1, tbl2), tab_spanner = c("with unknown", "without unknown"))

# Treatment & demo
tbl1 <- analysis_data %>% filter(drugname == "carboplatin" & chemotherapy_type == "Adjuvant") %>% 
  distinct(patientid, .keep_all = TRUE) %>% 
  select(c(raceeth,  "relative_dose_intensity", RDI_grp)) %>% 
  tbl_summary(by = raceeth,
              label = list(relative_dose_intensity ~ "carb/adjuvant \nRDI")) %>%
  bold_labels() %>% add_p()
tbl2 <- analysis_data %>% filter(drugname == "carboplatin" & chemotherapy_type == "Adjuvant") %>% 
  distinct(patientid, .keep_all = TRUE) %>% 
  select(c(bmi_cat,  "relative_dose_intensity", RDI_grp)) %>% 
  tbl_summary(by = bmi_cat,
              label = list(relative_dose_intensity ~ "carb/adjuvant \nRDI")) %>%
  bold_labels() %>% add_p()
tbl3 <- 
  tbl_merge(list(tbl1, tbl2), tab_spanner = c("Race/Ethnicity", "BMI"))

tbl1 <- analysis_data %>% filter(drugname == "carboplatin" & chemotherapy_type == "Neoadjuvant") %>% 
  distinct(patientid, .keep_all = TRUE) %>% 
  select(c(raceeth, "relative_dose_intensity", RDI_grp)) %>% 
  tbl_summary(by = raceeth,
              label = list(relative_dose_intensity ~ "carb/neoadjuvant \nRDI")) %>%
  bold_labels() %>% add_p()
tbl2 <- analysis_data %>% filter(drugname == "carboplatin" & chemotherapy_type == "Neoadjuvant") %>% 
  distinct(patientid, .keep_all = TRUE) %>% 
  select(c(bmi_cat, "relative_dose_intensity", RDI_grp)) %>% 
  tbl_summary(by = bmi_cat,
              label = list(relative_dose_intensity ~ "carb/neoadjuvant \nRDI")) %>%
  bold_labels() %>% add_p()
tbl4 <- 
  tbl_merge(list(tbl1, tbl2), tab_spanner = c("Race/Ethnicity", "BMI"))

tbl1 <- analysis_data %>% filter(str_detect(drugname, "taxel") & chemotherapy_type == "Adjuvant") %>% 
  distinct(patientid, .keep_all = TRUE) %>% 
  select(c(raceeth, "relative_dose_intensity", RDI_grp)) %>% 
  tbl_summary(by = raceeth,
              label = list(relative_dose_intensity ~ "taxel/adjuvant \nRDI")) %>%
  bold_labels() %>% add_p()
tbl2 <- analysis_data %>% filter(str_detect(drugname, "taxel") & chemotherapy_type == "Adjuvant") %>% 
  distinct(patientid, .keep_all = TRUE) %>% 
  select(c(bmi_cat, "relative_dose_intensity", RDI_grp)) %>% 
  tbl_summary(by = bmi_cat,
              label = list(relative_dose_intensity ~ "taxel/adjuvant \nRDI")) %>%
  bold_labels() %>% add_p()
tbl5 <- 
  tbl_merge(list(tbl1, tbl2), tab_spanner = c("Race/Ethnicity", "BMI"))

tbl1 <- analysis_data %>% filter(str_detect(drugname, "taxel") & chemotherapy_type == "Neoadjuvant") %>% 
  distinct(patientid, .keep_all = TRUE) %>% 
  select(c(raceeth, "relative_dose_intensity", RDI_grp)) %>% 
  tbl_summary(by = raceeth,
              label = list(relative_dose_intensity ~ "taxel/neoadjuvant \nRDI")) %>%
  bold_labels() %>% add_p()
tbl2 <- analysis_data %>% filter(str_detect(drugname, "taxel") & chemotherapy_type == "Neoadjuvant") %>% 
  distinct(patientid, .keep_all = TRUE) %>% 
  select(c(bmi_cat, "relative_dose_intensity", RDI_grp)) %>% 
  tbl_summary(by = bmi_cat,
              label = list(relative_dose_intensity ~ "taxel/neoadjuvant \nRDI")) %>%
  bold_labels() %>% add_p()
tbl6 <- 
  tbl_merge(list(tbl1, tbl2), tab_spanner = c("Race/Ethnicity", "BMI"))

tbl7 <- 
  tbl_stack(list(tbl3, tbl4, tbl5, tbl6))













#
summ <- therapy1 %>% group_by(patientid, linename, chemotherapy_type) %>% 
  count(linename, chemotherapy_type, drugname, name = "drug_repeat_per_line") %>% 
  group_by(patientid, linename, chemotherapy_type) %>% 
  summarise_at(vars(drugname, drug_repeat_per_line), paste, collapse = "-")

summ1 <- summ %>% 
  group_by(linename, drugname, chemotherapy_type) %>% 
  count(drugname, chemotherapy_type, drug_repeat_per_line, name = "nbr_of_patient") %>% 
  mutate(patient_received_drugname = sum(nbr_of_patient)) %>% arrange(desc(patient_received_drugname), desc(nbr_of_patient)) %>% 
  mutate(different_cycle = n())




# Create a map of more RDI pb