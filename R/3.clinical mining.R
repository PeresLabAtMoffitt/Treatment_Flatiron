#################################################################################################### I ### Treatment Mining
# 1.	Determining frontline treatment regimen

Treatment %>% distinct(patientid, .keep_all = TRUE) %>% 
  ggplot(aes(x= therapy))+
  geom_bar()+
  theme_minimal()+
  labs(title = "Frontline treatment")+
  coord_flip()

Treatment %>% arrange(episodedate) %>% distinct(patientid, .keep_all = TRUE) %>% 
  group_by(chemotherapy_type) %>% 
  summarize(count = n()) %>% mutate(percent=(count/sum(count)*100)) %>%
  ggplot(aes(x= chemotherapy_type, y= percent, fill = chemotherapy_type))+
  geom_bar(stat="identity")+
  labs(x = "", title = "The first chemotherapy received by a patient Adjuvant for ~70% of the time") +
  theme_minimal()+
  guides(fill = FALSE) +
  coord_flip()

# Treatment %>% distinct(patientid, .keep_all = TRUE) %>% 
#   ggplot(aes(x= chemo))+
#   geom_bar()+
#   theme_minimal()+
#   coord_flip()

# What are the RDI distribution for each type of chemo
summary(Frontline$relative_dose_intensity)
Frontline %>% filter(!is.na(relative_dose_intensity)) %>%
  ggplot(aes(x= chemotherapy_type, y= relative_dose_intensity))+
  geom_boxplot()+
  geom_jitter(shape=16)+
  theme_minimal()

Frontline %>% filter(!is.na(relative_dose_intensity)) %>%
  ggplot(aes(x= chemotherapy_type, y= relative_dose_intensity))+
  # geom_boxplot()+
  ylim(c(-.5, 5.5))+
  geom_jitter(shape=16, aes(color=relative_dose_intensity<0.85))+
  theme_minimal() +
  guides(color=FALSE)

Frontline %>% filter(!is.na(relative_dose_intensity)) %>%
  ggplot(aes(x= chemotherapy_type, y= relative_dose_intensity))+
  geom_violin()+
  ylim(c(-.5, 5.5))+
  # geom_jitter(shape=16, aes(color=relative_dose_intensity<0.85))+
  theme_minimal()

# What are the RDI distribution for each drug
Frontline %>% filter(!is.na(relative_dose_intensity)) %>%
  ggplot(aes(x= drugname, y= relative_dose_intensity))+
  geom_violin(aes(color=relative_dose_intensity<0.85))+
  ylim(c(-.5, 5.5))+
  # geom_jitter(shape=16, aes(color=relative_dose_intensity<0.85))+
  theme_minimal()+
  coord_flip()

Frontline %>% filter(!is.na(relative_dose_intensity)) %>%
  ggplot(aes(x= RDI_grp, fill = drugname))+
  geom_bar()+
  theme_minimal()+
  coord_flip()

Frontline %>% filter(!is.na(relative_dose_intensity)) %>%
  ggplot(aes(x= RDI_grp, fill = drugname))+
  geom_bar(position = "fill")+
  theme_minimal()+
  coord_flip()






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
