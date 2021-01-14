#################################################################################################### I ### Treatment Mining
# 1.	Determining frontline treatment regimen

Treatment %>% distinct(patientid, .keep_all = TRUE) %>% 
  ggplot(aes(x= therapy))+
  geom_bar()+
  theme_minimal()+
  labs(title = "Frontline treatment")+
  coord_flip()

Treatment %>% distinct(patientid, .keep_all = TRUE) %>% 
  ggplot(aes(x= chemotherapy_type))+
  geom_bar()+
  theme_minimal()+
  labs(title = "Chemotherapy type")+
  coord_flip()

Treatment %>% distinct(patientid, .keep_all = TRUE) %>% 
  ggplot(aes(x= chemo))+
  geom_bar()+
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