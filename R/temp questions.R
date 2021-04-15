# How many patients had the last Neoadj cycle switch to Adjuvant?

a <- therapy %>% filter(!is.na(surgerydate) & chemotherapy_type == "Adjuvant" & linenumber == 1 & linenumber_new == 2 & therapy == "Upfront Chemo") %>% 
  distinct(patientid)

# Patients who switch between cisplatin to carbo

a <- therapy %>% filter((str_detect(linename, "Cis") & drugname == "carboplatin")) %>% distinct(patientid)

b <- therapy %>% filter((str_detect(linename, "Carbo") & drugname == "cisplatin")) %>% distinct(patientid)
