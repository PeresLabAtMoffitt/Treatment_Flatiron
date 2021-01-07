# Verify issurgery
drugs1 <- drugs1 %>% 
  mutate(surg = ifelse(is.na(surgerydate), "No/unknown", "Yes"))
uid <- paste0(unique(drugs1$patientid[(drugs1$issurgery != drugs1$surg)]), collapse = "|") # 6 patients who had surgery with no date
table(drugs1$route)
a <- drugs1 %>% filter(str_detect(route, "Intradermal|Oral|Other/Miscellaneous|Subcutaneous"))
# write_csv(a, "Treatment with route other than intravenous or intraperitoneal.csv")