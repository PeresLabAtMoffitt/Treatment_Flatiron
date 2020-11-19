#################################################################################################### I ### Data Cleaning
creatinine <- creatinine %>% 
  filter(str_detect(labcomponent, "blood|serum")) %>% 
  mutate(creatinine = case_when(
    testresultcleaned > 0.8     ~ 0.8,
    TRUE                       ~ testresultcleaned
  )) %>% 
  select(c("patientid", cr_date= "testdate", "creatinine", unit_creat = "testunitscleaned"))

body_surface_area <- vitals %>% 
  filter(labcomponent == "Body Surface Area (BSA)") %>% 
  select(c("patientid", BSA_date = "testdate", BSA = "testresult")) %>% 
  mutate(BSA_units = "m2")

height <- vitals %>% 
  filter(test == "body height") %>% 
  select(c("patientid", "testdate", "testunits", "testresult")) %>% 
  mutate(height = case_when(
    testunits == "cm"     ~ testresult/2.54,
    testunits == "ft"     ~ testresult*12,
    testunits == "ft"     ~ testresult*12
    )) %>% 
  mutate(year = year(testdate)) %>% 
  arrange(testdate) %>% 
  distinct(patientid, year, .keep_all = TRUE)









############ BMI


Vitals <- Vitals %>% 
  filter(test == "body weight") %>% 
  select(c("patientid", "testdate", "testunits", "testresult")) %>% 
  mutate(weight = case_when(
    testunits == "kg"     ~ testresult*2.205,
    testunits == "oz"     ~ testresult/16,
    TRUE                  ~ testresult
  )) %>% 
  mutate(year = year(testdate)) %>% 
  arrange(testdate) %>% 
  distinct(patientid, year, .keep_all = TRUE) %>% 
  full_join(., Vitals_height, by = c("patientid", "year"))
mutate(BMI = )

# Vital <- dcast(setDT(Vitals), patientid+testdate ~ rowid(patientid),  value.var = c("test", "testunits", "testresult"))


table(Vitals_height$testunits)