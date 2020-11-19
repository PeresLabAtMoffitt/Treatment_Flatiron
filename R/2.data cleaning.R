#################################################################################################### I ### Data Cleaning
creatinine <- creatinine %>% 
  filter(str_detect(labcomponent, "blood|serum")) %>% 
  mutate(creatinine = case_when(
    testresultcleaned > 0.8     ~ 0.8,
    TRUE                       ~ testresultcleaned
  )) %>% 
  mutate(testdate = as.Date(testdate, format = "%m/%d/%y")) %>% 
  select(c("patientid", cr_date= "testdate", "creatinine", unit_creat = "testunitscleaned"))

body_surface_area <- vitals %>% 
  filter(labcomponent == "Body Surface Area (BSA)") %>% 
  mutate(testdate = as.Date(testdate, format = "%m/%d/%y")) %>% 
  select(c("patientid", BSA_date = "testdate", BSA = "testresult")) %>% 
  mutate(BSA_units = "m2")

vitals <- vitals %>% 
  mutate(testdate = as.Date(testdate, format = "%m/%d/%y"))

height <- vitals %>% 
  filter(test == "body height") %>% 
  mutate(height = case_when(
    !is.na(testresultcleaned)               ~ round(testresultcleaned/100, 3),
    testresult %in% c(1:7)                  ~ round(testresult/3.281, 3),
    !is.na(testresult)                      ~ round(testresult/39.37, 3)
    )) %>% 
  select(c("patientid", height_date = "testdate", "height")) %>% 
  mutate(height_units = "m") %>% 
  mutate(year = year(height_date))# %>%
  # arrange(height_date)# %>%
  # distinct(patientid, year, .keep_all = TRUE)

weight <- vitals %>% 
  filter(test == "body weight") %>% 
  select(c("patientid", weight = "testresultcleaned")) %>% 
  mutate(weight_units = "kg")# %>% 
  
  # mutate(year = year(testdate)) %>% 
  # arrange(testdate) %>% 
  # distinct(patientid, year, .keep_all = TRUE) 

weight1 <- vitals %>% 
  filter(test == "body weight") %>% 
  select(c("patientid", "testdate", "testresult")) %>% 
  filter(is.na(testresult))
uid <- paste(unique(weight1$patientid), collapse = '|')

weight2 <- vitals %>% 
  filter(test == "body weight") %>% 
  select(c("patientid", "testdate", "testresult")) %>% 
  filter(!is.na(testresult))
weight3 <- weight2[(grepl(uid, weight2$patientid)),]




############ BMI


%>% 
  full_join(., Vitals_height, by = c("patientid", "year"))
mutate(BMI = )

# Vital <- dcast(setDT(Vitals), patientid+testdate ~ rowid(patientid),  value.var = c("test", "testunits", "testresult"))

