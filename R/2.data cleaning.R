#################################################################################################### I ### Data Cleaning
creatinine <- creatinine %>% 
  filter(str_detect(labcomponent, "blood|serum")) %>% 
  mutate(creatinine = case_when(
    testresultcleaned > 0.8     ~ 0.8,
    TRUE                       ~ testresultcleaned
  )) %>% 
  mutate(testdate = as.Date(testdate, format = "%m/%d/%y")) %>% 
  select(c("patientid", cr_date= "testdate", "creatinine", unit_creat = "testunitscleaned"))

# body_surface_area, height and weight are all comimg from vitals
vitals <- vitals %>% 
  mutate(testdate = as.Date(testdate, format = "%m/%d/%y"))

body_surface_area <- vitals %>% 
  filter(labcomponent == "Body Surface Area (BSA)") %>% 
  group_by(patientid) %>%
  mutate(median_testresult = median(testresult)) %>% 
  ungroup() %>% 
  mutate(bsa = case_when(
    testresult > (median_testresult+0.4) |
      testresult < (median_testresult-0.4)      ~ NA_real_,
    TRUE                                        ~ testresult
  )) %>% 
  mutate(BSA = coalesce(testresultcleaned, bsa)) %>% 
  select(c("patientid", BSA_date = "testdate", BSA)) %>% # , testresultcleaned, testresult, median_testresult, bsa
  mutate(BSA_units = "m2")


height <- vitals %>% 
  filter(test == "body height") %>% 
  # To get more data, have data from testunits that can be used. Fill up testunits, Transfer in kg and coalesce.
  group_by(patientid) %>% 
  fill(testunits, .direction = "downup") %>% # F004D683A1695 have testresult, F615C37BF1470 is wrong
  ungroup() %>% 
  # Clean up outliers
  mutate(testresultcleaned = case_when(
    testresultcleaned > 231 | # Tallest women height
      testresultcleaned < 100 # Average height dwarf = 122 cm, 4 feet
                                     ~ NA_real_,
    TRUE                             ~ testresultcleaned
  )) %>% 
  mutate(testresult = case_when(
    testresult > 231 & testunits == "cm" |
      testresult < 100 & testunits == "cm" |
      testresult > 7.58 & testunits == "ft" |
      testresult < 3.28 & testunits == "ft" |
      testresult > 90.94 & testunits == "in" |
      testresult < 39.37 & testunits == "in"|
      testresult == 0
                                     ~ NA_real_,
    TRUE                             ~ testresult
  )) %>% 
  # Clean up outliers for the same patient. Calculate median for each patients/unit. Eliminate the ones with too much variation.
  group_by(patientid) %>%
  mutate(median_testresultcleaned = median(testresultcleaned)) %>% 
  ungroup() %>% 
  mutate(testresultcleaned_verified = case_when(
    testresultcleaned > (median_testresultcleaned + 5) | # choose 5 cm by default
      testresultcleaned < (median_testresultcleaned - 5)      ~ NA_real_, # F01413C06D921 weird
    TRUE                                                      ~ testresultcleaned
  )) %>% 
  # select(c("patientid", testdate, testresultcleaned, median_testresultcleaned, testresultcleaned_verified, 
  #          testunitscleaned, testunits, testresult)) %>% 
  
  group_by(patientid, testunits) %>%
  mutate(median_testresult = median(testresult)) %>% 
  ungroup() %>% 
  mutate(testresult_verified = case_when(
    testresult > (median_testresult + 3) & 
      testunits == "in" |
      testresult < (median_testresult - 3) & 
      testunits == "in"                         ~ NA_real_,
    testresult > (median_testresult + 5) & 
      testunits == "cm" |
      testresult < (median_testresult - 5) & 
      testunits == "cm"                         ~ NA_real_,
    TRUE                                        ~ testresult
  )) %>% 
  # select(c("patientid", testdate, testresult, median_testresult, testresult_verified,
  #          testunits, testunitscleaned, testresultcleaned, testresultcleaned_verified))
  
  mutate(height = case_when(
    !is.na(testresultcleaned_verified)          ~ round(testresultcleaned_verified/100, 3), # cm to m
    testunits == "cm"                           ~ round(testresult_verified/100, 3),
    testunits == "ft"                           ~ round(testresult_verified/3.281, 3), # ft to m
    testunits == "in"| 
      !is.na(testresult)                        ~ round(testresult_verified/39.37, 3) # in to m
    )) %>% 
  select(c("patientid", height_date = "testdate", "height")) %>% 
  mutate(height_units = "m") %>% 
  mutate(year = year(height_date))# %>%
  # arrange(height_date)# %>%
  # distinct(patientid, year, .keep_all = TRUE)


weight <- vitals %>%
  filter(test == "body weight")  %>% 
  # To get more data, have data from testunits that can be used. Fill up testunits, Transfer in kg and coalesce.
  group_by(patientid) %>% 
  fill(testunits, .direction = "downup") %>% # F004D683A1695 F020C6A8B9E50
  ungroup() %>% 
  mutate(kg = case_when(
    testunits == "lb"     ~ testresult/2.205, # Biggest 291.6 kg (643 lbs)
    testunits == "oz"     ~ testresult/32.274,
    TRUE                  ~ testresult
  )) %>%
  mutate(testresultcleaned = coalesce(testresultcleaned, kg)) %>% 
  select(c("patientid", weight_date = "testdate", weight = "testresultcleaned"))

############ BMI

# Idea to bind weight and height by year to calculate BMI. ...But better if it's close to drug administration
# %>% 
#   full_join(., Vitals_height, by = c("patientid", "year"))
# mutate(BMI = )

# Vital <- dcast(setDT(Vitals), patientid+testdate ~ rowid(patientid),  value.var = c("test", "testunits", "testresult"))

