#################################################################################################### I ### Basic Clinical Cleaning
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
  filter(test == "body weight") %>% 
  select(c("patientid", "testdate", "testunits", "testresult", "testunitscleaned", "testresultcleaned")) %>%
  # 1.Clean testresultcleaned
  mutate(weight = case_when(
    testresultcleaned >= 27 &                          # Biggest recorded 291.6 kg (642.9 lbs, 10286 oz)
      testresultcleaned < 292     ~ testresultcleaned, # lightest 5.5 kg (12 lbs, 194 oz)  F0027D3926C88
    TRUE ~ NA_real_
  )) %>% 
  # 2.Clean testresult to be used to find max and min
  mutate(testresult = case_when(
    testunits == "lb" &
      testresult > 12 &
      testresult < 643            ~ testresult,
    testunits == "kg" &
      testresult >= 5.5 &
      testresult < 292            ~ testresult,
    testunits == "oz" &
      testresult > 194 &
      testresult < 10286          ~ testresult,
    is.na(testunits) & 
    testresult > 5.5 & 
      testresult < 10286          ~ testresult, # combine oz, kg, lbs
    TRUE                          ~ NA_real_
  ))

# remove_outliers <- function(x, na.rm = TRUE, ...) {
#   qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
#   H <- 1 * IQR(x, na.rm = na.rm) # 1.5 remove non outliers
#   y <- x
#   y[x < (qnt[1] - H)] <- NA
#   y[x > (qnt[2] + H)] <- NA
#   y
# }

weight1 <- weight %>% 
  # 3. Clean outliers
  group_by(patientid, testunits) %>%
  mutate(median_testresult = median(testresult, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(testresult_verified = case_when(
    testunits == "kg" &
    (testresult > (median_testresult + 99) | # choose 5 cm by default
      testresult < (median_testresult - 99))      ~ 1000, # F01413C06D921 weird
    testunits == "lb" &
      (testresult > (median_testresult + 99) | # choose 5 cm by default
         testresult < (median_testresult - 99))      ~ 2000, # 91 for F00C16FEE3F74
    TRUE                                         ~ testresult
  )) %>% # FCC755E4F8722 keep the first value anyway, eliminate that F8CB4CAB1073A (has 96 lbs difference-median) ?
  # mutate(cal = testresult-median_testresult)












  # 3. Rescue testunits when testresult is close 
  # Calcultate max and min to delimitate value for each patient-Choose 20
  group_by(patientid, testunits) %>%
  mutate(max_kg = case_when(
    testunits == "kg" ~ max(testresult, na.rm = TRUE) + 20 # F181950A26A19
  )) %>%
  mutate(min_kg = case_when(
    testunits == "kg" ~ min(testresult, na.rm = TRUE) -20
  )) %>%
  # mutate(max_lbs = case_when(
  #   testunits == "lb" ~ max(testresult, na.rm = TRUE) +30 # F14EBD0D0FA81
  # )) %>%
  mutate(min_lbs = case_when(
    testunits == "lb" ~ min(testresult, na.rm = TRUE) - 30
  )) %>%
  ungroup() %>% 
  group_by(patientid) %>% 
  fill(max_kg, min_lbs, .direction = "downup") %>% # min_kg, max_lbs
  ungroup() %>%
  
  mutate(testunits_rescue = case_when(
    is.na(testunits) &
      testresult < max_kg+(max_kg/2)        ~ "kg", # F16847515CAF4
    is.na(testunits) &
      testresult > min_lbs-(min_lbs/2)      ~ "lb" # F004D683A1695, F10CAD0F4EC48
  )) %>% 
  # if is 2 times the weight in kg add lb F14EBD0D0FA81
  mutate(testunits_rescue1 = case_when(
    testresult < (max_kg * 2.205) &
      testresult > (min_kg * 2.205)         ~ "lb"
  )) %>% 
  mutate(testunits = coalesce(testunits, testunits_rescue, testunits_rescue1)) %>%
  
  
  
  
  # 3. Clean outliers
  group_by(patientid, testunits) %>% 
  mutate(mu = remove_outliers(testresult)) %>% # F8A3DD629CDA6, F020C6A8B9E50, F044601199C5D
  ungroup() %>% 
  group_by(patientid, testunitscleaned) %>% 
  mutate(muu = remove_outliers(testresultcleaned)) %>% # F0027D3926C88
  ungroup() %>% 
  # 4.Fill testunits when is present for the patient in other similar measure range
  # Calculate the max and min that a patient can be (necessary for when 2 units used for same patient)
  
  
  
  
   
   
  
  
  # 3. Clean and calculate kg from testresults for lb and oz to maximize data point available
  mutate(calculated_kg = case_when( 
    testunits == "lb" &
      testresult <= 643 &
      testresult >= 50          ~ testresult/2.205,
    testunits == "lb" &
      (testresult > 643 |
         testresult < 50)          ~ NA_real_,
    testunits == "oz" &
      testresult <= 10288 &
      testresult >= 800         ~ testresult/32.274,
    testunits == "oz" &
      (testresult > 10288 |
         testresult < 800)         ~ NA_real_,
    TRUE                        ~ 1000
  )) %>% 
  filter(!is.na(testresult), is.na(testresultcleaned))
  
  
  
  
  
  # To get more data, have data from testunits that can be used. Fill up testunits, Transfer in kg and coalesce.
  # arrange(testresult) %>% 
  # group_by(patientid) %>% 
  # fill(testunits, .direction = "downup") %>% # F004D683A1695 F020C6A8B9E50 ======> WRONG
  # ungroup() %>% 
  %>% 
  mutate(testresultcleaned = coalesce(testresultcleaned, calculated_kg)) %>%
  group_by(patientid) %>% # Cannot improve weight too much as value and unit are bad ex F8A3DD629CDA6
  mutate(median_testresultcleaned = median(testresultcleaned)) %>% # works for F1493F8F5A924, F16847515CAF4
  ungroup() %>%
  mutate(weight_verified = case_when(
    testresultcleaned > (median_testresultcleaned + 70) | # choose 70 kg by looking at data F54F47F4419E0
      testresultcleaned < (median_testresultcleaned - 70)      ~ 1000, 
    TRUE                                                      ~ testresultcleaned # FABC474931881
  )) %>% 
  # FA7C8143AEE58 41.8 kg var F847A533FAE7E, FC09E942CB0D7 FBE6E66AB2C95 45 was wrong F18E68D19D10F
  group_by(patientid) %>% # Cannot improve weight too much as value and unit are bad ex F8A3DD629CDA6
  mutate(median_weight = median(weight)) %>% # works for F1493F8F5A924, F16847515CAF4
  ungroup() %>%
  mutate(weight_verified = case_when(
    weight > (median_weight + 70) | # choose 45 cm by looking at data
      weight < (median_weight - 70)      ~ 1000, 
    TRUE                                                      ~ weight
  )) %>% 
  select(c("patientid", weight_date = "testdate", weight))

############ BMI

# Idea to bind weight and height by year to calculate BMI. ...But better if it's close to drug administration
# %>% 
#   full_join(., Vitals_height, by = c("patientid", "year"))
# mutate(BMI = )

# Vital <- dcast(setDT(Vitals), patientid+testdate ~ rowid(patientid),  value.var = c("test", "testunits", "testresult"))


#################################################################################################### I ### Treatment Cleaning
# considered maintenance therapy : bevacizumab , olaparib, rucaparib, niraparib, gemcitabine  
drugs <- drugs %>% 
  filter(episodedatasource == "Administrations") %>% 
  filter(str_detect(drugname, "taxel|platin")) %>% 
  left_join(., clinical_data %>% select(c("patientid", "issurgery", "surgerydate")),
            by = "patientid")

# Verify issurgery
drugs1 <- drugs %>% 
  mutate(surg = ifelse(is.na(surgerydate), "No/unknown", "Yes"))
unique(drugs1$patientid[(drugs1$issurgery != drugs1$surg)])

drugs1 <- drugs %>% 
  mutate(therapy = case_when(
    ismaintenancetherapy == "TRUE"    ~ "maintenance",
    episodedate <= surgerydate        ~ "neoadjuvant", # F63ABD6AEB12C, F4F7B1C5DF24F intraperitoneal Carboplatin
    episodedate > surgerydate         ~ "adjuvant"
  )) %>% 
  mutate(new_line = case_when(
    therapy == "neoadjuvant"      ~ linenumber
  )) %>% 
  select(c("issurgery", "surgerydate", "patientid", "linename", "linenumber", "linestartdate", "lineenddate", "episodedate", 
           "drugname", "amount", "units", "therapy", "new_line")) %>% 
  group_by(patientid) %>% 
  mutate(nbr_line_before_surgery = max(new_line, na.rm = TRUE)) %>% # FBCF69031DFF8
  ungroup(patientid)
  






