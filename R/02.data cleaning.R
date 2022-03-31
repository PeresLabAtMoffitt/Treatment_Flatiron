#################################################################################################### I ### Clinical Cleaning----
############# 1 ### Cleanup of creatinine----
creatinine <- creatinine %>%
  # remove creatine urine
  filter(str_detect(labcomponent, "blood|serum")) %>% 
  mutate(creatinine = case_when(
    testresultcleaned < 0.7                   ~ 0.7,
    testresultcleaned > 3                     ~ NA_real_, # F006DBE0C7A5E is an example of slow increase past 3, F641A8DC820C7 super high
    TRUE                                      ~ testresultcleaned
  )) %>% 
  mutate(testresult = str_remove(testresult, "<"),
         testresult = as.numeric(testresult)) %>% 
  # F092F8B827DDC kept umol/L as they actually are mg/dL range 45 to 90 μmol/L (0.5 to 1.0 mg/dL) for women.
  mutate(creatinine1 = case_when(
    testresult < 0.7                          ~ 0.7,
    testresult > 3                            ~ NA_real_,
    TRUE                                      ~ testresult
  )) %>% 
  mutate(creatinine = coalesce(creatinine, creatinine1)) %>% 
  filter(!is.na(creatinine)) %>% 
  mutate(testdate = as.Date(testdate, format = "%m/%d/%y")) %>% 
  # Bind to clinical to calculate value closest to dx
  left_join(., clinical_data %>% 
              select(patientid, diagnosisdate), 
            by = "patientid") %>% 
  mutate(interval = abs(interval(start= testdate, end= diagnosisdate)/
                          duration(n=1, unit="days"))) %>%
  group_by(patientid) %>% 
  mutate(interval_creat_x = min(interval)) %>% 
  mutate(creatinine_at_dx = case_when(
    interval == interval_creat_x          ~ creatinine
  )) %>% 
  fill(creatinine_at_dx, .direction = "updown") %>% 
  select(c(patientid, creat_date = "testdate", creatinine, 
           creat_units = "testunitscleaned", creatinine_at_dx))
write_rds(creatinine, "creatinine.rds")


############# 2 ### body_surface_area, height and weight prep----
vitals <- vitals %>% 
  select(-c(test, testbasename, resultdate, minnorm, maxnorm, minnormcleaned, maxnormcleaned))


############# 3 ### Cleanup of BSA----
body_surface_area <- vitals %>% 
  filter(labcomponent == "Body Surface Area (BSA)") %>% # only 399 patients data
  # Limit max = 4.14, min = 0.418 calculated for biggest to smallest person
  mutate(testresult = ifelse(testresult < 0.418 | testresult > 4.14, NA_real_, testresult)) %>% 
  group_by(patientid) %>%
  # I used the median to remove outlier within a patient
  mutate(median_testresult = median(testresult)) %>% 
  ungroup() %>% 
  # After checking, removing median which deviate more 0.3 would be bad
  mutate(BSA = case_when(
    testresult > (median_testresult + 0.4) |
      testresult < (median_testresult - 0.4)           ~ NA_real_, 
    TRUE                                               ~ testresult
  )) %>% 
  mutate(BSA = coalesce(testresultcleaned, BSA)) %>% 
  filter(!is.na(BSA)) %>% 
  mutate(BSA_units = "m2") %>% 
  # Bind to clinical to calculate value closest to dx
  left_join(., clinical_data %>% 
              select(patientid, diagnosisdate), 
            by = "patientid") %>% 
  mutate(interval = abs(interval(start= testdate, end= diagnosisdate)/
                          duration(n=1, unit="days"))) %>%
  group_by(patientid) %>% 
  mutate(interval_bsa_dx = min(interval)) %>% 
  mutate(bsa_at_dx = case_when(
    interval == interval_bsa_dx                        ~ BSA
  )) %>% 
  fill(bsa_at_dx, .direction = "updown") %>% 
  select(c(patientid, bsa_date = "testdate", BSA, 
           BSA_units, bsa_at_dx, interval_bsa_dx))

write_rds(body_surface_area, "body_surface_area.rds")


############# 4 ### Cleanup of Height----
height <- vitals %>% 
  filter(labcomponent == "Body Height") %>% 
  # 1. Clean up outliers compare to tallest ans smallest record
  mutate(testresultcleaned = case_when(
    testresultcleaned > 231 | # Tallest women height
      testresultcleaned < 120 # Average height dwarf = 122 cm, 4 feet, I take 120 by looking at the data
                                                       ~ NA_real_,
    TRUE                                               ~ testresultcleaned
  )) %>% 
  # To get more data, I have data from testresult that can be used
  # also have missing testunits F004D683A1695 
  # fill up testunits within patients before removing outliers 
  # then transfer in kg and coalesce with testresultcleaned.
  group_by(patientid) %>% 
  fill(testunits, .direction = "downup") %>% 
  ungroup() %>% 
  # Even if mistake was added FAAAACDA1754E it will be removed in the next step
  mutate(testresult = case_when(
    testresult > 231 & testunits == "cm" | 
      testresult < 100 & testunits == "cm" |
      testresult > 7.58 & testunits == "ft" |
      testresult < 3.28 & testunits == "ft" |
      testresult > 90.94 & testunits == "in" |
      testresult < 39.37 & testunits == "in"| # Example of a removed F615C37BF1470 (F91590CB83EC8 has initial mistakes too)
      testresult < 3.28 & is.na(testunits) |
      testresult > 231 & is.na(testunits)              ~ NA_real_,
    TRUE                                               ~ testresult
  )) %>% 
  
  # 2. Clean up outliers within patient by using the median for each patients/unit. Eliminate the ones with too much variation.
  group_by(patientid) %>%
  mutate(mean_testresultcleaned = mean(testresultcleaned)) %>% # FEBB819DF1D8F 6.2, FCC755E4F8722 - 8, F45BCB24511B8 - 12, F02BBE6E30D4A 18
  ungroup() %>% # F5B1E2D059869 24, remove FAC12EC567E3E = 35 FF683865721AE 38
  mutate(testresultcleaned_verified = case_when(
    testresultcleaned > (mean_testresultcleaned + 23) | # choose by looking at data
      testresultcleaned < (mean_testresultcleaned - 23)       ~ NA_real_, # F355C8B80BFE9 50% height 124 50% 144cm
    TRUE                                                      ~ testresultcleaned
  )) %>% 
  group_by(patientid, testunits) %>%
  mutate(mean_testresult = mean(testresult)) %>% 
  ungroup() %>% 
  mutate(testresult_verified = case_when(
    testresult > (mean_testresult + 9) & 
      testunits == "in" |
      testresult < (mean_testresult - 9) & 
      testunits == "in"                         ~ NA_real_, # F5B1E2D059869 = 9 keep FAC12EC567E3E FF683865721AE
    testresult > (mean_testresult + 23) & 
      testunits == "cm" |
      testresult < (mean_testresult - 23) & 
      testunits == "cm"                         ~ NA_real_, # removed F96C7348B19B4 F78D6DABCFD2F
    TRUE                                        ~ testresult
  )) %>% 
  mutate(height = case_when(
    !is.na(testresultcleaned_verified)          ~ round(testresultcleaned_verified / 100, 3), # cm to m
    testunits == "cm"                           ~ round(testresult_verified / 100, 3),
    testunits == "ft"                           ~ round(testresult_verified / 3.281, 3), # ft to m
    testunits == "in" | 
      !is.na(testresult)                        ~ round(testresult_verified / 39.37, 3) # in to m # FEAEBCA68FF6B F55CB96781E07 has NA
    )) %>% 
  filter(!is.na(height)) %>% 
  mutate(height_units = "m") %>% 
  # Bind to clinical to calculate value closest to dx
  left_join(., clinical_data %>% 
              select(patientid, diagnosisdate), 
            by = "patientid") %>% 
  mutate(interval = abs(interval(start= testdate, end= diagnosisdate)/
                          duration(n=1, unit="days"))) %>%
  group_by(patientid) %>% 
  mutate(interval_height_dx = min(interval)) %>% 
  mutate(height_at_dx = case_when(
    interval == interval_height_dx                     ~ height
  )) %>% 
  fill(height_at_dx, .direction = "updown") %>% 
  select(c(patientid, height_date = "testdate", height, 
           height_units, height_at_dx, interval_height_dx))
write_rds(height, "height.rds")


############# 5 ### Cleanup of weight----
weight <- vitals %>%
  filter(labcomponent == "Body Weight") %>% 
  select(c("patientid", "testdate", "testunits", "testresult", "testunitscleaned", "testresultcleaned")) %>%
  # 1.Clean testresultcleaned
  group_by(patientid) %>%
  mutate(median_testresultcleaned = median(testresultcleaned, na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(weight1 = case_when(
    testresultcleaned >= 27 &       # Biggest recorded 291.6 kg (642.9 lbs, 10286 oz)
      testresultcleaned < 292 &     # lightest 5.5 kg (12 lbs, 194 oz)  F0027D3926C88
      (testresultcleaned > (median_testresultcleaned - 39.6) & # F1493F8F5A924, F54F47F4419E0, F18E68D19D10F
         testresultcleaned < (median_testresultcleaned + 39.6))     ~ testresultcleaned, 
    TRUE ~ NA_real_
  )) %>% 
  # 2.Clean testresult to be used to find max and min
  mutate(testresult = case_when(
    testunits == "lb" &
      testresult > 12 &
      testresult < 643                                 ~ testresult,
    testunits == "kg" &
      testresult >= 5.5 &
      testresult < 292                                 ~ testresult,
    testunits == "oz" &
      testresult > 194 &
      testresult < 10286                               ~ testresult,
    is.na(testunits) & 
    testresult > 5.5 & 
      testresult < 10286                               ~ testresult, # combine oz, kg, lbs for when units is NA
    TRUE                                               ~ NA_real_
  ))

weight1 <- weight %>% 
  # 3. Calculate median for each units and remove data outside range for each patients/unit
  # 3. help distinguishing lbs to kg for same patient when unit is NA
  group_by(patientid, testunits) %>%
  mutate(median_testresult = median(testresult, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(testresult_verified = case_when(       
    testunits == "kg" &
    (testresult > (median_testresult + 38.5) | # choose 38.5 cm by default could do 32 if not for F534F9C4FF968
       # 12 F2F31607FD6AD , FA7C8143AEE58 gain 37kg, F847A533FAE7E gain 38, FBE6E66AB2C95 gain 41, FC09E942CB0D7 gain 49
       # F5B18C17A28F5 and F98077E9119EC FA53D0F672C7D FCFF0EDFE278F, FEF3273967DB3 wrong in cleaned too
      testresult < (median_testresult - 38.5))         ~ NA_real_, 
    testunits == "lb" &
      (testresult > (median_testresult + 99) | # choose 99 cm by default, # Problem when wrong first value FCC755E4F8722 (94.8) keep in case close to first dosinf
         testresult < (median_testresult - 99))        ~ NA_real_, # 91 for F00C16FEE3F74
    TRUE                                               ~ testresult
  )) %>%  # FCC755E4F8722 keep the first value anyway, eliminate that F8CB4CAB1073A (has 96 lbs difference-median)

  # 4. Rescue testunits when testresult_verified is close 
  # Calculate max and min to limit value for each patient-Choose 20
  group_by(patientid, testunits) %>%
  mutate(max_kg = case_when(
    testunits == "kg"                                  ~ max(testresult_verified, na.rm = TRUE) + 20 # F181950A26A19
  )) %>%
  mutate(min_kg = case_when(
    testunits == "kg"                                  ~ min(testresult_verified, na.rm = TRUE) - 20
  )) %>%
  mutate(max_lbs = case_when(
    testunits == "lb"                                  ~ max(testresult_verified, na.rm = TRUE) + 30 # F14EBD0D0FA81
  )) %>%
  mutate(min_lbs = case_when(
    testunits == "lb"                                  ~ min(testresult_verified, na.rm = TRUE) - 30
  )) %>%
  ungroup() %>% 
  group_by(patientid) %>% 
  fill(max_kg, min_kg, max_lbs, min_lbs, .direction = "downup") %>% # min_kg, max_lbs
  ungroup() %>%
  
  mutate(testunits_rescue = case_when(
    is.na(testunits) &
      testresult_verified < max_kg                     ~ "kg", # Good doesn't work with F16847515CAF4 , Perfect for F8A3DD629CDA6
    is.na(testunits) & # FE8DBCAC6DAFB
      testresult_verified > min_lbs                    ~ "lb" # F004D683A1695, F10CAD0F4EC48, FC109A6F719E7, FD25ED170CF07, FE06FF7C3E108, F044601199C5D
  ))

weight2 <- weight1 %>% 
  group_by(patientid) %>% 
  # 5. Rescue testunits when testresult_verified is twice or /2 means that lb or kg 
  # if is 2 times the weight in kg add lb F14EBD0D0FA81
  mutate(testunits_rescue1 = case_when(
    is.na(testunits) &
      testresult_verified < (max_kg * 2.205) &
      testresult_verified > (min_kg * 2.205)           ~ "lb",
    is.na(testunits) &
      testresult_verified < (max_lbs / 2.205) &
      testresult_verified > (min_lbs / 2.205)          ~ "kg"
  )) %>% 
  ungroup() %>% 
  mutate(testunits = coalesce(testunits, testunits_rescue, testunits_rescue1)) %>% 
  select(c("patientid", "testdate", "testunits", "testresult", "testresult_verified", "testunitscleaned", "testresultcleaned", "weight1"))

weight <- weight2 %>% 
  # 6.Fill up weight with new testresult_verified cleaned
  mutate(weight2 = case_when(
    is.na(testresultcleaned) &
      testunits == "kg"                                ~ testresult_verified, # F0027D3926C88, FE8DBCAC6DAFB, F8A3DD629CDA6, F020C6A8B9E50, F16847515CAF4
    is.na(testresultcleaned) & # F044601199C5D
      testunits == "lb"                                ~ (testresult_verified / 2.205), # FCB918617347C, F5E3D88009911, F14EBD0D0FA81, F16847515CAF4, FABC474931881
  )) %>% 
  mutate(weight = coalesce(weight1, weight2)) %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_units = "kg") %>% 
  # Bind to clinical to calculate value closest to dx
  left_join(., clinical_data %>% 
              select(patientid, diagnosisdate), 
            by = "patientid") %>% 
  mutate(interval = abs(interval(start= testdate, end= diagnosisdate)/
                          duration(n=1, unit="days"))) %>%
  group_by(patientid) %>% 
  mutate(interval_weight_dx = min(interval)) %>% 
  mutate(weight_at_dx = case_when(
    interval == interval_weight_dx                     ~ weight
  )) %>% 
  fill(weight_at_dx, .direction = "updown") %>% 
  select(c(patientid, weight_date = "testdate", weight, 
           weight_units, weight_at_dx, interval_weight_dx))
write_rds(weight, "weight.rds")

rm(weight1, weight2, vitals)


############# 6 ### Cleanup of AUC----
# Clean orderdrug for duplicated rows
orderdrug <- orderdrug %>% 
  `colnames<-`(str_to_lower(colnames(.))) %>% 
  filter(str_detect(drugname, "taxel|platin") & str_detect(route, "venous|peritoneal")) %>%
  select("patientid", "orderid", "drugname", "administereddate", "administeredamount", "administeredunits") %>%
  # 4 patients have duplicated order, need to keep the ones which are not NA 
  #  "MB280CD7F67AB09BDA03634F0B5780C28" "M93C0522CBC4B707B94E8B12B1E514E80" "M7C34D0B21D0C17E772EB31EADA398966" "M540AD166359C2FD4C7DEA9EB5F39116B"
  group_by(patientid, orderid) %>% 
  mutate(orderid_duplicated = n()) %>% 
  filter(orderid_duplicated == 1 | (orderid_duplicated == 2 & !is.na(administeredamount))) %>% 
  select(-orderid_duplicated)
############## We are clean -> no duplicated order and if multiple order for 1 administration date, we have corresponding auc in file

areaUC <- auc %>% 
  `colnames<-`(str_to_lower(colnames(.))) %>% 
  filter(str_detect(drugname, "taxel|platin") & is.na(iscanceled)) %>% 
  # 1. bind with orderdrug to get only the order administered, no duplicated order and more relativeorderedamount
  inner_join(., orderdrug, 
            by = c("patientid", "orderid", "drugname")) %>% 
  select(c("patientid", "orderid", "ordereddate", "expectedstartdate", "orderedamount", "orderedunits", "relativeorderedamount",
           "relativeorderedunits", "drugname", "administereddate", "administeredamount", "administeredunits")) %>%

  # 2. Clean wrong relativeorderedamount and orderedamount
  mutate(relativeorderedamount_verified = case_when( # F7D7377578248 what for the patients who have order amount
    str_detect(drugname, "carboplatin") &
      relativeorderedunits == "AUC" &
      relativeorderedamount >= 2 &
      relativeorderedamount <= 6          ~ relativeorderedamount, # F4E8C8717878B, FE3FF3A5AC465, F853DC24CD5E8
    str_detect(drugname, "taxel|platin") & # include all taxel + cisplatin + oxaliplatin
      relativeorderedunits == "mg/m2" &
      relativeorderedamount >= 50 &
      relativeorderedamount <= 175        ~ relativeorderedamount, # F21904E0D872F we should open more? F11B625154B62
    TRUE                                  ~ NA_real_
  )) %>% 
  mutate(orderedamount_verified = case_when( 
    str_detect(drugname, "carboplatin") &
      orderedunits == "AUC" &
      orderedamount >= 2 &
      orderedamount <= 6          ~ orderedamount, 
    str_detect(drugname, "taxel|platin") & 
      orderedunits == "mg/m2" &
      orderedamount >= 50 &
      orderedamount <= 175        ~ orderedamount, 
    TRUE                                  ~ NA_real_
  )) %>% 
  mutate(target_auc = coalesce(relativeorderedamount_verified, orderedamount_verified)) %>%
  
  # 3. Needs BSA for step 4 (calculate missing auc for taxels)
  # Merge with height
  left_join(. , height,  by = "patientid") %>%
  mutate(interval = abs(interval(start= height_date, end= ordereddate)/ # Use oredereddate because is closer to when target auc should have been calculated                     
                          duration(n=1, unit="days"))) %>% 
  arrange(interval) %>% 
  distinct(patientid, orderid, expectedstartdate, orderedamount, orderedunits, relativeorderedamount, relativeorderedunits, 
           drugname, administereddate, administeredamount, administeredunits, .keep_all = TRUE) %>% 
  # Merge with weight
  left_join(. , weight, by = "patientid") %>%
  mutate(interval = abs(interval(start= weight_date, end= ordereddate)/                      
                          duration(n=1, unit="days"))) %>% 
  arrange(interval) %>% 
  distinct(patientid, orderid, expectedstartdate, orderedamount, orderedunits, relativeorderedamount, relativeorderedunits, 
           drugname, administereddate, administeredamount, administeredunits, .keep_all = TRUE) %>% 
  # Merge with body_surface_area
  left_join(., body_surface_area, by = "patientid") %>%
  mutate(interval = abs(interval(start= bsa_date, end= ordereddate)/                      
                          duration(n=1, unit="days"))) %>% 
  arrange(interval) %>% 
  distinct(patientid, orderid, expectedstartdate, orderedamount, orderedunits, relativeorderedamount, relativeorderedunits, 
           drugname, administereddate, administeredamount, administeredunits, .keep_all = TRUE) %>% 
  # Calculate more bsa value with height and weight using Du Bois formula
  mutate(bsa_du_bois = 0.007184 * ((height*100)^0.725) * (weight^0.425)) %>% # get pretty close results 4269 patients result
  mutate(BSA = coalesce(BSA, bsa_du_bois)) %>% 
  select(-bsa_du_bois) %>% 
  
  # 4. Calculate more relativeorderedamount from mg (taxel/cisplatin only) 
  # Warning we don't use administeredamount because is the real amount given, 
  # We only want to calculate the dose which should have been given
  mutate(relativeorderedamount_calculated = case_when(
      is.na(target_auc) &
        str_detect(drugname, "taxel|cisplatin") &
        relativeorderedunits == "mg"                          ~ relativeorderedamount/BSA
    )) %>% 
  mutate(relativeorderedamount_calculated = case_when(
    relativeorderedamount_calculated >= 50 &
      relativeorderedamount_calculated <= 175                 ~ relativeorderedamount_calculated, 
    TRUE                                                      ~ NA_real_
  )) %>% 
  mutate(target_auc = coalesce(target_auc, relativeorderedamount_calculated)) %>% 
  
  mutate(orderedamount_calculated = case_when(
    is.na(target_auc) &
      str_detect(drugname, "taxel|cisplatin") &
      orderedunits == "mg"                                    ~ orderedamount/BSA
  )) %>% 
  mutate(orderedamount_calculated = case_when(
    orderedamount_calculated >= 50 &
      orderedamount_calculated <= 175                         ~ orderedamount_calculated, 
    TRUE                                                      ~ NA_real_
  )) %>% 
  mutate(target_auc = coalesce(target_auc, orderedamount_calculated)) %>% 
  select(c("patientid", "orderid", "ordereddate", 
           auc_date = "expectedstartdate", "target_auc", 
          "drugname")) %>% 
  filter(!is.na(target_auc))
  
write_rds(areaUC, "areaUC.rds")


#################################################################################################### II ### Clinical Cleaning----
clinical_data <- clinical_data %>% 
  # Recode
  mutate(raceeth = factor(raceeth, levels = c("NHWhite", "NHBlack", "Hispanic", "Other"))) %>% 
  mutate(race = case_when(
    str_detect(race, "Black")                   ~ "Black",
    str_detect(race, "Hispanic")                ~ "White",
    str_detect(race, "Other")                   ~ "Other",
    TRUE                                        ~ race
  )) %>% 
  mutate(ethnicity = (str_remove(ethnicity, " or Latino"))) %>% 
  mutate(vital = case_when(
    vital_status == 0                           ~ "Alive",
    vital_status == 1                           ~ "Dead"
  )) %>% 
  mutate(month_at_os = interval(start = diagnosisdate, end = followupdate)/
           duration(n=1, units = "months")) %>% 
  mutate(stagecat = case_when(
    stagecat == "Stage 1" |
      stagecat == "Stage 2"                     ~ "Early stage",
    stagecat == "Stage 3" |
      stagecat == "Stage 4"                     ~ "Late stage"
  )) %>% 
  mutate(across(.cols = c(histology), ~na_if(., "Unknown/not documented"))) %>% 
  mutate(histology1 = histology,
         histology = case_when(
    histology == "Serous"                       ~ "Serous",
    is.na(histology)                            ~ NA_character_,
    TRUE                                        ~ "Others"
  ), histology = factor(histology, levels = c("Serous", "Others"))) %>% 
  mutate(debulking = str_replace(debulking, "Unknown|No surgery", NA_character_)) %>% 
  mutate(across(.cols = c(stagecat, debulking), ~as.factor(.)))

write_rds(clinical_data, "clinical_data.rds")


#################################################################################################### III ### Treatment Cleaning----
# Select episodedatasource == "Administrations"-----
drugs <- drugs %>% 
  `colnames<-`(str_to_lower(colnames(.))) %>% 
  # Select only records for administration
  filter(episodedatasource == "Administrations") %>%
  # bind with clinical
  inner_join(., clinical_data, 
             by = "patientid") %>% 
  arrange(patientid, episodedate)

# Combine multiple administrations of the same drug at the same date-----
combined_drugs <- drugs %>% 
  # Combined Multiple Administrations----
# combined multiple amount of drug given the same day FCF86329BB40C F4E6EE1910940.May be 205 patients row like that
group_by(patientid, linename, linenumber, linestartdate, lineenddate, episodedate,
         drugname, units, ismaintenancetherapy) %>%
  summarise_at(vars(route, amount), paste, collapse = ";") %>% # keep them in the data then do sum
  mutate(route = case_when(
    str_detect(route, ";")            ~ "Intravenous", # I checked that all the combined administration are IV
    TRUE                              ~ route
  )) %>% 
  separate(col= amount, paste("amount_", 1:10, sep=""), sep = ";", extra = "warn",
           fill = "right") %>%
  ungroup() %>% 
  mutate(across(starts_with("amount"), ~ na_if(., "NA"))) %>% 
  purrr::keep(~!all(is.na(.))) %>%
  mutate(across(starts_with("amount"), ~ as.numeric(.))) %>% 
  mutate(amount = rowSums(select(.,starts_with("amount_")), na.rm = TRUE)) %>%
  # Found ug, ml, Ea, and NA they are all good, but 
  # ug need conversion
  # the AUC are mostly correct (real mg) but are actual AUC when < at 10 so remove
  mutate(amount =  case_when(
    units == "ug"               ~ amount / 10, # I checked all of them
    units == "AUC" &
      amount < 10               ~ NA_real_,
    TRUE                        ~ amount
  ))# %>% 

# mutate(drugname_type = case_when(
#   str_detect(drugname, "platin")       ~ "platin",
#   str_detect(drugname, "taxel")        ~ "taxel"
# ))

# Recode linenumber depending on surgery date (Categorize Neo and Adjuavnt)-------
therapy_line <- combined_drugs %>% 
  # bind again with clinical - was lost while summarizing
  inner_join(., clinical_data, 
             by = "patientid") %>% 
  arrange(patientid, episodedate) %>% 
  # Define Adjuvant, Neoadjuvant, chemo or surgery
  mutate(chemotherapy_type = case_when(
    # ismaintenancetherapy == "TRUE"    ~ "maintenance",
    episodedate <= surgerydate        ~ "Neoadjuvant", # F63ABD6AEB12C, F4F7B1C5DF24F intraperitoneal Carboplatin
    episodedate > surgerydate         ~ "Adjuvant",
    is.na(surgerydate) &
      !is.na(episodedate)             ~ "Chemotherapy only",
    !is.na(surgerydate) &
      is.na(episodedate)              ~ "Surgery only"
  )) %>% 
  mutate(chemotherapy_type = 
           factor(chemotherapy_type, levels = 
                    c("Neoadjuvant", "Adjuvant", "Chemotherapy only", "Surgery only"))) %>%
  group_by(patientid) %>% 
  mutate(had_neo = ifelse(str_detect(chemotherapy_type, "Neo"), "Yes", NA_character_)) %>% 
  mutate(had_adj = ifelse(str_detect(chemotherapy_type, "Adj"), "Yes", NA_character_)) %>% 
  fill(had_neo, had_adj, .direction = "updown") %>% 
  mutate(treatment_sequence = case_when(
    had_neo == "Yes" &
      had_adj == "Yes"                           ~ "neo + surg + adj",
    had_neo == "Yes" &
      is.na(had_adj)                             ~ "neo + surg",
    is.na(had_neo) &
      had_adj == "Yes"                           ~ "surg + adj",
    chemotherapy_type == "Chemotherapy only"     ~ "Chemotherapy only",
    chemotherapy_type == "Surgery only"          ~ "Surgery only"
  )) %>% 
  mutate(therapy = case_when(
    chemotherapy_type == "Neoadjuvant"           ~ "Upfront Chemo",
    chemotherapy_type == "Chemotherapy only"     ~ "Chemotherapy only",
    chemotherapy_type == "Surgery only"          ~ "Surgery only",
    is.na(had_neo) &
      had_adj == "Yes"                           ~ "Upfront Surgery"
  )) %>% 
  fill(therapy, .direction = "downup") %>% 
  
  # Redefine line number based on the original linenumber variable and the surgery date
  mutate(linenumber_n = dense_rank(interaction(linenumber, chemotherapy_type))) %>%  
  # Redefine line number based on the new linenumber variable and if a switch 
  # to maintenance is detected within the line
  group_by(patientid, linenumber_n, ismaintenancetherapy) %>% 
  mutate(maint_rank = row_number(ismaintenancetherapy),
         maint_rank = ifelse(maint_rank == 1, 1, 0)) %>% 
  group_by(patientid) %>% 
  mutate(linenumber_new = cumsum(maint_rank)) %>%  
  # F0000AD999ABF, F1C68CFAC2AF3, FBCF69031DFF8, F0040EA299CF0, FAACDC7205803,  FA019D2071321 no has big jump F95D860690A93 F0F1324B863A8
  select(patientid, linename, linenumber, linenumber_new, drugname, 
         episodedate, surgerydate, chemotherapy_type, everything()) %>% 
  ungroup() %>% 
  # Define line number restarting to 1 for each chemotherapy_type
  group_by(patientid, chemotherapy_type) %>% 
  mutate(linenumber_adj = dense_rank(linenumber_new)) %>% 
  ungroup() %>% 
  mutate(across(c("linenumber", "linenumber_new", "linenumber_adj"), as.factor)) %>% 
  # Redefine line dates
  group_by(patientid, linenumber_new) %>% 
  mutate(linestartdate = min(episodedate)) %>% 
  mutate(lineenddate = max(episodedate)) %>% 
  ungroup() %>% 
  select(-c(linenumber_n, maint_rank))

# 6.Add a 30 day rule to switch linenumber----
# group_by(patientid, linenumber_new) %>% 
# arrange(episodedate) %>% 
# mutate(episode_interval = episodedate - lag(episodedate), episode_interval = ifelse(is.na(episode_interval), 0, episode_interval)) %>%  # FDB58481AFFBB 48 days
# mutate(jump_line = ifelse(episode_interval > 31, 1, 0)) %>% # FA019D2071321 >31
# ungroup() %>% 
# group_by(patientid) %>% 
# mutate(jump_line = cumsum(jump_line)) %>% # F002DD2953889 fixed even when increase multiple time 
# mutate(linenumber_new = linenumber_new + jump_line) %>% 
#F002DD2953889 need to do 37 and 42 F0040EA299CF0, 35 F005602DA4DF0 => do at least 37

# Code cycle------
therapy_cycle <- therapy_line %>% 
  # Define cycle for each line
  # F003F2BEEDB37 need to change drugname pacli = pacli bp
  mutate(cycle_drugname = str_remove(drugname, " protein-bound")) %>%  
  # Need the number of time of the "major" drug is seen in the line
  group_by(patientid, linenumber_new, cycle_drugname) %>%
  mutate(drugname_count_perline = case_when(
    str_detect(drugname, "carboplatin|paclitaxel")     ~ n()
  )) %>%
  ungroup() %>% 
  arrange(patientid, episodedate, drugname) %>% 
  group_by(patientid, linenumber_new) %>%
  # Take the major drugname as a mark of new cycle start
  mutate(cycle_count_perline = min(drugname_count_perline, na.rm = TRUE)) %>%
  mutate(cycle_major_drugname = case_when(
    cycle_count_perline == drugname_count_perline &
      (cycle_drugname == "carboplatin" |
         cycle_drugname == "paclitaxel")                      ~ cycle_drugname,
    TRUE                                                   ~ NA_character_
  )) %>% 
  # select(-drugname_count_perline) %>% 
  # Code cycle number based on when major drug name appear again
  group_by(patientid, linenumber_new, drugname, cycle_major_drugname) %>%
  mutate(cycle_increment = row_number(cycle_major_drugname)) %>% # Look at F002DD2953889 inversed dose dense------------------
# F01D908A013B7 weird dose dense

# F003F2BEEDB37 change of dose dense 50% ok

arrange(patientid, episodedate, cycle_increment) %>% 
  group_by(patientid, linenumber_new) %>%
  fill(cycle_increment, .direction = "downup") %>%  # make sure I can do up for have one new drug compare to the linename FBB5186A15CD9 ok, FE593EDF5586A has a switched drugs will be removed
  group_by(patientid, chemotherapy_type, cycle_increment) %>% 
  # Code is_dose_dense to be 1 for the first drug in the cycle
  mutate(is_dose_dense = row_number(cycle_increment),
         is_dose_dense = ifelse(is_dose_dense == 1, 1, NA_real_)) %>% 
  group_by(patientid) %>% 
  mutate(total_cycle_increment = row_number(is_dose_dense)
  ) %>% 
  group_by(patientid, linenumber_adj) %>% 
  fill(total_cycle_increment, .direction = "downup") %>% 
  # cycle date
  group_by(patientid, linenumber_new, cycle_increment) %>% 
  mutate(cycle_start_date = min(episodedate)) %>% 
  
  # Look at dose dense
  # Code how many time a drug comes up in a cycle
  group_by(patientid, linenumber_new, cycle_increment, cycle_drugname) %>%  # weird dose dense F064D9EB733B3 F050B815FC126
  mutate(dose_dense = n()) %>% 
  mutate(dose_dense = max(dose_dense)) %>%  # too much cycle
  ungroup()

# # CODE FOR SKIPPED CYCLE
# group_by(patientid, linenumber_new) %>% 
# mutate(cycle_interval = cycle_start_date - lag(cycle_start_date), 
#        cycle_interval = ifelse(cycle_interval == 0, NA_real_, cycle_interval)) %>%  # FDB58481AFFBB 48 days
# 
# # take for granted that the cycle interval days are the routine cycle for the rest of the line FAACDC7205803
# mutate(n = row_number(drugname)) %>%
# 
# mutate(base_days_between_cycle = ifelse(n==2 , cycle_interval, NA_real_)) %>%
# fill(base_days_between_cycle, .direction = "downup") %>% 
# mutate(skipped_cycle_indays = ifelse(((cycle_interval - base_days_between_cycle) != 0) ,
#                                      (cycle_interval - base_days_between_cycle), NA_real_)) %>% # weird FDB58481AFFBB
# # Fix skipped_cycle_indays when only 1 drug FDB58481AFFBB skipped_cycle_indays = NA, no other choice...
# group_by(patientid, linenumber_new) %>%
# mutate(drug_count_perline = n()) %>%
# mutate(skipped_cycle_indays1 = ifelse(
#   (cycle_count_perline == drug_count_perline), NA_real_, skipped_cycle_indays 
# )) %>% # F001443D8B85C Pb with when different pattern by cycle => May not be able to have the skipped cycle
# # or need to compare with dose like if 100 should be 7 days and if 200 should be 14 days....
# group_by(patientid) %>% 
# mutate(mean_skipped_cycle_indays = mean(skipped_cycle_indays, na.rm = TRUE)) %>% 
# ungroup()

# rm(drugs, drugs1, therapy_line)
therapy_cycle %>% distinct(patientid) %>% nrow()

# Bind everything together to calculate target auc/target dose and expected dose--------
Treatment1 <- 
  # Merge with creatinine
  left_join(therapy_cycle, creatinine, by = "patientid") %>%
  mutate(interval_creat = abs(interval(start= creat_date, end= cycle_start_date)/                      
                                duration(n=1, unit="days"))) %>% 
  mutate(interval_creat = case_when(
    (therapy == "Upfront Surgery" &
       interval_creat < 31 &
       # creat_date < lineenddate &
       creat_date > surgerydate)                  ~ interval_creat,
    
    therapy == "Upfront Chemo" &
      (
        (chemotherapy_type == "Neoadjuvant" &
           interval_creat < 31 &
           creat_date < surgerydate) |
          (chemotherapy_type == "Adjuvant" &
             interval_creat < 31 &
             # creat_date < lineenddate &
             creat_date > surgerydate)
      )                                          ~ interval_creat,
    TRUE                                         ~ NA_real_
  )) %>%
  arrange(interval_creat) %>% 
  distinct(patientid, episodedate, drugname, amount, linenumber_new, cycle_count_perline, cycle_increment, 
           cycle_start_date, .keep_all = TRUE) %>% 
  
  # Merge with height
  left_join(. , height,  by = "patientid") %>%
  mutate(interval_height = abs(interval(start= height_date, end= cycle_start_date)/
                                 duration(n=1, unit="days"))) %>%
  arrange(interval_height) %>%
  distinct(patientid, episodedate, drugname, amount, linenumber_new, cycle_count_perline, cycle_increment,
           cycle_start_date, .keep_all = TRUE) %>%
  
  # Merge with weight
  left_join(. , weight, by = "patientid") %>%
  mutate(interval_weight = abs(interval(start= weight_date, end= cycle_start_date)/
                                 duration(n=1, unit="days"))) %>%
  mutate(interval_weight = case_when(
    (therapy == "Upfront Surgery" &
       interval_weight < 31 &
       # weight_date < lineenddate &
       weight_date > surgerydate)                  ~ interval_weight,
    
    therapy == "Upfront Chemo" &
      (
        (chemotherapy_type == "Neoadjuvant" &
           interval_weight < 31 &
           weight_date < surgerydate) |
          (chemotherapy_type == "Adjuvant" &
             interval_weight < 31 &
             # weight_date < lineenddate &
             weight_date > surgerydate)
      )                                          ~ interval_weight,
    TRUE                                         ~ NA_real_
  )) %>%
  arrange(interval_weight) %>%
  distinct(patientid, episodedate, drugname, amount, linenumber_new, cycle_count_perline, cycle_increment,
           cycle_start_date, .keep_all = TRUE) %>%
  
  # Calculate ideal dody weight
  mutate(ideal_weight = case_when(
    height <= 1.524               ~ 45.5 + 2.3,
    height > 1.524                ~ 45.5 + (2.3 * (((height - 1.524) / 0.0254)))
  )) %>% 
  mutate(adjusted_weight = ((weight - ideal_weight) * 0.40) + ideal_weight
  ) %>% 
  # Calculate bmi
  mutate(bmi_at_dx = weight_at_dx / (height_at_dx * height_at_dx)) %>%
  mutate(bmi_cat = case_when(
    bmi_at_dx < 25                    ~ "Underweight and normal weight",
    bmi_at_dx >= 25 &
      bmi_at_dx < 30                  ~ "Overweight",
    bmi_at_dx >= 30                   ~ "Obese"
  )) %>%
  mutate(bmi_cat = factor(bmi_cat, levels = c("Underweight and normal weight", "Overweight", "Obese"))) %>%
  
  mutate(actual_bmi = weight / (height * height)) %>%
  mutate(actual_bmi_cat = case_when(
    actual_bmi < 25                    ~ "Underweight and normal weight",
    actual_bmi >= 25 &
      actual_bmi < 30                  ~ "Overweight",
    actual_bmi >= 30                   ~ "Obese"
  )) %>%
  mutate(actual_bmi_cat = factor(actual_bmi_cat, 
                                 levels = c("Underweight and normal weight", "Overweight", "Obese"))) %>%
  # select(patientid, weight, height, bmi_at_dx, ideal_weight, adjusted_weight, adjusted_bmi)
  # Calculate CrCl
  # inner_join(., clinical_data %>% select(-surgerydate),
  #            by = "patientid") %>% 
  mutate(CrCl = (
    (
      ((140 - ageatdx) * weight) /
        (72 * creatinine)
    ) *
      0.85)
  ) %>% 
  mutate(CrCl = case_when(
    CrCl > 125                        ~ 125,
    TRUE                              ~ CrCl
  )) %>% 
  mutate(adjusted_CrCl = case_when(
    actual_bmi >= 25                  ~ CrCl,
    actual_bmi < 25                   ~ (((140 - ageatdx) * adjusted_weight) / (72 * creatinine)) * 0.85
  )) %>% 
  mutate(adjusted_CrCl = case_when(
    adjusted_CrCl > 125               ~ 125,
    TRUE                              ~ adjusted_CrCl
  )) %>% 
  
  # Merge with body_surface_area
  left_join(., body_surface_area, by = "patientid") %>%
  mutate(interval_bsa = abs(interval(start= bsa_date, end= cycle_start_date)/
                              duration(n=1, unit="days"))) %>%
  mutate(interval_bsa = case_when(
    (therapy == "Upfront Surgery" &
       interval_bsa < 31 &
       # bsa_date < lineenddate &
       bsa_date > surgerydate)                  ~ interval_bsa,
    
    therapy == "Upfront Chemo" &
      (
        (chemotherapy_type == "Neoadjuvant" &
           interval_bsa < 31 &
           bsa_date < surgerydate) |
          (chemotherapy_type == "Adjuvant" &
             interval_bsa < 31 &
             # bsa_date < lineenddate &
             bsa_date > surgerydate)
      )                                          ~ interval_bsa,
    TRUE                                         ~ NA_real_
  )) %>%
  arrange(interval_bsa) %>%
  distinct(patientid, episodedate, drugname, amount, linenumber_new, cycle_count_perline, cycle_increment,
           cycle_start_date, .keep_all = TRUE) %>%
  # Calculate more bsa value with height and weight using Du Bois formula
  mutate(bsa_du_bois = 0.007184 * ((height*100)^0.725) * (weight^0.425)) %>% # get pretty close results 4269 patients result
  mutate(BSA = coalesce(BSA, bsa_du_bois)) %>%
  mutate(bsa_du_bois = 0.007184 * ((height_at_dx*100)^0.725) * (weight_at_dx^0.425)) %>% # get pretty close results 4269 patients result
  mutate(bsa_at_dx = coalesce(bsa_at_dx, bsa_du_bois)) %>%
  select(-bsa_du_bois) %>%
  
  # Merge with auc
  left_join(., areaUC, by = c("patientid", "drugname")) %>%
  mutate(interval_auc = abs(interval(start= auc_date, end= cycle_start_date)/ # use episode date to be more precise? No it's not better
                              duration(n=1, unit="days"))) %>% 
  mutate(interval_auc = case_when(
    (therapy == "Upfront Surgery" &
       interval_auc < 31 &
       # auc_date < lineenddate &
       auc_date > surgerydate)                  ~ interval_auc,
    
    therapy == "Upfront Chemo" &
      (
        (chemotherapy_type == "Neoadjuvant" &
           interval_auc < 31 &
           auc_date < surgerydate) |
          (chemotherapy_type == "Adjuvant" &
             interval_auc < 31 &
             # auc_date < lineenddate &
             auc_date > surgerydate)
      )                                          ~ interval_auc,
    TRUE                                         ~ NA_real_
  )) %>%
  arrange(interval_auc) %>% 
  distinct(patientid, episodedate, drugname, amount, linenumber_new, cycle_count_perline, cycle_increment, 
           cycle_start_date, .keep_all = TRUE) %>% 
  
  # Calculate expected dose
  mutate(expected_dose = case_when(
    drugname == "carboplatin"                 ~ target_auc * (CrCl + 25),
    drugname == "paclitaxel"                  ~ BSA * target_auc 
    # F19EBBFC60652 Can we fill target_auc? No depends of the type of cycle, or maybe by grouping by cycle type and drug
  )) %>% 
  mutate(expected_dose = case_when(
    expected_dose > 900 &
      target_auc == 6              ~ 900,
    expected_dose > 750 &
      target_auc == 5              ~ 750,
    TRUE                           ~ expected_dose
  )) %>% 
  mutate(adjusted_expected_dose = case_when(
    drugname == "carboplatin"                 ~ target_auc * (adjusted_CrCl + 25),
    drugname == "paclitaxel"                  ~ BSA * target_auc 
    # F19EBBFC60652 Can we fill target_auc? No depends of the type of cycle, or maybe by grouping by cycle type and drug
  )) %>% 
  mutate(adjusted_expected_dose = case_when(
    adjusted_expected_dose > 900 &
      target_auc == 6              ~ 900,
    adjusted_expected_dose > 750 &
      target_auc == 5              ~ 750,
    TRUE                           ~ adjusted_expected_dose
  )) %>% 
  group_by(patientid) %>%
  mutate(first_treatment_date = case_when(
    str_detect(therapy, "Chemo")                      ~ min(episodedate),
    str_detect(therapy, "Surgery")                    ~ surgerydate,
  )) %>% 
  ungroup() %>% 
  mutate(month_at_os_from_treatment = interval(start = first_treatment_date, end = followupdate)/
           duration(n=1, units = "months")) %>% 
  arrange(patientid, episodedate)

Treatment1 %>% distinct(patientid) %>% nrow()

# Create variables for filtering patients
Treatment <- Treatment1 %>% 
  filter(route == "Intravenous") %>% 
  # step 1: Flatiron recommendation cleanup
  # 90 days rule between diagnosis and treatment
  group_by(patientid) %>% 
  mutate(first_drug_date = first(episodedate)) %>% 
  ungroup() %>% 
  mutate(days_at_first_drug = interval(start = diagnosisdate, end = first_drug_date)/
           duration(n=1, units = "days")) %>% 
  mutate(days_at_surg = interval(start = diagnosisdate, end = surgerydate)/
           duration(n=1, units = "days")) %>% 
  mutate(days_at_first_treatment = case_when(
    therapy == "Upfront Chemo" |
      therapy == "Chemotherapy only"             ~ interval(start = diagnosisdate, end = first_drug_date)/
      duration(n=1, units = "days"),
    therapy == "Upfront Surgery" |
      therapy == "Surgery Only"                  ~ interval(start = diagnosisdate, end = surgerydate)/
      duration(n=1, units = "days")
  )) %>% 
  # 30 days rule excluding patient who didn’t survive for at least 30 days after diagnosis
  mutate(thirty_days_exclusion = interval(start = diagnosisdate, end = dateofdeath)/
           duration(n=1, units = "days")
  ) %>% 
  mutate(flatiron_rules = case_when(
    days_at_first_treatment > 90                 ~ "exclude",
    thirty_days_exclusion <= 30                  ~ "exclude",
    exclude == "1"                               ~ "exclude"
  )) %>%
  group_by(patientid) %>% 
  fill(flatiron_rules, .direction = "downup") %>% 
  ungroup() %>% 
  
  # step 2: Filter patients with clinical characteristics fitting to the study criteria
  # include only patients who received platin and/or taxane
  mutate(had_carbo_pacli = case_when(
    drugname == "paclitaxel" |
      drugname == "carboplatin"             ~ "Yes"
  )) %>% 
  group_by(patientid) %>% 
  fill(had_carbo_pacli, .direction = "updown") %>% 
  ungroup() %>% 
  # include patients with single or dual P/T +/- Bev as frontline treatment
  mutate(line_keep = case_when(# F002B429BE6C6
    (linenumber_adj == 1 &
       (linename == "Carboplatin,Paclitaxel" |
          linename == "Bevacizumab,Carboplatin,Paclitaxel" |
          linename == "Carboplatin" |
          linename == "Paclitaxel" |
          linename == "Bevacizumab,Paclitaxel" |
          linename == "Bevacizumab,Carboplatin"))            ~ NA_character_,
    linenumber_adj == 1                                       ~ "exclude"
  )) %>% 
  group_by(patientid) %>%
  fill(line_keep, .direction = "updown") %>% 
  ungroup() %>% 
  
  # step 3: Filter out patients with missing data
  # remove patients with long gap between surgery and adjuvant start (3 months)
  mutate(interval_surgery_to_adj_start = case_when(
    chemotherapy_type == "Adjuvant" &
      linenumber_adj == 1       
    ~ interval(start = surgerydate, end = linestartdate) / duration(n = 1, units = "months")
  )) %>% 
  mutate(gap_surg_drugs = case_when(
    interval_surgery_to_adj_start > 3                  ~ "exclude"
  )) %>% 
  group_by(patientid) %>% 
  fill(gap_surg_drugs, .direction = "updown") %>% 
  ungroup() %>% 
  
  # step 4: Include patients with treatments following the standard of clinical care
  # exclude patients with more than 1 line as Neo
  mutate(neo_more_more_than_1line = case_when(
    chemotherapy_type == "Neoadjuvant" &
      linenumber_new != 1                 ~ "exclude"
  )) %>% 
  group_by(patientid) %>% 
  fill(neo_more_more_than_1line, .direction = "updown") %>% 
  ungroup() %>% 
  # exclude patients with maintenance within their frontline
  mutate(frontline_has_maintenance = case_when(
    linenumber_adj == 1 &
      ismaintenancetherapy == "TRUE"             ~ "exclude"
  )) %>% 
  group_by(patientid) %>% 
  fill(frontline_has_maintenance, .direction = "updown") %>% 
  ungroup() %>% 
  # exclude patients with more than 6 cycles in Neo, more than 9 cycles total 
  # in upfront chemo or more than 9 cycles in upfront surgery
  mutate(too_many_cycle = case_when(
    (linenumber_new == 1 &
       therapy == "Upfront Chemo" &
       total_cycle_increment > 6) |
      
      (linenumber_new == 2 &
         therapy == "Upfront Chemo" &
         total_cycle_increment > 9) |
      
      (linenumber_new == 1 &
         therapy == "Upfront Surgery" &
         total_cycle_increment > 9)             ~ "exclude"
  )) %>% 
  group_by(patientid) %>%
  fill(too_many_cycle, .direction = "updown") %>%
  ungroup() %>% 
  # Exclude patients that received wrong dose dense of 1 carbo + 2 paclitaxels
  # Removes also the patients who has only carbo or only pacli
  # dose_dense will need to be changed if we change and include these patients
  arrange(patientid, episodedate, drugname) %>% 
  mutate(wrong_pacli_dosedense = case_when(
    drugname == "paclitaxel" &
      is.na(too_many_cycle) &
      (dose_dense == 2 |
         dose_dense > 3) &
      linenumber_adj == 1
    # ((linenumber_new == 1 &
    #  therapy == "Upfront Chemo") |
    # (linenumber_new == 2 &
    #    therapy == "Upfront Chemo") |
    # (linenumber_new == 1 &
    #    therapy == "Upfront Surgery"))
    ~ "exclude"
  )) %>% 
  group_by(patientid) %>%
  fill(wrong_pacli_dosedense, .direction = "updown") %>% 
  ungroup() %>% 
  # Exclude patients who switch drug within a line
  mutate(switch_drugs = case_when(
    str_detect(linename, "Pacli") & 
      drugname == "docetaxel" & 
      linenumber_adj == 1                      ~ "exclude",
    str_detect(linename, "Car") & 
      drugname == "cisplatin" & 
      linenumber_adj == 1                      ~ "exclude"
  )) %>% 
  group_by(patientid) %>%
  fill(switch_drugs, .direction = "updown") %>% 
  ungroup()



# group_by(patientid, wrong_pacli_dosedense) %>% 
# mutate(n_dose_dense = n()) %>% 
# 
# 
# 
# is.na(too_many_cycle)


Treatment %>% distinct(patientid) %>% nrow()

write_rds(Treatment, "Treatment.rds")






