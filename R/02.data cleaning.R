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
