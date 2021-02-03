#################################################################################################### I ### Basic Clinical Cleaning----
#################################################################################################### 1 ### Cleanup of creatinine----
creatinine <- creatinine %>% 
  filter(str_detect(labcomponent, "blood|serum")) %>% 
  mutate(creatinine = case_when(
    testresultcleaned < 0.8        ~ 0.8,
    testresultcleaned > 3          ~ NA_real_,
    TRUE                           ~ testresultcleaned
  )) %>% 
  # F092F8B827DDC use umol/L ?? range 45 to 90 μmol/L (0.5 to 1.0 mg/dL) for women.
  mutate(creatinine1 = case_when(
    testresult < 0.8               ~ 0.8,
    testresult > 3                 ~ NA_real_,
    TRUE                           ~ testresult
  )) %>% 
  mutate(creatinine = coalesce(creatinine, creatinine1)) %>% 
  filter(!is.na(creatinine)) %>% 
  mutate(testdate = as.Date(testdate, format = "%m/%d/%y")) %>% 
  select(c("patientid", creat_date = "testdate", "creatinine", creat_units = "testunitscleaned"))

# body_surface_area, height and weight are all coming from vitals
vitals <- vitals %>% 
  mutate(testdate = as.Date(testdate, format = "%m/%d/%y"))


#################################################################################################### 2 ### Cleanup of BSA----
body_surface_area <- vitals %>% 
  filter(labcomponent == "Body Surface Area (BSA)") %>% # only 399 patients data
  # Limit max = 4.14, min = 0.418
  mutate(testresult = ifelse(testresult < 0.418 | testresult > 4.14, NA_real_, testresult)) %>% 
  group_by(patientid) %>%
  mutate(median_testresult = median(testresult)) %>% 
  ungroup() %>% 
  mutate(BSA = case_when(
    testresult > (median_testresult + 0.4) |
      testresult < (median_testresult - 0.4)      ~ NA_real_,
    TRUE                                          ~ testresult
  )) %>% 
  mutate(BSA = coalesce(testresultcleaned, BSA)) %>% 
  filter(!is.na(BSA)) %>% 
  mutate(BSA_units = "m2") %>% 
  select(c("patientid", bsa_date = "testdate", "BSA", "BSA_units"))


#################################################################################################### 3 ### Cleanup of Height----
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
    !is.na(testresultcleaned_verified)          ~ round(testresultcleaned_verified / 100, 3), # cm to m
    testunits == "cm"                           ~ round(testresult_verified / 100, 3),
    testunits == "ft"                           ~ round(testresult_verified / 3.281, 3), # ft to m
    testunits == "in" | 
      !is.na(testresult)                        ~ round(testresult_verified / 39.37, 3) # in to m
    )) %>% 
  filter(!is.na(height)) %>% 
  mutate(height_units = "m") %>% 
  select(c("patientid", height_date = "testdate", "height", "height_units"))


#################################################################################################### 4 ### Cleanup of weight----
weight <- vitals %>%
  filter(test == "body weight") %>% 
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

# remove_outliers <- function(x, na.rm = TRUE, ...) { ##################### Too restrictive
#   qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
#   H <- 1 * IQR(x, na.rm = na.rm) # 1.5 remove non outliers
#   y <- x
#   y[x < (qnt[1] - H)] <- NA
#   y[x > (qnt[2] + H)] <- NA
#   y
# }

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
         testresult < (median_testresult - 99))      ~ NA_real_, # 91 for F00C16FEE3F74
    TRUE                                             ~ testresult
  )) %>%  # FCC755E4F8722 keep the first value anyway, eliminate that F8CB4CAB1073A (has 96 lbs difference-median) ?
  # mutate(cal = testresult-median_testresult)

  # 4. Rescue testunits when testresult_verified is close 
  # Calculate max and min to limit value for each patient-Choose 20
  group_by(patientid, testunits) %>%
  mutate(max_kg = case_when(
    testunits == "kg" ~ max(testresult_verified, na.rm = TRUE) + 20 # F181950A26A19
  )) %>%
  mutate(min_kg = case_when(
    testunits == "kg" ~ min(testresult_verified, na.rm = TRUE) -20
  )) %>%
  mutate(max_lbs = case_when(
    testunits == "lb" ~ max(testresult_verified, na.rm = TRUE) +30 # F14EBD0D0FA81
  )) %>%
  mutate(min_lbs = case_when(
    testunits == "lb" ~ min(testresult_verified, na.rm = TRUE) - 30
  )) %>%
  ungroup() %>% 
  group_by(patientid) %>% 
  fill(max_kg, min_kg, max_lbs, min_lbs, .direction = "downup") %>% # min_kg, max_lbs
  ungroup() %>%
  
  mutate(testunits_rescue = case_when(
    is.na(testunits) &
      testresult_verified < max_kg        ~ "kg", # Good doesn't work with F16847515CAF4 , Perfect for F8A3DD629CDA6
    is.na(testunits) & # FE8DBCAC6DAFB
      testresult_verified > min_lbs      ~ "lb" # F004D683A1695, F10CAD0F4EC48, FC109A6F719E7, FD25ED170CF07, FE06FF7C3E108, F044601199C5D
  ))

weight2 <- weight1 %>% 
  group_by(patientid) %>% 
  # 5. Rescue testunits when testresult_verified is twice or /2 means that lb or kg 
  # if is 2 times the weight in kg add lb F14EBD0D0FA81
  mutate(testunits_rescue1 = case_when(
    is.na(testunits) &
      testresult_verified < (max_kg * 2.205) &
      testresult_verified > (min_kg * 2.205)         ~ "lb",
    is.na(testunits) &
      testresult_verified < (max_lbs / 2.205) &
      testresult_verified > (min_lbs / 2.205)        ~ "kg"
  )) %>% 
  ungroup() %>% 
  mutate(testunits = coalesce(testunits, testunits_rescue, testunits_rescue1)) %>% 
  select(c("patientid", "testdate", "testunits", "testresult", "testresult_verified", "testunitscleaned", "testresultcleaned", "weight1"))

weight <- weight2 %>% 
  # 6.Fill up weight with new testresult_verified cleaned
  mutate(weight2 = case_when(
    is.na(testresultcleaned) &
      testunits == "kg"         ~ testresult_verified, # F0027D3926C88, FE8DBCAC6DAFB, F8A3DD629CDA6, F020C6A8B9E50, F16847515CAF4
    is.na(testresultcleaned) & # F044601199C5D
      testunits == "lb"         ~ (testresult_verified / 2.205), # FCB918617347C, F5E3D88009911, F14EBD0D0FA81, F16847515CAF4, FABC474931881
  )) %>% 
  mutate(weight = coalesce(weight1, weight2)) %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_units = "kg") %>% 
  select(c("patientid", weight_date = "testdate", "weight", "weight_units"))


rm(weight1, weight2, vitals)

#################################################################################################### 5 ### Cleanup of AUC----
areaUC <- auc %>% 
  filter(str_detect(drugname, "taxel|platin")) %>% 
  filter(!is.na(relativeorderedamount)) %>% 
  # select(c("patientid", "expectedstartdate", orderedamount, orderedunits, "relativeorderedamount",
  #          "relativeorderedunits", "drugname", iscanceled)) %>%
  mutate(target_auc = case_when( # F7D7377578248 what for the patients who have order amount
    # relativeorderedunits == "mg/kg" |
    #   relativeorderedunits == "mg" |
    #   relativeorderedunits == "m"         ~ NA_real_,
    str_detect(drugname, "taxel") &
      relativeorderedunits == "mg/m2" &
      relativeorderedamount >= 50 &
      relativeorderedamount <= 175        ~ relativeorderedamount, # F21904E0D872F we should open more? F11B625154B62
    str_detect(drugname, "platin") &
      relativeorderedunits == "AUC" &
      relativeorderedamount >= 2 &
      relativeorderedamount <= 6          ~ relativeorderedamount, # F4E8C8717878B, FE3FF3A5AC465, F853DC24CD5E8
    TRUE                                  ~ NA_real_
  )) %>%
  select(c("patientid", auc_date = "expectedstartdate", "target_auc", 
           auc_units = "relativeorderedunits", "drugname")) %>% 
  filter(!is.na(target_auc))


#################################################################################################### II ### Clanical Cleaning----
which(duplicated(clinical_data$patientid))



which(duplicated(drugs1))
a <- (unique(drugs1$patientid))

#################################################################################################### II ### Treatment Cleaning----
# considered maintenance therapy : bevacizumab , olaparib, rucaparib, niraparib, gemcitabine
# Just use taxel and platin for now
# Remove maintenance
drugs1 <- drugs %>% 
  filter(episodedatasource == "Administrations" & ismaintenancetherapy == "FALSE" & !is.na(amount)) %>% 
  filter(str_detect(drugname, "taxel|platin") & str_detect(route, "venous|peritoneal")) %>% 
  select(-c(ismaintenancetherapy, enhancedcohort, episodedatasource, drugcategory, detaileddrugcategory, route)) %>%
  # combined multiple amount of drug given the same day FCF86329BB40C
  group_by(patientid, linename, linenumber, linestartdate, lineenddate,episodedate, drugname, units) %>%
  summarise_at(vars(amount), paste, collapse = ";") %>% # don't do sum ti keep the multiple administration in df
  separate(col= amount, paste("amount_", 1:10, sep=""), sep = ";", extra = "warn",
           fill = "right") %>%
  purrr::keep(~!all(is.na(.))) %>%
  ungroup() %>% 
  mutate(amount_1 = as.numeric(amount_1), amount_2 = as.numeric(amount_2),
         amount_3 = as.numeric(amount_3), amount_4 = as.numeric(amount_4)) %>%
  mutate(amount = rowSums(select(.,amount_1:amount_4), na.rm = TRUE)) %>% 
  # bind with clinical
  full_join(., clinical_data %>% select(c("patientid", "ageatdx", "issurgery", "surgerydate")), # , "extentofdebulking", "debulking", "diff_surg_dx"
            by = "patientid") %>% 
  # Limit to the patients we have date of surgery when had surgery
  filter(!(issurgery == "Yes" & is.na(surgerydate)))

therapy <- drugs1 %>% 
  mutate(chemotherapy_type = case_when(
    # ismaintenancetherapy == "TRUE"    ~ "maintenance",
    episodedate <= surgerydate        ~ "Neoadjuvant", # F63ABD6AEB12C, F4F7B1C5DF24F intraperitoneal Carboplatin
    episodedate > surgerydate         ~ "Adjuvant",
    issurgery == "No/unknown"         ~ "Chemotherapy only",
    issurgery == "Yes" &
      is.na(surgerydate)              ~ "Surgery date unknown"
  )) %>% 
  # arrange(desc(chemotherapy_type)) %>% 
  mutate(chemotherapy_type = factor(chemotherapy_type, levels = c("Neoadjuvant", "Adjuvant", "Chemotherapy only", "Surgery date unknown"))) %>%
  group_by(patientid) %>% 
  
  mutate(chemo = case_when(
    chemotherapy_type == "Neoadjuvant"          ~ "Upfront Chemo",
    issurgery == "No/unknown" &
      !is.na(episodedate)                       ~ "Chemotherapy only"
    )) %>% 
  fill(chemo, .direction = "downup") %>% 
  
  mutate(surg = case_when(
    is.na(chemo) & 
      issurgery == "Yes" &
      chemotherapy_type == "Adjuvant"           ~ "Upfront Surgery",
    chemo != "Upfront Chemo" & 
      issurgery == "Yes" &
      is.na(episodedate)                        ~ "Surgery only"
  )) %>% 
  fill(surg, .direction = "downup") %>% 
  mutate(therapy = coalesce(chemo, surg)) %>% 
  select( -surgerydate) %>% 
  # Redefine line number based on the original linenumber variable and the surgery date
  mutate(linenumber_new = dense_rank(interaction(linenumber, chemotherapy_type))) %>%  # F1C68CFAC2AF3, FBCF69031DFF8, F0040EA299CF0, # F0000AD999ABF, FAACDC7205803, F95D860690A93 still weird FA019D2071321
  ungroup() %>% 
  
  # Add a 30 day rule to switch linenumber
  group_by(patientid, linenumber_new) %>% 
  arrange(episodedate) %>% 
  mutate(episode_interval = episodedate - lag(episodedate), episode_interval = ifelse(is.na(episode_interval), 0, episode_interval)) %>%  # FDB58481AFFBB 48 days
  mutate(jump_line = ifelse(episode_interval > 30, 1, 0)) %>%
  # mutate(jump_line = ifelse(is.na(jump_line), 0, jump_line)) %>% 
  ungroup() %>% 
  group_by(patientid) %>% 
  mutate(jump_line = cumsum(jump_line)) %>% 
  
  # fill(jump_line, .direction = "down") %>% 
  mutate(linenumber_new1 = linenumber_new + jump_line) %>% # F002DD2953889 problem when increase multiple time 
  #F002DD2953889 need to do 37 and 42 F0040EA299CF0, 35 F005602DA4DF0 => do at least 37
  
  # mutate(jump_line = factor(jump_line, levels = c("same_line", "jump_line"))) %>% 
  # mutate(linenumber_new1 = dense_rank(interaction(linenumber_new, jump_line))) %>% 
  # select(-c("amount","units", "ageatdx","issurgery", "surg","therapy"))
  
  
  
  
  
  
  
  # Calculate cycle for each line
  group_by(patientid, linenumber_new, drugname) %>% # Wrong line name F001443D8B85C ~~~~~~~~~~~~~~~~~~~~~~~~~~ Fix later QUESTION
  mutate(drugname_count_perline = n()) %>%  # F0000AD999ABF
  ungroup()

# plot mediacation as fromtline, pie?
# Next bind the dose from orders to get AUC

therapy1 <- therapy %>% 
  # mutate(is_taxel = ifelse(str_detect(drugname, "taxel"), "taxel", NA_character_)) %>% 
  # mutate(is_platin = ifelse(str_detect(drugname, "platin"), "platin", NA_character_)) %>% 
  select(-c("linenumber", "surg")) %>%
  arrange(episodedate) %>% 
  group_by(patientid, linenumber_new) %>%
  mutate(cycle_count_perline = min(drugname_count_perline)) %>% 
  mutate(cycle_drugname = case_when(
    cycle_count_perline == drugname_count_perline          ~ drugname,
    TRUE                                                   ~ NA_character_
  )) %>% 
  # select(-drugname_count_perline) %>% 
  group_by(patientid, linenumber_new, cycle_drugname) %>%
  mutate(cycle_inc = row_number(cycle_drugname)) %>% 
  group_by(patientid, linenumber_new, cycle_inc) %>%
  # mutate(cycle_count_1 = row_number(cycle_inc)) %>% 
  # mutate(cycle_c = ifelse(cycle_count_1 == 1, cycle_inc, NA_real_)) %>%  # Problem for F001443D8B85C (make a code before recoding linenumber_new like if drugname is not in the linename remove linename)
  arrange(patientid, episodedate, cycle_inc) %>% 
  group_by(patientid, linenumber_new) %>%
  mutate(cycle_increment = cycle_inc) %>% 
  fill(cycle_increment, .direction = "downup") %>%  # make sure I can do up for have one new drug compare to the linename FBB5186A15CD9, FE593EDF5586A
  # cycle date
  group_by(patientid, linenumber_new, cycle_increment) %>% 
  mutate(cycle_date = min(episodedate))

rm(drugs, drugs1, therapy)

# What to do for F4E6EE1910940? Got carb twice the same day with low dose. May be 205 patients row like that

#################################################################################################### III ### Merging----
# For each feature, binding with the variable, calculate an interval between cycle date and feature dates, 
# select the smallest interval
Treatment <- left_join(therapy1, creatinine, by = "patientid") %>%
  mutate(interval = abs(interval(start= creat_date, end= cycle_date)/                      
                          duration(n=1, unit="days"))) %>% 
  arrange(interval) %>% 
  distinct(patientid, episodedate, drugname, amount, linenumber_new, cycle_count_perline, cycle_increment, 
           cycle_date, .keep_all = TRUE) %>% 
  
  # Merge with height
  left_join(. , height,  by = "patientid") %>%
  mutate(interval = abs(interval(start= height_date, end= cycle_date)/                      
           duration(n=1, unit="days"))) %>% 
  arrange(interval) %>% 
  distinct(patientid, episodedate, drugname, amount, linenumber_new, cycle_count_perline, cycle_increment, 
           cycle_date, .keep_all = TRUE) %>% 
  
  # Merge with weight
  left_join(. , weight, by = "patientid") %>%
  mutate(interval = abs(interval(start= weight_date, end= cycle_date)/                      
                          duration(n=1, unit="days"))) %>% 
  arrange(interval) %>% 
  distinct(patientid, episodedate, drugname, amount, linenumber_new, cycle_count_perline, cycle_increment, 
           cycle_date, .keep_all = TRUE) %>% 
  # mutate(bmi = height / (weight * weight)) %>% 
  
  # Calculate CrCl
  mutate(CrCl = ((140 - ageatdx) * weight)/((72 * creatinine) * 0.85)) %>%
  
  # Merge with body_surface_area
  left_join(., body_surface_area, by = "patientid") %>%
  mutate(interval = abs(interval(start= bsa_date, end= cycle_date)/                      
                          duration(n=1, unit="days"))) %>% 
  arrange(interval) %>% 
  distinct(patientid, episodedate, drugname, amount, linenumber_new, cycle_count_perline, cycle_increment, 
           cycle_date, .keep_all = TRUE) %>% 
  # Calculate more bsa value with height and weight using Du Bois formula
  mutate(bsa_du_bois = 0.007184 * ((height*100)^0.725) * (weight^0.425)) %>% # get pretty close results 4269 patients result
  mutate(BSA = coalesce(BSA, bsa_du_bois)) %>% 
  select(-bsa_du_bois) %>% 

  # Merge with auc
  left_join(., areaUC, by = c("patientid", "drugname")) %>%
  mutate(interval = abs(interval(start= auc_date, end= cycle_date)/ # use episode date to be more precise? No it's not better
                          duration(n=1, unit="days"))) %>% 
  arrange(interval) %>% 
  distinct(patientid, episodedate, drugname, amount, linenumber_new, cycle_count_perline, cycle_increment, 
           cycle_date, .keep_all = TRUE) %>% 
  select(-interval) %>% 
  
  # Calculate expected dose
  mutate(expected_dose = case_when(
    drugname == "carboplatin"                 ~ target_auc * (CrCl + 25),
    str_detect(drugname, "taxel|platin")      ~ BSA * target_auc # F19EBBFC60652 Can we fill target_auc?
  )) %>% 
  mutate(expected_dose = case_when(
    expected_dose > 900 &
      target_auc == 6              ~ 900,
    expected_dose > 750 &
      target_auc == 5              ~ 750,
    TRUE                           ~ expected_dose
  )) %>% 
  arrange(patientid, episodedate)



#################################################################################################### III ### Merging----
Frontline <- Treatment %>% 
  # filter(therapy == "Upfront Chemo" | therapy == "Chemotherapy only") %>% 
  mutate(relative_dose_intensity = round(amount/expected_dose, 3)) %>% 
  mutate(RDI_grp = as.factor(findInterval(relative_dose_intensity, c(0.75, 0.85, 1, 1.5, 2) ))) %>% 
  mutate(RDI_grp = factor(RDI_grp, 
                          levels = c("0", "1","2","3", "4", "5"), 
                          labels = c("0 < RDI < 0.75", "0.75 <= RDI < 0.85", "0.85 >= RDI < 1", "1 <= RDI < 1.5", 
                                     "1.5 <= RDI < 2", "RDI >= 2")))


table(Frontline$RDI_grp)

# Fix problem with amount different from mg



