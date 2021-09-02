#################################################################################################### I ### Basic Clinical Cleaning----
#################################################################################################### 1 ### Cleanup of creatinine----
creatinine <- creatinine %>%
  # remove creatine urine
  filter(str_detect(labcomponent, "blood|serum")) %>% 
  mutate(creatinine = case_when(
    testresultcleaned < 0.8        ~ 0.8,
    testresultcleaned > 3          ~ NA_real_, # F006DBE0C7A5E is an example of slow increase past 3, F641A8DC820C7 super high
    TRUE                           ~ testresultcleaned
  )) %>% 
  # F092F8B827DDC kept umol/L as they actually are mg/dL range 45 to 90 μmol/L (0.5 to 1.0 mg/dL) for women.
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
  mutate(testdate = as.Date(testdate, format = "%m/%d/%y")) %>% 
  select(-c(loinc, test, testbasename, resultdate, minnorm, maxnorm, minnormcleaned, maxnormcleaned))


#################################################################################################### 2 ### Cleanup of BSA----
body_surface_area <- vitals %>% 
  filter(labcomponent == "Body Surface Area (BSA)") %>% # only 399 patients data
  # Limit max = 4.14, min = 0.418 calculated for biggest to smallest person
  mutate(testresult = ifelse(testresult < 0.418 | testresult > 4.14, NA_real_, testresult)) %>% 
  group_by(patientid) %>%
  # I used the median to remove outlier within a patient
  mutate(median_testresult = median(testresult)) %>% 
  ungroup() %>% 
  mutate(BSA = case_when(
    testresult > (median_testresult + 0.4) |
      testresult < (median_testresult - 0.4)      ~ NA_real_, # Checked all, removeing 0.3 would be bad
    TRUE                                          ~ testresult
  )) %>% 
  mutate(BSA = coalesce(testresultcleaned, BSA)) %>% 
  filter(!is.na(BSA)) %>% 
  mutate(BSA_units = "m2") %>% 
  select(c("patientid", bsa_date = "testdate", "BSA", "BSA_units"))


#################################################################################################### 3 ### Cleanup of Height----
height <- vitals %>% 
  filter(labcomponent == "Body Height") %>% 
  # 1. Clean up outliers compare to tallest ans smallest record
  # testresultcleaned has all testunitscleaned associated with value
  mutate(testresultcleaned = case_when(
    testresultcleaned > 231 | # Tallest women height
      testresultcleaned < 120 # Average height dwarf = 122 cm, 4 feet, I take 120 by looking at the data
                                     ~ NA_real_,
    TRUE                             ~ testresultcleaned
  )) %>% 
  
  # To get more data, I have data from testresult that can be used
  # also have missing testunits F004D683A1695 
  # fill up testunits within patients before removing outliers           then transfer in kg and coalesce with testresultcleaned.
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
      testresult > 231 & is.na(testunits)
                                     ~ NA_real_,
    TRUE                             ~ testresult
  )) %>% 
  
  # 2. Clean up outliers within patient by using the median for each patients/unit. Eliminate the ones with too much variation.
  group_by(patientid) %>%
  mutate(mean_testresultcleaned = mean(testresultcleaned)) %>% # FEBB819DF1D8F 6.2, FCC755E4F8722 - 8, F45BCB24511B8 - 12, F02BBE6E30D4A 18
  ungroup() %>% # F5B1E2D059869 24, remove FAC12EC567E3E = 35 FF683865721AE 38
  mutate(testresultcleaned_verified = case_when(
    testresultcleaned > (mean_testresultcleaned + 23) | # choose by looking at data
      testresultcleaned < (mean_testresultcleaned - 23)      ~ NA_real_, # F355C8B80BFE9 50% height 124 50% 144cm
    TRUE                                                      ~ testresultcleaned
  )) %>% 
  # select(c("patientid", testdate, testresultcleaned, mean_testresultcleaned, testresultcleaned_verified, 
  #          testunitscleaned, testunits, testresult)) %>% 
  
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
  # select(c("patientid", testdate, testresult, mean_testresult, testresult_verified,
  #          testunits, testunitscleaned, testresultcleaned, testresultcleaned_verified))
  
  mutate(height = case_when(
    !is.na(testresultcleaned_verified)          ~ round(testresultcleaned_verified / 100, 3), # cm to m
    testunits == "cm"                           ~ round(testresult_verified / 100, 3),
    testunits == "ft"                           ~ round(testresult_verified / 3.281, 3), # ft to m
    testunits == "in" | 
      !is.na(testresult)                        ~ round(testresult_verified / 39.37, 3) # in to m # FEAEBCA68FF6B F55CB96781E07 has NA
    )) %>% 
  filter(!is.na(height)) %>% 
  mutate(height_units = "m") %>% 
  select(c("patientid", height_date = "testdate", "height", "height_units"))


#################################################################################################### 4 ### Cleanup of weight----
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
  # mutate(difference = (median_testresultcleaned)-testresultcleaned) # Can improve by > 30 and > 10 from previous measure so need to create a gap var ---------
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
      testresult < 10286          ~ testresult, # combine oz, kg, lbs for when units is NA
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
  mutate(testresult_verified = case_when(       # Can improve by > 30 and > 10 from previous measure so need to create a gap var --------
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
  # mutate(difference = (median_testresultcleaned)-testresultcleaned)

  # 4. Rescue testunits when testresult_verified is close 
  # Calculate max and min to limit value for each patient-Choose 20
  group_by(patientid, testunits) %>%
  mutate(max_kg = case_when(
    testunits == "kg" ~ max(testresult_verified, na.rm = TRUE) + 20 # F181950A26A19
  )) %>%
  mutate(min_kg = case_when(
    testunits == "kg" ~ min(testresult_verified, na.rm = TRUE) - 20
  )) %>%
  mutate(max_lbs = case_when(
    testunits == "lb" ~ max(testresult_verified, na.rm = TRUE) + 30 # F14EBD0D0FA81
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
# Clean orderdrug for duplicated rows
orderdrug <- orderdrug %>% 
  filter(str_detect(drugname, "taxel|platin") & str_detect(route, "venous|peritoneal")) %>%
  select("patientid", "orderid", "drugname", "administereddate", "administeredamount", "administeredunits") %>%
  # 4 patients have duplicated order, need to keep the ones which are not NA 
  #  "MB280CD7F67AB09BDA03634F0B5780C28" "M93C0522CBC4B707B94E8B12B1E514E80" "M7C34D0B21D0C17E772EB31EADA398966" "M540AD166359C2FD4C7DEA9EB5F39116B"
  group_by(patientid, orderid) %>% 
  mutate(orderid_duplicated = n()) %>% 
  filter(orderid_duplicated == 1 | (orderid_duplicated == 2 & !is.na(administeredamount))) %>% select(-orderid_duplicated)
  # remove dupl id, id, dose?

# orderdrug$orderid[duplicated(orderdrug$orderid)] # This duplicate is ok as it was both given to the patient
# which(duplicated(orderdrug$orderid))













# Rescue some of the auc from the drugs/order --------------------- Needs to do it after cleaning auc itself
drugs_order_episode <- drugs %>% 
  # 1. Filtering----
  filter(episodedatasource == "Orders" & str_detect(drugname, "taxel|platin") & 
           str_detect(route, "venous|peritoneal") & str_detect(units, "AUC|mg/m2") &
           !is.na(amount) &
           ((str_detect(drugname, "platin") & units == "AUC") | (str_detect(drugname, "taxel") & units == "mg/m2"))
  )

merged <- left_join(areaUC %>% 
                      select(c("patientid", "orderid", "ordereddate", "expectedstartdate", "orderedamount", "orderedunits", "relativeorderedamount",
                               "relativeorderedunits", "drugname")),
                    drugs_order_episode %>% 
                      select("patientid", "drugname", "episodedate", "amount", "units"), 
                    by = c("patientid", "drugname", "expectedstartdate" = "episodedate")) %>% 
  # Create few duplicate order id but they all already have a relativeorderedamount value so can use distinct
  distinct(orderid, .keep_all = TRUE) %>% 
  filter(is.na(relativeorderedamount))
  mutate(relativeorderedamount = coalesce(relativeorderedamount, amount)) %>% 
  select(c("patientid", "orderid", "ordereddate", "expectedstartdate", "orderedamount", "orderedunits", "relativeorderedamount",
           "relativeorderedunits", "drugname"))
# WAS IT WORTH? HOW MANY DID WE RESCUE? only 34 but look good

rm(drugs_order_episode)














areaUC <- auc %>% 
  filter(str_detect(drugname, "taxel|platin") | is.na(iscanceled)) %>% 
  select(c("patientid", "orderid", "ordereddate", "expectedstartdate", "orderedamount", "orderedunits", "relativeorderedamount",
           "relativeorderedunits", "drugname")) %>%
  # 1. bind with orderdrug to get only the order administered, no duplicated order and more relativeorderedamount
  inner_join(., orderdrug, 
            by = c("patientid", "orderid", "drugname")) %>% 
  # which(areaUC$expectedstartdate != areaUC$administereddate)
  # Sometimes we have orderamount but no administeredamount or amount (episode file)
  # 2. rescue relativeorderedamount with orderedamount
  mutate(relativeorderedamount = case_when(
    is.na(relativeorderedamount) &
      str_detect(orderedunits, "AUC") &
      orderedamount < 7                          ~ coalesce(relativeorderedamount, orderedamount),
    is.na(relativeorderedamount) &
      str_detect(orderedunits, "mg/m2")          ~ coalesce(relativeorderedamount, orderedamount),
    TRUE                                         ~ relativeorderedamount
    )) %>% 
  
  # 3. rescue relativeorderedamount by calculating it from orderedamount in mg for taxels and cisplatin
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
  
  # Calculate bmi
  mutate(bmi = weight / (height * height)) %>% 
  mutate(bmi_cat = case_when(
    bmi < 25                    ~ "Underweight and normal weight",
    bmi >= 25 &
      bmi < 30                  ~ "Overweight",
    bmi >= 30                   ~ "Obese"
  )) %>% 
  mutate(bmi_cat = factor(bmi_cat, levels = c("Underweight and normal weight", "Overweight", "Obese"))) %>% 
  # # Calculate CrCl
  # mutate(CrCl = ((140 - ageatdx) * weight)/((72 * creatinine) * 0.85)) %>%
  
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
  # Finally
  mutate(relativeorderedamount_calculated = case_when(
    orderedunits == "mg" &
      str_detect(drugname, "taxel|cisplatin")  ~ orderedamount/BSA
  )) %>% select(patientid, orderid, orderedamount, relativeorderedamount, relativeorderedamount_calculated, BSA, drugname, everything()) %>%
  filter(!is.na(relativeorderedamount_calculated) | !is.na(relativeorderedamount)) %>% 
  mutate(relativeorderedamount1 = coalesce(relativeorderedamount, relativeorderedamount_calculated))

  
  
# F7B7E571CFE6B have auc in orderdrug
# F60A6EF7DF122 not in orderdrug when canceled
# FFA932C16A7B2 2013-04-04
  # F093EE997F30F got 485 = 6 only once 

  # select(-orderedamount) %>% 
areaUC1 <- areaUC %>%   
  # 2. multiple order for the same expectedstartdate. Sometimes different dose -> need to keep and combine.
  # somtimes same dase ordered at same/different date -> remove these duplicates # F0000AD999ABF
  # group_by("patientid", "expectedstartdate", "orderedamount", "orderedunits", "relativeorderedamount", # Don't add orderdate or orderid in the grouping
  #          "relativeorderedunits", "drugname.x", "quantity", "quantityunits") %>%
  distinct(patientid, orderedamount, relativeorderedamount, relativeorderedamount_calculated, BSA, drugname, ordereddate, # Cleaning will depend which auc we prioritize F3ABAE4D533AF
           # 2018-01-11
           # paclitaxel
           # 366;175
           # 2018-01-11;2018-01-11
           # mg;mg/m2
           # 188.482431175146;175
           # NA;NA
           expectedstartdate, orderedunits, relativeorderedunits, administereddate, administeredamount, administeredunits,
           height_date, height, height_units, weight_date, weight, weight_units, bmi, bmi_cat, bsa_date, BSA_units) %>% # PB cannot do distinct if quantity are different F8DAD4C161E58 2017-09-26 -> need rescue relativeorderedamount first
  
  group_by(patientid, expectedstartdate, drugname) %>%
  summarise_at(vars(orderedamount, ordereddate, orderedunits, relativeorderedamount, relativeorderedunits),
               paste, collapse = ";") %>% # don't do sum to keep the multiple administration in df # F239BA21D42D2
  # separate(col= amount, paste("amount_", 1:10, sep=""), sep = ";", extra = "warn",
  #          fill = "right") %>%
  purrr::keep(~!all(is.na(.))) %>%
  ungroup() %>%
  
  
  
  # mutate(relativeorderedamount_cal = case_when(
  #   drugname == "carboplatin" & quantityunits == "ml"         ~ (10 * quantity) / (Crcl + 25)),    # Need to be verified + what for "each"------------------
  #   drugname == "cisplatin" & quantityunits == "ml"           ~ (BSA * 1* quantity),
  #   str_detect(drugname, "taxel") & quantityunits == "ml"     ~ (BSA * 1* quantity)
  # ) %>% 
  # mutate(relativeorderedamount = coalesce(relativeorderedamount, relativeorderedamount_cal)) %>% 
  # select(-relativeorderedamount_cal) %>% 
           
  
  
  # filter(!is.na(relativeorderedamount)) %>% 
  mutate(target_auc = case_when( # F7D7377578248 what for the patients who have order amount
    # relativeorderedunits == "mg/kg" |
    #   relativeorderedunits == "mg" |
    #   relativeorderedunits == "m"         ~ NA_real_,
    str_detect(drugname, "taxel|platin") & # include all taxel + cisplatin + oxaliplatin
      relativeorderedunits == "mg/m2" &
      relativeorderedamount >= 50 &
      relativeorderedamount <= 175        ~ relativeorderedamount, # F21904E0D872F we should open more? F11B625154B62
    str_detect(drugname, "carboplatin") &
      relativeorderedunits == "AUC" &
      relativeorderedamount >= 2 &
      relativeorderedamount <= 6          ~ relativeorderedamount, # F4E8C8717878B, FE3FF3A5AC465, F853DC24CD5E8
    TRUE                                  ~ NA_real_
  )) %>%
  select(c("patientid", auc_date = "expectedstartdate", "target_auc", 
           auc_units = "relativeorderedunits", "drugname")) %>% 
  filter(!is.na(target_auc))


#################################################################################################### II ### Clinical Cleaning----
clinical_data <- clinical_data %>% 
  mutate(raceeth = factor(raceeth, levels = c("NHWhite", "NHBlack", "Hispanic", "Other"))) %>% 
  mutate(race = case_when(
    str_detect(race, "Black")    ~ "Black",
    str_detect(race, "Hispanic") ~ "White",
    str_detect(race, "Other")    ~ "Other",
    TRUE                         ~ race
  )) %>% 
  mutate(ethnicity = (str_remove(ethnicity, " or Latino"))) %>% 
  mutate(new_eth = case_when(
    is.na(ethnicity) &
      is.na(race)              ~ "Other or Uk",
    is.na(ethnicity) &
      !is.na(race)             ~ "NH",
    ethnicity == "Hispanic"    ~ "Hispanic"
  )) %>% 
  unite(new_raceeth, c(new_eth, race), sep = " ", remove = FALSE, na.rm = TRUE) %>% 
  mutate(new_raceeth1 = ifelse(str_detect(new_raceeth, "Black|Hispanic"), "Minority", new_raceeth)) %>%  # min = Asian Hisp
  # Lauren recoded followupdate to have the days of death with Flatiron rules
  mutate(dateofdeath = case_when(
    vitalstatus == 1 ~ followupdate,
    TRUE             ~ NA_Date_
  )) %>% 
  mutate(vital = case_when(
    vitalstatus == 0     ~ "Alive",
    vitalstatus == 1     ~ "Dead"
  )) %>% 
  # mutate(aprox_month_at_os = followuptime/30.417) %>% 
  mutate(month_at_os = interval(start = diagnosisdate, end = followupdate)/
           duration(n=1, units = "months")) %>% 
  mutate(stagecat = na_if(stagecat, "Unk Stage")) %>% 
  mutate(across(.cols = c(histology, groupstage, tstage), ~na_if(., "Unknown/not documented")))

# ? Where does Asian Hispanic go, Black hispanic, NA race and H eth?
# Sweta said to remove Asian but what for Hispanic Asian?
# a <- clinical_data %>% select(patientid, raceeth, race, ethnicity, new_eth, new_raceeth, new_raceeth1)




#################################################################################################### II ### Treatment Cleaning----
# considered maintenance therapy : bevacizumab , olaparib, rucaparib, niraparib, gemcitabine
# Just use taxel and platin for now
# Precleaning
drugs1 <- drugs %>% 
  # 1. Filtering----
  filter(episodedatasource == "Administrations" & ismaintenancetherapy == "FALSE" & !is.na(amount)) %>%
  # filter(!(linenumber == 1 & !str_detect(linename, "taxel|platin"))) %>% When want to remove patients who didn't have taxel/platin as 1st line -> remove 260 patients
  filter(str_detect(drugname, "taxel|platin") & str_detect(route, "venous|peritoneal")) %>% 
  select(-c(ismaintenancetherapy, enhancedcohort, episodedatasource, drugcategory, detaileddrugcategory, route)) %>%
  # 2. Combined Multiple Administrations----
  # combined multiple amount of drug given the same day FCF86329BB40C F4E6EE1910940.May be 205 patients row like that
# There are a small number of patients with multiple records in the MedicationAdministration table for the same drug on the same day. We reviewed several instances where this occurs in the original EMR records and found, in each case, legitimate clinical reasons substantiating the multiple administrations per day. Although these instances were legitimate medication administrations, Flatiron Health data are generated from real-world clinical practice and are subject to miscoding and errors by the clinical oncology team, as are all EMR data. Below are three patient vignettes taken from our review of EMR notes, illustrating the variety of reasons why this may occur.
# For clinical reasons, a patient was given higher than normal dose which was listed as two administrations of the normal dose.
# Three administrations of the same drug were listed on a Friday. The EMR note stated that one dose was administered in the office and the patient was sent home with the two remaining doses for the weekend. 
# Patient had an allergic reaction in the past and was administered multiple smaller doses in the same day as part of a desensitization protocol.
  group_by(patientid, linename, linenumber, linestartdate, lineenddate,episodedate, drugname, units) %>%
  summarise_at(vars(amount), paste, collapse = ";") %>% # don't do sum ti keep the multiple administration in df
  separate(col= amount, paste("amount_", 1:10, sep=""), sep = ";", extra = "warn",
           fill = "right") %>%
  purrr::keep(~!all(is.na(.))) %>%
  ungroup() %>% 
  mutate(amount_1 = as.numeric(amount_1), amount_2 = as.numeric(amount_2),
         amount_3 = as.numeric(amount_3), amount_4 = as.numeric(amount_4)) %>%
  mutate(amount = rowSums(select(.,amount_1:amount_4), na.rm = TRUE)) %>%
  # Found ug, ml, Ea, AUC and NA they are all good, but the AUC & < at 10 are wrong and ug need conversion
  mutate(amount =  case_when(
    units == "ug"               ~ amount / 10, # I checked all of them
    units == "AUC" &
      amount < 10               ~ NA_real_,
    TRUE                        ~ amount
  )) %>% 
  
  mutate(drugname_type = case_when(
    str_detect(drugname, "platin")       ~ "platin",
    str_detect(drugname, "taxel")        ~ "taxel"
  )) %>% 

  # 3.bind with clinical----
  full_join(., clinical_data %>% select(-c(primaryphysicianid)),
            by = "patientid") %>% 
  # Limit to the patients we have date of surgery when had surgery
  filter(!(issurgery == "Yes" & is.na(surgerydate)))

therapy <- drugs1 %>% 
  # 4. Define Adjuvant, Neoadjuvant, chemo or surgery----
  mutate(chemotherapy_type = case_when(
    # ismaintenancetherapy == "TRUE"    ~ "maintenance",
    episodedate <= surgerydate        ~ "Neoadjuvant", # F63ABD6AEB12C, F4F7B1C5DF24F intraperitoneal Carboplatin
    episodedate > surgerydate         ~ "Adjuvant",
    issurgery == "No/unknown" &
      !is.na(episodedate)             ~ "Chemotherapy only",
    issurgery == "Yes" &
      is.na(episodedate)              ~ "Surgery only"
    )) %>% 
  mutate(chemotherapy_type = factor(chemotherapy_type, levels = c("Neoadjuvant", "Adjuvant", "Chemotherapy only", "Surgery only"))) %>%
  group_by(patientid) %>% 
  mutate(had_neo = ifelse(str_detect(chemotherapy_type, "Neo"), "Yes", NA_character_)) %>% 
  fill(had_neo, .direction = "updown") %>% 
  mutate(had_adj = ifelse(str_detect(chemotherapy_type, "Adj"), "Yes", NA_character_)) %>% 
  fill(had_adj, .direction = "updown") %>% 
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
  # mutate(therapy1 = case_when(
  #   chemotherapy_type == "Adjuvant"              ~ "Upfront Surgery",
  # )) %>% 
  # fill(therapy1, .direction = "downup") %>% 
  # mutate(therapy = coalesce(therapy, therapy1)) %>% 
  # select( -therapy1) %>% 
  
  # 5.Redefine line number based on the original linenumber variable and the surgery date----
  mutate(linenumber_new = dense_rank(interaction(linenumber, chemotherapy_type))) %>%  # F0000AD999ABF, F1C68CFAC2AF3, FBCF69031DFF8, F0040EA299CF0, FAACDC7205803,  FA019D2071321 no has big jump F95D860690A93 still weird
  select(patientid, linename, linenumber, linenumber_new, drugname, episodedate, surgerydate, chemotherapy_type, everything()) %>% 
  ungroup() %>% 
  
  # 6.Add a 30 day rule to switch linenumber----
  group_by(patientid, linenumber_new) %>% 
  arrange(episodedate) %>% 
  mutate(episode_interval = episodedate - lag(episodedate), episode_interval = ifelse(is.na(episode_interval), 0, episode_interval)) %>%  # FDB58481AFFBB 48 days
  mutate(jump_line = ifelse(episode_interval > 31, 1, 0)) %>% # FA019D2071321 >31
  ungroup() %>% 
  # group_by(patientid) %>% 
  # mutate(jump_line = cumsum(jump_line)) %>% # F002DD2953889 fixed even when increase multiple time 
  # mutate(linenumber_new = linenumber_new + jump_line) %>% 
  #F002DD2953889 need to do 37 and 42 F0040EA299CF0, 35 F005602DA4DF0 => do at least 37
  
  # 7.Calculate cycle for each line----
  mutate(cycle_drugname = str_remove(drugname, " protein-bound")) %>%  # F003F2BEEDB37 need to change drugname pacli = pacli bp
  group_by(patientid, linenumber_new, cycle_drugname) %>% # Wrong line name F001443D8B85C ~~~~~~~~~~~~~~~~~~~~~~~~~~ Fix later QUESTION Dr Chern is ok
  mutate(drugname_count_perline = n()) %>%  # F0000AD999ABF
  ungroup()


# plot mediacation as fromtline, pie?
# Next bind the dose from orders to get AUC

therapy1 <- therapy %>% 
  select(-c("linenumber")) %>%
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
  

  # + What to do when it is a cycle change F003F2BEEDB37 , 
  
  
  group_by(patientid, linenumber_new, cycle_inc) %>%
  # mutate(cycle_count_1 = row_number(cycle_inc)) %>% 
  # mutate(cycle_c = ifelse(cycle_count_1 == 1, cycle_inc, NA_real_)) %>%  # Problem for F001443D8B85C (make a code before recoding linenumber_new like if drugname is not in the linename remove linename)
  arrange(patientid, episodedate, cycle_inc) %>% 
  group_by(patientid, linenumber_new) %>%
  mutate(cycle_increment = cycle_inc) %>% 
  fill(cycle_increment, .direction = "downup") %>%  # make sure I can do up for have one new drug compare to the linename FBB5186A15CD9, FE593EDF5586A
  # cycle date
  group_by(patientid, linenumber_new, cycle_increment) %>% 
  mutate(cycle_start_date = min(episodedate)) %>% 
  # select(patientid, linename, linenumber_new, drugname, episodedate, episode_interval,
  #        cycle_count_perline, cycle_drugname, drugname_count_perline, cycle_increment, cycle_start_date) %>% 
  
  group_by(patientid, linenumber_new) %>% 
  mutate(cycle_interval = cycle_start_date - lag(cycle_start_date), 
         cycle_interval = ifelse(cycle_interval == 0, NA_real_, cycle_interval)) %>%  # FDB58481AFFBB 48 days
  
  # take for granted that the cycle interval days are the routine cycle for the rest of the line FAACDC7205803
  mutate(n = row_number(drugname)) %>%

  mutate(base_days_between_cycle = ifelse(n==2 , cycle_interval, NA_real_)) %>%
  fill(base_days_between_cycle, .direction = "downup") %>% 
  mutate(skipped_cycle_indays = ifelse(((cycle_interval - base_days_between_cycle) != 0) ,
                                       (cycle_interval - base_days_between_cycle), NA_real_)) %>% # weird FDB58481AFFBB
  # Fix skipped_cycle_indays when only 1 drug FDB58481AFFBB skipped_cycle_indays = NA, no other choice...
  group_by(patientid, linenumber_new) %>%
  mutate(drug_count_perline = n()) %>%
  mutate(skipped_cycle_indays1 = ifelse(
    (cycle_count_perline == drug_count_perline), NA_real_, skipped_cycle_indays 
  )) %>% # F001443D8B85C Pb with when different pattern by cycle => May not be able to have the skipped cycle
  # or need to compare with dose like if 100 should be 7 days and if 200 should be 14 days....
  group_by(patientid) %>% 
  mutate(mean_skipped_cycle_indays = mean(skipped_cycle_indays, na.rm = TRUE)) %>% 
  ungroup()


rm(drugs, drugs1, therapy)


#################################################################################################### III ### Merging----
# For each feature, binding with the variable, calculate an interval between cycle date and feature dates, 
# select the smallest interval
Treatment <- 
  # Merge with creatinine
  left_join(therapy1, creatinine, by = "patientid") %>%
  mutate(interval = abs(interval(start= creat_date, end= cycle_start_date)/                      
                          duration(n=1, unit="days"))) %>% 
  arrange(interval) %>% 
  distinct(patientid, episodedate, drugname, amount, linenumber_new, cycle_count_perline, cycle_increment, 
           cycle_start_date, .keep_all = TRUE) %>% 
  
  # # Merge with height
  # left_join(. , height,  by = "patientid") %>%
  # mutate(interval = abs(interval(start= height_date, end= cycle_start_date)/                      
  #          duration(n=1, unit="days"))) %>% 
  # arrange(interval) %>% 
  # distinct(patientid, episodedate, drugname, amount, linenumber_new, cycle_count_perline, cycle_increment, 
  #          cycle_start_date, .keep_all = TRUE) %>% 
  # 
  # # Merge with weight
  # left_join(. , weight, by = "patientid") %>%
  # mutate(interval = abs(interval(start= weight_date, end= cycle_start_date)/                      
  #                         duration(n=1, unit="days"))) %>% 
  # arrange(interval) %>% 
  # distinct(patientid, episodedate, drugname, amount, linenumber_new, cycle_count_perline, cycle_increment, 
  #          cycle_start_date, .keep_all = TRUE) %>%
  # 
  # # Calculate bmi
  # mutate(bmi = weight / (height * height)) %>% 
  # mutate(bmi_cat = case_when(
  #   bmi < 25                    ~ "Underweight and normal weight",
  #   bmi >= 25 &
  #     bmi < 30                  ~ "Overweight",
  #   bmi >= 30                   ~ "Obese"
  # )) %>% 
  # mutate(bmi_cat = factor(bmi_cat, levels = c("Underweight and normal weight", "Overweight", "Obese"))) %>% 
  # # Calculate CrCl
  # mutate(CrCl = ((140 - ageatdx) * weight)/((72 * creatinine) * 0.85)) %>%
  # 
  # # Merge with body_surface_area
  # left_join(., body_surface_area, by = "patientid") %>%
  # mutate(interval = abs(interval(start= bsa_date, end= cycle_start_date)/                      
  #                         duration(n=1, unit="days"))) %>% 
  # arrange(interval) %>% 
  # distinct(patientid, episodedate, drugname, amount, linenumber_new, cycle_count_perline, cycle_increment, 
  #          cycle_start_date, .keep_all = TRUE) %>% 
  # # Calculate more bsa value with height and weight using Du Bois formula
  # mutate(bsa_du_bois = 0.007184 * ((height*100)^0.725) * (weight^0.425)) %>% # get pretty close results 4269 patients result
  # mutate(BSA = coalesce(BSA, bsa_du_bois)) %>% 
  # select(-bsa_du_bois) %>% 

  # Merge with auc
  left_join(., areaUC, by = c("patientid", "drugname")) %>%
  mutate(interval = abs(interval(start= auc_date, end= cycle_start_date)/ # use episode date to be more precise? No it's not better
                          duration(n=1, unit="days"))) %>% 
  arrange(interval) %>% 
  distinct(patientid, episodedate, drugname, amount, linenumber_new, cycle_count_perline, cycle_increment, 
           cycle_start_date, .keep_all = TRUE) %>% 
  select(-interval) %>% 
  
  # Calculate expected dose
  mutate(expected_dose = case_when(
    drugname == "carboplatin"                 ~ target_auc * (CrCl + 25),
    str_detect(drugname, "taxel|platin")      ~ BSA * target_auc 
    # F19EBBFC60652 Can we fill target_auc? No depends of the type of cycle, or maybe by grouping by cycle type and drug
  )) %>% 
  mutate(expected_dose = case_when(
    expected_dose > 900 &
      target_auc == 6              ~ 900,
    expected_dose > 750 &
      target_auc == 5              ~ 750,
    TRUE                           ~ expected_dose
  )) %>% 
  arrange(patientid, episodedate)

# Treatment1 <- Treatment %>% 
#   select(c(patientid, "linename", "drugname", "cycle_increment", "amount", "target_auc", "auc_units", "expected_dose"))

# OrderedAmount and Quantity
# In the MedicationOrder table, ordered quantities are represented differently depending on their source. For example, orders that are placed via e-prescribing and/or flowsheet typically have OrderedAmount populated with corresponding OrderedUnits, while other medications recorded in the patient’s history typically have Quantity populated. In the majority of cases, the Quantity variable is used for medications ordered from sources other than the flowsheet and are frequently oral medications. In a relatively small number of cases, all three variables may contain distinct information; for example, an order for tamoxifen might include 20 (OrderedAmount) mg (OrderedUnits) and 30 (Quantity) capsules.
# OrderedAmount: Typically represents the dosage in mg
# Quantity: Typically represents the quantity of drug ordered in tablets or capsules
# RelativeOrderedAmount: Typically represents an order scaled to a patient’s size (for example, an order based on patient weight)
# While these variables cannot be combined, business rules can be implemented to impute the blank fields (for example, assume the recommended dose or most common days supply).

#################################################################################################### III ### Merging nand RDI----
Frontline <- Treatment %>% 
  # filter(therapy == "Upfront Chemo" | therapy == "Chemotherapy only") %>% 
  # group_by(patientid, linenumber_new, cycle_increment, drugname) %>% # ! -----------------------Change drugname if combine taxel....
  # mutate(amount_cycle = sum(amount)) %>% 
  # select(patientid, linenumber_new, cycle_increment, drugname, amount, amount_cycle, 
  #        target_auc, expected_dose, everything()) %>% # F01E4E6DD63C5, F1FA39C41A087 ----------- No we have xpected dose for each episode
  # ungroup() %>% 
  # distinct(patientid, linenumber_new, cycle_increment, drugname, .keep_all = TRUE) %>% 
  
  # Create a linenumber restating at 1 for adj to be able to calculate RDI for first line of Adjuvant
    group_by(patientid, chemotherapy_type) %>% 
  mutate(linenumber_adj = dense_rank(linenumber_new)) %>% 
  
  # Calculate 1 RDI per drug per day, for whole Neoadjuvant and for first line of Adjuvant
  mutate(relative_dose_intensity = case_when(
    chemotherapy_type == "Adjuvant" &
      linenumber_adj == 1                  ~ amount/expected_dose,
    chemotherapy_type == "Neoadjuvant"     ~ amount/expected_dose
  )) %>%
  # select(patientid, linenumber_new, chemotherapy_type, drugname_type, drugname,
  #        relative_dose_intensity, linenumber_adj) %>%
  
  # Average RDI by taxel or platin by line (will combine pacli-doce and cis-carbo-oxalo)
  group_by(patientid, chemotherapy_type, drugname_type) %>%
  mutate(mean_RDI_per_chemotype_drug = case_when(
    chemotherapy_type == "Adjuvant" &
      linenumber_adj == 1                  ~ round(mean(relative_dose_intensity, na.rm = TRUE), 3),
    chemotherapy_type == "Neoadjuvant"     ~ round(mean(relative_dose_intensity, na.rm = TRUE), 3)
  )) %>% 
  ungroup() %>% 
  
  # Then let's see if we need to average that for the whole Neo BUT 
  # For now average rdi over whole neo, average rdi for the first line over adjuvant
  # when plot meed 1 rdi per drugname_type per chemotherapy_type aka distinct(patientid, chemotherapy_type, drugname_type)
  
  
  # group_by(patientid, chemotherapy_type, drugname_type) %>% 
  # mutate(RDI_neo = case_when(
  #   # chemotherapy_type == "Adjuvant"       ~ first(mean_RDI_per_drug_line), # F002B429BE6C6
  #   chemotherapy_type == "Adjuvant" &
  #     linenumber_adj == 1                  ~ mean(mean_RDI_per_drug_line),
  #   chemotherapy_type == "Neoadjuvant"     ~ mean(mean_RDI_per_drug_line) # F0040EA299CF0
  # )) %>% 
  
  # mutate(RDI_adj = case_when(
  #   chemotherapy_type == "Adjuvant" &
  #     linenumber_adj == 1                  ~ mean(mean_RDI_per_drug_line), # F002B429BE6C6
  #   # chemotherapy_type == "Neoadjuvant"     ~ mean(mean_RDI_per_drug_line) # F0040EA299CF0
  # )) %>% 
  # mutate(RDI = coalesce(RDI_neo, RDI_adj))
  

  
  
  # group_by(patientid, chemotherapy_type, drugname) %>% # ------------------------ Or should I do by cycle, then avg again
  # mutate(mean_RDI_per_drug_chemotype = mean(relative_dose_intensity)) %>% 
  # ungroup() %>% 
  mutate(RDI_grp = as.factor(findInterval(mean_RDI_per_chemotype_drug, c(0.85) ))) %>% 
  mutate(RDI_grp = factor(RDI_grp, 
                           levels = c("1", "0"), 
                           labels = c("RDI >= 0.85", "RDI < 0.85"))) %>% 
  mutate(RDI_grp1 = as.factor(findInterval(mean_RDI_per_chemotype_drug, c(0.75, 0.85, 1, 1.5, 2) ))) %>% 
  mutate(RDI_grp1 = factor(RDI_grp1, 
                          levels = c("3", "0", "1","2","4", "5"), 
                          labels = c("0.85 >= RDI < 1", "0 < RDI < 0.75", "0.75 <= RDI < 0.85", "1 <= RDI < 1.5", 
                                     "1.5 <= RDI < 2", "RDI >= 2"))) %>% 
  # mutate(delay_incare_dx_to_treat = case_when(
  #   str_detect(therapy, "Chemo")     ~ (interval(start = diagnosisdate, end = episodedate)/
  #     duration(n=1, units = "days")),
  #   str_detect(therapy, "Surgery")   ~ (interval(start = diagnosisdate, end = surgerydate)/
  #     duration(n=1, units = "days"))
  # )) %>% 
  # group_by(patientid) %>% 
  # mutate(first_treatment_date = case_when(
  #   treatment_sequence == "Chemotherapy only"             ~ min(episodedate),
  #   str_detect(treatment_sequence, "neo")                 ~ min(episodedate),
  #   treatment_sequence == "surg + adj"                    ~ surgerydate,
  #   treatment_sequence == "Surgery only"                  ~ surgerydate
  # )) %>% 
  group_by(patientid) %>% 
  mutate(first_treatment_date = case_when(
    str_detect(therapy, "Chemo")                      ~ min(episodedate),
    str_detect(therapy, "Surgery")                    ~ surgerydate,
  )) %>% 
  mutate(delay_incare_dx_to_treat = interval(start = diagnosisdate, end = first_treatment_date)/
                                          duration(n=1, units = "days")) %>% 
  mutate(month_at_os_from_treatment = interval(start = first_treatment_date, end = followupdate)/
                   duration(n=1, units = "months")) %>% 
  ungroup() %>% 
  # %>% 
  # select(patientid, linenumber_new, drugname, surgerydate, episodedate, treatment_sequence, first_treatment_date, month_at_os_from_treatment)
  mutate(thirty_days_exclusion = interval(start = diagnosisdate, end = dateofdeath)/
           duration(n=1, units = "days"), 
         thirty_days_exclusion = ifelse(thirty_days_exclusion <= 30, "Exclude", NA_character_))





write_rds(Frontline, "Frontline.rds")
table(Frontline$RDI_grp)

# Need to recode more line because when change of cycle F5996791F0F8A the expected dose will be different...?
# Need to distinguish pacli and pacli-bound when recode cycle number otherwise can mess up cycle and expected dose per cycle F003F2BEEDB37
# Or shouldn't because the amount is good here


# A vial of 5 ml contains 30 mg of paclitaxel. A vial of 16.7 ml contains 100 mg of paclitaxel. A vial of 25 ml contains 150 mg of paclitaxel. 
# Paclitaxel 6 mg/ml Concentrate





