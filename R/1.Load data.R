# library(drake)
library(tidyverse)
library(data.table)
library(lubridate)
# library(gtsummary)
# library(survival)
# library(survminer)


#################################################################################################### I ### Load data
path <- fs::path("","Volumes","Peres_Research")

demo_data <-
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/ovarian/ovarian_demographics.csv")) %>% 
  select(-c("practiceid", "practicetype", "primaryphysicianid"))
clinical_data <- # For Dx date, histo, stage, surgerydate
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/ovarian/ovarian_enhanced_ovarian.csv"))
# visit <- # For all the visit date, may need it to know the treatment date (no drug name)
#   read_csv(paste0(path,
#                   "/Flatiron/Data from Flatiron/ovarian/ovarian_visit.csv"))

lab <-
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/ovarian/ovarian_lab.csv"))
Vitals <-
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/ovarian/ovarian_vitals.csv"))



drug_data <- # For each drug date
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/ovarian/ovarian_drugepisode.csv"))

drugs2_data <-
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/ovarian/ovarian_enhanced_ovarian_orals.csv"))

# drug_admin_data <- # For chemo date same as ovarian_drugepisode.csv + other types of drugs date (corticoid, GF)
#   read_csv(paste0(path,
#                   "/Flatiron/Data from Flatiron/ovarian/ovarian_medicationadministration.csv"))

# ovarian_medicationorder.csv_data <- # Not useful, some missing (plus dates are expected dates) and dose can be found in episode
#   read_csv(paste0(path,
#                   "/Flatiron/Data from Flatiron/ovarian/ovarian_medicationorder.csv"))

# line_therapy <- # wrongly combine all "line" together, can find if it is maintenance therapy in episode
#   read_csv(paste0(path,
#                   "/Flatiron/Data from Flatiron/ovarian/ovarian_lineoftherapy.csv"))




#################################################################################################### I ### Load data
Global_data <- full_join(demo_data, clinical_data, by = "patientid")

Vitals_height <- Vitals %>% 
  filter(test == "body height") %>% 
  select(c("patientid", "testdate", "testunits", "testresult")) %>% 
  mutate(height = case_when(
    testunits == "cm"     ~ testresult/2.54,
    testunits == "ft"     ~ testresult*12,
    TRUE                  ~ testresult
  )) %>% 
  mutate(year = year(testdate)) %>% 
  arrange(testdate) %>% 
  distinct(patientid, year, .keep_all = TRUE)

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







