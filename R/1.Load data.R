# library(drake)
library(tidyverse)
library(data.table)
library(lubridate)
# library(gtsummary)
# library(survival)
# library(survminer)


#################################################################################################### I ### Load data
path <- fs::path("","Volumes","Peres_Research")

# demo_data <-
#   read_csv(paste0(path,
#                   "/Flatiron/Data from Flatiron/ovarian/ovarian_demographics.csv")) %>% 
#   select(-c("practiceid", "practicetype", "primaryphysicianid"))
clinical_data <- # For Dx date, histo, stage, surgerydate
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/ovarian/NEW_ovarianCLEANwithbiom.csv"))
creatinine <-
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/ovarian/NEW_labs_creatinine.csv"))
vitals <-
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/ovarian/NEW_vitals_anthro.csv"))
# visit <- # For all the visit date, may need it to know the treatment date (no drug name)
#   read_csv(paste0(path,
#                   "/Flatiron/Data from Flatiron/ovarian/ovarian_visit.csv"))



drug <- # For each drug date
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









