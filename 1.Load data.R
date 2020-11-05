library(drake)
library(tidyverse)
library(gtsummary)
library(survival)
library(survminer)


################################################################# I ### Load data
path <- fs::path("","Volumes","Peres_Research")

demo_data <-
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/ovarian/ovarian_demographics.csv"))
clinical_data <- # For Dx date, histo, stage, surgerydate
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/ovarian/ovarian_enhanced_ovarian.csv"))
visit <- # For all the visit date, may need it to know the treatment date (no drug name)
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/ovarian/ovarian_visit.csv"))


drug_data <- # For each drug date
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/ovarian/ovarian_drugepisode.csv"))

drugs2_data <-
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/ovarian/ovarian_enhanced_ovarian_orals.csv"))

drug_admin_data <-
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/ovarian/ovarian_medicationadministration.csv"))

ovarian_medicationorder.csv_data <-
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/ovarian/ovarian_medicationorder.csv"))

