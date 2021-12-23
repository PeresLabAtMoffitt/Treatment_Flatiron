#################################################################################################### I ### Load data
path <- fs::path("","Volumes","Peres_Research")


# demo_import <- function(path){
#   read_csv(paste0(path,
#                   "/Flatiron/Data from Flatiron/ovarian/ovarian_demographics.csv"))
# }
data_import <- function(path){
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/edm_ovarian_472021/NEW_ovarianCLEANwithbiom.csv"))
}
vitals_import <- function(path){
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/edm_ovarian_472021/NEW_vitals_anthro.csv"))
}

cr_import <- function(path){
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/edm_ovarian_472021/NEW_labs_creatinine.csv"))
}

auc_import <- function(path){
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/edm_ovarian_472021/MedicationOrder.csv"))
}

drugs_import <- function(path){
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/edm_ovarian_472021/DrugEpisode.csv"))
}

orderdrug_import <- function(path){
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/edm_ovarian_472021/MedicationAdministration.csv"))
}


