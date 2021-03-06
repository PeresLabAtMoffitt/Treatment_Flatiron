#################################################################################################### I ### Load data
path <- fs::path("","Volumes","Peres_Research")


# demo_import <- function(path){
#   read_csv(paste0(path,
#                   "/Flatiron/Data from Flatiron/ovarian/ovarian_demographics.csv"))
# }
data_import <- function(path){
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/ovarian/NEW_ovarianCLEANwithbiom.csv"))
}
vitals_import <- function(path){
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/ovarian/NEW_vitals_anthro.csv"))
}

cr_import <- function(path){
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/ovarian/NEW_labs_creatinine.csv"))
}

auc_import <- function(path){
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/ovarian/ovarian_medicationorder.csv"))
}

drugs_import <- function(path){
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/ovarian/ovarian_drugepisode.csv"))
}

orderdrug_import <- function(path){
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/ovarian/ovarian_medicationadministration.csv"))
}


