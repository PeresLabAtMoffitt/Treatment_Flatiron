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
                  "/Flatiron/Data from Flatiron/ovarian/ovarian_vitals.csv"))
}

cr_import <- function(path){
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/ovarian/NEW_labs_creatinine.csv"))
}

drugs_import <- function(path){
  read_csv(paste0(path,
                  "/Flatiron/Data from Flatiron/ovarian/ovarian_drugepisode.csv"))
}




