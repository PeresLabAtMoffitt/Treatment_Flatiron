# Load your packages from packages.R
source("R/packages.R")
# Load the code as function that drake need to run
source("R/function.R") 
# Load the plan that drake has to execute
source("R/plan.R")    

# End drake
config <- drake_config(plan, parallelism = "future", jobs = 4, verbose = 1)
if (!interactive()) config

make(plan)
loadd(demo_data, clinical_data, Vitals# ,
      # Labs
      )

# Cleaning
rm(demo_import, data_import, vitals_import,
   labs_import,
   plan, config)
