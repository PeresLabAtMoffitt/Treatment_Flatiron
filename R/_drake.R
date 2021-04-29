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
loadd(clinical_data, vitals, drugs, auc, orderdrug,
      creatinine
      )

# Cleaning
rm(data_import, vitals_import, cr_import, auc_import, drugs_import, orderdrug_import,
   plan, config)
