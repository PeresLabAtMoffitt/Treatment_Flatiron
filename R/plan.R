plan <- drake_plan(
  
  clinical_data = data_import(fs::path("","Volumes","Peres_Research")),
  vitals = vitals_import(fs::path("","Volumes","Peres_Research")),
  creatinine = cr_import(fs::path("","Volumes","Peres_Research")),
  drugs = drugs_import(fs::path("","Volumes","Peres_Research"))
)


