plan <- drake_plan(
  
  demo_data = demo_import(fs::path("","Volumes","Peres_Research")),
  clinical_data = data_import(fs::path("","Volumes","Peres_Research")),
  Vitals = vitals_import(fs::path("","Volumes","Peres_Research"))#,
  # Labs = labs_import(fs::path("","Volumes","Peres_Research"))
)
