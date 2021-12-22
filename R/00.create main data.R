# Import library
library(tidyverse)

#################################
### Create main vital data ###
#################################
# Load data
path <- fs::path("", "Volumes", "Peres_Research")
vitals <- 
  read_csv(paste0(
    path,
    "/Flatiron/Data from Flatiron/edm_ovarian_472021/Vitals.csv"
  ))
labs <-
  read_csv(paste0(
    path,
    "/Flatiron/Data from Flatiron/edm_ovarian_472021/Lab.csv"
  ))
labs <-
  read_csv("/Volumes/Peres_Research/Flatiron/Data from Flatiron/ovarian/Lab.csv")


# Filter
# Restricting vitals to only data on body height, body surface, and body weight
vitals_anthro <-
  vitals %>%
  filter(
    TestBaseName == "body height" |
      TestBaseName == "body weight" |
      TestBaseName == "body surface") %>% 
  select(-c(LOINC)) %>% 
  `colnames<-`(str_to_lower(colnames(.)))
write_csv(
  vitals_anthro,
  paste0(
    path,
    "/Flatiron/Data from Flatiron/edm_ovarian_472021/NEW_vitals_anthro.csv"
  )
)
# Restricting labs to only creatinine
labs_creat <- 
  labs %>% 
  filter(TestBaseName == "creatinine") %>% 
  select(-c(LOINC, LabSource)) %>% 
  `colnames<-`(str_to_lower(colnames(.)))
write_csv(
  labs_creat, 
  paste0(
    path,
    "/Flatiron/Data from Flatiron/edm_ovarian_472021/NEW_labs_creatinine.csv"
  )
)


#################################
### Create main clinical data ###
#################################
# Load data
demos <- 
  read_csv(paste0(
    path,
    "/Flatiron/Data from Flatiron/edm_ovarian_472021/Demographics.csv"
  ))
mort <- 
  read_csv(paste0(
    path,
    "/Flatiron/Data from Flatiron/edm_ovarian_472021/Enhanced_Mortality_V2.csv"
  ))
dx <- 
  read_csv(paste0(
    path,
    "/Flatiron/Data from Flatiron/edm_ovarian_472021/Enhanced_Ovarian.csv"
  ))
visit <- 
  read_csv(paste0(
    path,
    "/Flatiron/Data from Flatiron/edm_ovarian_472021/Visit.csv"
  ))
orals <- 
  read_csv(paste0(
    path,
    "/Flatiron/Data from Flatiron/edm_ovarian_472021/Enhanced_Ovarian_Orals.csv"
  ))

# Create last visit date for censoring
visit <- 
  bind_rows(visit %>% 
              rename(datelastcontact = VisitDate), 
            orals %>% 
              rename(datelastcontact = EndDate)
  ) %>% 
  arrange(desc(datelastcontact)) %>% 
  distinct(PatientID, .keep_all = TRUE) %>% 
  select(PatientID, PracticeID, datelastcontact)

# Create clinical data
clinical_dataa <-
  full_join(demos, dx,
            by = "PatientID") %>%
  full_join(., mort %>%
              mutate(DateOfDeath = as.Date(paste(
                DateOfDeath, "-15", sep = ""))),
            by = "PatientID") %>% 
  full_join(., visit, 
            by = "PatientID") %>% 
  # variables for survival
  mutate(vital_status = ifelse(is.na(DateOfDeath), 0, 1)) %>% 
  mutate(followupdate = coalesce(DateOfDeath, datelastcontact)) %>% 
  # stage
  mutate(stagecat = case_when(
    GroupStage %in% 
      c("I","IA","IB","IC","IC1","IC2","IC3")       ~ "Stage 1",
    GroupStage %in% 
      c("II","IIA","IIB","IIC")                     ~ "Stage 2",
    GroupStage %in% 
      c("III","IIIA","IIIA1","IIIA1(i)",
        "IIIA1(ii)","IIIA2","IIIB","IIIC")          ~ "Stage 3",
    GroupStage %in% 
      c("IV","IVA","IVB")                           ~ "Stage 4", 
    TRUE                                            ~ NA_character_
    )) %>% 
  # race ethnicity
  mutate(
    racecat = case_when(
      Race == "White"                               ~ 1, # White
      Race == "Black or African American"           ~ 2, # Black
      Race == "Asian"                               ~ 3, # Asian
      TRUE                                          ~ 4  # Other
    )) %>% 
  mutate(
    ethcat = case_when(
      Ethnicity == "Hispanic or Latino"             ~ 1,
      TRUE                                          ~ 0
      )) %>% 
  mutate(raceeth = case_when(
    racecat == 1 & ethcat == 0                      ~ "NHWhite",
    racecat == 1 & ethcat == 1                      ~ "Hispanic",
    racecat == 4 & ethcat == 1                      ~ "Hispanic",
    racecat == 2 & ethcat == 0                      ~ "NHBlack",
    racecat == 2 & ethcat == 1                      ~ "Other", # not including Black Hispanics as Black or Hispanic
    racecat == 3 & ethcat == 0                      ~ "Other",
    racecat == 3 & ethcat == 1                      ~ "Other", # not including Asian Hispanics as Hispanic
    racecat == 4 & ethcat == 0                      ~ "Other"
  )) %>% 
  # histology
  mutate(Histology = ifelse(Histology == "Transitional cell", "Serous", Histology)) %>% 
  mutate(ageatdx = year(DiagnosisDate) - BirthYear) %>% 
  mutate(agecat = case_when(
    ageatdx < 45                                    ~ "<45 years",
    ageatdx >= 45 & ageatdx < 60                    ~ "45-59 years",
    ageatdx >= 60 & ageatdx < 75                    ~ "60-74 years",
    ageatdx >= 75                                   ~ ">=75 years"
  )) %>% 
  # debulking
  mutate(resdz = case_when(
    ResidualDiseaseStatus == "No residual disease"              ~ "Optimal",
    ResidualDiseaseStatus == "Unknown/not documented"           ~ "Unknown",
    ResidualDiseaseStatus == "Residual disease" & 
      SizeOfResidualDisease %in% 
      c("Greater than 1 cm","Greater than 2 cm",
        "Less than or equal to 2 cm")                           ~ "Suboptimal",
    ResidualDiseaseStatus == "Residual disease" & 
      SizeOfResidualDisease == "Less than or equal to 1 cm"     ~ "Optimal",
    ResidualDiseaseStatus == "Residual disease" & 
      SizeOfResidualDisease == "Unknown"                        ~ "ResdzUnk"
  )) %>% 
  mutate(debulking = case_when(
    ExtentOfDebulking == "Optimal" | 
      resdz == "Optimal"                                        ~ "Optimal",
    ExtentOfDebulking == "Suboptimal" | 
      resdz == "Suboptimal"                                     ~ "Suboptimal",
    resdz == "Suboptimal" & 
      ExtentOfDebulking == "Optimal"                            ~ "Suboptimal",
    resdz == "Optimal" & 
      ExtentOfDebulking == "Suboptimal"                         ~ "Suboptimal",
    resdz == "ResdzUnk" & 
      ExtentOfDebulking == "Unknown/not documented"             ~ "Suboptimal", 
    IsSurgery== "No/unknown"                                    ~ "No surgery",
    TRUE                                                        ~ "Unknown"
  )) %>% 
  select(PatientID, Gender, 
         Race, Ethnicity, racecat, ethcat, raceeth,
         DiagnosisDate, ageatdx, agecat, 
         DateOfDeath, vital_status, followupdate,
         Histology, stagecat, 
         IsSurgery, SurgeryDate, ExtentOfDebulking,
         ResidualDiseaseStatus, SizeOfResidualDisease,
         resdz, debulking, PracticeID) %>% 
  `colnames<-`(str_to_lower(colnames(.)))








