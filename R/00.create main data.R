# Import library
library(tidyverse)
library(lubridate)

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
episode <- 
  read_csv(paste0(
    path,
    "/Flatiron/Data from Flatiron/edm_ovarian_472021/DrugEpisode.csv"
  ))
biom <- 
  read_csv(paste0(
    path,
    "/Flatiron/Data from Flatiron/edm_ovarian_472021/Enhanced_OvarianBiomarkers.csv"
  )) %>% 
  `colnames<-`(str_to_lower(colnames(.)))

# Prep biomarkers
hrd <- biom %>% filter(biomarkername == "HRD")
gis <- biom %>% filter(biomarkername == "GIS")
brca <- biom %>% filter(biomarkername == "BRCA")

######### BRCA data cleaning ##############
brca <- brca %>% 
  # recoding unknown tissue for BRCA using lab and test name
  mutate(sampletype = case_when( 
    sampletype == "Unknown/not documented" & 
      (brca$testname == "Caris Life Sciences" |
         testname == "Foundation Focus CDxBRCA" |
         testname == "Foundation One" |
         testname == "In-house pathology lab")             ~ "Tissue",
    sampletype == "Unknown/not documented" & 
      (testname == "GeneDx" |
         testname == "Integrated Oncology/LabCorp" |
         testname == "Invitae" |
         testname == "Myriad MyRisk" |
         testname == "Quest BRCAvantage")                  ~ "Blood",
    TRUE                                                   ~ sampletype
  ))

# Creating different brca datasets
brca_germ <- brca %>% filter(sampletype == "Blood" | sampletype == "Saliva")
brca_tumor <- brca %>% filter(sampletype == "Tissue")
brca_unknown <- brca %>% filter(sampletype == "Unknown/not documented")

######### GERMLINE ############
brca_germ <- brca_germ %>% group_by(patientid) %>% mutate(count=n())
# 2804 patients - 3206 rows (more than one test for some patients)
brca_germ$number <- 1
brca_germ <- brca_germ %>% group_by(patientid) %>% arrange(desc(resultdate)) %>% mutate(ticker = cumsum(number)) # creating a variable to filter out the most recent result
check <- brca_germ %>% filter(count>1) %>% arrange(patientid, ticker) # checking to make sure the ticker worked properly for cases with more than one test
brca_germ2 <- brca_germ %>% filter(ticker==1) # restricting to the most recent result
brca_germ2 <- brca_germ2 %>% 
  # strictly BRCA 1 in germline
  mutate(brca1_germline = case_when(
    biomarkerstatus == "Both BRCA1 and BRCA2 Mutations Identified" |
      biomarkerstatus == "BRCA1 Mutation Identified"                    ~ "Mutation",
    biomarkerstatus == "No BRCA Mutation"                               ~ "No mutation",
    TRUE                                                                ~ NA_character_
  )) %>% 
  # strictly BRCA 2 in germline
  mutate(brca2_germline = case_when(
    biomarkerstatus == "Both BRCA1 and BRCA2 Mutations Identified" |
      biomarkerstatus == "BRCA2 Mutation Identified"                    ~ "Mutation",
    biomarkerstatus == "No BRCA Mutation"                               ~ "No mutation",
    TRUE                                                                ~ NA_character_
  )) %>% 
  # All BRCA in germline
  mutate(brca_germline = case_when(
    biomarkerstatus == "Both BRCA1 and BRCA2 Mutations Identified" |
      biomarkerstatus == "BRCA1 Mutation Identified" |
      biomarkerstatus == "BRCA2 Mutation Identified" |
      biomarkerstatus=="BRCA Mutation NOS"                              ~ "Mutation",
    biomarkerstatus == "No BRCA Mutation"                               ~ "No mutation",
    biomarkerstatus == "Genetic Variant of Unknown Significance (VUS)" |
      biomarkerstatus=="Genetic Variant Favor Polymorphism"             ~ "VUS",
    TRUE                                                                ~ NA_character_
  ))
# ######### BRCA GERMLINE dataset with only cases that have a result ########## NOT necessary ?
# brca_germ_keep <- brca_germ2 %>% filter(brca_germline=="Mutation" | brca_germline=="No mutation" | brca_germline=="VUS")
# keep <- c("patientid","brca1_germline","brca2_germline", "brca_germline")
# brca_germ_keep <- brca_germ_keep[,names(brca_germ_keep) %in% keep]
# unique(brca_germ_keep) # no duplicates
# check <- brca_germ_keep %>% group_by(patientid) %>% summarise(n()) # no duplicates

######### TUMOR #########
brca_tumor <- brca_tumor %>% group_by(patientid) %>% mutate(count=n())
# 1078 patients - 1171 rows (more than one test for some patients)
brca_tumor$number <- 1
brca_tumor <- brca_tumor %>% group_by(patientid) %>% arrange(desc(resultdate)) %>% mutate(ticker = cumsum(number)) # creating a variable to filter out the most recent result
check <- brca_tumor %>% filter(count>1) %>% arrange(patientid, ticker) # checking to make sure the ticker worked properly for cases with more than one test
brca_tumor2 <- brca_tumor %>% filter(ticker==1) # restricting to the most recent result
brca_tumor2 <- brca_tumor2 %>% 
  # strictly BRCA 1 in tumor
  mutate(brca1_tumor = case_when(
    biomarkerstatus == "Both BRCA1 and BRCA2 Mutations Identified" |
      biomarkerstatus == "BRCA1 Mutation Identified"                    ~ "Mutation",
    biomarkerstatus == "No BRCA Mutation"                               ~ "No mutation",
    TRUE                                                                ~ NA_character_
  )) %>% 
  # strictly BRCA 2 in tumor
  mutate(brca2_tumor = case_when(
    biomarkerstatus == "Both BRCA1 and BRCA2 Mutations Identified" |
      biomarkerstatus == "BRCA2 Mutation Identified"                    ~ "Mutation",
    biomarkerstatus == "No BRCA Mutation"                               ~ "No mutation",
    TRUE                                                                ~ NA_character_
  )) %>% 
  # All BRCA in tumor
  mutate(brca_tumor = case_when(
    biomarkerstatus == "Both BRCA1 and BRCA2 Mutations Identified" |
      biomarkerstatus == "BRCA1 Mutation Identified" |
      biomarkerstatus == "BRCA2 Mutation Identified" |
      biomarkerstatus=="BRCA Mutation NOS"                              ~ "Mutation",
    biomarkerstatus == "No BRCA Mutation"                               ~ "No mutation",
    biomarkerstatus == "Genetic Variant of Unknown Significance (VUS)" |
      biomarkerstatus=="Genetic Variant Favor Polymorphism"             ~ "VUS",
    TRUE                                                                ~ NA_character_
  ))
# ######### BRCA TUMOR dataset with only cases that have a result ########## NOT necessary ?
# brca_tumor_keep <- brca_tumor2 %>% filter(brca_tumor=="Mutation" | brca_tumor=="No mutation" | brca_tumor=="VUS")
# keep <- c("patientid","brca1_tumor","brca2_tumor", "brca_tumor")
# brca_tumor_keep <- brca_tumor_keep[,names(brca_tumor_keep) %in% keep]
# unique(brca_tumor_keep) # no duplicates
# check <- brca_tumor_keep %>% group_by(patientid) %>% summarise(n()) # no duplicates

######### UNKNOWN TISSUE #########
brca_unknown <- brca_unknown %>% group_by(patientid) %>% mutate(count=n())
# 580 patients - 591 rows (more than one test for some patients)
brca_unknown$number <- 1
brca_unknown <- brca_unknown %>% group_by(patientid) %>% arrange(desc(resultdate)) %>% mutate(ticker = cumsum(number)) # creating a variable to filter out the most recent result
check <- brca_unknown %>% filter(count>1) %>% arrange(patientid, ticker) # checking to make sure the ticker worked properly for cases with more than one test
brca_unknown2 <- brca_unknown %>% filter(ticker==1) # restricting to the most recent result
brca_unknown2 <- brca_unknown2 %>% 
  # strictly BRCA 1 in unknown
  mutate(brca1_unknown = case_when(
    biomarkerstatus == "Both BRCA1 and BRCA2 Mutations Identified" |
      biomarkerstatus == "BRCA1 Mutation Identified"                    ~ "Mutation",
    biomarkerstatus == "No BRCA Mutation"                               ~ "No mutation",
    TRUE                                                                ~ NA_character_
  )) %>% 
  # strictly BRCA 2 in unknown
  mutate(brca2_unknown = case_when(
    biomarkerstatus == "Both BRCA1 and BRCA2 Mutations Identified" |
      biomarkerstatus == "BRCA2 Mutation Identified"                    ~ "Mutation",
    biomarkerstatus == "No BRCA Mutation"                               ~ "No mutation",
    TRUE                                                                ~ NA_character_
  )) %>% 
  # All BRCA in unknown
  mutate(brca_unknown = case_when(
    biomarkerstatus == "Both BRCA1 and BRCA2 Mutations Identified" |
      biomarkerstatus == "BRCA1 Mutation Identified" |
      biomarkerstatus == "BRCA2 Mutation Identified" |
      biomarkerstatus=="BRCA Mutation NOS"                              ~ "Mutation",
    biomarkerstatus == "No BRCA Mutation"                               ~ "No mutation",
    biomarkerstatus == "Genetic Variant of Unknown Significance (VUS)" |
      biomarkerstatus=="Genetic Variant Favor Polymorphism"             ~ "VUS",
    TRUE                                                                ~ NA_character_
  ))
# ######### BRCA UNKNOWN dataset with only cases that have a result ########## NOT necessary ?
# brca_unknown_keep <- brca_unknown2 %>% filter(brca_unknown=="Mutation" | brca_unknown=="No mutation" | brca_unknown=="VUS")
# keep <- c("patientid","brca1_unknown","brca2_unknown", "brca_unknown")
# brca_unknown_keep <- brca_unknown_keep[,names(brca_unknown_keep) %in% keep]
# unique(brca_unknown_keep) # no duplicates
# check <- brca_unknown_keep %>% group_by(patientid) %>% summarise(n()) # no duplicates
######### CREATING A COMBINED BRCA DATASET ############
brca_all <- 
  full_join(brca_germ2, brca_tumor2, 
            by="patientid") %>% 
  full_join(., brca_unknown2, 
            by="patientid")


########## HRD data cleaning - all should be tissue so no need to worry about sample type ###########
table(hrd$sampletype)
hrd <- hrd %>% 
  mutate(hrd_tissue = case_when(
    biomarkerstatus == "Positive"                             ~ "Positive",
    biomarkerstatus == "Negative"                             ~ "Negative",
    biomarkerstatus == "Results pending" |
      biomarkerstatus == "Unknown" |
      biomarkerstatus == "Unsuccessful/indeterminate test"    ~ NA_character_,
    TRUE                                                      ~ NA_character_
  )) %>% 
  arrange(desc(hrd_tissue)) %>% 
  distinct(patientid, .keep_all = TRUE)


######## GIS data cleaning - all should be tissue so no need to worry about sample type ###########
table(gis$sampletype)
gis <- gis %>% 
  mutate(gis_tissue = case_when(
    biomarkerstatus == "Positive"                             ~ "Positive",
    biomarkerstatus == "Negative"                             ~ "Negative",
    biomarkerstatus == "Results pending" |
      biomarkerstatus == "Unknown" |
      biomarkerstatus == "Unsuccessful/indeterminate test"    ~ NA_character_,
    TRUE                                                      ~ NA_character_
  )) %>% 
  arrange(desc(gis_tissue)) %>% 
  distinct(patientid, .keep_all = TRUE)

######### Merging all biomarker data into one file ############
all_biomarker <- 
  full_join(brca_all, hrd, 
            by="patientid") %>% 
  full_join(., gis, 
            by="patientid") %>% 
  select(patientid, 
         brca1_germline, brca2_germline, brca_germline,
         brca1_tumor, brca2_tumor, brca_tumor,
         brca1_unknown, brca2_unknown, brca_unknown,
         hrd_tissue, gis_tissue)


####### Create last visit date for censoring ####### 
last_visit <- 
  bind_rows(visit %>% 
              rename(datelastcontact = VisitDate), 
            orals %>% 
              rename(datelastcontact = EndDate),
            episode %>% 
              filter(EpisodeDataSource == "Abstraction") %>% 
              rename(datelastcontact = EpisodeDate),
            dx %>% 
              rename(datelastcontact = SurgeryDate)
  ) %>% 
  arrange(desc(datelastcontact)) %>% 
  distinct(PatientID, .keep_all = TRUE) %>% 
  select(PatientID, PracticeID, datelastcontact)

#######  Create clinical data ####### 
clinical_data <-
  full_join(demos, dx,
            by = "PatientID") %>%
  full_join(., mort %>%
              mutate(DateOfDeath = as.Date(paste(
                DateOfDeath, "-15", sep = ""))),
            by = "PatientID") %>% 
  full_join(., last_visit, 
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
  `colnames<-`(str_to_lower(colnames(.))) %>% 
  mutate(exclude = case_when(
    histology == "Borderline"                                  ~ 1,
    # practiceid == "SE3D6373EFD3B"                              ~ 1,
    # practicetype == "ACADEMIC" & 
    #   dxyear < 2015                                            ~ 1,
    TRUE                                                       ~ NA_real_
  )) %>% 
  # Join with biomarkers
  full_join(., all_biomarker,
            by="patientid")

write_csv(
  clinical_data,
  paste0(
    path,
    "/Flatiron/Data from Flatiron/edm_ovarian_472021/NEW_ovarianCLEANwithbiom.csv"
  ))


# End creating full clinical data

