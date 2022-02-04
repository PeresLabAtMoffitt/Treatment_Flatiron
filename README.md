# AIM
The project aim to investigate whether clinical characteristics, treatment patterns and guideline-adherent treatment status 
differs by race/ethnicity in ovarian cancer treatment using Flatiron database.
Also we will assess whether obesity adversely affects first-line chemotherapy dosing and efficacy as well as survival, 
and determine if these negative consequences of obesity disproportionately affects minority populations


# Data
To save time loading the data, I used the `drake` package to cache the files.
To do so, you will need to run the "_drake.R" script (make sure that line 17 "make(plan)" is uncommented).
From now the files will be cached and will be loaded with the `loadd` function.

# Cleaning 
Then you can run the "02.data cleaning.R" script which does all the clinical cleaning.

# Rmd
The Rmd files run the Treament cleaning, patients selection and analysis.
