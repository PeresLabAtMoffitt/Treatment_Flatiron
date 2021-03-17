###### Analysis
Frontline <- read_rds("Frontline.rds")
# analysis_data <- Frontline %>% 
#   distinct(patientid, .keep_all = TRUE) %>% 
#   filter(exclude != "1")

Frontline %>% 
  distinct(patientid, .keep_all = TRUE) %>% 
  filter(exclude != "1") %>% 
  select(vitalstatus, bmi, bmi_cat, RDI_grp, # FOR ADJ NEO, CARB
         raceeth) %>% 
  tbl_summary() %>% bold_labels()



#################################################################################################### II ### KM Demo----
# OS by race
ggsurvplot(survfit(Surv(aprox_month_at_os, vitalstatus) ~ raceeth, data=Frontline %>% 
                     distinct(patientid, .keep_all = TRUE) %>% 
                     filter(exclude != "1")),
           title = "OS from date of diagnosis", 
           font.main = c(24, "bold", "black"), 
           font.x = c(20, "bold", "black"), font.y = c(20, "bold", "black"), 
           font.legend = c(14, "bold", "black"), font.tickslab = c(18, "bold", "black"), 
           size = 1.5,
           
           pval=TRUE, 
           # legend.labs=c(""),
           # palette = c("black", "green","blue", "red"), 
           # Add risk table
           risk.table = "abs_pct", risk.table.title = "Risk table (number(%))",
           risk.table.y.text = FALSE, risk.table.fontsize = 4.5, tables.height = 0.3,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           
           legend.title = "", xlab = "Time (in months)" 
) + guides(colour = guide_legend(nrow = 2))

# OS by raceeth
ggsurvplot(survfit(Surv(aprox_month_at_os, vitalstatus) ~ raceeth, data=Frontline %>% 
                     distinct(patientid, .keep_all = TRUE) %>% 
                     filter(exclude != "1")),
           title = "OS from date of diagnosis", 
           font.main = c(24, "bold", "black"), 
           font.x = c(20, "bold", "black"), font.y = c(20, "bold", "black"), 
           font.legend = c(14, "bold", "black"), font.tickslab = c(18, "bold", "black"), 
           size = 1.5,
           
           pval=TRUE, 
           # legend.labs=c(""),
           # palette = c("black", "green","blue", "red"), 
           # Add risk table
           risk.table = "abs_pct", risk.table.title = "Risk table (number(%))",
           risk.table.y.text = FALSE, risk.table.fontsize = 4.5, tables.height = 0.3,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           
           legend.title = "", xlab = "Time (in months)" 
) + guides(colour = guide_legend(nrow = 2))

# OS by histology
ggsurvplot(survfit(Surv(aprox_month_at_os, vitalstatus) ~ histology, data=Frontline %>% 
                     distinct(patientid, .keep_all = TRUE) %>% 
                     filter(exclude != "1")),
           title = "OS from date of diagnosis", 
           font.main = c(24, "bold", "black"), 
           font.x = c(20, "bold", "black"), font.y = c(20, "bold", "black"), 
           font.legend = c(14, "bold", "black"), font.tickslab = c(18, "bold", "black"), 
           size = 1.5,
           
           pval=TRUE, 
           # legend.labs=c(""),
           # palette = c("black", "green","blue", "red"), 
           # Add risk table
           risk.table = "abs_pct", risk.table.title = "Risk table (number(%))",
           risk.table.y.text = FALSE, risk.table.fontsize = 4.5, tables.height = 0.3,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           
           legend.title = "", xlab = "Time (in months)" 
) + guides(colour = guide_legend(nrow = 2))

# OS by tstage
ggsurvplot(survfit(Surv(aprox_month_at_os, vitalstatus) ~ tstage, data=Frontline %>% 
                     distinct(patientid, .keep_all = TRUE) %>% 
                     filter(exclude != "1")),
           title = "OS from date of diagnosis", 
           font.main = c(24, "bold", "black"), 
           font.x = c(20, "bold", "black"), font.y = c(20, "bold", "black"), 
           font.legend = c(14, "bold", "black"), font.tickslab = c(18, "bold", "black"), 
           size = 1.5,
           
           pval=TRUE, 
           # legend.labs=c(""),
           # palette = c("black", "green","blue", "red"), 
           # Add risk table
           risk.table = "abs_pct", risk.table.title = "Risk table (number(%))",
           risk.table.y.text = FALSE, risk.table.fontsize = 4.5, tables.height = 0.3,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           
           legend.title = "", xlab = "Time (in months)" 
) + guides(colour = guide_legend(nrow = 2))

# OS by bmi_cat
ggsurvplot(survfit(Surv(aprox_month_at_os, vitalstatus) ~ bmi_cat, data=Frontline %>% 
                     distinct(patientid, .keep_all = TRUE) %>% 
                     filter(exclude != "1")),
           title = "OS from date of diagnosis", 
           font.main = c(24, "bold", "black"), 
           font.x = c(20, "bold", "black"), font.y = c(20, "bold", "black"), 
           font.legend = c(14, "bold", "black"), font.tickslab = c(18, "bold", "black"), 
           size = 1.5,
           
           pval=TRUE, 
           # legend.labs=c(""),
           # palette = c("black", "green","blue", "red"), 
           # Add risk table
           risk.table = "abs_pct", risk.table.title = "Risk table (number(%))",
           risk.table.y.text = FALSE, risk.table.fontsize = 4.5, tables.height = 0.3,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           
           legend.title = "", xlab = "Time (in months)" 
) + guides(colour = guide_legend(nrow = 2))

# OS by brca1_tumor
ggsurvplot(survfit(Surv(aprox_month_at_os, vitalstatus) ~ brca1_tumor, data=Frontline %>% 
                     distinct(patientid, .keep_all = TRUE) %>% 
                     filter(exclude != "1")),
           title = "OS from date of diagnosis", 
           font.main = c(24, "bold", "black"), 
           font.x = c(20, "bold", "black"), font.y = c(20, "bold", "black"), 
           font.legend = c(14, "bold", "black"), font.tickslab = c(18, "bold", "black"), 
           size = 1.5,
           
           pval=TRUE, 
           # legend.labs=c(""),
           # palette = c("black", "green","blue", "red"), 
           # Add risk table
           risk.table = "abs_pct", risk.table.title = "Risk table (number(%))",
           risk.table.y.text = FALSE, risk.table.fontsize = 4.5, tables.height = 0.3,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           
           legend.title = "", xlab = "Time (in months)" 
) + guides(colour = guide_legend(nrow = 2))

# OS by brca2_tumor
ggsurvplot(survfit(Surv(aprox_month_at_os, vitalstatus) ~ brca2_tumor, data=Frontline %>% 
                     distinct(patientid, .keep_all = TRUE) %>% 
                     filter(exclude != "1")),
           title = "OS from date of diagnosis", 
           font.main = c(24, "bold", "black"), 
           font.x = c(20, "bold", "black"), font.y = c(20, "bold", "black"), 
           font.legend = c(14, "bold", "black"), font.tickslab = c(18, "bold", "black"), 
           size = 1.5,
           
           pval=TRUE, 
           # legend.labs=c(""),
           # palette = c("black", "green","blue", "red"), 
           # Add risk table
           risk.table = "abs_pct", risk.table.title = "Risk table (number(%))",
           risk.table.y.text = FALSE, risk.table.fontsize = 4.5, tables.height = 0.3,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           
           legend.title = "", xlab = "Time (in months)" 
) + guides(colour = guide_legend(nrow = 2))

# OS by brca2_tumor
ggsurvplot(survfit(Surv(aprox_month_at_os, vitalstatus) ~ brca1_germline, data=Frontline %>% 
                     distinct(patientid, .keep_all = TRUE) %>% 
                     filter(exclude != "1")),
           title = "OS from date of diagnosis", 
           font.main = c(24, "bold", "black"), 
           font.x = c(20, "bold", "black"), font.y = c(20, "bold", "black"), 
           font.legend = c(14, "bold", "black"), font.tickslab = c(18, "bold", "black"), 
           size = 1.5,
           
           pval=TRUE, 
           # legend.labs=c(""),
           # palette = c("black", "green","blue", "red"), 
           # Add risk table
           risk.table = "abs_pct", risk.table.title = "Risk table (number(%))",
           risk.table.y.text = FALSE, risk.table.fontsize = 4.5, tables.height = 0.3,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           
           legend.title = "", xlab = "Time (in months)" 
) + guides(colour = guide_legend(nrow = 2))

# OS by brca2_tumor
ggsurvplot(survfit(Surv(aprox_month_at_os, vitalstatus) ~ brca2_germline, data=Frontline %>% 
                     distinct(patientid, .keep_all = TRUE) %>% 
                     filter(exclude != "1")),
           title = "OS from date of diagnosis", 
           font.main = c(24, "bold", "black"), 
           font.x = c(20, "bold", "black"), font.y = c(20, "bold", "black"), 
           font.legend = c(14, "bold", "black"), font.tickslab = c(18, "bold", "black"), 
           size = 1.5,
           
           pval=TRUE, 
           # legend.labs=c(""),
           # palette = c("black", "green","blue", "red"), 
           # Add risk table
           risk.table = "abs_pct", risk.table.title = "Risk table (number(%))",
           risk.table.y.text = FALSE, risk.table.fontsize = 4.5, tables.height = 0.3,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           
           legend.title = "", xlab = "Time (in months)" 
) + guides(colour = guide_legend(nrow = 2))

#################################################################################################### II ### KM Treatment----
# OS by issurgery
ggsurvplot(survfit(Surv(aprox_month_at_os, vitalstatus) ~ issurgery, data=Frontline %>% 
                     distinct(patientid, .keep_all = TRUE) %>% 
                     filter(exclude != "1")),
           title = "OS from date of diagnosis", 
           font.main = c(24, "bold", "black"), 
           font.x = c(20, "bold", "black"), font.y = c(20, "bold", "black"), 
           font.legend = c(14, "bold", "black"), font.tickslab = c(18, "bold", "black"), 
           size = 1.5,
           
           pval=TRUE, 
           # legend.labs=c(""),
           # palette = c("black", "green","blue", "red"), 
           # Add risk table
           risk.table = "abs_pct", risk.table.title = "Risk table (number(%))",
           risk.table.y.text = FALSE, risk.table.fontsize = 4.5, tables.height = 0.3,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           
           legend.title = "", xlab = "Time (in months)" 
) + guides(colour = guide_legend(nrow = 2))

# OS by extentofdebulking
ggsurvplot(survfit(Surv(aprox_month_at_os, vitalstatus) ~ extentofdebulking, data=Frontline %>% 
                     distinct(patientid, .keep_all = TRUE) %>% 
                     filter(exclude != "1")),
           title = "OS from date of diagnosis", 
           font.main = c(24, "bold", "black"), 
           font.x = c(20, "bold", "black"), font.y = c(20, "bold", "black"), 
           font.legend = c(14, "bold", "black"), font.tickslab = c(18, "bold", "black"), 
           size = 1.5,
           
           pval=TRUE, 
           # legend.labs=c(""),
           # palette = c("black", "green","blue", "red"), 
           # Add risk table
           risk.table = "abs_pct", risk.table.title = "Risk table (number(%))",
           risk.table.y.text = FALSE, risk.table.fontsize = 4.5, tables.height = 0.3,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           
           legend.title = "", xlab = "Time (in months)" 
) + guides(colour = guide_legend(nrow = 2))

# OS by residualdiseasestatus
ggsurvplot(survfit(Surv(aprox_month_at_os, vitalstatus) ~ residualdiseasestatus, data=Frontline %>% 
                     distinct(patientid, .keep_all = TRUE) %>% 
                     filter(exclude != "1")),
           title = "OS from date of diagnosis", 
           font.main = c(24, "bold", "black"), 
           font.x = c(20, "bold", "black"), font.y = c(20, "bold", "black"), 
           font.legend = c(14, "bold", "black"), font.tickslab = c(18, "bold", "black"), 
           size = 1.5,
           
           pval=TRUE, 
           # legend.labs=c(""),
           # palette = c("black", "green","blue", "red"), 
           # Add risk table
           risk.table = "abs_pct", risk.table.title = "Risk table (number(%))",
           risk.table.y.text = FALSE, risk.table.fontsize = 4.5, tables.height = 0.3,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           
           legend.title = "", xlab = "Time (in months)" 
) + guides(colour = guide_legend(nrow = 2))



# OS by chemotherapy_type
ggsurvplot(survfit(Surv(aprox_month_at_os, vitalstatus) ~ chemotherapy_type, data=Frontline %>% 
                     distinct(patientid, .keep_all = TRUE) %>% 
                     filter(exclude != "1")),
           title = "OS from date of diagnosis", 
           font.main = c(24, "bold", "black"), 
           font.x = c(20, "bold", "black"), font.y = c(20, "bold", "black"), 
           font.legend = c(14, "bold", "black"), font.tickslab = c(18, "bold", "black"), 
           size = 1.5,
           
           pval=TRUE, 
           # legend.labs=c(""),
           # palette = c("black", "green","blue", "red"), 
           # Add risk table
           risk.table = "abs_pct", risk.table.title = "Risk table (number(%))",
           risk.table.y.text = FALSE, risk.table.fontsize = 4.5, tables.height = 0.3,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           
           legend.title = "", xlab = "Time (in months)" 
) + guides(colour = guide_legend(nrow = 2))

# OS by chemo
ggsurvplot(survfit(Surv(aprox_month_at_os, vitalstatus) ~ chemo, data=Frontline %>% 
                     distinct(patientid, .keep_all = TRUE) %>% 
                     filter(exclude != "1")),
           title = "OS from date of diagnosis", 
           font.main = c(24, "bold", "black"), 
           font.x = c(20, "bold", "black"), font.y = c(20, "bold", "black"), 
           font.legend = c(14, "bold", "black"), font.tickslab = c(18, "bold", "black"), 
           size = 1.5,
           
           pval=TRUE, 
           # legend.labs=c(""),
           # palette = c("black", "green","blue", "red"), 
           # Add risk table
           risk.table = "abs_pct", risk.table.title = "Risk table (number(%))",
           risk.table.y.text = FALSE, risk.table.fontsize = 4.5, tables.height = 0.3,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           
           legend.title = "", xlab = "Time (in months)" 
) + guides(colour = guide_legend(nrow = 2))

# OS by therapy
ggsurvplot(survfit(Surv(aprox_month_at_os, vitalstatus) ~ therapy, data=Frontline %>% 
                     distinct(patientid, .keep_all = TRUE) %>% 
                     filter(exclude != "1")),
           title = "OS from date of diagnosis", 
           font.main = c(24, "bold", "black"), 
           font.x = c(20, "bold", "black"), font.y = c(20, "bold", "black"), 
           font.legend = c(14, "bold", "black"), font.tickslab = c(18, "bold", "black"), 
           size = 1.5,
           
           pval=TRUE, 
           # legend.labs=c(""),
           # palette = c("black", "green","blue", "red"), 
           # Add risk table
           risk.table = "abs_pct", risk.table.title = "Risk table (number(%))",
           risk.table.y.text = FALSE, risk.table.fontsize = 4.5, tables.height = 0.3,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           
           legend.title = "", xlab = "Time (in months)" 
) + guides(colour = guide_legend(nrow = 2))

# OS by RDI_grp
ggsurvplot(survfit(Surv(aprox_month_at_os, vitalstatus) ~ RDI_grp, data=Frontline %>% 
                     distinct(patientid, .keep_all = TRUE) %>% 
                     filter(exclude != "1")),
           title = "OS from date of diagnosis", 
           font.main = c(24, "bold", "black"), 
           font.x = c(20, "bold", "black"), font.y = c(20, "bold", "black"), 
           font.legend = c(14, "bold", "black"), font.tickslab = c(18, "bold", "black"), 
           size = 1.5,
           
           pval=TRUE, 
           # legend.labs=c(""),
           # palette = c("black", "green","blue", "red"), 
           # Add risk table
           risk.table = "abs_pct", risk.table.title = "Risk table (number(%))",
           risk.table.y.text = FALSE, risk.table.fontsize = 4.5, tables.height = 0.3,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           
           legend.title = "", xlab = "Time (in months)" 
) + guides(colour = guide_legend(nrow = 2))

# OS by RDI_grp for platin
ggsurvplot(survfit(Surv(aprox_month_at_os, vitalstatus) ~ RDI_grp, data=Frontline %>% 
                     filter((exclude != "1") & str_detect(drugname, "platin")) %>% 
                     
                     distinct(patientid, .keep_all = TRUE)), 
           title = "OS from date of diagnosis", 
           font.main = c(24, "bold", "black"), 
           font.x = c(20, "bold", "black"), font.y = c(20, "bold", "black"), 
           font.legend = c(14, "bold", "black"), font.tickslab = c(18, "bold", "black"), 
           size = 1.5,
           
           pval=TRUE, 
           # legend.labs=c(""),
           # palette = c("black", "green","blue", "red"), 
           # Add risk table
           risk.table = "abs_pct", risk.table.title = "Risk table (number(%))",
           risk.table.y.text = FALSE, risk.table.fontsize = 4.5, tables.height = 0.3,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           
           legend.title = "", xlab = "Time (in months)" 
) + guides(colour = guide_legend(nrow = 2))

# OS by RDI_grp for taxel
ggsurvplot(survfit(Surv(aprox_month_at_os, vitalstatus) ~ RDI_grp, data=Frontline %>% 
                     filter((exclude != "1") & str_detect(drugname, "taxel")) %>% 
                     
                     distinct(patientid, .keep_all = TRUE)), 
           title = "OS from date of diagnosis", 
           font.main = c(24, "bold", "black"), 
           font.x = c(20, "bold", "black"), font.y = c(20, "bold", "black"), 
           font.legend = c(14, "bold", "black"), font.tickslab = c(18, "bold", "black"), 
           size = 1.5,
           
           pval=TRUE, 
           # legend.labs=c(""),
           # palette = c("black", "green","blue", "red"), 
           # Add risk table
           risk.table = "abs_pct", risk.table.title = "Risk table (number(%))",
           risk.table.y.text = FALSE, risk.table.fontsize = 4.5, tables.height = 0.3,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           
           legend.title = "", xlab = "Time (in months)" 
) + guides(colour = guide_legend(nrow = 2))


######## ad taxel as neo or as adju

#################################################################################################### II ### Regression----

# Regression vital status prediction
model <- glm(vitalstatus ~ bmi + bmi_cat + RDI_grp + raceeth, data = analysis_data, family = binomial)
tbl1 <- tbl_regression(model)
tbl2 <- tbl_regression(model, exponentiate = TRUE)
tbl_merge(list(tbl1, tbl2), tab_spanner = c("**Estimate**", "**Exp**"))

# extentofdebulking + residualdiseasestatus + 

model <- glm(vitalstatus ~ 
               histology + tstage + brca1_tumor + brca2_tumor + 
             issurgery +
             chemotherapy_type, 
             data = analysis_data, family = binomial)
tbl1 <- tbl_regression(model)
tbl2 <- tbl_regression(model, exponentiate = TRUE)
tbl_merge(list(tbl1, tbl2), tab_spanner = c("**Estimate**", "**Exp**"))


# Regression RDI prediction
model <- glm(RDI_grp ~ agecat + bmi_cat + raceeth + stagecat + histology + chemotherapy_type, data = Frontline %>% 
               distinct(patientid, .keep_all = TRUE) %>% 
               filter(exclude != "1"), family = binomial)
tbl1 <- tbl_regression(model)
tbl2 <- tbl_regression(model, exponentiate = TRUE)
tbl_merge(list(tbl1, tbl2), tab_spanner = c("**Estimate**", "**Exp**"))

#  for carbo
model <- glm(RDI_grp ~ agecat + raceeth + stagecat + histology + bmi_cat, data = Frontline %>% 
               filter(drugname == "carboplatin" & chemotherapy_type == "Adjuvant") %>% 
               distinct(patientid, .keep_all = TRUE) %>% 
               filter(exclude != "1"), family = binomial)
tbl1 <- tbl_regression(model)
tbl2 <- tbl_regression(model, exponentiate = TRUE)
tbl3 <- tbl_merge(list(tbl1, tbl2), tab_spanner = c("**Estimate**", "**Exp**"))

model <- glm(RDI_grp ~ agecat + raceeth + stagecat + histology + bmi_cat, data = Frontline %>% 
               filter(drugname == "carboplatin" & chemotherapy_type == "Neoadjuvant") %>% 
               distinct(patientid, .keep_all = TRUE) %>% 
               filter(exclude != "1"), family = binomial)
tbl1 <- tbl_regression(model)
tbl2 <- tbl_regression(model, exponentiate = TRUE)
tbl4 <- tbl_merge(list(tbl1, tbl2), tab_spanner = c("**Estimate**", "**Exp**"))

# tbl5 <- 
  tbl_merge(list(tbl3, tbl4), tab_spanner = c("**Adjuvant**", "**Neoadjuvant**"))

#  for taxel
model <- glm(RDI_grp ~ agecat + raceeth + stagecat + histology + bmi_cat, data = Frontline %>% 
               filter(str_detect(drugname,"taxel") & (chemotherapy_type == "Adjuvant")) %>% 
               distinct(patientid, .keep_all = TRUE) %>% 
               filter(exclude != "1"), family = binomial)
tbl1 <- tbl_regression(model)
tbl2 <- tbl_regression(model, exponentiate = TRUE)
tbl3 <- tbl_merge(list(tbl1, tbl2), tab_spanner = c("**Estimate**", "**Exp**"))

model <- glm(RDI_grp ~ agecat + raceeth + stagecat + histology + bmi_cat, data = Frontline %>% 
               filter(str_detect(drugname,"taxel") & (chemotherapy_type == "Neoadjuvant")) %>% 
               distinct(patientid, .keep_all = TRUE) %>% 
               filter(exclude != "1"), family = binomial)
tbl1 <- tbl_regression(model)
tbl2 <- tbl_regression(model, exponentiate = TRUE)
tbl4 <- tbl_merge(list(tbl1, tbl2), tab_spanner = c("**Estimate**", "**Exp**"))

# tbl6 <- 
  tbl_merge(list(tbl4, tbl3), tab_spanner = c("**Adjuvant**", "**Neoadjuvant**"))





