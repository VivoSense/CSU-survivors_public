# compare variables across studies 1 and 2
# written by shelby bachman, shelby.bachman@vivosense.com


# compare demographic variables -------------------------------------------

# age
compare_datasets_age <- t.test(data_survivors$age_demographics[data_survivors$study == 1], 
                               data_survivors$age_demographics[data_survivors$study == 2],
                               paired = FALSE, 
                               var.equal = FALSE)

# sex
sex_mat <- as.table(rbind(as.numeric(table(data_survivors$sex[data_survivors$study == 1])),
                             as.numeric(table(data_survivors$sex[data_survivors$study == 2]))))
dimnames(sex_mat) <- list(study = c(1, 2),
                          sex = c('M', 'F'))
compare_datasets_sex <- chisq.test(sex_mat)
rm(sex_mat)

# education
edu_mat <- as.table(rbind(c(0, as.numeric(table(data_survivors$education[data_survivors$study == 1], useNA = 'always'))),
                          as.numeric(table(data_survivors$education[data_survivors$study == 2], useNA = 'always'))))
dimnames(edu_mat) <- list(study = c(1, 2),
                          edu_level = c(2, 3, 4, 5, 'NA'))
compare_datasets_edu <- chisq.test(edu_mat)

# BMI
compare_datasets_bmi <- t.test(data_survivors$bmi[data_survivors$study == 1], 
                               data_survivors$bmi[data_survivors$study == 2],
                               paired = FALSE, 
                               var.equal = FALSE)

# compile vector of p-values
compare_datasets_pval_demographics <- c(
  compare_datasets_age$p.value %>% papaja::printp(add_equals = TRUE),
  compare_datasets_sex$p.value %>% papaja::printp(add_equals = TRUE),
  NA,
  compare_datasets_bmi$p.value %>% papaja::printp(add_equals = TRUE),
  compare_datasets_edu$p.value %>% papaja::printp(add_equals = TRUE),
  NA,
  NA,
  NA,
  NA,
  NA
 )


# compare cancer characteristics ------------------------------------------

# time since diagnosis
compare_datasets_tsd <- t.test(data_survivors$time_since_diagnosis_months[data_survivors$study == 1], 
                               data_survivors$time_since_diagnosis_months[data_survivors$study == 2],
                               paired = FALSE, 
                               var.equal = FALSE)

# time since last treatment
compare_datasets_tst <- t.test(data_survivors$time_since_last_treatment_months[data_survivors$study == 1], 
                               data_survivors$time_since_last_treatment_months[data_survivors$study == 2],
                               paired = FALSE, 
                               var.equal = FALSE)

# stage
stage_mat <- as.table(rbind(as.numeric(table(data_survivors$cancer_stage[data_survivors$study == 1])),
                            c(0, 0, as.numeric(table(data_survivors$cancer_stage[data_survivors$study == 2])))))
dimnames(stage_mat) <- list(study = c(1, 2),
                            stage = c('0', 'I', 'II', 'III', 'IV', 'Unsure'))
compare_datasets_stage <- chisq.test(stage_mat)

# number of treatments
treatment_mat <- as.table(rbind(as.numeric(table(data_survivors$n_treatment_types[data_survivors$study == 1])),
                          c(as.numeric(table(data_survivors$n_treatment_types[data_survivors$study == 2])), 0)))
dimnames(treatment_mat) <- list(study = c(1, 2),
                          n_treatments = c(1, 2, 3, 4))
compare_datasets_ntreatment <- chisq.test(treatment_mat)

# compile vector of p-values
compare_datasets_pval_cancerchars <- c(
  compare_datasets_tsd$p.value %>% papaja::printp(add_equals = TRUE),
  compare_datasets_tst$p.value %>% papaja::printp(add_equals = TRUE),
  compare_datasets_stage$p.value %>% papaja::printp(add_equals = TRUE),
  rep(NA, 10),
  compare_datasets_ntreatment$p.value %>% papaja::printp(add_equals = TRUE),
  rep(NA, 3)
)
