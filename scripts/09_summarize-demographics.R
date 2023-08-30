# summarize demographic variables and cancer characteristics
# written by shelby bachman, shelby.bachman@vivosense.com


# summarize n in both datasets --------------------------------------------

n_study1 <- data_survivors %>%
  filter(study == 1) %>%
  nrow()

n_study2 <- data_survivors %>%
  filter(study == 2) %>%
  nrow()

n_combined <- data_survivors %>%
  nrow()


# summarize demographics for study 1 --------------------------------------

summary_demographics_study1 <- as.data.frame(
  rbind(
    summarize_variable_numeric(data_survivors$age_demographics[data_survivors$study == 1], 
                               description = 'Age (years)',
                               rounding_level_meansd = 1,
                               rounding_level_range = 0),
    summarize_variable_categorical(data_survivors$sex[data_survivors$study == 1],
                                   description = 'Sex', 
                                   levels = c('Female', 'Male')),
    summarize_variable_numeric(data_survivors$bmi[data_survivors$study == 1], 
                               description = 'Body mass index',
                               rounding_level_meansd = 1,
                               rounding_level_range = 0),
    summarize_variable_categorical(data_survivors$education[data_survivors$study == 1],
                                   description = 'Education level',
                                   levels = c(1, 2, 3, 4, 5, NA))
  )
) %>%
  rowwise() %>%
  # set non-grouped variables to appear bold in html
  mutate(
    level = cell_spec(level, 'html',
                      bold = ifelse(level %in% c('Age (years)', 'Body mass index'), TRUE, FALSE))
  )


# summarize demographics for study 2 --------------------------------------

summary_demographics_study2 <- as.data.frame(
  rbind(
    summarize_variable_numeric(data_survivors$age_demographics[data_survivors$study == 2], 
                               description = 'Age (years)',
                               rounding_level_meansd = 1,
                               rounding_level_range = 0),
    summarize_variable_categorical(data_survivors$sex[data_survivors$study == 2],
                                   description = 'Sex', 
                                   levels = c('Female', 'Male')),
    summarize_variable_numeric(data_survivors$bmi[data_survivors$study == 2], 
                               description = 'Body mass index',
                               rounding_level_meansd = 1,
                               rounding_level_range = 0),
    summarize_variable_categorical(data_survivors$education[data_survivors$study == 2],
                                   description = 'Education level',
                                   levels = c(1, 2, 3, 4, 5, NA))
  )
) %>%
  rowwise() %>%
  # set non-grouped variables to appear bold in html
  mutate(
    level = cell_spec(level, 'html',
                      bold = ifelse(level %in% c('Age (years)', 'Body mass index'), TRUE, FALSE))
  )


# summarize demographic variables for combined dataset --------------------

summary_demographics <- as.data.frame(
  rbind(
    summarize_variable_numeric(data_survivors$age_demographics, 
                               description = 'Age (years)',
                               rounding_level_meansd = 1,
                               rounding_level_range = 0),
    summarize_variable_categorical(data_survivors$sex,
                                   description = 'Sex', 
                                   levels = c('Female', 'Male')),
    summarize_variable_numeric(data_survivors$bmi, 
                               description = 'Body mass index',
                               rounding_level_meansd = 1,
                               rounding_level_range = 0),
    summarize_variable_categorical(data_survivors$education,
                                   description = 'Education level',
                                   levels = c(1, 2, 3, 4, 5, NA))
  )
) %>%
  rowwise() %>%
  # set non-grouped variables to appear bold in html
  mutate(
    level = cell_spec(level, 'html',
                      bold = ifelse(level %in% c('Age (years)', 'Body mass index'), TRUE, FALSE))
  )


# summarize cancer characteristics for study 1 ----------------------------

summary_cancerchars_study1 <- as.data.frame(
  rbind(
    summarize_variable_numeric(data_survivors$time_since_diagnosis_months[data_survivors$study == 1],
                               description = 'Time since diagnosis (months)',
                               rounding_level_meansd = 1,
                               rounding_level_range = 0),
    
    summarize_variable_numeric(data_survivors$time_since_last_treatment_months[data_survivors$study == 1],
                               description = 'Time since last treatment (months)',
                               rounding_level_meansd = 1,
                               rounding_level_range = 0),
    
    summarize_variable_categorical(data_survivors$cancer_stage[data_survivors$study == 1],
                                   description = 'Cancer stage at diagnosis',
                                   levels = c('0', 'I', 'II', 'III', 'IV', 'Unsure')),
    
    summarize_variable_categorical(data_survivors$had_treatment[data_survivors$study == 1], 
                                   description = 'Had treatment', 
                                   levels = 'Had treatment'),
    
    summarize_variable_categorical(data_survivors$had_chemo[data_survivors$study == 1], 
                                   description = 'Chemotherapy', 
                                   levels = 1) %>%
      mutate(level = 'Had chemotherapy', .after = variable),
    
    summarize_variable_categorical(data_survivors$had_radiation[data_survivors$study == 1], 
                                   description = 'Radiation', 
                                   levels = 1) %>%
      mutate(level = 'Had radiation', .after = variable),
    
    summarize_variable_categorical(data_survivors$had_surgery[data_survivors$study == 1], 
                                   description = 'Surgery', 
                                   levels = 1) %>%
      mutate(level = 'Had surgery', .after = variable),
    
    summarize_variable_categorical(data_survivors$had_other[data_survivors$study == 1], 
                                   description = 'Other treatment', 
                                   levels = 1) %>%
      mutate(level = 'Had other', .after = variable),
    
    summarize_variable_categorical(data_survivors$n_treatment_types[data_survivors$study == 1], 
                                   description = 'Had treatment', 
                                   levels = c(1, 2, 3, 4))
    
  )
) %>%
  rowwise() %>%
  # set non-grouped variables to appear bold in html
  mutate(
    level = cell_spec(level, 'html',
                      bold = ifelse(level %in% c('Time since diagnosis (months)', 'Time since last treatment (months)'), TRUE, FALSE)) 
  )


# summarize cancer characteristics for dataset #2 -------------------------

summary_cancerchars_study2 <- as.data.frame(
  rbind(
    summarize_variable_numeric(data_survivors$time_since_diagnosis_months[data_survivors$study == 2],
                               description = 'Time since diagnosis (months)',
                               rounding_level_meansd = 1,
                               rounding_level_range = 0),
    
    summarize_variable_numeric(data_survivors$time_since_last_treatment_months[data_survivors$study == 2],
                               description = 'Time since last treatment (months)',
                               rounding_level_meansd = 1,
                               rounding_level_range = 0),
    
    summarize_variable_categorical(data_survivors$cancer_stage[data_survivors$study == 2],
                                   description = 'Cancer stage at diagnosis',
                                   levels = c('0', 'I', 'II', 'III', 'IV', 'Unsure')),
    
    summarize_variable_categorical(data_survivors$had_treatment[data_survivors$study == 2], 
                                   description = 'Had treatment', 
                                   levels = 'Had treatment'),
    
    summarize_variable_categorical(data_survivors$had_chemo[data_survivors$study == 2], 
                                   description = 'Chemotherapy', 
                                   levels = 1) %>%
      mutate(level = 'Had chemotherapy', .after = variable),
    
    summarize_variable_categorical(data_survivors$had_radiation[data_survivors$study == 2], 
                                   description = 'Radiation', 
                                   levels = 1) %>%
      mutate(level = 'Had radiation', .after = variable),
    
    summarize_variable_categorical(data_survivors$had_surgery[data_survivors$study == 2], 
                                   description = 'Surgery', 
                                   levels = 1) %>%
      mutate(level = 'Had surgery', .after = variable),
    
    summarize_variable_categorical(data_survivors$had_other[data_survivors$study == 2], 
                                   description = 'Other treatment', 
                                   levels = 1) %>%
      mutate(level = 'Had other', .after = variable),
    
    summarize_variable_categorical(data_survivors$n_treatment_types[data_survivors$study == 2], 
                                   description = 'Had treatment', 
                                   levels = c(1, 2, 3, 4))
    
  )
) %>%
  rowwise() %>%
  # set non-grouped variables to appear bold in html
  mutate(
    level = cell_spec(level, 'html',
                      bold = ifelse(level %in% c('Time since diagnosis (months)', 'Time since last treatment (months)'), TRUE, FALSE)) 
  )


# summarize cancer characteristics for combined dataset -------------------

summary_cancerchars <- as.data.frame(
  rbind(
    summarize_variable_numeric(data_survivors$time_since_diagnosis_months,
                               description = 'Time since diagnosis (months)',
                               rounding_level_meansd = 1,
                               rounding_level_range = 0),
    
    summarize_variable_numeric(data_survivors$time_since_last_treatment_months,
                               description = 'Time since last treatment (months)',
                               rounding_level_meansd = 1,
                               rounding_level_range = 0),
    
    summarize_variable_categorical(data_survivors$cancer_stage,
                                   description = 'Cancer stage at diagnosis',
                                   levels = c('0', 'I', 'II', 'III', 'IV', 'Unsure')),
    
    summarize_variable_categorical(data_survivors$had_treatment, 
                                   description = 'Had treatment', 
                                   levels = 'Had treatment'),
    
    summarize_variable_categorical(data_survivors$had_chemo, 
                                   description = 'Chemotherapy', 
                                   levels = 1) %>%
      mutate(level = 'Had chemotherapy', .after = variable),
    
    summarize_variable_categorical(data_survivors$had_radiation, 
                                   description = 'Radiation', 
                                   levels = 1) %>%
      mutate(level = 'Had radiation', .after = variable),
    
    summarize_variable_categorical(data_survivors$had_surgery, 
                                   description = 'Surgery', 
                                   levels = 1) %>%
      mutate(level = 'Had surgery', .after = variable),
    
    summarize_variable_categorical(data_survivors$had_other, 
                                   description = 'Other treatment', 
                                   levels = 1) %>%
      mutate(level = 'Had other', .after = variable),
    
    summarize_variable_categorical(data_survivors$n_treatment_types, 
                                   description = 'Had treatment', 
                                   levels = c(1, 2, 3, 4))
    
  )
) %>%
  rowwise() %>%
  # set non-grouped variables to appear bold in html
  mutate(
    level = cell_spec(level, 'html',
                      bold = ifelse(level %in% c('Time since diagnosis (months)', 'Time since last treatment (months)'), TRUE, FALSE)) 
  )


# handle cancer stage = 'unused' ------------------------------------------

# now that cancer stage is summarized, convert values of 'Unsure' to NA
data_survivors <- data_survivors %>%
  rowwise() %>%
  mutate(cancer_stage = ifelse(cancer_stage == 'Unsure', NA, cancer_stage))


# summarize cancer types for dataset #1 -----------------------------------

summary_cancertype_study1 <- 
  summarize_variable_categorical(data_survivors$cancer_type[data_survivors$study == 1],
                                 description = 'Cancer type at diagnosis',
                                 levels = data_survivors$cancer_type %>% unique()) %>%
  arrange(level)


# summarize cancer types for dataset #2 -----------------------------------

summary_cancertype_study2 <- 
  summarize_variable_categorical(data_survivors$cancer_type[data_survivors$study == 2],
                                 description = 'Cancer type at diagnosis',
                                 levels = data_survivors$cancer_type %>% unique()) %>%
  arrange(level)


# summarize cancer types for combined dataset -----------------------------

summary_cancertype <- 
  summarize_variable_categorical(data_survivors$cancer_type,
                                 description = 'Cancer type at diagnosis',
                                 levels = data_survivors$cancer_type %>% unique()) %>%
  arrange(level)


# create table comparing demographics for datasets 1 and 2 ----------------

summary_demographics_comparison <- as.data.frame(
  cbind(
    full_join(
      summary_demographics_study1 %>%
        rename(N_pct_1 = N_pct,
               mean_SD_1 = mean_SD,
               range_1 = range),
      
      summary_demographics_study2 %>%
        rename(N_pct_2 = N_pct,
               mean_SD_2 = mean_SD,
               range_2 = range),
      
      by = c('variable', 'level')
    ),
    comparison_p = compare_datasets_pval_demographics
  )
)


# create table comparing cancer characteristics for datasets 1 and 2 ------

summary_cancerchars_comparison <- as.data.frame(
  cbind(
    full_join(
      summary_cancerchars_study1 %>%
        rename(N_pct_1 = N_pct,
               mean_SD_1 = mean_SD,
               range_1 = range),
      
      summary_cancerchars_study2 %>%
        rename(N_pct_2 = N_pct,
               mean_SD_2 = mean_SD,
               range_2 = range),
      
      by = c('variable', 'level')
    ),
   comparison_p = compare_datasets_pval_cancerchars 
  )
)


# create table 1 (combined dataset summary) -------------------------------

table1 <- as.data.frame(
  rbind(
    summary_demographics %>%
      dplyr::select(-variable),
    summary_cancerchars %>%
      dplyr::select(-variable)
  )
)


# create table S1 (dataset comparison) ------------------------------------

tableS1 <- as.data.frame(
  rbind(
    summary_demographics_comparison %>%
      dplyr::select(-variable),
    summary_cancerchars_comparison %>%
      dplyr::select(-variable)
  )
)


# create table S2 (cancer types) ------------------------------------------

tableS2 <- summary_cancertype_study1 %>%
  dplyr::select(variable, level, N_pct_study1 = N_pct) %>%
  left_join(summary_cancertype_study2 %>%
              dplyr::select(variable, level, N_pct_study2 = N_pct),
            by = c('variable', 'level')) %>%
  left_join(summary_cancertype %>%
              dplyr::select(variable, level, N_pct_combined = N_pct),
            by = c('variable', 'level'))
