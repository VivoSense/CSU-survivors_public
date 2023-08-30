# summarize missingness in data
# written by shelby bachman, shelby.bachman@vivosense.com


# function to summarize missingness ---------------------------------------

summarize_missingness <- function(x) {
  n_missing <- sum(is.na(x))
  pct_missing <- (n_missing / length(x)) * 100
  return(paste(n_missing, '(', round(pct_missing, 2), '%)', sep = ''))
}


# summarize missingness for each study separately -------------------------

summary_missingness <- 
  as.data.frame(
    cbind(
      sapply(data_survivors %>%
               filter(study == 1) %>%
               dplyr::select(c('age_demographics', 
                               'sex',
                               'bmi',
                               'education',
                               'time_since_diagnosis_months',
                               'bl_qol_factg_total',
                               'bl_qol_physsubscale',
                               'bl_qol_physsubscale_5itemsub',
                               'promis_pf_t',
                               'vo2_hrr',
                               'TotalSedentaryTime.m.', 
                               'TotalRLMsCount',
                               'TimeInRLMsBouts>=1m',
                               'WeightedMedianCadence>=1m',
                               'PeakRLMs30s')
               ),
             summarize_missingness) %>% 
        as.data.frame(),
      sapply(data_survivors %>%
               filter(study == 2) %>%
               dplyr::select(c('age_demographics', 
                               'sex',
                               'bmi',
                               'education',
                               'time_since_diagnosis_months',
                               'bl_qol_factg_total',
                               'bl_qol_physsubscale',
                               'bl_qol_physsubscale_5itemsub',
                               'promis_pf_t',
                               'vo2_hrr',
                               'TotalSedentaryTime.m.', 
                               'TotalRLMsCount',
                               'TimeInRLMsBouts>=1m',
                               'WeightedMedianCadence>=1m',
                               'PeakRLMs30s')
               ),
             summarize_missingness) %>%
        as.data.frame()
    )
  )

# convert rownames to a column, rename columns
summary_missingness$measure <- rownames(summary_missingness)
rownames(summary_missingness) <- NULL
summary_missingness <- summary_missingness[,c(3, 1, 2)]
names(summary_missingness) <- c('measure', 'study_1', 'study_2')


# summarize missingness for combined dataset ------------------------------

summary_missingness_combined <- 
  sapply(data_survivors %>%
           dplyr::select(c('age_demographics', 
                           'sex',
                           'bmi',
                           'education',
                           'time_since_diagnosis_months',
                           'bl_qol_factg_total',
                           'bl_qol_physsubscale',
                           'bl_qol_physsubscale_5itemsub',
                           'promis_pf_t',
                           'vo2_hrr',
                           'TotalSedentaryTime.m.', 
                           'TotalRLMsCount',
                           'TimeInRLMsBouts>=1m',
                           'WeightedMedianCadence>=1m',
                           'PeakRLMs30s')
           ),
         summarize_missingness) %>% 
  as.data.frame()

# convert rownames to a column, rename columns
summary_missingness_combined$measure <- rownames(summary_missingness_combined)
rownames(summary_missingness_combined) <- NULL
summary_missingness_combined <- summary_missingness_combined[,c(2, 1)]
names(summary_missingness_combined) <- c('measure', 'combined')

# join with missingness data from individual datasets
summary_missingness <- summary_missingness %>%
  left_join(summary_missingness_combined,
            by = 'measure')
rm(summary_missingness_combined)


# calculate n participants with sub-max measures available ----------------

n_vo2 <- data_survivors %>%
  filter(!is.na(vo2_hrr)) %>%
  nrow()

pct_vo2 <- round((n_vo2 / (data_survivors %>% nrow()))*100, 1)
