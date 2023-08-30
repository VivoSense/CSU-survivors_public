# check for normality of each variable
# written by shelby bachman, shelby.bachman@vivosense.com


# perform shapiro-wilk tests, testing normality for variables of interest
summary_normality <- do.call(rbind, 
        
        lapply(data_survivors %>% 
                 dplyr::select(

                   'time_since_diagnosis_months',
                   
                   'bl_qol_factg_total',
                   'bl_qol_physsubscale',
                   'bl_qol_physsubscale_5itemsub',
    
                   'promis_pf_t',
                   
                   'vo2_hrr',

                   'TotalSedentaryTime.m.', 
                   'TotalRLMsCount',
                   'light_min',
                   'mvpa_min',
                   'TimeInRLMsBouts>=1m',
                   'WeightedMedianCadence>=1m',
                   'PeakRLMs30s'
                   
                 ), 
               
               function(x) shapiro.test(x)[c("statistic", "p.value")])
        
) %>%
  as.data.frame()

# add `variable` column
summary_normality$variable <- rownames(summary_normality)
rownames(summary_normality) <- c()
summary_normality %>%
  select(variable, statistic, p.value)

# add column indicating whether variable deviates from normality
summary_normality <- summary_normality %>%
  
  rowwise() %>%
  
  mutate(
    is_nonnormal = ifelse(p.value < .05, 1, 0))