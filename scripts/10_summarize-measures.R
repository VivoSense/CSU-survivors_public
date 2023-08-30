# summarize relevant measures for studies 1 and 2
# written by shelby bachman, shelby.bachman@vivosense.com


# summarize self-reported measures ----------------------------------------

measures_subjective <- c('bl_qol_factg_total',
                         'bl_qol_physsubscale',
                         'bl_qol_physsubscale_5itemsub',
                         'promis_pf_t')

measures_subjective_desc <- c('FACT-G: Total well-being (0-108)',
                              'Physical well-being subscale (0-28)',
                              'Physical well-being 5-item subset (0-20)',
                              'Linked PROMIS-PF: Physical function (19-61)')

summary_subjective <- as.data.frame(
  cbind(
    measure_name = measures_subjective_desc,
    sapply(data_survivors[, measures_subjective],
           function(x) round(mean(x, na.rm = TRUE), 1)
    ) %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = 'measure') %>%
      rename('mean' = '.'),
    sapply(data_survivors[, measures_subjective],
           function(x) round(sd(x, na.rm = TRUE), 1)
    ) %>%
      as.data.frame() %>%
      rename('sd' = '.'),
    sapply(data_survivors[, measures_subjective],
           function(x) paste(round(min(x, na.rm = TRUE), 1), ' - ', round(max(x, na.rm = TRUE), 1), sep = '') 
    ) %>%
      as.data.frame() %>%
      rename('range' = '.')
  )
) %>%
  rowwise() %>%
  # format mean (SD)
  mutate(mean_SD = paste(mean, ' (', sd, ')', sep = '')) %>%
  rowwise() %>%
  select(measure_name, mean_SD, range)


# summarize predicted VO2 -------------------------------------------------

measures_vo2 <- 'vo2_hrr'
measures_vo2_desc <- 'Predicted submaximal VO2'

summary_vo2 <- as.data.frame(
  cbind(
    measure_name = measures_vo2_desc,
    mean = data_survivors[,measures_vo2] %>% pull(measures_vo2) %>% mean(na.rm = TRUE) %>% round(1),
    sd = data_survivors[,measures_vo2] %>% pull(measures_vo2) %>% sd(na.rm = TRUE) %>% round(1),
    range = paste(
      round(min(data_survivors[,measures_vo2], na.rm = TRUE)), 
      ' - ',
      round(max(data_survivors[,measures_vo2], na.rm = TRUE)), 
      sep = '')
  )
) %>%
  rowwise() %>%
  # format mean (SD)
  mutate(mean_SD = paste(mean, ' (', sd, ')', sep = '')) %>%
  # select only relevant columns
  select(measure_name, mean_SD, range)


# summarize measures of real-world physical behavior ----------------------

measures_physbehav <- c('TotalSedentaryTime.m.', 
                        'TotalRLMsCount',
                        'light_min',
                        'mvpa_min',
                        'TimeInRLMsBouts>=1m',
                        'WeightedMedianCadence>=1m',
                        'PeakRLMs30s')

measures_physbehav_desc <- c('Daily sedentary time (min)',
                             'Daily step count',
                             'Daily time in light activity (min)',
                             'Daily time in mod-to-vig activity (min)',
                             'Daily time in stepping bouts >=1min (min)',
                             'Weighted median cadence in stepping bouts >=1min (steps/min)',
                             'Peak 30sec cadence (steps/min)')

summary_physbehav <- as.data.frame(
  cbind(
    measure_name = measures_physbehav_desc,
    sapply(data_survivors[,measures_physbehav],
           function(x) round(mean(x, na.rm = TRUE), 1)
    ) %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = 'measure') %>%
      dplyr::rename('mean' = '.'),
    sapply(data_survivors[,measures_physbehav],
           function(x) round(sd(x, na.rm = TRUE), 1)
    ) %>%
      as.data.frame() %>%
      dplyr::rename('sd' = '.'),
    
    sapply(data_survivors[,measures_physbehav],
           function(x) paste(round(min(x, na.rm = TRUE), 1), ' - ', round(max(x, na.rm = TRUE), 1), sep = '') 
    ) %>%
      as.data.frame() %>%
      dplyr::rename('range' = '.')
  )
) %>%
  rowwise() %>%
  # format mean (SD)
  mutate(mean_SD = paste(mean, ' (', sd, ')', sep = '')) %>%
  # select only relevant columns
  select(measure_name, mean_SD, range)


# create table 2 ----------------------------------------------------------

table2 <- as.data.frame(
  rbind(
    summary_subjective,
    summary_vo2,
    summary_physbehav
  )
)
