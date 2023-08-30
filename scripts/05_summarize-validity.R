# summarize validity and non-wear
# written by shelby bachman, shelby.bachman@vivosense.com


# apply exclusions based on valid days ------------------------------------

# calculate number of participants with <4 valid days
n_lessthan4validdays <- data_survivors %>%
  filter(NumberValidDays < 4) %>%
  nrow()

# summarize number of valid days and non-wear time ------------------------

summary_validdays <- paste(mean(data_survivors$NumberValidDays) %>% round(1),
                           ' days (SD = ',
                           sd(data_survivors$NumberValidDays) %>% round(1),
                           ', range = ',
                           min(data_survivors$NumberValidDays) %>% round(0),
                           ' - ',
                           max(data_survivors$NumberValidDays) %>% round(0),
                           ')',
                           sep = '')

summary_nonwear <- paste(mean(data_survivors$NonWearTime.m.) %>% round(1),
                         ' minutes (SD = ',
                         sd(data_survivors$NonWearTime.m.) %>% round(1),
                         ', range = ',
                         min(data_survivors$NonWearTime.m.) %>% round(1),
                         ' - ',
                         max(data_survivors$NonWearTime.m.) %>% round(1),
                         ')',
                         sep = '')
