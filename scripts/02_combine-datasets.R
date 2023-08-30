# combine data from studies 1 and 2
# written by shelby bachman, shelby.bachman@vivosense.com


# combine summary data ----------------------------------------------------

data_survivors <- as.data.frame(
  rbind(
    data_study1 %>%
      mutate(study = 1, .after = 'record_id'),
    
    data_study2 %>%
      mutate(study = 2, .after = 'record_id')
  )
)


# combine daily summary data ----------------------------------------------

data_survivors_daily <- as.data.frame(
  rbind(
    data_study1_daily %>%
      mutate(study = 1, .after = 'record_id'),
    
    data_study2_daily %>%
      mutate(study = 2, .after = 'record_id')
  )
)