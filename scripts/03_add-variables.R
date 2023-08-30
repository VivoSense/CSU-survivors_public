# create new variables
# written by shelby bachman, shelby.bachman@vivosense.com


# add variables to summary data -------------------------------------------

data_survivors <- data_survivors %>%
  rowwise() %>%
  
  mutate(

    ### calculate PWB 5-item subscale scores (for linking to PROMIS-PF)
    bl_qol_physsubscale_5itemsub = score_factg_5item_subscale(gp1 = bl_qol_phys1,
                                                              gp3 = bl_qol_phys3,
                                                              gp4 = bl_qol_phys4, 
                                                              gp6 = bl_qol_phys6, 
                                                              gp7 = bl_qol_phys7),
    
    ### link PWB 5-item subscale score to PROMIS PF T-score
    link_factg_to_promis(bl_qol_physsubscale_5itemsub),
    
    ### calculate minutes in light and mod-to-vigorous activity
    mvpa_min = `TimeInRLMsInCadenceBand<75spmInBouts>=1m<5m` + `TimeInRLMsInCadenceBand<75spmInBouts>=5m<10m` + `TimeInRLMsInCadenceBand<75spmInBouts>=10m<20m` + `TimeInRLMsInCadenceBand<75spmInBouts>=20m`,
    light_min = 1440 - `NonWearTime.m.` - `PrimaryLyingTime.m.` - `TotalSedentaryTime.m.` - mvpa_min,
    
    ### calculate time since last treatment was completed
    time_since_last_treatment_months = ifelse(is.na(time_since_chemo_months) & is.na(time_since_radiation_months) & is.na(time_since_surgery_months) & is.na(time_since_other_months), NA,
                                                   min(time_since_chemo_months, time_since_radiation_months, time_since_surgery_months, time_since_other_months, na.rm = TRUE))
    
  )


# add variables to daily summary data -------------------------------------

data_survivors_daily <- data_survivors_daily %>%
  rowwise() %>%
  
  mutate(
    
    ### calculate minutes in light and mod-to-vigorous activity
    mvpa_min = `TimeInRLMsInCadenceBand<75spmInBouts>=1m<5m` + `TimeInRLMsInCadenceBand<75spmInBouts>=5m<10m` + `TimeInRLMsInCadenceBand<75spmInBouts>=10m<20m` + `TimeInRLMsInCadenceBand<75spmInBouts>=20m`,
    light_min = 1440 - `NonWearTime.m.` - `PrimaryLyingTime.m.` - `TotalSedentaryTime.m.` - mvpa_min
    
  )
