# transform non-normally distributed variables
# using a rank-based inverse normal (RIN) transformation
# written by shelby bachman, shelby.bachman@vivosense.com

# note that columns with missing values
# are transformed with a function designed to handle missing values (see `01_define-functions.R`)


# apply transformations to self-reported measures -------------------------

# FACT-G total and subscales
data_survivors <- apply_rin_transform_withNAs(data_survivors, 'bl_qol_factg_total', 'bl_qol_factg_total_rin')
data_survivors <- apply_rin_transform_withNAs(data_survivors, 'bl_qol_physsubscale', 'bl_qol_physsubscale_rin')
data_survivors <- apply_rin_transform_withNAs(data_survivors, 'bl_qol_physsubscale_5itemsub', 'bl_qol_physsubscale_5itemsub_rin')
    
# linked PROMIS-PF
data_survivors <- apply_rin_transform_withNAs(data_survivors, 'promis_pf_t', 'promis_pf_t_rin')


# apply transformations to measures of real-world physical behavior -------

data_survivors_temp <- data_survivors %>%
  select(c('TotalSedentaryTime.m.', 
           'TotalRLMsCount',
           'light_min',
           'mvpa_min',
           'TimeInRLMsBouts>=1m',
           'WeightedMedianCadence>=1m',
           'PeakRLMs30s'))
mat_rin <- lapply(data_survivors_temp, RankNorm)
df_rin <- as.data.frame(mat_rin)
names(df_rin) <- paste0(names(mat_rin), '_rin')
data_survivors <- cbind(data_survivors, df_rin)
rm(data_survivors_temp, mat_rin, df_rin)
