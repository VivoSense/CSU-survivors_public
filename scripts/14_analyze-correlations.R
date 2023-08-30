# test associations between measures of interest
# written by shelby bachman, shelby.bachman@vivosense.com


# load packages -----------------------------------------------------------

# check for necessary R packages
# and install as needed
packages <- c('ppcor', 'psych')

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
    }
  }
)


# settings for correlation plots ------------------------------------------

col_low <- '#E64B35FF'
col_mid <- 'white'
col_high <- '#4DBBD5FF'
col_na <- 'grey85'


# intercorrelations: self-reported measures ------------------------------

cormat_subjective <- correlation::correlation(data = data_survivors %>%
                                                dplyr::select(bl_qol_factg_total, 
                                                              bl_qol_physsubscale,
                                                              bl_qol_physsubscale_5itemsub,
                                                              promis_pf_t), 
                                              rename = c('FACT-G Total', 
                                                         'FACT-G Physical', 
                                                         'FACT-G Physical 5-item', 
                                                         'Linked PROMIS-PF'),
                                              method = 'spearman',
                                              p_adjust = 'none')

cormat_subjective <- cormat_subjective %>%
  # create 'Text' column for plotting
  rowwise() %>%
  mutate(text = ifelse(p < .001, paste(round(rho, 2), '***', sep = ''),
                       ifelse((p >= .001 & p < .01), paste(round(rho, 2), '**', sep = ''),
                              ifelse((p >= .01 & p < .05), paste(round(rho, 2), '*', sep = ''),
                                     ifelse(p >= .05, paste(round(rho, 2), sep = ''), NA)))))

# set order of Parameter1 and Parameter2 variables for plotting
cormat_subjective$Parameter1 <- factor(cormat_subjective$Parameter1,
                                       levels = c('FACT-G Total', 
                                                  'FACT-G Physical', 
                                                  'FACT-G Physical 5-item'))
cormat_subjective$Parameter2 <- factor(cormat_subjective$Parameter2,
                                       levels = rev(c('FACT-G Physical', 
                                                      'FACT-G Physical 5-item',
                                                      'Linked PROMIS-PF')))
# create visualization of correlations
figureS3 <- ggplot(cormat_subjective,
                   aes(Parameter1, Parameter2, 
                       fill = rho)) +
  geom_tile(color = 'white') +
  geom_text(aes(label = text),
            color = 'black', 
            size = 3) +
  scale_fill_gradient2(
    low = col_low,
    mid = col_mid,
    high = col_high,
    midpoint = 0,
    na.value = col_na,
    limit = c(-1, 1),
    space = "Lab",
    name = "Rho"
  ) +
  theme_minimal() +
  coord_fixed() +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, size = 8, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_blank(), 
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8),
        plot.margin = margin(1, 1, 1, 1)) 

# save figure
ggsave(here('figures', 'figureS3.svg'),
       figureS3,
       device = 'svg', bg = 'white', dpi = 600,
       width = 7, height = 7)


# find largest p-value for all subjective intercorrelations ---------------

max_p_subjective <- max(cormat_subjective$p)


# intercorrelations: measures of real-world physical behavior -------------

cormat_physbehav <- correlation::correlation(data = data_survivors %>%
                                               dplyr::select(TotalSedentaryTime.m., 
                                                             TotalRLMsCount, 
                                                             light_min, 
                                                             mvpa_min,
                                                             `TimeInRLMsBouts>=1m`,
                                                             `WeightedMedianCadence>=1m`,
                                                             PeakRLMs30s), 
                                             rename = c('Sedentary time', 
                                                        'Step count', 
                                                        'Light activity', 
                                                        'Moderate-to-vigorous activity',
                                                        'Time in stepping bouts >=1m',
                                                        'Cadence in stepping bouts >=1m',
                                                        'Peak 30s cadence'),
                                             method = 'spearman',
                                             p_adjust = 'none')

cormat_physbehav <- cormat_physbehav %>%
  # create 'Text' column for plotting
  rowwise() %>%
  mutate(text = ifelse(p < .001, paste(round(rho, 2), '***', sep = ''),
                       ifelse((p >= .001 & p < .01), paste(round(rho, 2), '**', sep = ''),
                              ifelse((p >= .01 & p < .05), paste(round(rho, 2), '*', sep = ''),
                                     ifelse(p >= .05, paste(round(rho, 2), sep = ''), NA)))))

# set order of Parameter1 and Parameter2 variables for plotting
cormat_physbehav$Parameter1 <- factor(cormat_physbehav$Parameter1,
                                      levels = c('Sedentary time', 
                                                 'Step count', 
                                                 'Light activity', 
                                                 'Moderate-to-vigorous activity',
                                                 'Time in stepping bouts >=1m',
                                                 'Cadence in stepping bouts >=1m'))
cormat_physbehav$Parameter2 <- factor(cormat_physbehav$Parameter2,
                                      levels = rev(c('Step count', 
                                                     'Light activity', 
                                                     'Moderate-to-vigorous activity',
                                                     'Time in stepping bouts >=1m',
                                                     'Cadence in stepping bouts >=1m',
                                                     'Peak 30s cadence')))
# plot figure S3
figureS4 <- ggplot(cormat_physbehav,
                   aes(Parameter1, Parameter2, 
                       fill = rho)) +
  geom_tile(color = 'white') +
  geom_text(aes(label = text),
            color = 'black', 
            size = 3) +
  scale_fill_gradient2(
    low = col_low,
    mid = col_mid,
    high = col_high,
    midpoint = 0,
    na.value = col_na,
    limit = c(-1, 1),
    space = "Lab",
    name = "Rho"
  ) +
  theme_minimal() +
  coord_fixed() +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, size = 8, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_blank(), 
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8),
        plot.margin = margin(1, 1, 1, 1)) 

# save figure
ggsave(here('figures', 'figureS4.svg'),
       figureS4,
       device = 'svg', bg = 'white', dpi = 600,
       width = 7, height = 7)


# find largest p-value for intercorrelations of digital measures ----------

# max p for intercorrelated measures
max_p_objective <- cormat_physbehav %>%
  filter(Parameter1 %in% c('Step count', 'Sedentary time', 'Light activity', 'Moderate-to-vigorous activity', 'Time in stepping bouts >=1m'),
         Parameter2 %in% c('Step count', 'Sedentary time', 'Light activity', 'Moderate-to-vigorous activity', 'Time in stepping bouts >=1m')) %>%
  group_by(1) %>%
  summarize(max_p = max(p))
max_p_objective <- max_p_objective$max_p

# cadence: number of significant corrs, max p
max_p_cadence <- cormat_physbehav %>%
  filter(Parameter1 %in% c('Cadence in stepping bouts >=1m') | Parameter2 %in% c('Cadence in stepping bouts >=1m')) %>%
  filter(p < .05) %>%
  group_by(1) %>%
  summarize(n = n(),
            max_p = max(p))

# peak 30s cadence: number of significant corrs, max p
max_p_30scadence <- cormat_physbehav %>%
  filter(Parameter1 %in% c('Peak 30s cadence') | Parameter2 %in% c('Peak 30s cadence')) %>%
  filter(p < .05) %>%
  group_by(1) %>%
  summarize(n = n(),
            max_p = max(p))

# compute pairwise Spearman correlations
cormat_all <- correlation::correlation(
  data = data_survivors %>%
    dplyr::select(
      bl_qol_factg_total,
      bl_qol_physsubscale,
      bl_qol_physsubscale_5itemsub,
      promis_pf_t,
      vo2_hrr,
      TotalSedentaryTime.m., 
      TotalRLMsCount, 
      light_min,
      mvpa_min, 
      `TimeInRLMsBouts>=1m`,
      `WeightedMedianCadence>=1m`, 
      PeakRLMs30s
    ),
  rename = c('FACT-G Total', 
             'FACT-G Physical',
             'FACT-G Physical 5-item',
             'Linked PROMIS-PF',
             'Submaximal VO2',
             'Sedentary time', 
             'Step count', 
             'Light activity', 
             'Moderate-to-vigorous activity',
             'Time in stepping bouts >=1m',
             'Cadence in stepping bouts >=1m',
             'Peak 30s cadence'),
  method = 'spearman', 
  p_adjust = 'none'
)

cormat_all <- cormat_all %>%
  # create 'Text' column for plotting
  rowwise() %>%
  mutate(text = ifelse(p < .001, paste(round(rho, 2), '***', sep = ''),
                       ifelse((p >= .001 & p < .01), paste(round(rho, 2), '**', sep = ''),
                              ifelse((p >= .01 & p < .05), paste(round(rho, 2), '*', sep = ''),
                                     ifelse(p >= .05, paste(round(rho, 2), sep = ''), NA)))))

# define sets of measures for each axis
measures_ax1 <- c('FACT-G Total',
                   'FACT-G Physical', 
                  'FACT-G Physical 5-item',
                   'Linked PROMIS-PF',
                   'Submaximal VO2')
measures_ax2 <- c('Sedentary time', 
                  'Step count', 
                  'Light activity', 
                  'Moderate-to-vigorous activity',
                  'Time in stepping bouts >=1m',
                  'Cadence in stepping bouts >=1m',
                  'Peak 30s cadence')

# select only relevant correlations
cormat_all <- cormat_all %>%
  filter(Parameter1 %in% measures_ax1 | Parameter2 %in% measures_ax1) %>%
  filter(!(Parameter1 %in% measures_ax1 & Parameter2 %in% measures_ax1))

# set order of Parameter1 and Parameter2 variables for plotting
cormat_all$Parameter2 <- factor(cormat_all$Parameter2,
                                    levels = c('Sedentary time', 
                                               'Step count', 
                                               'Light activity', 
                                               'Moderate-to-vigorous activity',
                                               'Time in stepping bouts >=1m',
                                               'Cadence in stepping bouts >=1m',
                                               'Peak 30s cadence'))
cormat_all$Parameter1 <- factor(cormat_all$Parameter1,
                                levels = rev(measures_ax1))

# create figure
figure1 <- ggplot(cormat_all,
                   aes(Parameter2, Parameter1, 
                       fill = rho)) +
  geom_tile(color = 'white') +
  geom_text(aes(label = text),
            color = 'black', 
            size = 3) +
  scale_fill_gradient2(
    low = col_low,
    mid = col_mid,
    high = col_high,
    midpoint = 0,
    na.value = col_na,
    limit = c(-1, 1),
    space = "Lab",
    name = "Rho"
  ) +
  theme_minimal() +
  coord_fixed() +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, size = 8, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_blank(), 
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8),
        plot.margin = margin(1, 1, 1, 1)) 

# save figure
ggsave(here('figures', 'figure1.svg'),
       figure1,
       device = 'svg', bg = 'white', dpi = 600,
       width = 6, height = 5)


# calculate max p-values for objective-subjective correlations ------------

# min p for FACT-G total
min_p_objective_factgtotal <- cormat_all %>%
  filter(Parameter1 == 'FACT-G Total') %>%
  group_by(1) %>%
  summarize(min_p = min(p))
min_p_objective_factgtotal <- min_p_objective_factgtotal$min_p


# min p for FACT-G physical and linked PROMIS-PF (except with time in stepping bouts)
min_p_objective_promis_factgphys <- cormat_all %>%
  filter(Parameter1 %in% c('FACT-G Physical',
                           'FACT-G Physical 5-item',
                           'Linked PROMIS-PF')) %>%
  filter(! Parameter2 == 'Time in stepping bouts >=1m') %>%
  group_by(1) %>%
  summarize(min_p = min(p))
min_p_objective_promis_factgphys <- min_p_objective_promis_factgphys$min_p

# max p for significant correlations with predicted Vo2
max_p_objective_vo2 <- cormat_all %>%
  filter(Parameter1 == 'Submaximal VO2') %>%
  filter(! Parameter2 == 'Cadence in stepping bouts >=1m') %>%
  group_by(1) %>%
  summarize(max_p = max(p))
max_p_objective_vo2 <- max_p_objective_vo2$max_p


# partial correlations with measures of real-world physical behavior --------

# make sex a factor with sum contrast coding
data_survivors$sex <- factor(data_survivors$sex,
                             levels = c('Male', 'Female'))

contrasts(data_survivors$sex) <- c(-0.5, 0.5)

# create numeric version of sex variable
data_survivors <- data_survivors %>%
  rowwise() %>%
  mutate(sex_numeric = ifelse(sex == 'Female', 1, 
                                     ifelse(sex == 'Male', 2, NA)))

# create numeric version of cancer stage variable
data_survivors <- data_survivors %>%
  rowwise() %>%
  mutate(cancer_stage_numeric = ifelse(cancer_stage == '0', 1, 
                                       ifelse(cancer_stage == 'I', 2,
                                              ifelse(cancer_stage == 'II', 3,
                                                     ifelse(cancer_stage == 'III', 4,
                                                            ifelse(cancer_stage == 'IV', 5, NA))))))


# function to compute partial correlation
compute_pcor <- function(var1, var2, method, data) {
  
  # select variables to keep in dataframe
  vars <- c(var1, var2, 
            'age_demographics', 
            'sex_numeric', 
            'bmi', 
            'time_since_diagnosis_months', 
            'cancer_stage_numeric')
  
  # select relevant variables, omitting incomplete cases
  data <- data[vars]
  data <- data %>%
    na.omit()
  
  # compute partial correlation
  pcor_out <- partial.r(data,
                        # x: variables for testing correlation
                        x = c(var1, var2),
                        # y: variables to be partialed out
                        y = c('age_demographics',
                              'sex_numeric', 
                              'bmi', 
                              'time_since_diagnosis_months',
                              'cancer_stage_numeric'),
                        use = 'pairwise.complete',
                        method = method)
  pcor_out_2 <- corr.p(pcor_out,
                       n = data %>% nrow() - 5,  # applied to results of partial corr, n is set to n-s (where s is the number of variables partialed out)
                       alpha = TRUE, 
                       ci = TRUE)
  
  # return results for var1-var2 partial correlation
  result <- NULL
  result$estimate <- pcor_out[1,2]
  result$p.value <- pcor_out_2$p[1,2]
  result$n <- pcor_out_2$n
  result$ci_lower <- pcor_out_2$ci$lower
  result$ci_upper <- pcor_out_2$ci$upper
  return(result)
}


# compute partial correlations with fact-g total
pcor_factg <- NULL
pcor_factg$sed_time <- compute_pcor(var1 = 'bl_qol_factg_total', var2 = 'TotalSedentaryTime.m.', method = 'spearman', data = data_survivors)
pcor_factg$RLM_count <- compute_pcor(var1 = 'bl_qol_factg_total', var2 = 'TotalRLMsCount', method = 'spearman', data = data_survivors)
pcor_factg$light_min <- compute_pcor(var1 = 'bl_qol_factg_total', var2 = 'light_min', method = 'spearman', data = data_survivors)
pcor_factg$mvpa_min <- compute_pcor(var1 = 'bl_qol_factg_total', var2 = 'mvpa_min', method = 'spearman', data = data_survivors)
pcor_factg$time_bouts <- compute_pcor(var1 = 'bl_qol_factg_total', var2 = 'TimeInRLMsBouts>=1m', method = 'spearman', data = data_survivors)
pcor_factg$cadence <- compute_pcor(var1 = 'bl_qol_factg_total', var2 = 'WeightedMedianCadence>=1m', method = 'spearman', data = data_survivors)
pcor_factg$peak_RLMs <- compute_pcor(var1 = 'bl_qol_factg_total', var2 = 'PeakRLMs30s', method = 'spearman', data = data_survivors)

# compute partial correlations with fact-g physical
pcor_factg_phys <- NULL
pcor_factg_phys$sed_time <- compute_pcor(var1 = 'bl_qol_physsubscale', var2 = 'TotalSedentaryTime.m.', method = 'spearman', data = data_survivors)
pcor_factg_phys$RLM_count <- compute_pcor(var1 = 'bl_qol_physsubscale', var2 = 'TotalRLMsCount', method = 'spearman', data = data_survivors)
pcor_factg_phys$light_min <- compute_pcor(var1 = 'bl_qol_physsubscale', var2 = 'light_min', method = 'spearman', data = data_survivors)
pcor_factg_phys$mvpa_min <- compute_pcor(var1 = 'bl_qol_physsubscale', var2 = 'mvpa_min', method = 'spearman', data = data_survivors)
pcor_factg_phys$time_bouts <- compute_pcor(var1 = 'bl_qol_physsubscale', var2 = 'TimeInRLMsBouts>=1m', method = 'spearman', data = data_survivors)
pcor_factg_phys$cadence <- compute_pcor(var1 = 'bl_qol_physsubscale', var2 = 'WeightedMedianCadence>=1m', method = 'spearman', data = data_survivors)
pcor_factg_phys$peak_RLMs <- compute_pcor(var1 = 'bl_qol_physsubscale', var2 = 'PeakRLMs30s', method = 'spearman', data = data_survivors)

# compute partial correlations with fact-g physical 5-item subset
pcor_factg_phys5item <- NULL
pcor_factg_phys5item$sed_time <- compute_pcor(var1 = 'bl_qol_physsubscale_5itemsub', var2 = 'TotalSedentaryTime.m.', method = 'spearman', data = data_survivors)
pcor_factg_phys5item$RLM_count <- compute_pcor(var1 = 'bl_qol_physsubscale_5itemsub', var2 = 'TotalRLMsCount', method = 'spearman', data = data_survivors)
pcor_factg_phys5item$light_min <- compute_pcor(var1 = 'bl_qol_physsubscale_5itemsub', var2 = 'light_min', method = 'spearman', data = data_survivors)
pcor_factg_phys5item$mvpa_min <- compute_pcor(var1 = 'bl_qol_physsubscale_5itemsub', var2 = 'mvpa_min', method = 'spearman', data = data_survivors)
pcor_factg_phys5item$time_bouts <- compute_pcor(var1 = 'bl_qol_physsubscale_5itemsub', var2 = 'TimeInRLMsBouts>=1m', method = 'spearman', data = data_survivors)
pcor_factg_phys5item$cadence <- compute_pcor(var1 = 'bl_qol_physsubscale_5itemsub', var2 = 'WeightedMedianCadence>=1m', method = 'spearman', data = data_survivors)
pcor_factg_phys5item$peak_RLMs <- compute_pcor(var1 = 'bl_qol_physsubscale_5itemsub', var2 = 'PeakRLMs30s', method = 'spearman', data = data_survivors)

# compute partial correlations with promis-pf
pcor_promispf <- NULL
pcor_promispf$sed_time <- compute_pcor(var1 = 'promis_pf_t', var2 = 'TotalSedentaryTime.m.', method = 'spearman', data = data_survivors)
pcor_promispf$RLM_count <- compute_pcor(var1 = 'promis_pf_t', var2 = 'TotalRLMsCount', method = 'spearman', data = data_survivors)
pcor_promispf$light_min <- compute_pcor(var1 = 'promis_pf_t', var2 = 'light_min', method = 'spearman', data = data_survivors)
pcor_promispf$mvpa_min <- compute_pcor(var1 = 'promis_pf_t', var2 = 'mvpa_min', method = 'spearman', data = data_survivors)
pcor_promispf$time_bouts <- compute_pcor(var1 = 'promis_pf_t', var2 = 'TimeInRLMsBouts>=1m', method = 'spearman', data = data_survivors)
pcor_promispf$cadence <- compute_pcor(var1 = 'promis_pf_t', var2 = 'WeightedMedianCadence>=1m', method = 'spearman', data = data_survivors)
pcor_promispf$peak_RLMs <- compute_pcor(var1 = 'promis_pf_t', var2 = 'PeakRLMs30s', method = 'spearman', data = data_survivors)

# compute partial correlations with predicted vo2
pcor_vo2 <- NULL
pcor_vo2$sed_time <- compute_pcor(var1 = 'vo2_hrr', var2 = 'TotalSedentaryTime.m.', method = 'spearman', data = data_survivors)
pcor_vo2$RLM_count <- compute_pcor(var1 = 'vo2_hrr', var2 = 'TotalRLMsCount', method = 'spearman', data = data_survivors)
pcor_vo2$light_min <- compute_pcor(var1 = 'vo2_hrr', var2 = 'light_min', method = 'spearman', data = data_survivors)
pcor_vo2$mvpa_min <- compute_pcor(var1 = 'vo2_hrr', var2 = 'mvpa_min', method = 'spearman', data = data_survivors)
pcor_vo2$time_bouts <- compute_pcor(var1 = 'vo2_hrr', var2 = 'TimeInRLMsBouts>=1m', method = 'spearman', data = data_survivors)
pcor_vo2$cadence <- compute_pcor(var1 = 'vo2_hrr', var2 = 'WeightedMedianCadence>=1m', method = 'spearman', data = data_survivors)
pcor_vo2$peak_RLMs <- compute_pcor(var1 = 'vo2_hrr', var2 = 'PeakRLMs30s', method = 'spearman', data = data_survivors)

# arrange partial correlation coefficients and p-values into a dataframe
df_pcor <- data.frame(
  var1 = c(rep('FACT-G Total', 7), 
           rep('FACT-G Physical', 7), 
           rep('FACT-G Physical 5-item', 7), 
           rep('Linked PROMIS-PF', 7), 
           rep('Submaximal VO2', 7)),
  var2 = rep(c('Sedentary time', 
               'Step count', 
               'Light activity',
               'Mod-to-vig activity', 
               'Time in bouts >=1m', 
               'Cadence in bouts >=1m', 
               'Peak 30s cadence'), 5),
  estimate = c(pcor_factg$sed_time$estimate, pcor_factg$RLM_count$estimate, pcor_factg$light_min$estimate, pcor_factg$mvpa_min$estimate, 
               pcor_factg$time_bouts$estimate, pcor_factg$cadence$estimate, pcor_factg$peak_RLMs$estimate,
               
               pcor_factg_phys$sed_time$estimate, pcor_factg_phys$RLM_count$estimate, pcor_factg_phys$light_min$estimate, pcor_factg_phys$mvpa_min$estimate, 
               pcor_factg_phys$time_bouts$estimate, pcor_factg_phys$cadence$estimate, pcor_factg_phys$peak_RLMs$estimate,
               
               pcor_factg_phys5item$sed_time$estimate, pcor_factg_phys5item$RLM_count$estimate, pcor_factg_phys5item$light_min$estimate, pcor_factg_phys5item$mvpa_min$estimate, 
               pcor_factg_phys5item$time_bouts$estimate, pcor_factg_phys5item$cadence$estimate, pcor_factg_phys5item$peak_RLMs$estimate,

               pcor_promispf$sed_time$estimate, pcor_promispf$RLM_count$estimate, pcor_promispf$light_min$estimate, pcor_promispf$mvpa_min$estimate, 
               pcor_promispf$time_bouts$estimate, pcor_promispf$cadence$estimate, pcor_promispf$peak_RLMs$estimate,
               
               pcor_vo2$sed_time$estimate, pcor_vo2$RLM_count$estimate, pcor_vo2$light_min$estimate, pcor_vo2$mvpa_min$estimate, 
               pcor_vo2$time_bouts$estimate, pcor_vo2$cadence$estimate, pcor_vo2$peak_RLMs$estimate),
  
  p.value = c(pcor_factg$sed_time$p.value, pcor_factg$RLM_count$p.value, pcor_factg$light_min$p.value, pcor_factg$mvpa_min$p.value, 
              pcor_factg$time_bouts$p.value, pcor_factg$cadence$p.value, pcor_factg$peak_RLMs$p.value,
              
              pcor_factg_phys$sed_time$p.value, pcor_factg_phys$RLM_count$p.value, pcor_factg_phys$light_min$p.value, pcor_factg_phys$mvpa_min$p.value, 
              pcor_factg_phys$time_bouts$p.value, pcor_factg_phys$cadence$p.value, pcor_factg_phys$peak_RLMs$p.value,
              
              pcor_factg_phys5item$sed_time$p.value, pcor_factg_phys5item$RLM_count$p.value, pcor_factg_phys5item$light_min$p.value, pcor_factg_phys5item$mvpa_min$p.value, 
              pcor_factg_phys5item$time_bouts$p.value, pcor_factg_phys5item$cadence$p.value, pcor_factg_phys5item$peak_RLMs$p.value,
              
              pcor_promispf$sed_time$p.value, pcor_promispf$RLM_count$p.value, pcor_promispf$light_min$p.value, pcor_promispf$mvpa_min$p.value, 
              pcor_promispf$time_bouts$p.value, pcor_promispf$cadence$p.value, pcor_promispf$peak_RLMs$p.value,
              
              pcor_vo2$sed_time$p.value, pcor_vo2$RLM_count$p.value, pcor_vo2$light_min$p.value, pcor_vo2$mvpa_min$p.value, 
              pcor_vo2$time_bouts$p.value, pcor_vo2$cadence$p.value, pcor_vo2$peak_RLMs$p.value)) %>%
  # create 'Text' column for plotting
  rowwise() %>%
  mutate(text = ifelse(p.value < .001, paste(round(estimate, 2), '***', sep = ''),
                       ifelse((p.value >= .001 & p.value < .01), paste(round(estimate, 2), '**', sep = ''),
                              ifelse((p.value >= .01 & p.value < .05), paste(round(estimate, 2), '*', sep = ''),
                                     ifelse(p.value >= .05, paste(round(estimate, 2), sep = ''), NA)))))

# set order of variables for plotting
df_pcor$var1 <- factor(df_pcor$var1,
                       levels = c('Submaximal VO2', 'Linked PROMIS-PF', 'FACT-G Physical 5-item', 'FACT-G Physical', 'FACT-G Total',
                                  'Mod-to-vig activity', 'Time in bouts >=1m', 'Cadence in bouts >=1m', 'Peak 30s cadence'))
df_pcor$var2 <- factor(df_pcor$var2,
                       levels = c('Sedentary time', 'Step count', 'Light activity',
                                  'Mod-to-vig activity', 'Time in bouts >=1m', 'Cadence in bouts >=1m', 'Peak 30s cadence'))

# create figure
figureS5 <- ggplot(df_pcor,
                   aes(var2, var1, 
                       fill = estimate)) +
  geom_tile(color = 'white') +
  geom_text(aes(label = text),
            color = 'black', 
            size = 3) +
  scale_fill_gradient2(
    low = col_low,
    mid = col_mid,
    high = col_high,
    midpoint = 0,
    na.value = col_na,
    limit = c(-1, 1),
    space = "Lab",
    name = "Partial rho"
  ) +
  theme_minimal() +
  coord_fixed() +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, size = 8, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_blank(), 
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8),
        plot.margin = margin(1, 1, 1, 1)) 

# save figure
ggsave(here('figures', 'figureS5.svg'),
       figureS5,
       device = 'svg', bg = 'white', dpi = 600,
       width = 6, height = 5)


# calculate max p-values for objective-subjective partial correlations ------------

# min p for FACT-G physical and linked PROMIS-PF (except with time in stepping bouts)
min_p_objective_promis_factgphys_partial <- df_pcor %>%
  filter(var1 %in% c('FACT-G Physical',
                     'FACT-G Physical 5-item',
                    'Linked PROMIS-PF')) %>%
  filter(! var2 == 'Time in bouts >=1m') %>%
  group_by(1) %>%
  summarize(min_p = min(p.value))
min_p_objective_promis_factgphys_partial <- min_p_objective_promis_factgphys_partial$min_p


# correlations: V02 vs. self-reported measures ------------------------

# compute pairwise Spearman correlations
cormat_vo2 <- correlation::correlation(
  data = data_survivors %>%
    dplyr::select(
      vo2_hrr,
      bl_qol_factg_total,
      bl_qol_physsubscale,
      bl_qol_physsubscale_5itemsub,
      promis_pf_t
    ),
  rename = c('Submaximal VO2', 
             'FACT-G Total', 
             'FACT-G Physical',
             'FACT-G Physical 5-item',
             'Linked PROMIS-PF'),
  method = 'spearman', 
  p_adjust = 'none'
)

cormat_vo2 <- cormat_vo2 %>%
  # create 'Text' column for plotting
  rowwise() %>%
  mutate(text = ifelse(p < .001, paste(round(rho, 2), '***', sep = ''),
                       ifelse((p >= .001 & p < .01), paste(round(rho, 2), '**', sep = ''),
                              ifelse((p >= .01 & p < .05), paste(round(rho, 2), '*', sep = ''),
                                     ifelse(p >= .05, paste(round(rho, 2), sep = ''), NA)))))

# define sets of measures for each axis
measures_ax1 <- c('FACT-G Total',
                   'FACT-G Physical', 
                  'FACT-G Physical 5-item',
                   'Linked PROMIS-PF')
measures_ax2 <- c('Submaximal VO2')

# select only relevant correlations
cormat_vo2 <- cormat_vo2 %>%
  filter(Parameter1 %in% measures_ax2)

# set order of Parameter2 variable for plotting
cormat_vo2$Parameter2 <- factor(cormat_vo2$Parameter2,
                                levels = rev(measures_ax1))

# create figure
figureS6A <- ggplot(cormat_vo2,
       aes(Parameter1, Parameter2, fill = rho)) +
  geom_tile(color = 'white') +
  geom_text(aes(label = text),
            color = 'black', 
            size = 3) +
  scale_fill_gradient2(
    low = col_low,
    mid = col_mid,
    high = col_high,
    midpoint = 0,
    na.value = col_na,
    limit = c(-1, 1),
    space = "Lab",
    name = "Rho"
  ) +
  theme_minimal() +
  coord_fixed() +
  theme(axis.text.x = element_text(size = 8),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_blank(), 
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8),
        plot.margin = margin(1, 1, 1, 1))



# partial correlations: VO2 vs. self-reported measures ----------------

# compute partial correlations with VO2
pcor_vo2 <- NULL
pcor_vo2$factg_total <- compute_pcor(var1 = 'vo2_hrr', var2 = 'bl_qol_factg_total', method = 'spearman', data = data_survivors)
pcor_vo2$factg_phys <- compute_pcor(var1 = 'vo2_hrr', var2 = 'bl_qol_physsubscale', method = 'spearman', data = data_survivors)
pcor_vo2$factg_phys5item <- compute_pcor(var1 = 'vo2_hrr', var2 = 'bl_qol_physsubscale_5itemsub', method = 'spearman', data = data_survivors)
pcor_vo2$promis_pf <- compute_pcor(var1 = 'vo2_hrr', var2 = 'promis_pf_t', method = 'spearman', data = data_survivors)

# arrange partial correlation coefficients and p-values into a dataframe
df_pcor_vo2 <- data.frame(
  var1 = rep('Submaximal VO2', 4),
  var2 = measures_ax1,
  estimate = c(pcor_vo2$factg_total$estimate, 
               pcor_vo2$factg_phys$estimate, 
               pcor_vo2$factg_phys5item$estimate, 
               pcor_vo2$promis_pf$estimate),
  
  p.value = c(pcor_vo2$factg_total$p.value, 
              pcor_vo2$factg_phys$p.value, 
              pcor_vo2$factg_phys5item$p.value, 
              pcor_vo2$promis_pf$p.value)) %>%
  
  # create 'Text' column for plotting
  rowwise() %>%
  mutate(text = ifelse(p.value < .001, paste(round(estimate, 2), '***', sep = ''),
                       ifelse((p.value >= .001 & p.value < .01), paste(round(estimate, 2), '**', sep = ''),
                              ifelse((p.value >= .01 & p.value < .05), paste(round(estimate, 2), '*', sep = ''),
                                     ifelse(p.value >= .05, paste(round(estimate, 2), sep = ''), NA)))))

# set order of variables for plotting
df_pcor_vo2$var2 <- factor(df_pcor_vo2$var2,
                       levels = rev(measures_ax1))

# create figure
figureS6B <- ggplot(df_pcor_vo2, 
                    aes(var1, var2, fill = estimate)) +
  geom_tile(color = 'white') +
  geom_text(aes(label = text),
            color = 'black', 
            size = 3) +
  scale_fill_gradient2(
    low = col_low,
    mid = col_mid,
    high = col_high,
    midpoint = 0,
    na.value = col_na,
    limit = c(-1, 1),
    space = "Lab",
    name = "Partial rho"
  ) +
  theme_minimal() +
  coord_fixed() +
  theme(axis.text.x = element_text(size = 8),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_blank(), 
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8),
        plot.margin = margin(1, 1, 1, 1)) 


# create and save composite figure ----------------------------------------

figureS6 <- ggarrange(
  figureS6A,
  figureS6B,
  nrow = 1, ncol = 2,
  labels = c('A', 'B'), label.x = c(0, 0), font.label = list(size = 10),
  align = 'hv'
)

# create and save composite figure
ggsave(here('figures', 'figureS6.svg'),
       figureS6,
       device = 'svg', bg = 'white', dpi = 600,
       width = 6, height = 5)


# calculate min p-values for vo2-subjective correlations ------------------

# min p for spearman correlation
min_p_vo2_subjective <- cormat_vo2 %>%
  group_by(1) %>%
  summarize(min_p = min(p))
min_p_vo2_subjective <- min_p_vo2_subjective$min_p
  
# min p for partial spearman correlation
min_p_vo2_subjective_partial <- df_pcor_vo2 %>%
  group_by(1) %>%
  summarize(min_p = min(p.value))
min_p_vo2_subjective_partial <- min_p_vo2_subjective_partial$min_p

