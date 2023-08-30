# analyze associations with activity fragmentation metrics
# written by shelby bachman, shelby.bachman@vivosense.com


# prerequisites -----------------------------------------------------------

# activity fragmentation metrics were calculated using the R package 'ActFrag'
# and were stored in a dataframe, data_AF
# where each column is an id (`record_id`) and the columns are the various metrics


# join activity fragmentation metrics with main data ----------------------

data_survivors <- data_survivors %>%
  full_join(data_AF, by = c('record_id', 'study'))


# examine normality of activity fragmentation metrics ---------------------

# perform shapiro-wilk tests 
summary_normality_AF <- do.call(rbind, 
                             lapply(data_survivors %>% 
                                      dplyr::select(
                                        mean_r:h_a
                                      ), 
                                    function(x) shapiro.test(x)[c("statistic", "p.value")])
) %>%
  as.data.frame()

# add `variable` column
summary_normality_AF$variable <- rownames(summary_normality_AF)
rownames(summary_normality_AF) <- c()
summary_normality_AF <- summary_normality_AF %>%
  dplyr::select(variable, statistic, p.value)


# intercorrelations between activity fragmentation metrics ----------------

# compute correlation matrix
cormat_AF <- correlation::correlation(data = data_survivors %>%
                                              dplyr::select(h_a, h_r,
                                                            Gini_a, Gini_r,
                                                            mean_a, mean_r,
                                                            alpha_a, alpha_r),
                                            method = 'spearman',
                                            p_adjust = 'none')

cormat_AF <- cormat_AF %>%
  # create 'Text' column for plotting
  rowwise() %>%
  mutate(text = ifelse(p < .001, paste(round(rho, 2), '***', sep = ''),
                       ifelse((p >= .001 & p < .01), paste(round(rho, 2), '**', sep = ''),
                              ifelse((p >= .01 & p < .05), paste(round(rho, 2), '*', sep = ''),
                                     ifelse(p >= .05, paste(round(rho, 2), sep = ''), NA)))))

# set order of Parameter1 and Parameter2 variables for plotting
cormat_AF$Parameter1 <- factor(cormat_AF$Parameter1,
                                      levels = cormat_AF$Parameter1 %>% unique())

cormat_AF$Parameter2 <- factor(cormat_AF$Parameter2,
                                      levels =  rev(cormat_AF$Parameter2 %>% unique()))


# correlations between activity fragmentation and real-world physical behavior --------

# compute correlation matrix
cormat_AF_physbehav <- correlation::correlation(data = data_survivors %>%
                                        dplyr::select(alpha_a, mean_r, Gini_r, h_a,
                                                      alpha_r, mean_a, Gini_a, h_r,
                                                      TotalSedentaryTime.m., TotalRLMsCount, 
                                                      light_min, mvpa_min, `TimeInRLMsBouts>=1m`,
                                                      `WeightedMedianCadence>=1m`, PeakRLMs30s), 
                                        rename = c('alpha_a', 'mean_r', 'Gini_r', 'h_a',
                                                   'alpha_r', 'mean_a', 'Gini_a', 'h_r',
                                                   'Sedentary time', 
                                                   'Step count', 
                                                   'Light activity', 
                                                   'Moderate-to-vigorous activity',
                                                   'Time in stepping bouts >=1m',
                                                   'Cadence in stepping bouts >=1m',
                                                   'Peak 30s cadence'),
                                      method = 'spearman',
                                      p_adjust = 'none')

cormat_AF_physbehav <- cormat_AF_physbehav %>%
  # create 'Text' column for plotting
  rowwise() %>%
  mutate(text = ifelse(p < .001, paste(round(rho, 2), '***', sep = ''),
                       ifelse((p >= .001 & p < .01), paste(round(rho, 2), '**', sep = ''),
                              ifelse((p >= .01 & p < .05), paste(round(rho, 2), '*', sep = ''),
                                     ifelse(p >= .05, paste(round(rho, 2), sep = ''), NA)))))

# subset to include only relevant part of correlation matrix
cormat_AF_physbehav <- cormat_AF_physbehav %>%
  filter(Parameter1 %in% c('alpha_a', 'mean_r', 'Gini_r', 'h_a',
                           'alpha_r', 'mean_a', 'Gini_a', 'h_r'),
         Parameter2 %in% c('Sedentary time', 
                           'Step count', 
                           'Light activity', 
                           'Moderate-to-vigorous activity',
                           'Time in stepping bouts >=1m',
                           'Cadence in stepping bouts >=1m',
                           'Peak 30s cadence'))

# set order of Parameter1 and Parameter2 variables for plotting
cormat_AF_physbehav$Parameter1 <- factor(cormat_AF_physbehav$Parameter1,
                                         levels = cormat_AF_physbehav$Parameter1 %>% unique())

cormat_AF_physbehav$Parameter2 <- factor(cormat_AF_physbehav$Parameter2,
                                         levels =  rev(cormat_AF_physbehav$Parameter2 %>% unique()))


# correlations between activity fragmentation and self-reported, VO2 --------

# compute correlation matrix
cormat_AF_subj_vo2 <- correlation::correlation(data = data_survivors %>%
                                                  dplyr::select(alpha_a, mean_r, Gini_r, h_a,
                                                                alpha_r, mean_a, Gini_a, h_r,
                                                                bl_qol_factg_total,
                                                                bl_qol_physsubscale, 
                                                                bl_qol_physsubscale_5itemsub, 
                                                                promis_pf_t, 
                                                                vo2_hrr), 
                                                rename = c('alpha_a', 'mean_r', 'Gini_r', 'h_a',
                                                           'alpha_r', 'mean_a', 'Gini_a', 'h_r',
                                                           'FACT-G Total',  
                                                           'FACT-G Physical',
                                                           'FACT-G Physical 5-item',
                                                           'Linked PROMIS-PF', 
                                                           'Submaximal VO2'),
                                                method = 'spearman',
                                                p_adjust = 'none')

cormat_AF_subj_vo2 <- cormat_AF_subj_vo2 %>%
  # create 'Text' column for plotting
  rowwise() %>%
  mutate(text = ifelse(p < .001, paste(round(rho, 2), '***', sep = ''),
                       ifelse((p >= .001 & p < .01), paste(round(rho, 2), '**', sep = ''),
                              ifelse((p >= .01 & p < .05), paste(round(rho, 2), '*', sep = ''),
                                     ifelse(p >= .05, paste(round(rho, 2), sep = ''), NA)))))

# subset to include only relevant part of correlation matrix
cormat_AF_subj_vo2 <- cormat_AF_subj_vo2 %>%
  filter(Parameter1 %in% c('alpha_a', 'mean_r', 'Gini_r', 'h_a',
                           'alpha_r', 'mean_a', 'Gini_a', 'h_r'),
         Parameter2 %in% c('FACT-G Total',  
                           'FACT-G Physical',
                           'FACT-G Physical 5-item',
                           'Linked PROMIS-PF', 
                           'Submaximal VO2'))

# set order of Parameter1 and Parameter2 variables for plotting
cormat_AF_subj_vo2$Parameter1 <- factor(cormat_AF_subj_vo2$Parameter1,
                                         levels = cormat_AF_subj_vo2$Parameter1 %>% unique())

cormat_AF_subj_vo2$Parameter2 <- factor(cormat_AF_subj_vo2$Parameter2,
                                         levels =  rev(cormat_AF_subj_vo2$Parameter2 %>% unique()))

# create correlation plot
figureS7A <- ggplot(cormat_AF_subj_vo2,
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
  theme(axis.text.x = element_text(angle = 30, vjust = 1, size = 8, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_blank(), 
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8),
        plot.margin = margin(1, 1, 1, 1)) 


# calculate minimum/maximum p-values from correlations --------------------

cormat_AF_subj_vo2 %>%
  filter(Parameter2 == 'FACT-G Total') %>%
  group_by(1) %>%
  summarize(min_p = min(p))

cormat_AF_subj_vo2 %>%
  filter(Parameter2 == 'FACT-G Physical') %>%
  group_by(1) %>%
  summarize(min_p = min(p))

cormat_AF_subj_vo2 %>%
  filter(Parameter2 == 'FACT-G Physical 5-item') %>%
  group_by(1) %>%
  summarize(min_p = min(p))

cormat_AF_subj_vo2 %>%
  filter(Parameter2 == 'Linked PROMIS-PF') %>%
  group_by(1) %>%
  summarize(min_p = min(p))


# partial correlations between activity fragmentation and self-reported, VO2 --------

# compute partial correlations between fragmentation metrics and fact-g total
pcor_af_factg <- NULL
pcor_af_factg$alpha_a <- compute_pcor(var1 = 'bl_qol_factg_total', var2 = 'alpha_a', method = 'spearman', data = data_survivors)
pcor_af_factg$mean_r <- compute_pcor(var1 = 'bl_qol_factg_total', var2 = 'mean_r', method = 'spearman', data = data_survivors)
pcor_af_factg$Gini_r <- compute_pcor(var1 = 'bl_qol_factg_total', var2 = 'Gini_r', method = 'spearman', data = data_survivors)
pcor_af_factg$h_a <- compute_pcor(var1 = 'bl_qol_factg_total', var2 = 'h_a', method = 'spearman', data = data_survivors)
pcor_af_factg$alpha_r <- compute_pcor(var1 = 'bl_qol_factg_total', var2 = 'alpha_r', method = 'spearman', data = data_survivors)
pcor_af_factg$mean_a <- compute_pcor(var1 = 'bl_qol_factg_total', var2 = 'mean_a', method = 'spearman', data = data_survivors)
pcor_af_factg$Gini_a <- compute_pcor(var1 = 'bl_qol_factg_total', var2 = 'Gini_a', method = 'spearman', data = data_survivors)
pcor_af_factg$h_r <- compute_pcor(var1 = 'bl_qol_factg_total', var2 = 'h_r', method = 'spearman', data = data_survivors)

# compute partial correlations between fragmentation metrics and fact-g physical
pcor_af_factg_phys <- NULL
pcor_af_factg_phys$alpha_a <- compute_pcor(var1 = 'bl_qol_physsubscale', var2 = 'alpha_a', method = 'spearman', data = data_survivors)
pcor_af_factg_phys$mean_r <- compute_pcor(var1 = 'bl_qol_physsubscale', var2 = 'mean_r', method = 'spearman', data = data_survivors)
pcor_af_factg_phys$Gini_r <- compute_pcor(var1 = 'bl_qol_physsubscale', var2 = 'Gini_r', method = 'spearman', data = data_survivors)
pcor_af_factg_phys$h_a <- compute_pcor(var1 = 'bl_qol_physsubscale', var2 = 'h_a', method = 'spearman', data = data_survivors)
pcor_af_factg_phys$alpha_r <- compute_pcor(var1 = 'bl_qol_physsubscale', var2 = 'alpha_r', method = 'spearman', data = data_survivors)
pcor_af_factg_phys$mean_a <- compute_pcor(var1 = 'bl_qol_physsubscale', var2 = 'mean_a', method = 'spearman', data = data_survivors)
pcor_af_factg_phys$Gini_a <- compute_pcor(var1 = 'bl_qol_physsubscale', var2 = 'Gini_a', method = 'spearman', data = data_survivors)
pcor_af_factg_phys$h_r <- compute_pcor(var1 = 'bl_qol_physsubscale', var2 = 'h_r', method = 'spearman', data = data_survivors)

# compute partial correlations between fragmentation metrics and fact-g physical 5-item subset
pcor_af_factg_phys5item <- NULL
pcor_af_factg_phys5item$alpha_a <- compute_pcor(var1 = 'bl_qol_physsubscale_5itemsub', var2 = 'alpha_a', method = 'spearman', data = data_survivors)
pcor_af_factg_phys5item$mean_r <- compute_pcor(var1 = 'bl_qol_physsubscale_5itemsub', var2 = 'mean_r', method = 'spearman', data = data_survivors)
pcor_af_factg_phys5item$Gini_r <- compute_pcor(var1 = 'bl_qol_physsubscale_5itemsub', var2 = 'Gini_r', method = 'spearman', data = data_survivors)
pcor_af_factg_phys5item$h_a <- compute_pcor(var1 = 'bl_qol_physsubscale_5itemsub', var2 = 'h_a', method = 'spearman', data = data_survivors)
pcor_af_factg_phys5item$alpha_r <- compute_pcor(var1 = 'bl_qol_physsubscale_5itemsub', var2 = 'alpha_r', method = 'spearman', data = data_survivors)
pcor_af_factg_phys5item$mean_a <- compute_pcor(var1 = 'bl_qol_physsubscale_5itemsub', var2 = 'mean_a', method = 'spearman', data = data_survivors)
pcor_af_factg_phys5item$Gini_a <- compute_pcor(var1 = 'bl_qol_physsubscale_5itemsub', var2 = 'Gini_a', method = 'spearman', data = data_survivors)
pcor_af_factg_phys5item$h_r <- compute_pcor(var1 = 'bl_qol_physsubscale_5itemsub', var2 = 'h_r', method = 'spearman', data = data_survivors)

# compute partial correlations between fragmentation metrics and promis-pf
pcor_af_promispf <- NULL
pcor_af_promispf$alpha_a <- compute_pcor(var1 = 'promis_pf_t', var2 = 'alpha_a', method = 'spearman', data = data_survivors)
pcor_af_promispf$mean_r <- compute_pcor(var1 = 'promis_pf_t', var2 = 'mean_r', method = 'spearman', data = data_survivors)
pcor_af_promispf$Gini_r <- compute_pcor(var1 = 'promis_pf_t', var2 = 'Gini_r', method = 'spearman', data = data_survivors)
pcor_af_promispf$h_a <- compute_pcor(var1 = 'promis_pf_t', var2 = 'h_a', method = 'spearman', data = data_survivors)
pcor_af_promispf$alpha_r <- compute_pcor(var1 = 'promis_pf_t', var2 = 'alpha_r', method = 'spearman', data = data_survivors)
pcor_af_promispf$mean_a <- compute_pcor(var1 = 'promis_pf_t', var2 = 'mean_a', method = 'spearman', data = data_survivors)
pcor_af_promispf$Gini_a <- compute_pcor(var1 = 'promis_pf_t', var2 = 'Gini_a', method = 'spearman', data = data_survivors)
pcor_af_promispf$h_r <- compute_pcor(var1 = 'promis_pf_t', var2 = 'h_r', method = 'spearman', data = data_survivors)

# compute partial correlations with predicted vo2
pcor_af_vo2 <- NULL
pcor_af_vo2$alpha_a <- compute_pcor(var1 = 'vo2_hrr', var2 = 'alpha_a', method = 'spearman', data = data_survivors)
pcor_af_vo2$mean_r <- compute_pcor(var1 = 'vo2_hrr', var2 = 'mean_r', method = 'spearman', data = data_survivors)
pcor_af_vo2$Gini_r <- compute_pcor(var1 = 'vo2_hrr', var2 = 'Gini_r', method = 'spearman', data = data_survivors)
pcor_af_vo2$h_a <- compute_pcor(var1 = 'vo2_hrr', var2 = 'h_a', method = 'spearman', data = data_survivors)
pcor_af_vo2$alpha_r <- compute_pcor(var1 = 'vo2_hrr', var2 = 'alpha_r', method = 'spearman', data = data_survivors)
pcor_af_vo2$mean_a <- compute_pcor(var1 = 'vo2_hrr', var2 = 'mean_a', method = 'spearman', data = data_survivors)
pcor_af_vo2$Gini_a <- compute_pcor(var1 = 'vo2_hrr', var2 = 'Gini_a', method = 'spearman', data = data_survivors)
pcor_af_vo2$h_r <- compute_pcor(var1 = 'vo2_hrr', var2 = 'h_r', method = 'spearman', data = data_survivors)

# arrange partial correlation coefficients and p-values into a dataframe
df_pcor_af <- data.frame(
  var1 = c(rep('FACT-G Total', 8), 
           rep('FACT-G Physical', 8),
           rep('FACT-G Physical 5-item', 8),
           rep('Linked PROMIS-PF', 8), 
           rep('Submaximal VO2', 8)),
  var2 = rep(c('alpha_a', 
               'mean_r', 
               'Gini_r',
               'h_a', 
               'alpha_r', 
               'mean_a', 
               'Gini_a',
               'h_r'), 5),
  estimate = c(pcor_af_factg$alpha_a$estimate, 
               pcor_af_factg$mean_r$estimate, 
               pcor_af_factg$Gini_r$estimate, 
               pcor_af_factg$h_a$estimate, 
               pcor_af_factg$alpha_r$estimate, 
               pcor_af_factg$mean_a$estimate, 
               pcor_af_factg$Gini_a$estimate,
               pcor_af_factg$h_r$estimate,
               
               pcor_af_factg_phys$alpha_a$estimate, 
               pcor_af_factg_phys$mean_r$estimate, 
               pcor_af_factg_phys$Gini_r$estimate, 
               pcor_af_factg_phys$h_a$estimate, 
               pcor_af_factg_phys$alpha_r$estimate, 
               pcor_af_factg_phys$mean_a$estimate, 
               pcor_af_factg_phys$Gini_a$estimate,
               pcor_af_factg_phys$h_r$estimate,
               
               pcor_af_factg_phys5item$alpha_a$estimate, 
               pcor_af_factg_phys5item$mean_r$estimate, 
               pcor_af_factg_phys5item$Gini_r$estimate, 
               pcor_af_factg_phys5item$h_a$estimate, 
               pcor_af_factg_phys5item$alpha_r$estimate, 
               pcor_af_factg_phys5item$mean_a$estimate, 
               pcor_af_factg_phys5item$Gini_a$estimate,
               pcor_af_factg_phys5item$h_r$estimate,
               
               pcor_af_promispf$alpha_a$estimate, 
               pcor_af_promispf$mean_r$estimate, 
               pcor_af_promispf$Gini_r$estimate, 
               pcor_af_promispf$h_a$estimate, 
               pcor_af_promispf$alpha_r$estimate, 
               pcor_af_promispf$mean_a$estimate, 
               pcor_af_promispf$Gini_a$estimate,
               pcor_af_promispf$h_r$estimate,
               
               pcor_af_vo2$alpha_a$estimate, 
               pcor_af_vo2$mean_r$estimate, 
               pcor_af_vo2$Gini_r$estimate, 
               pcor_af_vo2$h_a$estimate, 
               pcor_af_vo2$alpha_r$estimate, 
               pcor_af_vo2$mean_a$estimate, 
               pcor_af_vo2$Gini_a$estimate,
               pcor_af_vo2$h_r$estimate),
  
  p.value = c(pcor_af_factg$alpha_a$p.value, 
               pcor_af_factg$mean_r$p.value, 
               pcor_af_factg$Gini_r$p.value, 
               pcor_af_factg$h_a$p.value, 
               pcor_af_factg$alpha_r$p.value, 
               pcor_af_factg$mean_a$p.value, 
               pcor_af_factg$Gini_a$p.value,
               pcor_af_factg$h_r$p.value,
              
              pcor_af_factg_phys$alpha_a$p.value, 
              pcor_af_factg_phys$mean_r$p.value, 
              pcor_af_factg_phys$Gini_r$p.value, 
              pcor_af_factg_phys$h_a$p.value, 
              pcor_af_factg_phys$alpha_r$p.value, 
              pcor_af_factg_phys$mean_a$p.value, 
              pcor_af_factg_phys$Gini_a$p.value,
              pcor_af_factg_phys$h_r$p.value,
              
              pcor_af_factg_phys5item$alpha_a$p.value, 
              pcor_af_factg_phys5item$mean_r$p.value, 
              pcor_af_factg_phys5item$Gini_r$p.value, 
              pcor_af_factg_phys5item$h_a$p.value, 
              pcor_af_factg_phys5item$alpha_r$p.value, 
              pcor_af_factg_phys5item$mean_a$p.value, 
              pcor_af_factg_phys5item$Gini_a$p.value,
              pcor_af_factg_phys5item$h_r$p.value,
              
               pcor_af_promispf$alpha_a$p.value, 
               pcor_af_promispf$mean_r$p.value, 
               pcor_af_promispf$Gini_r$p.value, 
               pcor_af_promispf$h_a$p.value, 
               pcor_af_promispf$alpha_r$p.value, 
               pcor_af_promispf$mean_a$p.value, 
               pcor_af_promispf$Gini_a$p.value,
               pcor_af_promispf$h_r$p.value,
               
               pcor_af_vo2$alpha_a$p.value, 
               pcor_af_vo2$mean_r$p.value, 
               pcor_af_vo2$Gini_r$p.value, 
               pcor_af_vo2$h_a$p.value, 
               pcor_af_vo2$alpha_r$p.value, 
               pcor_af_vo2$mean_a$p.value, 
               pcor_af_vo2$Gini_a$p.value,
               pcor_af_vo2$h_r$p.value)) %>%
  
  # create 'Text' column for plotting
  rowwise() %>%
  mutate(text = ifelse(p.value < .001, paste(round(estimate, 2), '***', sep = ''),
                       ifelse((p.value >= .001 & p.value < .01), paste(round(estimate, 2), '**', sep = ''),
                              ifelse((p.value >= .01 & p.value < .05), paste(round(estimate, 2), '*', sep = ''),
                                     ifelse(p.value >= .05, paste(round(estimate, 2), sep = ''), NA)))))

# set order of variables for plotting
df_pcor_af$var1 <- factor(df_pcor_af$var1,
                       levels = c('Submaximal VO2', 'Linked PROMIS-PF', 'FACT-G Physical 5-item', 'FACT-G Physical', 'FACT-G Total'))
#                                  'FACT-G Social', 'FACT-G Functional', 'FACT-G Physical', 'FACT-G Total'))

df_pcor_af$var2 <- factor(df_pcor_af$var2,
                       levels = c('alpha_a', 'mean_r', 'Gini_r', 'h_a', 
                                  'alpha_r', 'mean_a', 'Gini_a', 'h_r'))

# create figure
figureS7B <- ggplot(df_pcor_af,
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

# create composite figure S9
figureS7 <- ggarrange(
  
  figureS7A,
  figureS7B,
  
  labels = c('A', 'B'),
  
  nrow = 2, ncol = 1,
  common.legend = FALSE
  
)

# save figure
ggsave(here('figures', 'figureS7.svg'),
       figureS7,
       device = 'svg', bg = 'white', dpi = 600,
       width = 6, height = 10)


# compare associations with activity fragmentation ------------------------

# for each measure, is the association with vo2 greater than that with fact-g physical

# select only variables involved in regression
data_survivors_AFregsubset <- data_survivors %>%
  dplyr::select(
    record_id,
    
    # demographics (covariates)
    age_demographics,
    sex,
    bmi,
    time_since_diagnosis_months,
    cancer_stage_numeric,
    
    # self-reported measures
    bl_qol_factg_total,
    bl_qol_physsubscale,
    bl_qol_physsubscale_5itemsub,
    promis_pf_t,
    
    # aerobic fitness
    vo2_hrr,
    
    # measures of activity fragmentation
    alpha_a, mean_r, Gini_r, h_a,
    alpha_r, mean_a, Gini_a, h_r
  )

# set contrast coding scheme to sum coding for sex (factor variable)
contrasts(data_survivors_AFregsubset$sex) <- c(-0.5, 0.5)

# standardize variables other than record_id and sex
# and bind to dataframe
data_survivors_AFregsubset <- 
  as.data.frame(
    cbind(
      data_survivors_AFregsubset,
      sapply(data_survivors_AFregsubset %>% 
               dplyr::select(c(-sex, record_id)), 
             scale, center = TRUE, scale = TRUE) %>%
        as.data.frame() %>%
        # add "_std" to end of each new column name
        rename_with(.fn = ~paste0(., '_std'))
    )
  )


# compare associations with alpha_a ---------------------------------------

compare_reg_alphaa_vo2_factg <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'alpha_a_std',
                     dv_label = 'alpha_a: Active time accumulation',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_factg_total_std',
                     iv_2_label = 'FACT-G',
                     iv_2_color = color_factg,
                     covariate_list = covariate_list
  )

compare_reg_alphaa_vo2_factgphys <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'alpha_a_std',
                     dv_label = 'alpha_a: Active time accumulation',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_std',
                     iv_2_label = 'FACT-G Physical',
                     iv_2_color = color_factgphys,
                     covariate_list = covariate_list
  )

compare_reg_alphaa_vo2_factgphys5item <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'alpha_a_std',
                     dv_label = 'alpha_a: Active time accumulation',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_5itemsub_std',
                     iv_2_label = 'FACT-G Physical 5-item',
                     iv_2_color = color_factgphys5sub,
                     covariate_list = covariate_list
  )

compare_reg_alphaa_vo2_promis <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'alpha_a_std',
                     dv_label = 'alpha_a: Active time accumulation',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'promis_pf_t_std',
                     iv_2_label = 'Linked PROMIS-PF',
                     iv_2_color = color_pf,
                     covariate_list = covariate_list
  )


# compare associations with mean_r ----------------------------------------

compare_reg_meanr_vo2_factg <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'mean_r_std',
                     dv_label = 'mean_r: Average sedentary bout duration',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_factg_total_std',
                     iv_2_label = 'FACT-G',
                     iv_2_color = color_factg,
                     covariate_list = covariate_list
  )

compare_reg_meanr_vo2_factgphys <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'mean_r_std',
                     dv_label = 'mean_r: Average sedentary bout duration',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_std',
                     iv_2_label = 'FACT-G Physical',
                     iv_2_color = color_factgphys,
                     covariate_list = covariate_list
  )

compare_reg_meanr_vo2_factgphys5item <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'mean_r_std',
                     dv_label = 'mean_r: Average sedentary bout duration',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_5itemsub_std',
                     iv_2_label = 'FACT-G Physical 5-item',
                     iv_2_color = color_factgphys5sub,
                     covariate_list = covariate_list
  )

compare_reg_meanr_vo2_promis <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'mean_r_std',
                     dv_label = 'mean_r: Average sedentary bout duration',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'promis_pf_t_std',
                     iv_2_label = 'Linked PROMIS-PF',
                     iv_2_color = color_pf,
                     covariate_list = covariate_list
  )


# compare associations with Gini_r ----------------------------------------

compare_reg_ginir_vo2_factg <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'Gini_r_std',
                     dv_label = 'gini_r: Sedentary time accumulation',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_factg_total_std',
                     iv_2_label = 'FACT-G',
                     iv_2_color = color_factg,
                     covariate_list = covariate_list
  )

compare_reg_ginir_vo2_factgphys <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'Gini_r_std',
                     dv_label = 'gini_r: Sedentary time accumulation',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_std',
                     iv_2_label = 'FACT-G Physical',
                     iv_2_color = color_factgphys,
                     covariate_list = covariate_list
  )

compare_reg_ginir_vo2_factgphys5item <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'Gini_r_std',
                     dv_label = 'gini_r: Sedentary time accumulation',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_5itemsub_std',
                     iv_2_label = 'FACT-G Physical 5-item',
                     iv_2_color = color_factgphys5sub,
                     covariate_list = covariate_list
  )

compare_reg_ginir_vo2_promis <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'Gini_r_std',
                     dv_label = 'gini_r: Sedentary time accumulation',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'promis_pf_t_std',
                     iv_2_label = 'Linked PROMIS-PF',
                     iv_2_color = color_pf,
                     covariate_list = covariate_list
  )


# compare associations with h_a -------------------------------------------

compare_reg_ha_vo2_factg <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'h_a_std',
                     dv_label = 'h_a: Frequency of transitioning\nfrom active to sedentary',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_factg_total_std',
                     iv_2_label = 'FACT-G',
                     iv_2_color = color_factg,
                     covariate_list = covariate_list
  )

compare_reg_ha_vo2_factgphys <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'h_a_std',
                     dv_label = 'h_a: Frequency of transitioning\nfrom active to sedentary',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_std',
                     iv_2_label = 'FACT-G Physical',
                     iv_2_color = color_factgphys,
                     covariate_list = covariate_list
  )


compare_reg_ha_vo2_factgphys5item <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'h_a_std',
                     dv_label = 'h_a: Frequency of transitioning\nfrom active to sedentary',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_5itemsub_std',
                     iv_2_label = 'FACT-G Physical 5-item',
                     iv_2_color = color_factgphys5sub,
                     covariate_list = covariate_list
  )

compare_reg_ha_vo2_promis <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'h_a_std',
                     dv_label = 'h_a: Frequency of transitioning\nfrom active to sedentary',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'promis_pf_t_std',
                     iv_2_label = 'Linked PROMIS-PF',
                     iv_2_color = color_pf,
                     covariate_list = covariate_list
  )


# compare associations with alpha_r ---------------------------------------

compare_reg_alphar_vo2_factg <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'alpha_r_std',
                     dv_label = 'alpha_r: Sedentary time accumulation',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_factg_total_std',
                     iv_2_label = 'FACT-G',
                     iv_2_color = color_factg,
                     covariate_list = covariate_list
  )

compare_reg_alphar_vo2_factgphys <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'alpha_r_std',
                     dv_label = 'alpha_r: Sedentary time accumulation',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_std',
                     iv_2_label = 'FACT-G Physical',
                     iv_2_color = color_factgphys,
                     covariate_list = covariate_list
  )

compare_reg_alphar_vo2_factgphys5item <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'alpha_r_std',
                     dv_label = 'alpha_r: Sedentary time accumulation',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_5itemsub_std',
                     iv_2_label = 'FACT-G Physical 5-item',
                     iv_2_color = color_factgphys5sub,
                     covariate_list = covariate_list
  )

compare_reg_alphar_vo2_promis <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'alpha_r_std',
                     dv_label = 'alpha_r: Sedentary time accumulation',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'promis_pf_t_std',
                     iv_2_label = 'Linked PROMIS-PF',
                     iv_2_color = color_pf,
                     covariate_list = covariate_list
  )


# compare associations with mean_a ----------------------------------------

compare_reg_meana_vo2_factg <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'mean_a_std',
                     dv_label = 'mean_a: Average active bout duration',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_factg_total_std',
                     iv_2_label = 'FACT-G',
                     iv_2_color = color_factg,
                     covariate_list = covariate_list
  )

compare_reg_meana_vo2_factgphys <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'mean_a_std',
                     dv_label = 'mean_a: Average active bout duration',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_std',
                     iv_2_label = 'FACT-G Physical',
                     iv_2_color = color_factgphys,
                     covariate_list = covariate_list
  )

compare_reg_meana_vo2_factgphys5item <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'mean_a_std',
                     dv_label = 'mean_a: Average active bout duration',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_5itemsub_std',
                     iv_2_label = 'FACT-G Physical 5-item',
                     iv_2_color = color_factgphys5sub,
                     covariate_list = covariate_list
  )

compare_reg_meana_vo2_promis <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'mean_a_std',
                     dv_label = 'mean_a: Average active bout duration',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'promis_pf_t_std',
                     iv_2_label = 'Linked PROMIS-PF',
                     iv_2_color = color_pf,
                     covariate_list = covariate_list
  )


# compare associations wtih Gini_a ----------------------------------------

compare_reg_ginia_vo2_factg <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'Gini_a_std',
                     dv_label = 'gini_a: Active time accumulation',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_factg_total_std',
                     iv_2_label = 'FACT-G',
                     iv_2_color = color_factg,
                     covariate_list = covariate_list
  )

compare_reg_ginia_vo2_factgphys <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'Gini_a_std',
                     dv_label = 'gini_a: Active time accumulation',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_std',
                     iv_2_label = 'FACT-G Physical',
                     iv_2_color = color_factgphys,
                     covariate_list = covariate_list
  )

compare_reg_ginia_vo2_factgphys5item <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'Gini_a_std',
                     dv_label = 'gini_a: Active time accumulation',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_5itemsub_std',
                     iv_2_label = 'FACT-G Physical 5-item',
                     iv_2_color = color_factgphys5sub,
                     covariate_list = covariate_list
  )

compare_reg_ginia_vo2_promis <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'Gini_a_std',
                     dv_label = 'gini_a: Actve time accumulation',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'promis_pf_t_std',
                     iv_2_label = 'Linked PROMIS-PF',
                     iv_2_color = color_pf,
                     covariate_list = covariate_list
  )


# compare associations with h_r -------------------------------------------

compare_reg_hr_vo2_factg <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'h_r_std',
                     dv_label = 'h_r: Frequency of transitioning\nfrom sedentary to active',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_factg_total_std',
                     iv_2_label = 'FACT-G',
                     iv_2_color = color_factg,
                     covariate_list = covariate_list
  )

compare_reg_hr_vo2_factgphys <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'h_r_std',
                     dv_label = 'h_r: Frequency of transitioning\nfrom sedentary to active',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_std',
                     iv_2_label = 'FACT-G Physical',
                     iv_2_color = color_factgphys,
                     covariate_list = covariate_list
  )

compare_reg_hr_vo2_factgphys5item <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'h_r_std',
                     dv_label = 'h_r: Frequency of transitioning\nfrom sedentary to active',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_5itemsub_std',
                     iv_2_label = 'FACT-G Physical 5-item',
                     iv_2_color = color_factgphys5sub,
                     covariate_list = covariate_list
  )

compare_reg_hr_vo2_promis <- 
  compare_reg_coeffs(df = data_survivors_AFregsubset, 
                     dv = 'h_r_std',
                     dv_label = 'h_r: Frequency of transitioning\nfrom sedentary to active',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'promis_pf_t_std',
                     iv_2_label = 'Linked PROMIS-PF',
                     iv_2_color = color_pf,
                     covariate_list = covariate_list
  )



# create figure S8 --------------------------------------------------------

figureS8 <- plot_grid(
  
  ggarrange(
    
    create_reg_comparison_plot_updated(comparison_result_vo2_factg_df = compare_reg_alphaa_vo2_factg[[1]],
                                       comparison_result_vo2_factg_lrt = compare_reg_alphaa_vo2_factg[[2]], 
                                       comparison_result_vo2_factgphys_df = compare_reg_alphaa_vo2_factgphys[[1]],
                                       comparison_result_vo2_factgphys_lrt = compare_reg_alphaa_vo2_factgphys[[2]], 
                                       comparison_result_vo2_factgphys5item_df = compare_reg_alphaa_vo2_factgphys5item[[1]],
                                       comparison_result_vo2_factgphys5item_lrt = compare_reg_alphaa_vo2_factgphys5item[[2]],
                                       comparison_result_vo2_promis_df = compare_reg_alphaa_vo2_promis[[1]],
                                       comparison_result_vo2_promis_lrt = compare_reg_alphaa_vo2_promis[[2]], 
                                       yvar_name = 'alpha_a: Active time accumulation', 
                                       subplot_title = 'alpha_a',
                                       ylim = c(-1.0, 0.9),
                                       bar_ycoord = 0.8) + 
              theme(legend.position = 'none'),
    
    create_reg_comparison_plot_updated(comparison_result_vo2_factg_df = compare_reg_meanr_vo2_factg[[1]],
                                       comparison_result_vo2_factg_lrt = compare_reg_meanr_vo2_factg[[2]],
                                       comparison_result_vo2_factgphys_df = compare_reg_meanr_vo2_factgphys[[1]],
                                       comparison_result_vo2_factgphys_lrt = compare_reg_meanr_vo2_factgphys[[2]], 
                                       comparison_result_vo2_factgphys5item_df = compare_reg_meanr_vo2_factgphys5item[[1]],
                                       comparison_result_vo2_factgphys5item_lrt = compare_reg_meanr_vo2_factgphys5item[[2]],
                                       comparison_result_vo2_promis_df = compare_reg_meanr_vo2_promis[[1]],
                                       comparison_result_vo2_promis_lrt = compare_reg_meanr_vo2_promis[[2]], 
                                       yvar_name = 'mean_r: Average sedentary bout duration', 
                                       subplot_title = 'mean_r',
                                       ylim = c(-1.0, 0.8),
                                       bar_ycoord = 0.7) + 
              theme(legend.position = 'none'),
    
    create_reg_comparison_plot_updated(comparison_result_vo2_factg_df = compare_reg_ginir_vo2_factg[[1]],
                                       comparison_result_vo2_factg_lrt = compare_reg_ginir_vo2_factg[[2]],
                                       comparison_result_vo2_factgphys_df = compare_reg_ginir_vo2_factgphys[[1]],
                                       comparison_result_vo2_factgphys_lrt = compare_reg_ginir_vo2_factgphys[[2]], 
                                       comparison_result_vo2_factgphys5item_df = compare_reg_ginir_vo2_factgphys5item[[1]],
                                       comparison_result_vo2_factgphys5item_lrt = compare_reg_ginir_vo2_factgphys5item[[2]],
                                       comparison_result_vo2_promis_df = compare_reg_ginir_vo2_promis[[1]],
                                       comparison_result_vo2_promis_lrt = compare_reg_ginir_vo2_promis[[2]], 
                                       yvar_name = 'Gini_r: Sedentary time accumulation', 
                                       subplot_title = 'Gini_r',
                                       ylim = c(-0.8, 0.8),
                                       bar_ycoord = 0.7) + 
              theme(legend.position = 'none'),
    
    create_reg_comparison_plot_updated(comparison_result_vo2_factg_df = compare_reg_ha_vo2_factg[[1]],
                                       comparison_result_vo2_factg_lrt = compare_reg_ha_vo2_factg[[2]],
                                       comparison_result_vo2_factgphys_df = compare_reg_ha_vo2_factgphys[[1]],
                                       comparison_result_vo2_factgphys_lrt = compare_reg_ha_vo2_factgphys[[2]], 
                                       comparison_result_vo2_factgphys5item_df = compare_reg_ha_vo2_factgphys5item[[1]],
                                       comparison_result_vo2_factgphys5item_lrt = compare_reg_ha_vo2_factgphys5item[[2]],
                                       comparison_result_vo2_promis_df = compare_reg_ha_vo2_promis[[1]],
                                       comparison_result_vo2_promis_lrt = compare_reg_ha_vo2_promis[[2]], 
                                       yvar_name = 'h_a: Frequency of transitioning\nfrom active to sedentary', 
                                       subplot_title = 'h_a',
                                       ylim = c(-1.4, 0.9),
                                       bar_ycoord = 0.8) + 
              theme(legend.position = 'none'),
    
    create_reg_comparison_plot_updated(comparison_result_vo2_factg_df = compare_reg_alphar_vo2_factg[[1]],
                                       comparison_result_vo2_factg_lrt = compare_reg_alphar_vo2_factg[[2]],
                                       comparison_result_vo2_factgphys_df = compare_reg_alphar_vo2_factgphys[[1]],
                                       comparison_result_vo2_factgphys_lrt = compare_reg_alphar_vo2_factgphys[[2]], 
                                       comparison_result_vo2_factgphys5item_df = compare_reg_alphar_vo2_factgphys5item[[1]],
                                       comparison_result_vo2_factgphys5item_lrt = compare_reg_alphar_vo2_factgphys5item[[2]],
                                       comparison_result_vo2_promis_df = compare_reg_alphar_vo2_promis[[1]],
                                       comparison_result_vo2_promis_lrt = compare_reg_alphar_vo2_promis[[2]], 
                                       yvar_name = 'alpha_r: Sedentary time accumulation', 
                                       subplot_title = 'alpha_r',
                                       ylim = c(-0.6, 1.2),
                                       bar_ycoord = 1.1) + 
              theme(legend.position = 'none'),
    
    create_reg_comparison_plot_updated(comparison_result_vo2_factg_df = compare_reg_meana_vo2_factg[[1]],
                                       comparison_result_vo2_factg_lrt = compare_reg_meana_vo2_factg[[2]],
                                       comparison_result_vo2_factgphys_df = compare_reg_meana_vo2_factgphys[[1]],
                                       comparison_result_vo2_factgphys_lrt = compare_reg_meana_vo2_factgphys[[2]], 
                                       comparison_result_vo2_factgphys5item_df = compare_reg_meana_vo2_factgphys5item[[1]],
                                       comparison_result_vo2_factgphys5item_lrt = compare_reg_meana_vo2_factgphys5item[[2]],
                                       comparison_result_vo2_promis_df = compare_reg_meana_vo2_promis[[1]],
                                       comparison_result_vo2_promis_lrt = compare_reg_meana_vo2_promis[[2]], 
                                       yvar_name = 'mean_a: Average active bout duration', 
                                       subplot_title = 'mean_a',
                                       ylim = c(-0.8, 1.3),
                                       bar_ycoord = 1.2) + 
              theme(legend.position = 'none'),
    
    create_reg_comparison_plot_updated(comparison_result_vo2_factg_df = compare_reg_ginia_vo2_factg[[1]],
                                       comparison_result_vo2_factg_lrt = compare_reg_ginia_vo2_factg[[2]],
                                       comparison_result_vo2_factgphys_df = compare_reg_ginia_vo2_factgphys[[1]],
                                       comparison_result_vo2_factgphys_lrt = compare_reg_ginia_vo2_factgphys[[2]], 
                                       comparison_result_vo2_factgphys5item_df = compare_reg_ginia_vo2_factgphys5item[[1]],
                                       comparison_result_vo2_factgphys5item_lrt = compare_reg_ginia_vo2_factgphys5item[[2]],
                                       comparison_result_vo2_promis_df = compare_reg_ginia_vo2_promis[[1]],
                                       comparison_result_vo2_promis_lrt = compare_reg_ginia_vo2_promis[[2]], 
                                       yvar_name = 'Gini_a: Active time accumulation', 
                                       subplot_title = 'Gini_a',
                                       ylim = c(-0.6, 1.4),
                                       bar_ycoord = 1.3) + 
              theme(legend.position = 'none'),
    
    create_reg_comparison_plot_updated(comparison_result_vo2_factg_df = compare_reg_hr_vo2_factg[[1]],
                                       comparison_result_vo2_factg_lrt = compare_reg_hr_vo2_factg[[2]],
                                       comparison_result_vo2_factgphys_df = compare_reg_hr_vo2_factgphys[[1]],
                                       comparison_result_vo2_factgphys_lrt = compare_reg_hr_vo2_factgphys[[2]], 
                                       comparison_result_vo2_factgphys5item_df = compare_reg_hr_vo2_factgphys5item[[1]],
                                       comparison_result_vo2_factgphys5item_lrt = compare_reg_hr_vo2_factgphys5item[[2]],
                                       comparison_result_vo2_promis_df = compare_reg_hr_vo2_promis[[1]],
                                       comparison_result_vo2_promis_lrt = compare_reg_hr_vo2_promis[[2]], 
                                       yvar_name = 'h_r: Frequency of transitioning\nfrom sedentary to active', 
                                       subplot_title = 'h_r',
                                       ylim = c(-0.6, 1.1),
                                       bar_ycoord = 1.0) + 
              theme(legend.position = 'none'),
    
    nrow = 4, ncol = 2, align = 'hv'
  ),
  
  legend, rel_widths = c(0.9, 0.1), nrow = 1, ncol = 2
  
)

ggsave(here('figures', 'figureS8.svg'),
       figureS8,
       device = 'svg', bg = 'white', 
       height = 11, width = 9)
