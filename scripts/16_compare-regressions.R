# test differences in associations with real-world physical behavior using a regression framework
# written by shelby bachman, shelby.bachman@vivosense.com


# standardize all variables involved in regressions -----------------------

# select only variables involved in regression
data_survivors_regsubset <- data_survivors %>%
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
    
    # measures of real-world physical behavior
    TotalSedentaryTime.m., 
    TotalRLMsCount, 
    light_min, 
    mvpa_min,
    `TimeInRLMsBouts>=1m`,
    `WeightedMedianCadence>=1m`,
    PeakRLMs30s
  )

# set contrast coding scheme to sum coding for sex (factor variable)
contrasts(data_survivors_regsubset$sex) <- c(-0.5, 0.5)

# standardize variables other than record_id and sex
# and bind to dataframe
data_survivors_regsubset <- 
  as.data.frame(
    cbind(
      data_survivors_regsubset,
      sapply(data_survivors_regsubset %>% 
               dplyr::select(c(-sex, record_id)), 
             scale, center = TRUE, scale = TRUE) %>%
        as.data.frame() %>%
        # add "_std" to end of each new column name
        rename_with(.fn = ~paste0(., '_std'))
    )
  )



# settings for visualizations ---------------------------------------------

# create title for composite plots
# indicating regression coefficient comparisons
title <- ggdraw() + 
  draw_label(
    'Comparison of regression coefficients',
    fontface = 'bold', size = 12,
    x = 0.5,
    hjust = 0
  )  +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7))

# colors for points for predicted VO2, FACT-G total, and linked PROMIS-PF
color_vo2 <- '#E64B35FF'
color_factg <- '#00A087FF'
color_factgphys <- '#4DBBD5FF'
color_factgphys5sub <- '#3C5488FF'
color_pf <- '#7E6148FF'


# function to compare and visualize regression coefficients ---------------

compare_reg_coeffs <- function(df, 
                               dv, dv_label, 
                               iv_1, iv_1_label, iv_1_color,
                               iv_2, iv_2_label, iv_2_color,
                               covariate_list) {
  
  ### build regression formulas
  formula_0 <- as.formula(
    paste(dv, ' ~ I(', iv_1, ' + ', iv_2, ') + ', paste(covariate_list, collapse = ' + '), sep = '')
  ) # constrained
  formula_1 <- as.formula(
    paste(dv, ' ~ ', iv_1, ' + ', iv_2, ' + ', paste(covariate_list, collapse = ' + '), sep = '')
  ) # unconstrained
  
  ### fit regression model (constrained)
  fit_0 <- lm(formula_0, data = df)
  
  ### fit regression model (unconstrained)
  fit_1 <- lm(formula_1, data = df)
  
  ### extract coefficients
  coefs_df <- data.frame(
    param = c(iv_1,
              iv_2),
    param_name = c(iv_1_label,
                   iv_2_label),
    param_color = c(iv_1_color,
                    iv_2_color),
    coef = c(summary(fit_1)$coefficients[2,1],
             summary(fit_1)$coefficients[3,1]),
    se = c(summary(fit_1)$coefficients[2,2],
           summary(fit_1)$coefficients[3,2]),
    ci_lower = c(confint(fit_1)[2,1],
                 confint(fit_1)[3,1]),
    ci_upper = c(confint(fit_1)[2,2],
                 confint(fit_1)[3,2])
  )
  
  ### perform likelihood ratio test
  mod_comp <- anova(fit_0, fit_1)
  
  ### format result of likelihood ratio test for inclusion in figure
  mod_comp_formatted <- 
    paste('F = ',
          round(mod_comp$`F`[2], 2),
          ', p ',
          papaja::printp(as.numeric(mod_comp$`Pr(>F)`[2]), add_equals = TRUE),
          sep = '')
  
  ### return results
  results <- vector(mode = 'list', length = 2)
  results[[1]] <- coefs_df
  results[[2]] <- mod_comp
  
  return(results)
  
}


# set parameters across analyses ------------------------------------------

covariate_list <- list('age_demographics_std',
                       'sex',
                       'bmi_std',
                       'time_since_diagnosis_months_std',
                       'cancer_stage_numeric_std')


# compare associations with sedentary time --------------------------------

compare_reg_sedentarytime_vo2_factg <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = 'TotalSedentaryTime.m._std',
                     dv_label = 'Sedentary time',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_factg_total_std',
                     iv_2_label = 'FACT-G',
                     iv_2_color = color_factg,
                     covariate_list = covariate_list
  )


compare_reg_sedentarytime_vo2_factg_phys <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = 'TotalSedentaryTime.m._std',
                     dv_label = 'Sedentary time',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_std',
                     iv_2_label = 'FACT-G Physical',
                     iv_2_color = color_factgphys,
                     covariate_list = covariate_list
  )

compare_reg_sedentarytime_vo2_factg_phys5item <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = 'TotalSedentaryTime.m._std',
                     dv_label = 'Sedentary time',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_5itemsub_std',
                     iv_2_label = 'FACT-G Physical 5-item',
                     iv_2_color = color_factgphys5sub,
                     covariate_list = covariate_list
  )

compare_reg_sedentarytime_vo2_promis <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = 'TotalSedentaryTime.m._std',
                     dv_label = 'Sedentary time',
                     iv_1 = 'promis_pf_t_std',
                     iv_1_label = 'Linked PROMIS-PF',
                     iv_1_color = color_pf,
                     iv_2 = 'vo2_hrr_std',
                     iv_2_label = 'Submaximal VO2',
                     iv_2_color = color_vo2,
                     covariate_list = covariate_list
  )


# compare associations with RLM count -------------------------------------

compare_reg_rlmcount_vo2_factg <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = 'TotalRLMsCount_std',
                     dv_label = 'Step count',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_factg_total_std',
                     iv_2_label = 'FACT-G',
                     iv_2_color = color_factg,
                     covariate_list = covariate_list
  )

compare_reg_rlmcount_vo2_factg_phys <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = 'TotalRLMsCount_std',
                     dv_label = 'Step count',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_std',
                     iv_2_label = 'FACT-G Physical',
                     iv_2_color = color_factgphys,
                     covariate_list = covariate_list
  )

compare_reg_rlmcount_vo2_factg_phys5item <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = 'TotalRLMsCount_std',
                     dv_label = 'Step count',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_5itemsub_std',
                     iv_2_label = 'FACT-G Physical 5-item',
                     iv_2_color = color_factgphys5sub,
                     covariate_list = covariate_list
  )

compare_reg_rlmcount_vo2_promis <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = 'TotalRLMsCount_std',
                     dv_label = 'Step count',
                     iv_1 = 'promis_pf_t_std',
                     iv_1_label = 'Linked PROMIS-PF',
                     iv_1_color = color_pf,
                     iv_2 = 'vo2_hrr_std',
                     iv_2_label = 'Submaximal VO2',
                     iv_2_color = color_vo2,
                     covariate_list = covariate_list
  )


# compare associations with light activity --------------------------------

compare_reg_lightactivity_vo2_factg <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = 'light_min_std',
                     dv_label = 'Light activity',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_factg_total_std',
                     iv_2_label = 'FACT-G',
                     iv_2_color = color_factg,
                     covariate_list = covariate_list
  )

compare_reg_lightactivity_vo2_factg_phys <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = 'light_min_std',
                     dv_label = 'Light activity',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_std',
                     iv_2_label = 'FACT-G Physical',
                     iv_2_color = color_factgphys,
                     covariate_list = covariate_list
  )

compare_reg_lightactivity_vo2_factg_phys5item <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = 'light_min_std',
                     dv_label = 'Light activity',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_5itemsub_std',
                     iv_2_label = 'FACT-G Physical 5-item',
                     iv_2_color = color_factgphys5sub,
                     covariate_list = covariate_list
  )

compare_reg_lightactivity_vo2_promis <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = 'light_min_std',
                     dv_label = 'Light activity',
                     iv_1 = 'promis_pf_t_std',
                     iv_1_label = 'Linked PROMIS-PF',
                     iv_1_color = color_pf,
                     iv_2 = 'vo2_hrr_std',
                     iv_2_label = 'Submaximal VO2',
                     iv_2_color = color_vo2,
                     covariate_list = covariate_list
  )


# compare associations with moderate-to-vigorous activity -----------------

compare_reg_modvigactivity_vo2_factg <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = 'mvpa_min_std',
                     dv_label = 'Mod-to-vig activity',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_factg_total_std',
                     iv_2_label = 'FACT-G',
                     iv_2_color = color_factg,
                     covariate_list = covariate_list
  )

compare_reg_modvigactivity_vo2_factg_phys <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = 'mvpa_min_std',
                     dv_label = 'Mod-to-vig activity',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_std',
                     iv_2_label = 'FACT-G Physical',
                     iv_2_color = color_factgphys,
                     covariate_list = covariate_list
  )

compare_reg_modvigactivity_vo2_factg_phys5item <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = 'mvpa_min_std',
                     dv_label = 'Mod-to-vig activity',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_5itemsub_std',
                     iv_2_label = 'FACT-G Physical 5-item',
                     iv_2_color = color_factgphys5sub,
                     covariate_list = covariate_list
  )

compare_reg_modvigactivity_vo2_promis <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = 'mvpa_min_std',
                     dv_label = 'Mod-to-vig activity',
                     iv_1 = 'promis_pf_t_std',
                     iv_1_label = 'Linked PROMIS-PF',
                     iv_1_color = color_pf,
                     iv_2 = 'vo2_hrr_std',
                     iv_2_label = 'Submaximal VO2',
                     iv_2_color = color_vo2,
                     covariate_list = covariate_list
  )


# compare associations with time in RLM bouts >=1min ----------------------

compare_reg_timeinbouts_vo2_factg <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = "`TimeInRLMsBouts>=1m_std`",
                     dv_label = 'Time in bouts >=1min',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_factg_total_std',
                     iv_2_label = 'FACT-G',
                     iv_2_color = color_factg,
                     covariate_list = covariate_list
  )

compare_reg_timeinbouts_vo2_factg_phys <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = "`TimeInRLMsBouts>=1m_std`",
                     dv_label = 'Time in bouts >=1min',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_std',
                     iv_2_label = 'FACT-G Physical',
                     iv_2_color = color_factgphys,
                     covariate_list = covariate_list
  )

compare_reg_timeinbouts_vo2_factg_phys5item <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = "`TimeInRLMsBouts>=1m_std`",
                     dv_label = 'Time in bouts >=1min',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_5itemsub_std',
                     iv_2_label = 'FACT-G Physical 5-item',
                     iv_2_color = color_factgphys5sub,
                     covariate_list = covariate_list
  )

compare_reg_timeinbouts_vo2_promis <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = "`TimeInRLMsBouts>=1m_std`",
                     dv_label = 'Time in bouts >=1min',
                     iv_1 = 'promis_pf_t_std',
                     iv_1_label = 'Linked PROMIS-PF',
                     iv_1_color = color_pf,
                     iv_2 = 'vo2_hrr_std',
                     iv_2_label = 'Submaximal VO2',
                     iv_2_color = color_vo2,
                     covariate_list = covariate_list
  )


# compare associations with cadence in RLM bouts >=1min -------------------

compare_reg_cadence_vo2_factg <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = "`WeightedMedianCadence>=1m_std`",
                     dv_label = 'Cadence in bouts >=1min',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_factg_total_std',
                     iv_2_label = 'FACT-G',
                     iv_2_color = color_factg,
                     covariate_list = covariate_list
  )

compare_reg_cadence_vo2_factg_phys <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = "`WeightedMedianCadence>=1m_std`",
                     dv_label = 'Cadence in bouts >=1min',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_std',
                     iv_2_label = 'FACT-G Physical',
                     iv_2_color = color_factgphys,
                     covariate_list = covariate_list
  )

compare_reg_cadence_vo2_factg_phys5item <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = "`WeightedMedianCadence>=1m_std`",
                     dv_label = 'Cadence in bouts >=1min',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_5itemsub_std',
                     iv_2_label = 'FACT-G Physical 5-item',
                     iv_2_color = color_factgphys5sub,
                     covariate_list = covariate_list
  )

compare_reg_cadence_vo2_promis <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = "`WeightedMedianCadence>=1m_std`",
                     dv_label = 'Cadence in bouts >=1min',
                     iv_1 = 'promis_pf_t_std',
                     iv_1_label = 'Linked PROMIS-PF',
                     iv_1_color = color_pf,
                     iv_2 = 'vo2_hrr_std',
                     iv_2_label = 'Submaximal VO2',
                     iv_2_color = color_vo2,
                     covariate_list = covariate_list
  )


# compare associations with peak RLMs -------------------------------------

compare_reg_peakRLMs_vo2_factg <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = "PeakRLMs30s_std",
                     dv_label = 'Peak 30s cadence',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_factg_total_std',
                     iv_2_label = 'FACT-G',
                     iv_2_color = color_factg,
                     covariate_list = covariate_list
  )

compare_reg_peakRLMs_vo2_factg_phys <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = "PeakRLMs30s_std",
                     dv_label = 'Peak 30s cadence',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_std',
                     iv_2_label = 'FACT-G Physical',
                     iv_2_color = color_factgphys,
                     covariate_list = covariate_list
  )

compare_reg_peakRLMs_vo2_factg_phys5item <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = "PeakRLMs30s_std",
                     dv_label = 'Peak 30s cadence',
                     iv_1 = 'vo2_hrr_std',
                     iv_1_label = 'Submaximal VO2',
                     iv_1_color = color_vo2,
                     iv_2 = 'bl_qol_physsubscale_5itemsub_std',
                     iv_2_label = 'FACT-G Physical 5-item',
                     iv_2_color = color_factgphys5sub,
                     covariate_list = covariate_list
  )

compare_reg_peakRLMs_vo2_promis <- 
  compare_reg_coeffs(df = data_survivors_regsubset, 
                     dv = "PeakRLMs30s_std",
                     dv_label = 'Peak 30s cadence',
                     iv_1 = 'promis_pf_t_std',
                     iv_1_label = 'Linked PROMIS-PF',
                     iv_1_color = color_pf,
                     iv_2 = 'vo2_hrr_std',
                     iv_2_label = 'Submaximal VO2',
                     iv_2_color = color_vo2,
                     covariate_list = covariate_list
  )


# function to plot regression coefficient comparisons ---------------------

create_reg_comparison_plot <- function(comparison_result_vo2_factg_df,
                                       comparison_result_vo2_factg_lrt,
                                       comparison_result_vo2_promis_df,
                                       comparison_result_vo2_promis_lrt,
                                       yvar_name,
                                       ylim,
                                       subplot_title,
                                       bar_ycoord) {
  
  # create tidy dataframe with regression coefficients and LRT results
  compare_reg <- as.data.frame(
    rbind(
      # associations with sedentary time
      comparison_result_vo2_factg_df %>% 
        rowwise() %>%
        mutate(y_name = yvar_name,
               subplot_title = subplot_title,
               comparison = 1,
               comparison_F = comparison_result_vo2_factg_lrt$F[2],
               comparison_df = comparison_result_vo2_factg_lrt$Df[2],
               comparison_p = comparison_result_vo2_factg_lrt$`Pr(>F)`[2]),
      comparison_result_vo2_promis_df %>% 
        rowwise() %>%
        mutate(y_name = yvar_name,
               subplot_title = subplot_title,
               comparison = 2,
               comparison_F = comparison_result_vo2_promis_lrt$F[2],
               comparison_df = comparison_result_vo2_promis_lrt$Df[2],
               comparison_p = comparison_result_vo2_promis_lrt$`Pr(>F)`[2])
    )
  ) %>%
    dplyr::select(y_name, subplot_title, comparison, x_name = param_name, param, 
                  coef, se, ci_lower, ci_upper, param_color, comparison_F, comparison_df, comparison_p) %>%
    rowwise() %>%
    mutate(comparison_sig = ifelse(comparison_p < .001, '***', 
                                   ifelse((comparison_p >= .001) & (comparison_p < .01), '**', 
                                          ifelse((comparison_p >= .01) & (comparison_p < .05), '*', 
                                                 ifelse(comparison_p >= .05, 'ns', NA)))))
  
  # create summarized version of dataframe, showing test result (for comparisons on plot)
  compare_reg_unique <- compare_reg %>%
    group_by(y_name, comparison) %>%
    summarize(comparison_sig = unique(comparison_sig))
  
  # set factor label for x-axis
  compare_reg$x_name <- factor(compare_reg$x_name, levels = c('Submaximal VO2', 'FACT-G', 'Linked PROMIS-PF'))
  
  # create plot
  p <- ggplot(data = compare_reg %>%
                arrange(desc(x_name)), #%>%
              aes(x = x_name,
                  y = coef)) +
    geom_errorbar(aes(ymin = ci_lower, 
                      ymax = ci_upper), 
                  width = 0.3) +
    geom_point(pch = 21,
               aes(fill = x_name),
               size = 3) +
    geom_hline(yintercept = 0, 
               linetype = 'dashed', 
               colour = 'darkgray') +
    scale_colour_identity() +
    scale_fill_manual(values = c('Submaximal VO2' = color_vo2,
                                 'FACT-G' = color_factg,
                                 'Linked PROMIS-PF' = color_pf)) +
    labs(x = '',
         fill = '',
         y = 'Standardized\nregression coefficient',
         subtitle = compare_reg$subplot_title[1]) +
    scale_x_discrete(expand = c(0.6,0.6)) +
    facet_wrap(~comparison, 
               scales = 'free_x') +
    coord_cartesian(ylim = ylim) +
    geom_bracket(data = compare_reg_unique,
                 aes(label = comparison_sig),
                 xmin = 1, xmax = 2,
                 y.position = bar_ycoord) +
    theme_pubr() +
    theme(plot.subtitle = element_text(size = 11, face = 'bold', hjust = 0.5),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(size = 10),
          axis.text.y = element_text(size = 8),
          strip.background = element_blank(),
          strip.text = element_blank(),
          legend.position = 'right',
          plot.margin = margin(0, 0, 0, 0))
  
  return(p)
  
}


# function to plot regression coefficient comparisons ---------------------

create_reg_comparison_plot_updated <- function(comparison_result_vo2_factg_df,
                                               comparison_result_vo2_factg_lrt,
                                               comparison_result_vo2_factgphys_df,
                                               comparison_result_vo2_factgphys_lrt,
                                               comparison_result_vo2_factgphys5item_df,
                                               comparison_result_vo2_factgphys5item_lrt,
                                               comparison_result_vo2_promis_df,
                                               comparison_result_vo2_promis_lrt,
                                               yvar_name,
                                               ylim,
                                               subplot_title,
                                               bar_ycoord) {
  
  # create tidy dataframe with regression coefficients and LRT results
  compare_reg <- as.data.frame(
    rbind(
      comparison_result_vo2_factg_df %>% 
        rowwise() %>%
        mutate(y_name = yvar_name,
               subplot_title = subplot_title,
               comparison = 1,
               comparison_F = comparison_result_vo2_factg_lrt$F[2],
               comparison_df = comparison_result_vo2_factg_lrt$Df[2],
               comparison_p = comparison_result_vo2_factg_lrt$`Pr(>F)`[2]),
      
      comparison_result_vo2_factgphys_df %>% 
        rowwise() %>%
        mutate(y_name = yvar_name,
               subplot_title = subplot_title,
               comparison = 2,
               comparison_F = comparison_result_vo2_factgphys_lrt$F[2],
               comparison_df = comparison_result_vo2_factgphys_lrt$Df[2],
               comparison_p = comparison_result_vo2_factgphys_lrt$`Pr(>F)`[2]),
      
      comparison_result_vo2_factgphys5item_df %>% 
        rowwise() %>%
        mutate(y_name = yvar_name,
               subplot_title = subplot_title,
               comparison = 3,
               comparison_F = comparison_result_vo2_factgphys5item_lrt$F[2],
               comparison_df = comparison_result_vo2_factgphys5item_lrt$Df[2],
               comparison_p = comparison_result_vo2_factgphys5item_lrt$`Pr(>F)`[2]),
      
      comparison_result_vo2_promis_df %>% 
        rowwise() %>%
        mutate(y_name = yvar_name,
               subplot_title = subplot_title,
               comparison = 4,
               comparison_F = comparison_result_vo2_promis_lrt$F[2],
               comparison_df = comparison_result_vo2_promis_lrt$Df[2],
               comparison_p = comparison_result_vo2_promis_lrt$`Pr(>F)`[2])
    )
  ) %>%
    dplyr::select(y_name, subplot_title, comparison, x_name = param_name, param, 
                  coef, se, ci_lower, ci_upper, param_color, comparison_F, comparison_df, comparison_p) %>%
    rowwise() %>%
    mutate(comparison_sig = ifelse(comparison_p < .001, '***', 
                                   ifelse((comparison_p >= .001) & (comparison_p < .01), '**', 
                                          ifelse((comparison_p >= .01) & (comparison_p < .05), '*', 
                                                 ifelse(comparison_p >= .05, 'ns', NA)))))
  
  # create summarized version of dataframe, showing test result (for comparisons on plot)
  compare_reg_unique <- compare_reg %>%
    group_by(y_name, comparison) %>%
    summarize(comparison_sig = unique(comparison_sig))
  
  # set factor label for x-axis
  compare_reg$x_name <- factor(compare_reg$x_name, levels = c('Submaximal VO2', 'FACT-G', 'FACT-G Physical', 'FACT-G Physical 5-item', 'Linked PROMIS-PF'))
  
  # create plot
  p <- ggplot(data = compare_reg %>%
                arrange(desc(x_name)), #%>%
              aes(x = x_name,
                  y = coef)) +
    geom_hline(yintercept = 0, 
               linetype = 'dashed', 
               colour = 'darkgray') +
    geom_errorbar(aes(ymin = ci_lower, 
                      ymax = ci_upper), 
                  width = 0.3) +
    geom_point(pch = 21,
               aes(fill = x_name),
               size = 3) +
    scale_colour_identity() +
    scale_fill_manual(values = c('Submaximal VO2' = color_vo2,
                                 'FACT-G' = color_factg,
                                 'FACT-G Physical' = color_factgphys,
                                 'FACT-G Physical 5-item' = color_factgphys5sub,
                                 'Linked PROMIS-PF' = color_pf)) +
    labs(x = '',
         fill = '',
         y = 'Standardized\nregression coefficient',
         subtitle = compare_reg$subplot_title[1]) +
    scale_x_discrete(expand = c(0.6,0.6)) +
    facet_wrap(~comparison, 
               nrow = 1, ncol = 4,
               scales = 'free_x') +
    coord_cartesian(ylim = ylim) +
    geom_bracket(data = compare_reg_unique,
                 aes(label = comparison_sig),
                 xmin = 1, xmax = 2,
                 y.position = bar_ycoord) +
    theme_pubr() +
    theme(plot.subtitle = element_text(size = 11, face = 'bold', hjust = 0.5),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(size = 10),
          axis.text.y = element_text(size = 8),
          strip.background = element_blank(),
          strip.text = element_blank(),
          legend.position = 'right',
          plot.margin = margin(0, 0, 0, 0))
  
  return(p)
  
}


# create figure 3 ---------------------------------------------------------

legend <- get_legend(
  create_reg_comparison_plot_updated(comparison_result_vo2_factg_df = compare_reg_sedentarytime_vo2_factg[[1]],
                                     comparison_result_vo2_factg_lrt = compare_reg_sedentarytime_vo2_factg[[2]],
                                     comparison_result_vo2_factgphys_df = compare_reg_sedentarytime_vo2_factg_phys[[1]],
                                     comparison_result_vo2_factgphys_lrt = compare_reg_sedentarytime_vo2_factg_phys[[2]],
                                     comparison_result_vo2_factgphys5item_df = compare_reg_sedentarytime_vo2_factg_phys5item[[1]],
                                     comparison_result_vo2_factgphys5item_lrt = compare_reg_sedentarytime_vo2_factg_phys5item[[2]],
                                     comparison_result_vo2_promis_df = compare_reg_sedentarytime_vo2_promis[[1]],
                                     comparison_result_vo2_promis_lrt = compare_reg_sedentarytime_vo2_promis[[2]], 
                                     yvar_name = 'sedentary time', 
                                     subplot_title = 'Sedentary time',
                                     ylim = c(-1.0, 0.9),
                                     bar_ycoord = 0.8)
)

figure3 <- plot_grid(
  create_reg_comparison_plot_updated(comparison_result_vo2_factg_df = compare_reg_sedentarytime_vo2_factg[[1]],
                                     comparison_result_vo2_factg_lrt = compare_reg_sedentarytime_vo2_factg[[2]],
                                     comparison_result_vo2_factgphys_df = compare_reg_sedentarytime_vo2_factg_phys[[1]],
                                     comparison_result_vo2_factgphys_lrt = compare_reg_sedentarytime_vo2_factg_phys[[2]],
                                     comparison_result_vo2_factgphys5item_df = compare_reg_sedentarytime_vo2_factg_phys5item[[1]],
                                     comparison_result_vo2_factgphys5item_lrt = compare_reg_sedentarytime_vo2_factg_phys5item[[2]],
                                     comparison_result_vo2_promis_df = compare_reg_sedentarytime_vo2_promis[[1]],
                                     comparison_result_vo2_promis_lrt = compare_reg_sedentarytime_vo2_promis[[2]], 
                                     yvar_name = 'sedentary time', 
                                     subplot_title = 'Sedentary time',
                                     ylim = c(-1.0, 0.9),
                                     bar_ycoord = 0.8) + 
            theme(legend.position = 'none'),
  
  create_reg_comparison_plot_updated(comparison_result_vo2_factg_df = compare_reg_rlmcount_vo2_factg[[1]],
                                     comparison_result_vo2_factg_lrt = compare_reg_rlmcount_vo2_factg[[2]],
                                     comparison_result_vo2_factgphys_df = compare_reg_rlmcount_vo2_factg_phys[[1]],
                                     comparison_result_vo2_factgphys_lrt = compare_reg_rlmcount_vo2_factg_phys[[2]],
                                     comparison_result_vo2_factgphys5item_df = compare_reg_rlmcount_vo2_factg_phys5item[[1]],
                                     comparison_result_vo2_factgphys5item_lrt = compare_reg_rlmcount_vo2_factg_phys5item[[2]],
                                     comparison_result_vo2_promis_df = compare_reg_rlmcount_vo2_promis[[1]],
                                     comparison_result_vo2_promis_lrt = compare_reg_rlmcount_vo2_promis[[2]], 
                                     yvar_name = 'step count', 
                                     subplot_title = 'Step count',
                                     ylim = c(-0.35, 1.3),
                                     bar_ycoord = 1.2) + 
            theme(legend.position = 'none'),
  
  create_reg_comparison_plot_updated(comparison_result_vo2_factg_df = compare_reg_lightactivity_vo2_factg[[1]],
                                     comparison_result_vo2_factg_lrt = compare_reg_lightactivity_vo2_factg[[2]],
                                     comparison_result_vo2_factgphys_df = compare_reg_lightactivity_vo2_factg_phys[[1]],
                                     comparison_result_vo2_factgphys_lrt = compare_reg_lightactivity_vo2_factg_phys[[2]],
                                     comparison_result_vo2_factgphys5item_df = compare_reg_lightactivity_vo2_factg_phys5item[[1]],
                                     comparison_result_vo2_factgphys5item_lrt = compare_reg_lightactivity_vo2_factg_phys5item[[2]],
                                     comparison_result_vo2_promis_df = compare_reg_lightactivity_vo2_promis[[1]],
                                     comparison_result_vo2_promis_lrt = compare_reg_lightactivity_vo2_promis[[2]], 
                                     yvar_name = 'time in light activity', 
                                     subplot_title = 'Time in light activity',
                                     ylim = c(-0.6, 1.1),
                                     bar_ycoord = 1.0) + 
            theme(legend.position = 'none'),
  
  create_reg_comparison_plot_updated(comparison_result_vo2_factg_df = compare_reg_modvigactivity_vo2_factg[[1]],
                                     comparison_result_vo2_factg_lrt = compare_reg_modvigactivity_vo2_factg[[2]],
                                     comparison_result_vo2_factgphys_df = compare_reg_modvigactivity_vo2_factg_phys[[1]],
                                     comparison_result_vo2_factgphys_lrt = compare_reg_modvigactivity_vo2_factg_phys[[2]],
                                     comparison_result_vo2_factgphys5item_df = compare_reg_modvigactivity_vo2_factg_phys5item[[1]],
                                     comparison_result_vo2_factgphys5item_lrt = compare_reg_modvigactivity_vo2_factg_phys5item[[2]],
                                     comparison_result_vo2_promis_df = compare_reg_modvigactivity_vo2_promis[[1]],
                                     comparison_result_vo2_promis_lrt = compare_reg_modvigactivity_vo2_promis[[2]], 
                                     yvar_name = 'time in mod-to-vig activity', 
                                     subplot_title = 'Time in moderate-to-\nvigorous activity', 
                                     ylim = c(-0.2, 0.5), 
                                     bar_ycoord = 0.4) + 
            theme(legend.position = 'none'),
  
  create_reg_comparison_plot_updated(comparison_result_vo2_factg_df = compare_reg_timeinbouts_vo2_factg[[1]],
                                     comparison_result_vo2_factg_lrt = compare_reg_timeinbouts_vo2_factg[[2]],
                                     comparison_result_vo2_factgphys_df = compare_reg_timeinbouts_vo2_factg_phys[[1]],
                                     comparison_result_vo2_factgphys_lrt = compare_reg_timeinbouts_vo2_factg_phys[[2]],
                                     comparison_result_vo2_factgphys5item_df = compare_reg_timeinbouts_vo2_factg_phys5item[[1]],
                                     comparison_result_vo2_factgphys5item_lrt = compare_reg_timeinbouts_vo2_factg_phys5item[[2]],
                                     comparison_result_vo2_promis_df = compare_reg_timeinbouts_vo2_promis[[1]],
                                     comparison_result_vo2_promis_lrt = compare_reg_timeinbouts_vo2_promis[[2]], 
                                     yvar_name = 'time in stepping bouts >=1min',
                                     subplot_title = 'Time in stepping bouts\n>=1min',
                                     ylim = c(-0.4, 1.1),
                                     bar_ycoord = 1.0) + 
            theme(legend.position = 'none'),
  
  create_reg_comparison_plot_updated(comparison_result_vo2_factg_df = compare_reg_cadence_vo2_factg[[1]],
                                     comparison_result_vo2_factg_lrt = compare_reg_cadence_vo2_factg[[2]],
                                     comparison_result_vo2_factgphys_df = compare_reg_cadence_vo2_factg_phys[[1]],
                                     comparison_result_vo2_factgphys_lrt = compare_reg_cadence_vo2_factg_phys[[2]],
                                     comparison_result_vo2_factgphys5item_df = compare_reg_cadence_vo2_factg_phys5item[[1]],
                                     comparison_result_vo2_factgphys5item_lrt = compare_reg_cadence_vo2_factg_phys5item[[2]],
                                     comparison_result_vo2_promis_df = compare_reg_cadence_vo2_promis[[1]],
                                     comparison_result_vo2_promis_lrt = compare_reg_cadence_vo2_promis[[2]], 
                                     yvar_name = 'cadence in stepping bouts >=1min', 
                                     subplot_title = 'Cadence in stepping bouts\n>=1min', 
                                     ylim = c(-0.4, 0.8),
                                     bar_ycoord = 0.7) + 
            theme(legend.position = 'none'),
  
  create_reg_comparison_plot_updated(comparison_result_vo2_factg_df = compare_reg_peakRLMs_vo2_factg[[1]],
                                     comparison_result_vo2_factg_lrt = compare_reg_peakRLMs_vo2_factg[[2]],
                                     comparison_result_vo2_factgphys_df = compare_reg_peakRLMs_vo2_factg_phys[[1]],
                                     comparison_result_vo2_factgphys_lrt = compare_reg_peakRLMs_vo2_factg_phys[[2]],
                                     comparison_result_vo2_factgphys5item_df = compare_reg_peakRLMs_vo2_factg_phys5item[[1]],
                                     comparison_result_vo2_factgphys5item_lrt = compare_reg_peakRLMs_vo2_factg_phys5item[[2]],
                                     comparison_result_vo2_promis_df = compare_reg_peakRLMs_vo2_promis[[1]],
                                     comparison_result_vo2_promis_lrt = compare_reg_peakRLMs_vo2_promis[[2]], 
                                     yvar_name = 'peak 30s cadence', 
                                     subplot_title = 'Peak 30s cadence',
                                     ylim = c(-0.4, 1.1),
                                     bar_ycoord = 1.0) + 
            theme(legend.position = 'none'),
  
  legend,
  
  nrow = 4, ncol = 2, align = 'hv'
)

ggsave(here('figures', 'figure3.svg'),
       figure3,
       device = 'svg', bg = 'white', dpi = 600,
       height = 11, width = 9)
