# compare real-world physical behavior measures based on splits of self-reported measures
# written by shelby bachman, shelby.bachman@vivosense.com


# prepare data for visualizing split comparisons --------------------------

data_fig2 <- data_survivors %>%
  # select only relevant variables
  dplyr::select(
    record_id,
    bl_qol_factg_total,
    bl_qol_physsubscale,
    bl_qol_physsubscale_5itemsub,
    promis_pf_t,
    vo2_hrr,
    TotalSedentaryTime.m., 
    TotalRLMsCount, 
    light_min,
    mvpa_min,
    mvpa_min_rin, 
    `TimeInRLMsBouts>=1m`,
    `WeightedMedianCadence>=1m`, 
    PeakRLMs30s,
    tertile_factg,
    tertile_factg_phys,
    tertile_factg_phys5item,
    tertile_pf,
    medsplit_vo2
  ) %>%
  # convert from wide to long
  pivot_longer(cols = tertile_factg:medsplit_vo2, 
               names_to = 'split', 
               values_to = 'level') %>%
  # rename split variable
  rowwise() %>%
  mutate(variable = case_when(
    split == 'tertile_factg' ~ 'FACT-G Total',
    split == 'tertile_factg_phys' ~ 'FACT-G Physical',
    split == 'tertile_factg_phys5item' ~ 'FACT-G Physical\n5-item',
    split == 'tertile_pf' ~ 'Linked\nPROMIS-PF',
    split == 'medsplit_vo2' ~ 'Submaximal\nVO2'
  ))

# make `variable` a factor
data_fig2$variable <- factor(data_fig2$variable, levels = c('FACT-G Total',
                                                            'FACT-G Physical',
                                                            'FACT-G Physical\n5-item',
                                                            'Linked\nPROMIS-PF', 
                                                            'Submaximal\nVO2'))

data_fig2 <- data_fig2 %>%
  filter(variable %in% c('FACT-G Total', 
                         'FACT-G Physical',
                         'FACT-G Physical\n5-item',
                         'Linked\nPROMIS-PF', 
                         'Submaximal\nVO2'))


# functions for creating boxplots to compare splits ------------------------

# function to plot tertile splits of self-reported measures (UPDATED)
# and median split of predicted, side-by-side
plot_boxplots_splits_combo_updated <- function(dataframe,
                                       objective_measure, 
                                       ylim, 
                                       comparison_y_pos,
                                       objective_measure_label,
                                       comparison_method,
                                       strip_position,
                                       subtitle_label) {
  
  p1 <- ggplot(data = dataframe[!is.na(dataframe['level']) & (dataframe['variable'] == 'FACT-G Total' | dataframe['variable'] == 'FACT-G Physical' | dataframe['variable'] == 'FACT-G Physical\n5-item' | dataframe['variable'] == 'Linked\nPROMIS-PF'),],
               aes_string(x = 'level',
                          y = objective_measure,
                          colour = 'level',
                          fill = 'level')) +
    geom_point(alpha = 0.7, 
               pch = 21, 
               size = 1,
               colour = 'black',
               position = position_jitter(width = 0.1)) +
    geom_boxplot(color = 'black',
                 width = 0.6,
                 outlier.shape = NA,
                 alpha = 0.6) +  
    stat_compare_means(paired = FALSE,
                       method = comparison_method, 
                       label = 'p.signif',
                       comparisons = comparisons_subjective) +
    scale_fill_manual(values = palette_3levels) +
    scale_x_discrete(expand = c(0.5, 0.5)) +
    coord_cartesian(ylim = ylim) +
    labs(x = '',
         y = objective_measure_label,
         colour = '',
         fill = '') +
    facet_wrap(~variable,
               nrow = 1, ncol = 4,
               strip.position = strip_position) +
    theme_pubr() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size = 10, angle = 30, hjust = 0.3),
          axis.title.y = element_text(size = 10),
          axis.text.y = element_text(size = 8),
          panel.spacing.x = unit(-1, "lines"),
          plot.margin = margin(5, 0, 5, 0),
          strip.placement = 'outside')
  
  
  p2 <- ggplot(data = dataframe[!is.na(dataframe['level']) & (dataframe['variable'] == 'Submaximal\nVO2'),],
               aes_string(x = 'level',
                          y = objective_measure,
                          colour = 'level',
                          fill = 'level')) +
    geom_point(alpha = 0.7, 
               pch = 21, 
               size = 1,
               colour = 'black',
               position = position_jitter(width = 0.1)) +
    geom_boxplot(color = 'black',
                 width = 0.6,
                 outlier.shape = NA,
                 alpha = 0.6) +  
    stat_compare_means(paired = FALSE,
                       method = comparison_method, 
                       label = 'p.signif',
                       comparisons = comparisons_vo2,
                       label.y = comparison_y_pos) +
    scale_fill_manual(values = palette_2levels) +
    scale_x_discrete(expand = c(0.5, 0.5)) +
    coord_cartesian(ylim = ylim) +
    labs(x = '',
         y = objective_measure_label,
         colour = '',
         fill = '') +
    facet_wrap(~variable, 
               strip.position = strip_position) +
    theme_pubr() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size = 10, angle = 30, hjust = 0.3, vjust = 0.5),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          panel.spacing.x = unit(-1, "lines"),
          plot.margin = margin(-1, 0, 5, 0),
          strip.placement = 'outside')
  
  # combine individual plots
  p <- ggarrange(p1, p2,
                 nrow = 1, ncol = 2, 
                 widths = c(0.85, 0.15), align = 'h',
                 common.legend = TRUE, legend = 'none'
  )
  
  # add subtitle to plot
  p0 <- as_ggplot(
    text_grob(subtitle_label, size = 10, face = 'bold', just = c('center', 'top'))
  ) +
    theme(plot.margin = margin(0, 0, 0, 0))
  
  p <- ggarrange(p0, p,
                 ncol = 1, nrow = 2,
                 heights = c(0.08, 0.92),
                 align = 'v')
  
  return(p)
}

# set comparisons for visualizing split comparisons -----------------------

# comparisons of self-reported measure splits (tertile)
comparisons_subjective <- list( c("Low", "Medium"), 
                                c("Medium", "High"), 
                                c("Low", "High") )

# comparisons of predicted VO2 split (median)
comparisons_vo2 <- list( c("Low", "High") )


# set color palettes for visualizing split comparisons --------------------

palette_3levels <- c('High' = '#00A087FF',
                     'Medium' = '#3C5488FF',
                     'Low' = '#F39B7FFF')

palette_2levels <- c('High' = '#00A087FF',
                     'Low' = '#F39B7FFF')


# create legend for composite plots ----------------------------------------

legend <- cowplot::get_legend(
  plot_boxplots_splits_combo_updated(dataframe = data_fig2,
                                     objective_measure = 'TotalSedentaryTime.m.', 
                                     ylim = c(300, 1000), 
                                     comparison_y_pos = 875,
                                     objective_measure_label = 'Sedentary time (min)', 
                                     comparison_method = 'wilcox.test',
                                     strip_position = 'bottom',
                                     subtitle_label = 'Sedentary time') +
  theme(legend.position = 'right',
        legend.text = element_text(size = 10))
)


# create figure 2 ---------------------------------------------------------

figure2 <- plot_grid(
  
  plot_boxplots_splits_combo_updated(dataframe = data_fig2,
                                     objective_measure = 'TotalSedentaryTime.m.', 
                                     ylim = c(300, 1000), 
                                     comparison_y_pos = 875,
                                     objective_measure_label = 'Sedentary time (min)', 
                                     comparison_method = 'wilcox.test',
                                     strip_position = 'bottom',
                                     subtitle_label = 'Sedentary time'),
  
  plot_boxplots_splits_combo_updated(dataframe = data_fig2,
                                     objective_measure = 'TotalRLMsCount', 
                                     ylim =  c(1400, 23000),  
                                     comparison_y_pos = 20000,
                                     objective_measure_label = 'Step count', 
                                     comparison_method = 'wilcox.test',
                                     strip_position = 'bottom',
                                     subtitle_label = 'Step count'),
  
  plot_boxplots_splits_combo_updated(dataframe = data_fig2,
                                     objective_measure = 'light_min', 
                                     ylim = c(103, 700), 
                                     comparison_y_pos = 600,
                                     objective_measure_label = 'Time in light activity (min)', 
                                     comparison_method = 't.test',
                                     strip_position = 'bottom',
                                     subtitle_label = 'Time in light activity'),
  
  plot_boxplots_splits_combo_updated(dataframe = data_fig2,
                                     objective_measure = 'mvpa_min',
                                     ylim = c(-2.2, 4.0), 
                                     comparison_y_pos = 3.0,
                                     objective_measure_label = 'Time in moderate to-vigorous\nactivity (min), RIN', 
                                     comparison_method = 'wilcox.test',
                                     strip_position = 'bottom',
                                     subtitle_label = 'Time in moderate-to-\nvigorous activity'),
  
  plot_boxplots_splits_combo_updated(dataframe = data_fig2,
                                     objective_measure = '`TimeInRLMsBouts>=1m`', 
                                     ylim = c(0.2, 140), 
                                     comparison_y_pos = 122,
                                     objective_measure_label = 'Time in stepping bouts\n>=1min (min)', 
                                     comparison_method = 'wilcox.test',
                                     strip_position = 'bottom',
                                     subtitle = 'Time in stepping bouts\n>=1min'),
          
  plot_boxplots_splits_combo_updated(dataframe = data_fig2,
                                     objective_measure = '`WeightedMedianCadence>=1m`', 
                                     ylim = c(56, 150), 
                                     comparison_y_pos = 135,
                                     objective_measure_label = 'Cadence in stepping bouts\n>=1min (steps/min)', 
                                     comparison_method = 't.test',
                                     strip_position = 'bottom',
                                     subtitle = 'Cadence in stepping bouts\n>=1min'),
  
  plot_boxplots_splits_combo_updated(dataframe = data_fig2,
                                     objective_measure = 'PeakRLMs30s', 
                                     ylim = c(42, 100), 
                                     comparison_y_pos = 91,
                                     objective_measure_label = 'Peak 30s cadence (steps/min)', 
                                     comparison_method = 'wilcox.test',
                                     strip_position = 'bottom',
                                     subtitle = 'Peak 30s cadence'),
  
  legend,
  
  nrow = 4, ncol = 2,
  align = 'hv'
  
)

# save figure as SVG
ggsave(here('figures', 'figure2.svg'),
       figure2,
       device = 'svg', bg = 'white',
       height = 16, width = 10)


# perform pairwise comparisons of splits ----------------------------------

# note: welch's t-tests used for comparisons of measures that did not exhibit deviations from normality,
# whereas mann-whitney u-tests used for comparisons of measures that did exhibit deviations from normality

# note: p-values are adjusted with Holm's method (default) for comparisons of self-reported measures
# because there are only 2 levels ofvo2 split, no adjustment is performed for those comparisons

### load rstatix, required for testing with data in long form
packages <- c('rstatix')

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
    }
  }
)


### example for sedentary time
comparison_sedtime_factg <- data_fig2 %>%
  filter(!is.na(level),
         variable == 'FACT-G Total') %>%
  group_by(1) %>%
  wilcox_test(`TotalSedentaryTime.m.` ~ level) %>%
  adjust_pvalue()

comparison_sedtime_factgphys <- data_fig2 %>%
  filter(!is.na(level),
         variable == 'FACT-G Physical') %>%
  group_by(1) %>%
  wilcox_test(`TotalSedentaryTime.m.` ~ level) %>%
  adjust_pvalue()

comparison_sedtime_factgphys5sub <- data_fig2 %>%
  filter(!is.na(level),
         variable == 'FACT-G Physical\n5-item') %>%
  group_by(1) %>%
  wilcox_test(`TotalSedentaryTime.m.` ~ level) %>%
  adjust_pvalue()

comparison_sedtime_promispf <- data_fig2 %>%
  filter(!is.na(level),
         variable == 'Linked\nPROMIS-PF') %>%
  group_by(1) %>%
  wilcox_test(`TotalSedentaryTime.m.` ~ level) %>%
  adjust_pvalue()

comparison_sedtime_vo2 <- data_fig2 %>%
  filter(!is.na(level),
         variable == 'Submaximal\nVO2') %>%
  group_by(1) %>%
  wilcox_test(`TotalSedentaryTime.m.` ~ level)

### repeat same process for each other measure of real-world physical behavior