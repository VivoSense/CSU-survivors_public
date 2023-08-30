# summarize distributions of self-reported measures
# written by shelby bachman, shelby.bachman@vivosense.com


# visualize distributions of self-reported measures -----------------------

figureS1 <- ggarrange(
  ggplot(data_survivors,
         aes(x = bl_qol_factg_total)) +
    geom_histogram(colour = 'black', 
                   fill = 'lightgray',
                   binwidth = fd(data_survivors$bl_qol_factg_total, na.rm = TRUE)) +
    annotate(geom = 'text', 
             x = 0,
             y = 20, 
             label = paste('Skewness: ', 
                           psych::skew(data_survivors$bl_qol_factg_total %>% as.matrix(), na.rm = TRUE) %>% round(2),
                           '\n',
                           'Kurtosis: ',
                           psych::kurtosi(data_survivors$bl_qol_factg_total %>% as.matrix(), na.rm = TRUE) %>% round(2),
                           sep = ''),
             hjust = 0) +
    coord_cartesian(xlim = c(0, 108),
                    ylim = c(0, 30)) +
    labs(x = 'FACT-G total well-being\n(Possible scores: 0-108)', 
         y = 'Frequency',
         subtitle = 'FACT-G total well-being') +
    theme_pubr() +
    theme(plot.subtitle = element_text(face = 'bold', hjust = 0.5)),
  
  
  ggplot(data_survivors,
         aes(x = bl_qol_physsubscale)) +
    geom_histogram(colour = 'black', 
                   fill = 'lightgray',
                   binwidth = fd(data_survivors$bl_qol_physsubscale, na.rm = TRUE)) +
    annotate(geom = 'text', 
             x = 0,
             y = 25, 
             label = paste('Skewness: ', 
                           psych::skew(data_survivors$bl_qol_physsubscale %>% as.matrix(), na.rm = TRUE) %>% round(2),
                           '\n',
                           'Kurtosis: ',
                           psych::kurtosi(data_survivors$bl_qol_physsubscale %>% as.matrix(), na.rm = TRUE) %>% round(2),
                           sep = ''),
             hjust = 0) +
    coord_cartesian(xlim = c(0, 28),
                    ylim = c(0, 30)) +
    labs(x = 'FACT-G physical well-being\n(Possible scores: 0-28)', 
         y = 'Frequency',
         subtitle = 'FACT-G physical well-being') +
    theme_pubr() +
    theme(plot.subtitle = element_text(face = 'bold', hjust = 0.5)),
  
  
  ggplot(data_survivors,
         aes(x = bl_qol_physsubscale_5itemsub)) +
    geom_histogram(colour = 'black', 
                   fill = 'lightgray',
                   binwidth = fd(data_survivors$bl_qol_physsubscale_5itemsub, na.rm = TRUE)) +
    annotate(geom = 'text', 
             x = 0,
             y = 25, 
             label = paste('Skewness: ', 
                           psych::skew(data_survivors$bl_qol_physsubscale_5itemsub %>% as.matrix(), na.rm = TRUE) %>% round(2),
                           '\n',
                           'Kurtosis: ',
                           psych::kurtosi(data_survivors$bl_qol_physsubscale_5itemsub %>% as.matrix(), na.rm = TRUE) %>% round(2),
                           sep = ''),
             hjust = 0) +
    coord_cartesian(xlim = c(0, 21),
                    ylim = c(0, 30)) +
    labs(x = 'FACT-G physical well-being 5-item subset\n(Possible scores: 0-20)', 
         y = 'Frequency',
         subtitle = 'FACT-G physical well-being\n5-item subset') +
    theme_pubr() +
    theme(plot.subtitle = element_text(face = 'bold', hjust = 0.5)),
  
  
  ggplot(data_survivors,
         aes(x = promis_pf_t)) +
    geom_histogram(colour = 'black', 
                   fill = 'lightgray',
                   binwidth = fd(data_survivors$promis_pf_t, na.rm = TRUE)) +
    annotate(geom = 'text', 
             x = 19,
             y = 25, 
             label = paste('Skewness: ', 
                           psych::skew(data_survivors$promis_pf_t %>% as.matrix(), na.rm = TRUE) %>% round(2),
                           '\n',
                           'Kurtosis: ',
                           psych::kurtosi(data_survivors$promis_pf_t %>% as.matrix(), na.rm = TRUE) %>% round(2),
                           sep = ''),
             hjust = 0) +
    coord_cartesian(xlim = c(19, 62),
                    ylim = c(0, 30)) +
    labs(x = 'Linked PROMIS-PF T-scores\n(Possible scores: 19-61)', 
         y = 'Frequency',
         subtitle = 'Linked PROMIS-PF') +
    theme_pubr() +
    theme(plot.subtitle = element_text(face = 'bold', hjust = 0.5)),
  
  ggplot(data_survivors,
         aes(x = vo2_hrr)) +
    geom_histogram(colour = 'black', 
                   fill = 'lightgray',
                   binwidth = fd(data_survivors$vo2_hrr, na.rm = TRUE)) +
    annotate(geom = 'text', 
             x = 8,
             y = 25, 
             label = paste('Skewness: ', 
                           psych::skew(data_survivors$vo2_hrr %>% as.matrix(), na.rm = TRUE) %>% round(2),
                           '\n',
                           'Kurtosis: ',
                           psych::kurtosi(data_survivors$vo2_hrr %>% as.matrix(), na.rm = TRUE) %>% round(2),
                           sep = ''),
             hjust = 0) +
    coord_cartesian(xlim = c(8, 50),
                    ylim = c(0, 30)) +
    labs(x = 'Predicted submaximal VO2\n(mL/kg/min)', 
         y = 'Frequency',
         subtitle = 'Submaximal VO2') +
    theme_pubr() +
    theme(plot.subtitle = element_text(face = 'bold', hjust = 0.5)),
  
  nrow = 2, ncol = 4, align = 'hv'
  
)

# save figure
ggsave(here('figures', 'figureS1.svg'),
       figureS1,
       device = 'svg', bg = 'white', dpi = 600,
       width = 13, height = 7)


# visualize distributions of physical behavior measures -------------------

figureS2 <- ggarrange(
  ggplot(data_survivors,
         aes(x = TotalSedentaryTime.m.)) +
    geom_histogram(colour = 'black', 
                   fill = 'lightgray',
                   binwidth = fd(data_survivors$TotalSedentaryTime.m., na.rm = TRUE)) +
    annotate(geom = 'text', 
             x = 250,
             y = 25, 
             label = paste('Skewness: ', 
                           psych::skew(data_survivors$TotalSedentaryTime.m. %>% as.matrix(), na.rm = TRUE) %>% round(2),
                           '\n',
                           'Kurtosis: ',
                           psych::kurtosi(data_survivors$TotalSedentaryTime.m. %>% as.matrix(), na.rm = TRUE) %>% round(2),
                           sep = ''),
             hjust = 0) +
    coord_cartesian(xlim = c(250, 900),
                    ylim = c(0, 30)) +
    labs(x = 'Sedentary time (min)', 
         y = 'Frequency',
         subtitle = 'Sedentary time') +
    theme_pubr() +
    theme(plot.subtitle = element_text(face = 'bold', hjust = 0.5)),
  
  
  ggplot(data_survivors,
         aes(x = TotalRLMsCount)) +
    geom_histogram(colour = 'black', 
                   fill = 'lightgray',
                   binwidth = fd(data_survivors$TotalRLMsCount, na.rm = TRUE)) +
    annotate(geom = 'text', 
             x = 1200,
             y = 25, 
             label = paste('Skewness: ', 
                           psych::skew(data_survivors$TotalRLMsCount %>% as.matrix(), na.rm = TRUE) %>% round(2),
                           '\n',
                           'Kurtosis: ',
                           psych::kurtosi(data_survivors$TotalRLMsCount %>% as.matrix(), na.rm = TRUE) %>% round(2),
                           sep = ''),
             hjust = 0) +
    coord_cartesian(xlim = c(1200, 18000),
                    ylim = c(0, 30)) +
    labs(x = 'Step count', 
         y = 'Frequency',
         subtitle = 'Step count') +
    theme_pubr() +
    theme(plot.subtitle = element_text(face = 'bold', hjust = 0.5)),
  
  
  ggplot(data_survivors,
         aes(x = light_min)) +
    geom_histogram(colour = 'black', 
                   fill = 'lightgray',
                   binwidth = fd(data_survivors$light_min, na.rm = TRUE)) +
    annotate(geom = 'text', 
             x = 100,
             y = 25, 
             label = paste('Skewness: ', 
                           psych::skew(data_survivors$light_min %>% as.matrix(), na.rm = TRUE) %>% round(2),
                           '\n',
                           'Kurtosis: ',
                           psych::kurtosi(data_survivors$light_min %>% as.matrix(), na.rm = TRUE) %>% round(2),
                           sep = ''),
             hjust = 0) +
    coord_cartesian(xlim = c(100, 600),
                    ylim = c(0, 30)) +
    labs(x = 'Time in light activity\n(min)', 
         y = 'Frequency',
         subtitle = 'Time in light activity') +
    theme_pubr() +
    theme(plot.subtitle = element_text(face = 'bold', hjust = 0.5)),
  
  
  ggplot(data_survivors,
         aes(x = mvpa_min)) +
    geom_histogram(colour = 'black', 
                   fill = 'lightgray',
                   binwidth = fd(data_survivors$mvpa_min, na.rm = TRUE)) +
    annotate(geom = 'text', 
             x = 20,
             y = 25, 
             label = paste('Skewness: ', 
                           psych::skew(data_survivors$mvpa_min %>% as.matrix(), na.rm = TRUE) %>% round(2),
                           '\n',
                           'Kurtosis: ',
                           psych::kurtosi(data_survivors$mvpa_min %>% as.matrix(), na.rm = TRUE) %>% round(2),
                           sep = ''),
             hjust = 0) +
    coord_cartesian(xlim = c(0, 60),
                    ylim = c(0, 30)) +
    labs(x = 'Time in moderate-to-\nvigorous activity (min)', 
         y = 'Frequency',
         subtitle = 'Time in moderate-to-\nvigorous activity') +
    theme_pubr() +
    theme(plot.subtitle = element_text(face = 'bold', hjust = 0.5)),
  
  
  ggplot(data_survivors,
         aes(x = `TimeInRLMsBouts>=1m`)) +
    geom_histogram(colour = 'black', 
                   fill = 'lightgray',
                   binwidth = fd(data_survivors$`TimeInRLMsBouts>=1m`, na.rm = TRUE)) +
    annotate(geom = 'text', 
             x = 40,
             y = 25, 
             label = paste('Skewness: ', 
                           psych::skew(data_survivors$`TimeInRLMsBouts>=1m` %>% as.matrix(), na.rm = TRUE) %>% round(2),
                           '\n',
                           'Kurtosis: ',
                           psych::kurtosi(data_survivors$`TimeInRLMsBouts>=1m` %>% as.matrix(), na.rm = TRUE) %>% round(2),
                           sep = ''),
             hjust = 0) +
    coord_cartesian(xlim = c(0, 110),
                    ylim = c(0, 30)) +
    labs(x = 'Time in stepping bouts >=1min\n(min)', 
         y = 'Frequency',
         subtitle = 'Time in stepping bouts\n>=1min') +
    theme_pubr() +
    theme(plot.subtitle = element_text(face = 'bold', hjust = 0.5)),
  
  
  ggplot(data_survivors,
         aes(x = `WeightedMedianCadence>=1m`)) +
    geom_histogram(colour = 'black', 
                   fill = 'lightgray',
                   binwidth = fd(data_survivors$`WeightedMedianCadence>=1m`, na.rm = TRUE)) +
    annotate(geom = 'text', 
             x = 50,
             y = 25, 
             label = paste('Skewness: ', 
                           psych::skew(data_survivors$`WeightedMedianCadence>=1m` %>% as.matrix(), na.rm = TRUE) %>% round(2),
                           '\n',
                           'Kurtosis: ',
                           psych::kurtosi(data_survivors$`WeightedMedianCadence>=1m` %>% as.matrix(), na.rm = TRUE) %>% round(2),
                           sep = ''),
             hjust = 0) +
    coord_cartesian(xlim = c(50, 130),
                    ylim = c(0, 30)) +
    labs(x = 'Cadence in stepping bouts >=1min\n(steps/min)', 
         y = 'Frequency',
         subtitle = 'Cadence in stepping bouts\n>=1min') +
    theme_pubr() +
    theme(plot.subtitle = element_text(face = 'bold', hjust = 0.5)),
  
  
  ggplot(data_survivors,
         aes(x = PeakRLMs30s)) +
    geom_histogram(colour = 'black', 
                   fill = 'lightgray',
                   binwidth = fd(data_survivors$PeakRLMs30s, na.rm = TRUE)) +
    annotate(geom = 'text', 
             x = 40,
             y = 25, 
             label = paste('Skewness: ', 
                           psych::skew(data_survivors$PeakRLMs30s %>% as.matrix(), na.rm = TRUE) %>% round(2),
                           '\n',
                           'Kurtosis: ',
                           psych::kurtosi(data_survivors$PeakRLMs30s %>% as.matrix(), na.rm = TRUE) %>% round(2),
                           sep = ''),
             hjust = 0) +
    coord_cartesian(xlim = c(40, 90),
                    ylim = c(0, 30)) +
    labs(x = 'Peak 30s cadence\n(steps/min)', 
         y = 'Frequency',
         subtitle = 'Peak 30s cadence') +
    theme_pubr() +
    theme(plot.subtitle = element_text(face = 'bold', hjust = 0.5)),
  
  nrow = 2, ncol = 4,
  align = 'hv'
  
)

# save figure
ggsave(here('figures', 'figureS2.svg'),
       figureS2,
       device = 'svg', bg = 'white', dpi = 600,
       width = 13, height = 7)
