# perform tertile and median splits of relevant variables
# written by shelby bachman, shelby.bachman@vivosense.com


# add column indicating row number (alt ID for visualizing splits) --------

data_survivors$record_id_alt <- 1:nrow(data_survivors)


# split based on FACT-G ---------------------------------------------------

# tertile split of FACT-G total
data_survivors$tertile_factg <- ntile(data_survivors$bl_qol_factg_total, 3)  
data_survivors$tertile_factg <- factor(data_survivors$tertile_factg, levels = c(1, 2, 3),
                                         labels = c('Low', 'Medium', 'High'))

# calculate tertiles
tertiles_factg <- as.numeric(quantile(data_survivors$bl_qol_factg_total, c(0:3/3), na.rm = TRUE))

# redo split to account for values equal to tertiles
data_survivors <- data_survivors %>%
  mutate(tertile_factg = case_when(
    bl_qol_factg_total < 86 ~ 'Low',
    bl_qol_factg_total >= 86 & bl_qol_factg_total < 94 ~ 'Medium',
    bl_qol_factg_total >= 94 ~ 'High'
  ))

# make tertile split variable a factor
data_survivors$tertile_factg <- factor(data_survivors$tertile_factg, 
                                           levels = c('Low', 'Medium', 'High'))

# visualize split
p_split_factg <- ggplot(data = data_survivors %>% filter(!is.na(tertile_factg)),
       aes(x = bl_qol_factg_total,
           y = record_id_alt,
           colour = factor(tertile_factg),
           fill = factor(tertile_factg))) +
  geom_segment(aes(x = tertiles_factg[2], xend = tertiles_factg[2], 
                   y = 0, yend = 100), 
               colour = 'darkgray', lty = 'dashed') +
  geom_segment(aes(x = tertiles_factg[3], xend = tertiles_factg[3], 
                   y = 0, yend = 100), 
               colour = 'darkgray', lty = 'dashed') +
  geom_point(alpha = 0.7, pch = 21, colour = 'black') +
  annotate(geom = 'text', x = 80, y = 104, 
           label = 'N = 28', colour = '#66C2A5', size = 4, fontface = 'bold') +
  annotate(geom = 'text', x = 90, y = 104, 
           label = 'N = 28', colour = '#FC8D62', size = 4, fontface = 'bold') +
  annotate(geom = 'text', x = 100, y = 104, 
           label = 'N = 29', colour = '#8DA0CB', size = 4, fontface = 'bold') +
  scale_colour_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  scale_x_continuous(limits = c(40, 108)) +
  coord_cartesian(ylim = c(0, 105)) +
  labs(x = 'FACT-G total well-being score (possible range: 0-108)',
       y = '',
       colour = 'Tertile split of FACT-G total',
       fill = 'Tertile split of FACT-G total') +
  theme_pubr()


# split based on FACT-G physical ------------------------------------------

# tertile split of FACT-G physical
data_survivors$tertile_factg_phys <- ntile(data_survivors$bl_qol_physsubscale, 3)  
data_survivors$tertile_factg_phys <- factor(data_survivors$tertile_factg_phys, levels = c(1, 2, 3),
                                      labels = c('Low', 'Medium', 'High'))

# calculate tertiles
tertiles_factg_phys <- as.numeric(quantile(data_survivors$bl_qol_physsubscale, c(0:3/3), na.rm = TRUE))

# redo split to account for values equal to tertiles
data_survivors <- data_survivors %>%
  mutate(tertile_factg_phys = case_when(
    bl_qol_physsubscale < 25 ~ 'Low',
    bl_qol_physsubscale >= 25 & bl_qol_physsubscale < 27 ~ 'Medium',
    bl_qol_physsubscale >= 27 ~ 'High'
  ))

# make tertile split variable a factor
data_survivors$tertile_factg_phys <- factor(data_survivors$tertile_factg_phys, 
                                   levels = c('Low', 'Medium', 'High'))

# visualize split
p_split_factg_phys <- ggplot(data = data_survivors %>% filter(!is.na(tertile_factg_phys)),
                        aes(x = bl_qol_physsubscale,
                            y = record_id_alt,
                            colour = factor(tertile_factg_phys),
                            fill = factor(tertile_factg_phys))) +
  geom_segment(aes(x = tertiles_factg_phys[2], xend = tertiles_factg_phys[2], 
                   y = 0, yend = 100), 
               colour = 'darkgray', lty = 'dashed') +
  geom_segment(aes(x = tertiles_factg_phys[3], xend = tertiles_factg_phys[3], 
                   y = 0, yend = 100), 
               colour = 'darkgray', lty = 'dashed') +
  geom_point(alpha = 0.7, pch = 21, colour = 'black') +
  annotate(geom = 'text', x = 20, y = 103, 
           label = '< 25\nN = 28', colour = '#66C2A5', size = 4, fontface = 'bold') +
  annotate(geom = 'text', x = 26, y = 103, 
           label = '>= 25 & < 27\nN = 27', colour = '#FC8D62', size = 4, fontface = 'bold') +
  annotate(geom = 'text', x = 32, y = 103, 
           label = '>= 27\nN = 30', colour = '#8DA0CB', size = 4, fontface = 'bold') +
  scale_colour_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  scale_x_continuous(limits = c(10, 32)) +
  coord_cartesian(ylim = c(0, 105)) +
  labs(x = 'FACT-G physical well-being score (possible range: 0-28)',
       y = '',
       colour = 'Tertile split of FACT-G physical',
       fill = 'Tertile split of FACT-G physical') +
  theme_pubr()


# split based on FACT-G physical 5-item subset --------------------------

# tertile split of FACT-G physical
data_survivors$tertile_factg_phys5item <- ntile(data_survivors$bl_qol_physsubscale_5itemsub, 3)  
data_survivors$tertile_factg_phys5item <- factor(data_survivors$bl_qol_physsubscale_5itemsub, levels = c(1, 2, 3),
                                            labels = c('Low', 'Medium', 'High'))

# calculate tertiles
tertiles_factg_phys5item <- as.numeric(quantile(data_survivors$bl_qol_physsubscale_5itemsub, c(0:3/3), na.rm = TRUE))

# redo split to account for values equal to tertiles
data_survivors <- data_survivors %>%
  mutate(tertile_factg_phys5item = case_when(
    bl_qol_physsubscale_5itemsub < 18 ~ 'Low',
    bl_qol_physsubscale_5itemsub >= 18 & bl_qol_physsubscale_5itemsub < 20 ~ 'Medium',
    bl_qol_physsubscale_5itemsub >= 20 ~ 'High'
  ))

# make tertile split variable a factor
data_survivors$tertile_factg_phys5item <- factor(data_survivors$tertile_factg_phys5item, 
                                            levels = c('Low', 'Medium', 'High'))

# visualize split
p_split_factg_phys5item <- ggplot(data = data_survivors %>% filter(!is.na(tertile_factg_phys5item)),
                             aes(x = bl_qol_physsubscale_5itemsub,
                                 y = record_id_alt,
                                 colour = factor(tertile_factg_phys5item),
                                 fill = factor(tertile_factg_phys5item))) +
  geom_segment(aes(x = tertiles_factg_phys5item[2], xend = tertiles_factg_phys5item[2], 
                   y = 0, yend = 100), 
               colour = 'darkgray', lty = 'dashed') +
  geom_segment(aes(x = tertiles_factg_phys5item[3], xend = tertiles_factg_phys5item[3], 
                   y = 0, yend = 100), 
               colour = 'darkgray', lty = 'dashed') +
  geom_point(alpha = 0.7, pch = 21, colour = 'black') +
  annotate(geom = 'text', x = 16, y = 103, 
           label = '< 18\nN = 34', colour = '#66C2A5', size = 4, fontface = 'bold') +
  annotate(geom = 'text', x = 18, y = 103, 
           label = '>= 18 & < 20\nN = 32', colour = '#FC8D62', size = 4, fontface = 'bold') +
  annotate(geom = 'text', x = 20, y = 103, 
           label = '>= 20\nN = 19', colour = '#8DA0CB', size = 4, fontface = 'bold') +
  scale_colour_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  scale_x_continuous(limits = c(10, 23)) +
  coord_cartesian(ylim = c(0, 105)) +
  labs(x = 'FACT-G physical well-being 5-item subset (possible range: 0-20)',
       y = '',
       colour = 'Tertile split of FACT-G physical 5-item subset',
       fill = 'Tertile split of FACT-G physical 5-item subset') +
  theme_pubr()


# split based on PROMIS-PF ------------------------------------------------

# tertile split of PROMIS-PF
data_survivors$tertile_promispf <- ntile(data_survivors$promis_pf_t, 3)  
data_survivors$tertile_promispf <- factor(data_survivors$tertile_promispf, levels = c(1, 2, 3),
                                         labels = c('Low', 'Medium', 'High'))

# calculate tertiles
tertiles_promispf <- as.numeric(quantile(data_survivors$promis_pf_t, c(0:3/3), na.rm = TRUE))

# redo split to account for values equal to tertiles
data_survivors <- data_survivors %>%
  mutate(tertile_pf = case_when(
    promis_pf_t <= 48 ~ 'Low',
    promis_pf_t > 48 & promis_pf_t <= 54 ~ 'Medium',
    promis_pf_t > 54 ~ 'High'
  ))

# make tertile split variable a factor
data_survivors$tertile_pf <- factor(data_survivors$tertile_pf, 
                                   levels = c('Low', 'Medium', 'High'))

# visualize new split
p_split_promispf <- ggplot(data = data_survivors %>% filter(!is.na(tertile_pf)),
                           aes(x = promis_pf_t,
                               y = record_id_alt,
                               colour = factor(tertile_pf),
                               fill = factor(tertile_pf))) +
  geom_point(alpha = 0.7, pch = 21, colour = 'black') +
  geom_segment(aes(x = 49, xend = 49, y = 0, yend = 100), 
               colour = 'darkgray', lty = 'dashed') +
  geom_segment(aes(x = 57, xend = 57, y = 0, yend = 100), 
               colour = 'darkgray', lty = 'dashed') +
  annotate(geom = 'text', x = 45, y = 104, 
           label = 'N = 34', colour = '#66C2A5', size = 4, fontface = 'bold') +
  annotate(geom = 'text', x = 53, y = 104, 
           label = 'N = 32', colour = '#FC8D62', size = 4, fontface = 'bold') +
  annotate(geom = 'text', x = 61, y = 104, 
           label = 'N = 19', colour = '#8DA0CB', size = 4, fontface = 'bold') +
  scale_colour_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  scale_x_continuous(limits = c(30, 61)) +
  coord_cartesian(ylim = c(0, 105)) +
  labs(x = 'Linked PROMIS-PF T-score (possible range: 19-61)',
       y = '',
       colour = 'Tertile split of linked PROMIS-PF',
       fill = 'Tertile split of linked PROMIS-PF') +
  theme_pubr()

# remove first split variable
data_survivors <- data_survivors %>%
  dplyr::select(-tertile_promispf)


# split based on VO2 max --------------------------------------------------

# median split of VO2 max
data_survivors$medsplit_vo2 <- ntile(data_survivors$vo2_hrr, 2)  
data_survivors$medsplit_vo2 <- factor(data_survivors$medsplit_vo2, 
                                     levels = c(1, 2),
                                     labels = c('Low', 'High'))

# calculate tertiles
splits_vo2 <- as.numeric(quantile(data_survivors$vo2_hrr, c(0:2/2), na.rm = TRUE))

# visualize split
p_split_vo2 <- ggplot(data = data_survivors %>% filter(!is.na(medsplit_vo2)),
                       aes(x = vo2_hrr,
                           y = record_id_alt,
                           colour = factor(medsplit_vo2),
                           fill = factor(medsplit_vo2))) +
  geom_segment(aes(x = splits_vo2[2], xend = splits_vo2[2], 
                   y = 0, yend = 100), 
               colour = 'darkgray', lty = 'dashed') +
  geom_point(alpha = 0.7, pch = 21, colour = 'black') +
  annotate(geom = 'text', x = 23, y = 104, 
           label = 'N = 25', colour = '#66C2A5', size = 4, fontface = 'bold') +
  annotate(geom = 'text', x = 35, y = 104, 
           label = 'N = 24', colour = '#FC8D62', size = 4, fontface = 'bold') +
  scale_colour_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  coord_cartesian(ylim = c(0, 105)) +
  labs(x = 'VO2 max (mL/kg/min)',
       y = '',
       colour = 'Median split of VO2 max',
       fill = 'Median split of VO2 max') +
  theme_pubr()
