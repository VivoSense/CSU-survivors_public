# summarize ceiling effects for self-reported measures
# written by shelby bachman, shelby.bachman@vivosense.com


# calculate ceiling effects for self-reported measures --------------------

ceiling_effects_factg_total <- calculate_ceiling_effects(
  scores = data_survivors$bl_qol_factg_total,
  max_score = 108
)[[4]]

ceiling_effects_factg_phys <- calculate_ceiling_effects(
  scores = data_survivors$bl_qol_physsubscale,
  max_score = 28
)[[4]]

ceiling_effects_factg_phys5item <- calculate_ceiling_effects(
  scores = data_survivors$bl_qol_physsubscale_5itemsub,
  max_score = 20
)[[4]]

ceiling_effects_promispf <- calculate_ceiling_effects(
  scores = data_survivors$promis_pf_t,
  max_score = 61
)[[4]]



# create table summarizing ceiling effects by measure ---------------------

ceiling_effects <- data.frame(
  measure = c('FACT-G Total',
              'FACT-G Physical',
              'FACT-G Physical 5-item subset',
              'Linked PROMIS-PF'),
  N_percent = c(ceiling_effects_factg_total,
                ceiling_effects_factg_phys,
                ceiling_effects_factg_phys5item,
                ceiling_effects_promispf)
)
