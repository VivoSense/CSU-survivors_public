# define functions
# written by shelby bachman, shelby.bachman@vivosense.com


# function to score 5-item FACT-G PWB subscale ----------------------------

# (needed for linking to PROMIS PF, doi.org/10.1002/cncr.30981)

score_factg_5item_subscale <- function(gp1, gp3, gp4, gp6, gp7) {
  
  # reverse all items (4-X)
  gp1_rev <- 4-gp1
  gp3_rev <- 4-gp3
  gp4_rev <- 4-gp4
  gp6_rev <- 4-gp6
  gp7_rev <- 4-gp7
  
  # sum the reversed scores
  subscale_score = sum(gp1_rev, gp3_rev, gp4_rev, gp6_rev, gp7_rev)
  
  return(subscale_score)
}


# function to link 5-item FACT-G PWB to PROMIS-PF T -----------------------

# (linking evidence here: doi.org/10.1002/cncr.30981)
# (linking table here: https://static1.squarespace.com/static/60c7c36a1afd4a3ab90af0a6/t/60da54bda06c4f1c76f6953a/1624921277981/FACT-PWB-Appendix+B+-+Sum+Score+Conversion+Tables_FACT_PWB.pdf)

link_factg_to_promis <- function(subscale_score) {
  
  # FACT-G PWB subscale -> PROMIS PF linking table
  factg_promis_table <- data.frame(
    factg_subscale = 0:20,
    promis_pf_t = c(19, 22, 24, 26, 27, 29, 30,
                    32, 33, 35, 36, 37, 39, 40,
                    42, 44, 46, 48, 50, 54, 61),
    promis_pf_sd = c(4.2, 4.1, 4.0, 3.9, 3.9, 3.8, 3.8,
                     3.8, 3.8, 3.8, 3.8, 3.9, 3.9, 4.0,
                     4.2, 4.4, 4.6, 4.8, 5.0, 5.3, 6.7)
  )
  
  # if subscale score is NA, return NA
  # otherwise, link score
  if (is.na(subscale_score)) {
    
    return(NA)
    
  } else {
    
    # identify matching row of linking table
    promis_pf <- factg_promis_table[factg_promis_table$factg_subscale == subscale_score, 2:3]
    
    # return t-score & sd as dataframe
    return(promis_pf)
  }
  
}


# function to perform rank-based inverse normal transformation with NAs --------

# (this function takes the column, removes NAs, performs the transformations, and rebinds the data)
apply_rin_transform_withNAs <- function(dataset, col_name, col_name_transformed) {
  
  # select only ID and relevant columns
  df_subset <- dataset %>%
    dplyr::select(record_id, col_name)
  
  # remove rows where relevant column is NA
  df_subset <- df_subset[!is.na(df_subset[,col_name]),]
  
  # apply RIN transformation, creating new column
  df_subset[, col_name_transformed] <- RankNorm(pull(df_subset, col_name))
  
  # keep only ID and new column
  df_subset <- df_subset[,c('record_id', col_name_transformed)]
  
  # bind new column to original dataset,
  # joining by ID
  df_updated <- dataset %>%
    left_join(df_subset, by = 'record_id')
  
  # return updated dataframe
  message('Adding new column: ', col_name_transformed, ' to the dataset...')
  return(df_updated)
  
}


# function to create crossbar ---------------------------------------------

stat_sum_df <- function(fun, geom="crossbar", ...) {
  stat_summary(fun.data = fun, colour = "black", 
               alpha = 0.6, geom = geom, width = 0.1, ...)
}


# function to summarize a numeric variable --------------------------------

summarize_variable_numeric <- function(values, description,
                                       rounding_level_meansd,
                                       rounding_level_range) {
  ######
  ### arguments ###
  # values: variable values (vector, numeric)
  # description: description of the variable (string)
  # rounding_level_meansd: number of decimal places to round mean/SD (integer)
  # rounding_level_range: number of decimal places to round range (integer)
  ######
  
  # create mean (SD) string
  values_mean <- mean(values, na.rm = TRUE)
  values_sd <- sd(values, na.rm = TRUE)
  values_meansd <- paste(
    round(values_mean, rounding_level_meansd), 
    ' (', 
    round(values_sd, rounding_level_meansd), 
    ')'  , 
    sep = ''
  )
  
  # create range = min - max string
  values_min <- min(values, na.rm = TRUE)
  values_max <- max(values, na.rm = TRUE)
  values_range <- paste( 
    round(values_min, rounding_level_range), 
    ' - ', 
    round(values_max, rounding_level_range), 
    sep = ''
  )
  
  # create table combining mean (SD) & range
  table_values <- data.frame(
    variable = description,
    level = description,
    N_pct = NA,
    mean_SD = values_meansd,
    range = values_range
  )
  return(table_values)
  
}


# function to summarize a categorical variable ----------------------------

summarize_variable_categorical <- function(values, description, levels) {
  ######
  ### arguments ###
  # values: variable values (vector, numeric)
  # description: description of the variable (string)
  # levels: order of variable levels (vector, character)
  ######
  
  # total length of values
  length_values <- length(values)
  
  # initialize table to store results
  table_values <- data.frame()
  
  # loop over levels of variable
  for (ii in 1:length(levels)) {
    
    # if this level is NA, count NAs
    if (is.na(levels[ii])) {
      values_n <- sum(is.na(values))
    } else {
      values_n <- sum(values == levels[ii], na.rm = TRUE)
    }
    
    # create N (pct) string
    values_pct <- paste(
      round(
        (values_n / length_values)*100, 1), 
      '%', 
      sep = '')
    values_npct <- paste(
      values_n, 
      ' (', 
      values_pct, 
      ')', 
      sep = ''
    )
    
    # create new row for table
    table_values_thislevel <- data.frame(
      variable = description,
      level = levels[ii],
      N_pct = values_npct,
      mean_SD = NA,
      range = NA
    )
    
    # add to table
    table_values <- as.data.frame(
      rbind(table_values,
            table_values_thislevel)
    )
  }
  
  return(table_values)
  
}


# function to calculate ceiling effects -----------------------------------

calculate_ceiling_effects <- function(scores, max_score) {
  # this function calculates ceiling effects for a given set of scores
  # where ceiling effects are defined as X/N individuals with the max score
  
  ### arguments:
  # scores: vector containing scores for all participants
  # max_score: maximum possible score on assessment
  ### output:
  # list with 2 elements
  # the first with N participants at ceiling
  # the second with N participants total (e.g. no NA score)
  # the third with % participants at ceiling
  # the fourth containing a summary % (N) of participants at ceiling
  ###
  
  output <- vector(mode = 'list', length = 2)
  
  n_ceil <- sum(scores == max_score, na.rm = TRUE)
  n_total <- sum(!is.na(scores))
  pct_ceil <- (n_ceil / n_total)*100
  
  output[[1]] <- n_ceil
  output[[2]] <- n_total
  output[[3]] <- pct_ceil
  output[[4]] <- paste(round(pct_ceil, 1),
                       '% (N=',
                       n_ceil,
                       ')', 
                       sep = '')
  
  return(output)
}


# function for calculating binwidth using Freedman-Diaconis rule ----------

fd <- function(x, na.rm = TRUE) {
  bw <- 2 * IQR(x, na.rm = na.rm) / length(x)^(1/3)
  return(bw)
}
