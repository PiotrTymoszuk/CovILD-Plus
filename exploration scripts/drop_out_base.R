# Comparing baseline and acute COVID-19 features between the participants 
# missing the one-year follow-up visit and those with the complete year visit 
# and hence included in the analysis

  insert_head()
  
# container -----
  
  drop_base <- list()
  
# globals -----
  
  insert_msg('Globals')
  
  ## variables of interest
  
  drop_base$variables <- c('age', 
                          'sex', 
                          'cat_WHO', 
                          'weight_class', 
                          'smoking', 
                          'no_comorb', 
                          'comorb_present', 
                          'cardiovascular_comorb', 
                          'hypertension_comorb',
                          'pulmonary_comorb', 
                          'copd_comorb', 
                          'asthma_comorb', 
                          'intenst_lung_comorb', 
                          'endometabolic_comorb', 
                          'hyperchol_comorb', 
                          'diabetes_comorb', 
                          'ckd_comorb', 
                          'gastro_comorb', 
                          'cldis_comorb')
  
  ## analysis table
  
  drop_base$analysis_tbl <- cov_data$raw_data %>% 
    filter(time == 0) %>% 
    mutate(included = ifelse(ID %in% cov_data$year_complete, 'yes', 'no'), 
           included = factor(included, c('no', 'yes')), 
           weight_class = factor(weight_class, 
                                 c('normal', 'overweight', 'obesity')), 
           cat_WHO = car::recode(cat_WHO, 
                                 "'A' = 'Ambulatory'; 
                                 'HM' = 'Moderate'; 
                                 'HS' = 'Severe'"), 
           cat_WHO = factor(cat_WHO, c('Ambulatory', 'Moderate', 'Severe'))) %>% 
    select(ID, included, all_of(drop_base$variables))
  
  ## statistical test type, plot type
  
  drop_base$var_lexicon <- 
    tibble(variable = drop_base$variables, 
           eff_type = map_lgl(drop_base$analysis_tbl[drop_base$variables], 
                              is.factor), 
           plot_type = map_lgl(drop_base$analysis_tbl[drop_base$variables], 
                               is.factor)) %>% 
    mutate(eff_type = ifelse(eff_type, 'cramer_v', 'wilcoxon_r'), 
           plot_type = ifelse(plot_type, 'stack', 'violin')) %>% 
    mutate(y_lab = ifelse(plot_type == 'violin', 
                          translate_var(variable, out_value = 'axis_lab'), 
                          '% of strata'), 
           plot_title = translate_var(variable))
  
# descriptive stats -----
  
  insert_msg('Descriptive stats')
  
  drop_base$desc_stats <- explore(drop_base$analysis_tbl, 
                                 split_factor = 'included', 
                                 variables = drop_base$variables, 
                                 what = 'table', 
                                 pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', 'excluded', 'included'))
  
# Testing for differences -----
  
  insert_msg('Testing')
  
  drop_base$test_results <- 
    compare_variables(drop_base$analysis_tbl, 
                      variables = drop_base$variables, 
                      split_factor = 'included', 
                      what = 'eff_size', 
                      types = drop_base$var_lexicon$eff_type, 
                      ci = FALSE, 
                      pub_styled = TRUE, 
                      adj_method = 'BH') %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '))
  
# Plotting -----
  
  insert_msg('Plotting')
  
  drop_base$plots <- 
    list(variable = drop_base$var_lexicon$variable, 
         type = drop_base$var_lexicon$plot_type, 
         plot_title = drop_base$var_lexicon$plot_title, 
         plot_subtitle = drop_base$test_results$plot_cap, 
         y_lab = drop_base$var_lexicon$y_lab) %>% 
    pmap(plot_variable,  
         drop_base$analysis_tbl, 
         split_factor = 'included', 
         scale = 'percent', 
         cust_theme = globals$common_theme, 
         x_lab = 'Complete year visit') %>% 
    map(~.x + 
          labs(tag = .x$labels$tag %>% 
                 stri_replace_all(fixed = '\n', replacement = ', ') %>% 
                 paste0('\n', .))) %>% 
    set_names(drop_base$variables)
  
  ## adjustment of the numeric variable plots
  
  drop_base$plots[c('age', 'no_comorb')] <- 
    drop_base$plots[c('age', 'no_comorb')] %>% 
    map(~.x + 
          scale_fill_manual(values = c('steelblue', 'darkolivegreen4')) + 
          guides(fill = 'none'))
  
  ## adjustment of the categorical variable plots
  
  drop_base$plots[c('cat_WHO', 'weight_class', 'smoking')] <- 
    drop_base$plots[c('cat_WHO', 'weight_class', 'smoking')] %>% 
    map(~.x + 
          scale_fill_manual(values = unname(globals$sev_colors), 
                            name = ''))
  
  drop_base$plots[!names(drop_base$plots) %in% c('age', 'no_comorb', 'cat_WHO', 'weight_class', 'smoking')] <- 
    drop_base$plots[!names(drop_base$plots) %in% c('age', 'no_comorb', 'cat_WHO', 'weight_class', 'smoking')] %>% 
    map(~.x + scale_fill_manual(values = c('steelblue', 'coral3'), 
                                name = ''))
  
# END ------
  
  insert_tail()