# Compares baseline characteristic and acute COVID-19 severity 
# in individuals treated with steroids and without such treatment

insert_head()

# container ----

  steroid_base <- list()

# globals -----

  insert_msg('Globals')

  ## variables of interest

  steroid_base$variables <- c('age', 
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

  steroid_base$analysis_tbl <- cov_data$data_tbl %>% 
    filter(time == 0) %>% 
    mutate(weight_class = factor(weight_class, 
                                 c('normal', 'overweight', 'obesity')), 
           cat_WHO = car::recode(cat_WHO, 
                                 "'A' = 'Ambulatory'; 
                                   'HM' = 'Moderate'; 
                                   'HS' = 'Severe'"), 
           cat_WHO = factor(cat_WHO, c('Ambulatory', 'Moderate', 'Severe'))) %>% 
    select(ID, treat_steroids, all_of(steroid_base$variables))
  
  ## statistical test type, plot type
  
  steroid_base$var_lexicon <- 
    tibble(variable = steroid_base$variables, 
           eff_type = map_lgl(steroid_base$analysis_tbl[steroid_base$variables], 
                              is.factor), 
           plot_type = map_lgl(steroid_base$analysis_tbl[steroid_base$variables], 
                               is.factor)) %>% 
    mutate(eff_type = ifelse(eff_type, 'cramer_v', 'wilcoxon_r'), 
           plot_type = ifelse(plot_type, 'stack', 'violin')) %>% 
    mutate(y_lab = ifelse(plot_type == 'violin', 
                          translate_var(variable, out_value = 'axis_lab'), 
                          '% of strata'), 
           plot_title = translate_var(variable))
  
# descriptive stats -----
  
  insert_msg('Descriptive stats')
  
  steroid_base$desc_stats <- explore(steroid_base$analysis_tbl, 
                                     split_factor = 'treat_steroids', 
                                     variables = steroid_base$variables, 
                                     what = 'table', 
                                     pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', 'no_steroids', 'steroids'))
  
# Testing for differences ------
  
  insert_msg('Testing')
  
  steroid_base$test_results <- 
    compare_variables(steroid_base$analysis_tbl, 
                      variables = steroid_base$variables, 
                      split_factor = 'treat_steroids', 
                      what = 'eff_size', 
                      types = steroid_base$var_lexicon$eff_type, 
                      ci = FALSE, 
                      pub_styled = TRUE, 
                      adj_method = 'BH') %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '))
  
  # Plotting -----
  
  insert_msg('Plotting')
  
  steroid_base$plots <- 
    list(variable = steroid_base$var_lexicon$variable, 
         type = steroid_base$var_lexicon$plot_type, 
         plot_title = steroid_base$var_lexicon$plot_title, 
         plot_subtitle = steroid_base$test_results$plot_cap, 
         y_lab = steroid_base$var_lexicon$y_lab) %>% 
    pmap(plot_variable,  
         steroid_base$analysis_tbl, 
         split_factor = 'treat_steroids', 
         scale = 'percent', 
         cust_theme = globals$common_theme, 
         x_lab = 'Steroid treatment') %>% 
    map(~.x + 
          labs(tag = .x$labels$tag %>% 
                 stri_replace_all(fixed = '\n', replacement = ', ') %>% 
                 paste0('\n', .))) %>% 
    set_names(steroid_base$variables)
  
  ## adjustment of the numeric variable plots
  
  steroid_base$plots[c('age', 'no_comorb')] <- 
    steroid_base$plots[c('age', 'no_comorb')] %>% 
    map(~.x + 
          scale_fill_manual(values = c('steelblue', 'indianred3')) + 
          guides(fill = 'none'))
  
  ## adjustment of the categorical variable plots
  
  steroid_base$plots[c('cat_WHO', 'weight_class', 'smoking')] <- 
    steroid_base$plots[c('cat_WHO', 'weight_class', 'smoking')] %>% 
    map(~.x + 
          scale_fill_manual(values = unname(globals$sev_colors), 
                            name = ''))
  
  steroid_base$plots[!names(steroid_base$plots) %in% c('age', 'no_comorb', 'cat_WHO', 'weight_class', 'smoking')] <- 
    steroid_base$plots[!names(steroid_base$plots) %in% c('age', 'no_comorb', 'cat_WHO', 'weight_class', 'smoking')] %>% 
    map(~.x + scale_fill_manual(values = c('steelblue', 'coral3'), 
                                name = ''))
  
# END -----
  
  insert_tail()