# Generates summary plots of the symptom, CT, LTF and cardiological abnormality
# for the whole cohort and severity groups.

  insert_head()
  
# container list ------
  
  prev_sum <- list()
  
# globals: variables and analysis tables -------
  
  insert_msg('Variables and analysis table')

  ## variable grouping
  
  prev_sum$var_groups <- list(symptoms = c('fatigue_sympt', 
                                           'Chalder_FS_bimodal', 
                                           'sleep_sympt', 
                                           'dyspnoe_sympt', 
                                           'cough_sympt', 
                                           'night_sweat_sympt', 
                                           'gastro_sympt', 
                                           'anosmia_sympt', 
                                           'derma_sympt', 
                                           'hair_loss_sympt'), 
                              pulmo = c('FVC_red', 
                                        'FEV1_red', 
                                        'FEV1_FVC_red', 
                                        'TLC_red', 
                                        'DLCO_red'), 
                              ct = c('ct_severity_any', 
                                     'ct_severity_5'), 
                              cardio = c('diastolic_dysf', 
                                         'EF_red'), 
                              iron = c('anemia', 
                                       'FT_elv', 
                                       'TSAT_red'), 
                              inflammation = c('Ddimer_elv', 
                                               'CRP_elv', 
                                               'PCT_elv', 
                                               'IL6_elv'), 
                              psycho = c('EQ5DL_low', 
                                         'EQ5DL_mobility_bi', 
                                         'EQ5DL_selfcare_bi', 
                                         'EQ5DL_activities_bi', 
                                         'EQ5DL_pain_bi', 
                                         'EQ5DL_anxiety_bi', 
                                         'Stress_hi'))
  
  ## analysis table
  
  prev_sum$analysis_tbl <- cov_data$data_tbl %>% 
    filter(time == 4) %>% 
    select('ID', 'cat_WHO', all_of(unname(unlist(prev_sum$var_groups))))
  
# descriptive stats -------
  
  insert_msg('Descriptive stats')
  
  prev_sum$desc_stats <- prev_sum$var_groups %>% 
    map(~explore(prev_sum$analysis_tbl[c('cat_WHO', .x)] %>% 
                   filter(complete.cases(.)), 
                 split_factor = 'cat_WHO', 
                 variables = .x, 
                 what = 'table', 
                 pub_styled = TRUE) %>% 
          reduce(left_join, by = 'variable') %>% 
          set_names(c('variable', 'A', 'HM', 'HS'))) %>% 
    map(~map_dfc(.x, stri_replace, regex = 'no:.*\\nyes:\\s{1}', replacement = '') %>% 
          map_dfc(stri_replace, fixed = '% (', replacement = '% (n = '))
    
# Testing for differences between the severity groups ------
  
  insert_msg('Testing for the differences between the severity groups')
  
  prev_sum$test_results <- prev_sum$var_groups %>% 
    map(~compare_variables(prev_sum$analysis_tbl[c('cat_WHO', .x)] %>% 
                             filter(complete.cases(.)), 
                           split_factor = 'cat_WHO', 
                           variables = .x, 
                           what = 'test', 
                           types = 'chisq_test', 
                           ci = FALSE, 
                           pub_styled = TRUE, 
                           adj_method = 'BH'))
  
# plots -------
  
  insert_msg('Bubble plots')
  
  prev_sum$plots <- list(variables = prev_sum$var_groups, 
                         plot_title = c('Symptoms, 1 year', 
                                        'LFT, 1 year', 
                                        'Chest CT, 1 year', 
                                        'TTE, 1 year', 
                                        'Iron turnover, 1 year', 
                                        'Inflammation and microvascular damage, 1 year', 
                                        'Psychosocial status, 1 year')) %>% 
    pmap(plot_prev_bubble, 
         data = prev_sum$analysis_tbl)
  
# END -----
  
  insert_tail()