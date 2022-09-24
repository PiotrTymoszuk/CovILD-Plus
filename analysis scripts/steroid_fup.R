# Two months and one-year outcomes in participants 
# with and without steroid treatment

  insert_head()
  
# container -----
  
  steroid_fup <- list()
  
# Globals ------
  
  insert_msg('Variables and analysis table')
  
  ## variable grouping
  
  steroid_fup$var_groups <- list(symptoms = c('fatigue_sympt', 
                                              'Chalder_FS_bimodal', 
                                              'sleep_sympt', 
                                              'dyspnoe_sympt', 
                                              'cough_sympt', 
                                              'night_sweat_sympt', 
                                              'gastro_sympt', 
                                              'anosmia_sympt', 
                                              'derma_sympt', 
                                              'hair_loss_sympt'), 
                                 pulmo = c('lufo_red', 
                                           'FVC_red', 
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
  
  ## analysis tables
  
  steroid_fup$analysis_tbl <- cov_data$data_tbl %>% 
    mutate(cat_WHO = car::recode(cat_WHO, 
                                 "'HM' = 'Moderate'; 
                                 'HS' = 'Severe'"), 
           cat_WHO = factor(cat_WHO, c('Moderate', 'Severe')), 
           treat_steroids = car::recode(treat_steroids, 
                                        "'no' = 'no steroids'; 
                                        'yes' = 'steroids'"), 
           treat_steroids = factor(treat_steroids, c('no steroids', 'steroids'))) %>% 
    select(ID, time, treat_steroids, cat_WHO, 
           all_of(unname(unlist(steroid_fup$var_groups))))
  
  steroid_fup$analysis_tbl <- list(fup_2_month = 1, 
                                   fup_year = 4) %>%
    map(~filter(steroid_fup$analysis_tbl, 
                time == .x))
  
# descriptive stats -------
  
  insert_msg('Descriptive stats')
  
  ## two month descriptive stats: psych. features absent
  
  steroid_fup$stats_2_month <- c(moderate = 'Moderate', 
                                 severe = 'Severe') %>% 
    map(function(sev) c(list(symptoms = c('fatigue_sympt', 
                                          'sleep_sympt', 
                                          'dyspnoe_sympt', 
                                          'cough_sympt', 
                                          'night_sweat_sympt', 
                                          'gastro_sympt', 
                                          'anosmia_sympt')), 
                        steroid_fup$var_groups[c('pulmo', 'ct', 'cardio', 'iron', 'inflammation')]) %>% 
          map(~explore(steroid_fup$analysis_tbl$fup_2_month[c('treat_steroids', 'cat_WHO', .x)] %>% 
                         filter(complete.cases(.), 
                                cat_WHO == sev), 
                       split_factor = 'treat_steroids', 
                       variables = .x, 
                       what = 'table', 
                       pub_styled = TRUE) %>% 
                reduce(left_join, by = 'variable') %>% 
                set_names(c('variable', 'no_steroids', 'steroids'))))
  
  ## one-year descriptive stats

  steroid_fup$stats_year <- c(moderate = 'Moderate', 
                              severe = 'Severe') %>% 
    map(function(sev) steroid_fup$var_groups %>% 
          map(~explore(steroid_fup$analysis_tbl$fup_year[c('treat_steroids', 'cat_WHO', .x)] %>% 
                         filter(complete.cases(.), 
                                cat_WHO == sev), 
                       split_factor = 'treat_steroids', 
                       variables = .x, 
                       what = 'table', 
                       pub_styled = TRUE) %>% 
                reduce(left_join, by = 'variable') %>% 
                set_names(c('variable', 'no_steroids', 'steroids'))))
  
# Testing ------
  
  insert_msg('Testing for the differences between the steroid treatment groups')
  
  ## two month follow-up
  
  steroid_fup$test_2_month <- c(moderate = 'Moderate', 
                                severe = 'Severe') %>% 
    map(function(sev) c(list(symptoms = c('fatigue_sympt', 
                                          'sleep_sympt', 
                                          'dyspnoe_sympt', 
                                          'cough_sympt', 
                                          'night_sweat_sympt', 
                                          'gastro_sympt', 
                                          'anosmia_sympt')), 
                        steroid_fup$var_groups[c('pulmo', 'ct', 'cardio', 'iron', 'inflammation')]) %>% 
          map(~compare_variables(steroid_fup$analysis_tbl$fup_2_month[c('treat_steroids', 'cat_WHO', .x)] %>% 
                                   filter(complete.cases(.), 
                                          cat_WHO == sev), 
                                 split_factor = 'treat_steroids', 
                                 variables = .x, 
                                 what = 'eff_size',
                                 types = 'cramer_v', 
                                 ci = FALSE, 
                                 pub_styled = TRUE, 
                                 adj_method = 'BH')))
  
  ## year follow-up
  
  steroid_fup$test_year <- c(moderate = 'Moderate', 
                             severe = 'Severe') %>% 
    map(function(sev) steroid_fup$var_groups %>% 
          map(~compare_variables(steroid_fup$analysis_tbl$fup_year[c('treat_steroids', 'cat_WHO', .x)] %>% 
                         filter(complete.cases(.), 
                                cat_WHO == sev), 
                       split_factor = 'treat_steroids', 
                       variables = .x, 
                       what = 'eff_size',
                       types = 'cramer_v', 
                       ci = FALSE, 
                       adj_method = 'BH', 
                       pub_styled = TRUE)))
  
# N numbers to be presented in the plots ------
  
  insert_msg('N numbers')
  
  steroid_fup$n_2_month <- c(moderate = 'Moderate', 
                             severe = 'Severe') %>% 
    map(function(sev) c(list(symptoms = c('fatigue_sympt', 
                                          'sleep_sympt', 
                                          'dyspnoe_sympt', 
                                          'cough_sympt', 
                                          'night_sweat_sympt', 
                                          'gastro_sympt', 
                                          'anosmia_sympt')), 
                        steroid_fup$var_groups[c('pulmo', 'ct', 'cardio', 'iron', 'inflammation')]) %>% 
          map(~steroid_fup$analysis_tbl$fup_2_month[c('treat_steroids', 'cat_WHO', .x)] %>% 
                filter(complete.cases(.), 
                       cat_WHO == sev) %>% 
                count(treat_steroids)) %>% 
          map(~map2_chr(.x[[1]], .x[[2]], paste, sep = ': n = ') %>% 
                paste(collapse = ', ') %>% 
                paste0('\n', .)))
  
  steroid_fup$n_year <- c(moderate = 'Moderate', 
                          severe = 'Severe') %>% 
    map(function(sev) steroid_fup$var_groups %>% 
          map(~steroid_fup$analysis_tbl$fup_year[c('treat_steroids', 'cat_WHO', .x)] %>% 
                filter(complete.cases(.), 
                       cat_WHO == sev) %>% 
                count(treat_steroids)) %>% 
          map(~map2_chr(.x[[1]], .x[[2]], paste, sep = ': n = ') %>% 
                paste(collapse = ', ') %>% 
                paste0('\n', .)))
  
# Drawing bar plot panels ------
  
  insert_msg('Drawing bar plot panels')
  
  ## two month time point
  
  steroid_fup$plots_2_month <- c(moderate = 'Moderate', 
                                 severe = 'Severe') %>% 
    map2(., names(.), 
         function(sev_lab, sev) list(variables = steroid_fup$var_groups[c('symptoms', 
                                                                          'pulmo', 
                                                                          'ct', 
                                                                          'cardio', 
                                                                          'iron', 
                                                                          'inflammation')], 
                                     labeller = steroid_fup$var_groups[c('symptoms', 
                                                                         'pulmo', 
                                                                         'ct', 
                                                                         'cardio', 
                                                                         'iron', 
                                                                         'inflammation')] %>% 
                                       map(translate_var) %>% 
                                       map2(., map(., names), 
                                            ~stri_replace(.x, fixed = ' ', replacement = '\n') %>% 
                                              stri_replace(fixed = 'Reduced\nperformance (ECOG ≥1)', 
                                                           replacement = 'Reduced\nperformance\n(ECOG ≥1)') %>% 
                                              set_names(.y)) %>% 
                                       map(as_labeller), 
                                     plot_tag = steroid_fup$n_2_month[[sev]], 
                                     plot_title = paste0(c('COVID-19-related symptoms', 
                                                           'LFT abnormalities', 
                                                           'Lung CT abnormalities', 
                                                           'Cardiological abnormalities', 
                                                           'Iron turnover', 
                                                           'Inflammation'), 
                                                         ', ', sev, ' COVID-19')) %>% 
           pmap(draw_freq_panel, 
                steroid_fup$analysis_tbl$fup_2_month %>% 
                  filter(cat_WHO == sev_lab), 
                split_factor = 'treat_steroids', 
                rm_na = TRUE, 
                scale = 'percent', 
                show_labels = TRUE, 
                txt_size = 2.75, 
                x_lab = '% of strata', 
                y_lab = '', 
                plot_subtitle = '2-month follow-up', 
                cust_theme = globals$common_theme) %>% 
           map(~.x + 
                 scale_fill_manual(values = c(no = 'steelblue', 
                                              yes = 'coral3'), 
                                   labels = c(no = 'absent', 
                                              yes = 'present'), 
                                   name = '')))
  
  ## one year follow-up
  
  steroid_fup$plots_year <- c(moderate = 'Moderate', 
                              severe = 'Severe') %>% 
    map2(., names(.), 
         function(sev_lab, sev) list(variables = steroid_fup$var_groups[c('symptoms', 
                                                                          'pulmo', 
                                                                          'ct', 
                                                                          'cardio', 
                                                                          'iron', 
                                                                          'inflammation')], 
                                     labeller = steroid_fup$var_groups[c('symptoms', 
                                                                         'pulmo', 
                                                                         'ct', 
                                                                         'cardio', 
                                                                         'iron', 
                                                                         'inflammation')] %>% 
                                       map(translate_var) %>% 
                                       map2(., map(., names), 
                                            ~stri_replace(.x, fixed = ' ', replacement = '\n') %>% 
                                              stri_replace(fixed = 'Reduced\nperformance (ECOG ≥1)', 
                                                           replacement = 'Reduced\nperformance\n(ECOG ≥1)') %>% 
                                              set_names(.y)) %>% 
                                       map(as_labeller), 
                                     plot_tag = steroid_fup$n_year[[sev]][c('symptoms', 
                                                                            'pulmo', 
                                                                            'ct', 
                                                                            'cardio', 
                                                                            'iron', 
                                                                            'inflammation')], 
                                     plot_title = paste0(c('COVID-19-related symptoms', 
                                                           'LFT abnormalities', 
                                                           'Lung CT abnormalities', 
                                                           'Cardiological abnormalities', 
                                                           'Iron turnover', 
                                                           'Inflammation'), 
                                                         ', ', sev, ' COVID-19')) %>% 
           pmap(draw_freq_panel, 
                steroid_fup$analysis_tbl$fup_year %>% 
                  filter(cat_WHO == sev_lab), 
                split_factor = 'treat_steroids', 
                rm_na = TRUE, 
                scale = 'percent', 
                show_labels = TRUE, 
                txt_size = 2.75, 
                x_lab = '% of strata', 
                y_lab = '', 
                plot_subtitle = 'one-year follow-up', 
                cust_theme = globals$common_theme) %>% 
           map(~.x + 
                 scale_fill_manual(values = c(no = 'steelblue', 
                                              yes = 'coral3'), 
                                   labels = c(no = 'absent', 
                                              yes = 'present'), 
                                   name = '')))
  
# END ----
  
  insert_tail()