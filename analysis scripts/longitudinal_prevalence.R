# This script models the prevalence of symptoms over time

  insert_head()
  
# data container -----
  
  long_prev <- list()

# globals: variables and analysis table ------
  
  insert_msg('Globals setup')
  
  ## variables and group definition
  
  long_prev$variables <- globals$var_lexicon %>% 
    filter(var_type == 'factor', 
           var_collection == 'longitudinal', 
           !variable %in% c('fever_sympt', 'pain_sympt')) %>% 
    .$variable
  
  long_prev$acute_vars <- c('fatigue_sympt', 
                            'sympt_present', 
                            'sleep_sympt', 
                            'dyspnoe_sympt', 
                            'cough_sympt', 
                            'night_sweat_sympt', 
                            'gastro_sympt', 
                            'anosmia_sympt')
  
  long_prev$post_vars <- long_prev$variables[!long_prev$variables %in% long_prev$acute_vars]

  ## analysis tables
  
  long_prev$analysis_tbl <- cov_data$data_tbl %>% 
    select(ID, time, cat_WHO, all_of(long_prev$variables)) %>% 
    mutate(time = car::recode(time, "1 = 2; 2 = 3; 3 = 6; 4 = 12"), 
           time_cat = factor(time), 
           cat_WHO = unname(globals$sev_labels[cat_WHO]))
  
  long_prev$analysis_tbl <- long_prev$analysis_tbl %>% 
    dlply(.(cat_WHO)) %>% 
    c(list(Cohort = mutate(long_prev$analysis_tbl, cat_WHO = 'Cohort'))) %>% 
    map(as_tibble)
  
  long_prev$analysis_tbl_acute <- long_prev$analysis_tbl  %>% 
    map(select, ID, time, time_cat, long_prev$acute_vars)
  
  long_prev$analysis_tbl <- long_prev$analysis_tbl %>% 
    map(~.x[!names(.x) %in% long_prev$acute_vars]) %>% 
    map(filter, time != 0)
  
# descriptive stats -----
  
  insert_msg('Descriptive stats')
  
  long_prev$desc_stats_acute <- long_prev$analysis_tbl_acute %>% 
    map(function(sub_set) long_prev$acute_vars %>% 
          map_dfr(~explore(data = complete_cases(sub_set[c('ID', 'time_cat', .x)]), 
                       variables = .x, 
                       split_factor = 'time_cat', 
                       what = 'table', 
                       pub_styled = TRUE) %>% 
                reduce(left_join, by = 'variable') %>% 
                set_names(c('variable', paste0('fup_', 0:4)))))
  
  long_prev$desc_stats <- long_prev$analysis_tbl %>% 
    map(function(sub_set) long_prev$post_vars %>% 
          map_dfr(~explore(data = complete_cases(sub_set[c('ID', 'time_cat', .x)]), 
                           variables = .x, 
                           split_factor = 'time_cat', 
                           what = 'table', 
                           pub_styled = TRUE) %>% 
                    reduce(left_join, by = 'variable') %>% 
                    set_names(c('variable', paste0('fup_', 1:4)))))
  
  long_prev$desc_stats <- map2(long_prev$desc_stats_acute, 
                               long_prev$desc_stats, 
                               outer_rbind) %>% 
    map(~map_dfc(.x, stri_replace, regex = '^no:.*\\nyes:\\s{1}', replacement = '') %>% 
          mutate(n_number = stri_extract(fup_1, regex = 'n = \\d{1,2}')) %>% 
          map_dfc(stri_replace, regex = '\\nComplete:.*$', replacement = '') %>% 
          map_dfc(stri_replace, fixed = '% (', replacement = '% (n = '))
  
# serial modeling with 2nd GLMERs, error safe due to constancy/convergence problems for some features ------  
  
  insert_msg('Serial modeling with second-order mixed-effect models')
  
  ## acute variables

  long_prev$models_acute <- long_prev$analysis_tbl_acute %>% 
    map(function(sub_set) long_prev$acute_vars %>% 
          map(~model_kinetic(data = complete_cases(sub_set[c('ID', 'time', .x)]), 
                             response = .x, 
                             time = 'time', 
                             ID = 'ID', 
                             order = 2, 
                             family = 'binomial')) %>% 
          set_names(long_prev$acute_vars))

  ## follow-up variables, some models do not converge!
  
  long_prev$models <- long_prev$analysis_tbl %>% 
    map(function(sub_set) long_prev$post_vars %>% 
          map(~safely(model_kinetic)(data = complete_cases(sub_set[c('ID', 'time', .x)]), 
                                     response = .x, 
                                     time = 'time', 
                                     ID = 'ID', 
                                     order = 2, 
                                     family = 'binomial')) %>% 
          set_names(long_prev$post_vars) %>% 
          map(~.x[['result']]) %>% 
          compact)

  ## merging
  
  long_prev$models <- map2(long_prev$models_acute, 
                           long_prev$models, 
                           c)
# LRT -----
  
  insert_msg('LRT stats')
  
  ## LRT, calculating the lambdas
  
  long_prev$lrt <- long_prev$models %>% 
    map(lrt_list, .parallel = TRUE)
  
  long_prev$lrt <- long_prev$lrt %>% 
    map(~map_dfr(.x, mutate, p_value = `Pr..Chisq.`)) %>% 
    map(mutate, p_adj = p.adjust(p_value, 'BH'))

# Extracting the LRT globals significance to show in the plots ------
  
  insert_msg('Extracting the  and corrected significance to present in the plots')
  
  long_prev$plot_captions <- long_prev$lrt %>% 
    map(filter, order == 'global') %>% 
    map(mutate, 
        plot_cap = paste0('\u03BB = ', signif(lambda, 2)), 
        plot_cap = ifelse(p_adj < 0.001, 
                          paste0(plot_cap, ', p < 0.001'), 
                          ifelse(p_adj >= 0.05, 
                                 paste0(plot_cap, ', ns (p = ', signif(p_adj, 2), ')'), 
                                 paste0(plot_cap, ', p = ', signif(p_adj, 2))))) %>% 
    map(~.x$plot_cap)
  
# plotting -----
  
  insert_msg('Serial plotting')
  
  ## n numbers to be shown in the plot captions
  
  long_prev$plots <- list(x = long_prev$models, 
                          y = long_prev$plot_captions, 
                          z = globals$sev_colors, 
                          lab = globals$sev_labels) %>% 
    pmap(function(x, y, z, lab) list(x = x, 
                                     plot_title = lab, 
                                     plot_subtitle = y, 
                                     point_color = z, 
                                     outcome_color = z, 
                                     fitted_color = z, 
                                     y_lab = translate_var(names(x), out_value = 'axis_lab_long'), 
                                     fitted = FALSE) %>% 
           pmap(plot, 
                cust_theme = globals$common_theme, 
                x_lab = 'Time post COVID-19, months', 
                type = 'frequency') %>% 
           map(~.x + 
                 scale_x_continuous(breaks = c(0, 2, 3, 6, 12)) + 
                 expand_limits(y = 0) + 
                 labs(subtitle = paste(.x$labels$subtitle, 
                                       stri_replace(.x$labels$tag, fixed = '\n', replacement = ''), 
                                       sep = ', ')) + 
                 theme(plot.tag = element_blank())))
  
# END -----
  
  insert_tail()