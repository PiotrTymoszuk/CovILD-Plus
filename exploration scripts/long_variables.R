# Explorative data analysis for longitudinal variables
# Checked: distribution and variance equality at particular time points
# The proper kinetic analysis accounting for the repeated values is done 
# with a separate script

  insert_head()
  
# container list ------
  
  eda_long <- list()
  
# globals: variables and analysis tables -----
  
  insert_msg('Globals setup')
  
  ## variable lexicon
  
  eda_long$variables <- globals$var_lexicon %>% 
    filter(var_collection == 'longitudinal') %>% 
    mutate(test = ifelse(var_type == 'numeric', 'kruskal_test', 'chisq_test'))
  
  ## numeric and categorical variables
  
  eda_long$num_variables <- eda_long$variables %>% 
    filter(var_type == 'numeric') %>% 
    .$variable
  
  eda_long$cat_variables <- eda_long$variables %>% 
    filter(var_type == 'factor') %>% 
    .$variable
  
  ## analysis table: severity groups and the whole cohort in a list, 
  ## time 0 (acute COVID-19) is omitted, since only the symptoms were recorded 
  ## at this time point
  
  eda_long$analysis_tbl <- cov_data$data_tbl %>% 
    select(ID, cat_WHO, time, all_of(eda_long$variables$variable)) %>% 
    filter(time != 0) %>% 
    mutate(time = factor(time))
  
  eda_long$analysis_tbl <- eda_long$analysis_tbl %>% 
    mutate(cat_WHO = unname(globals$sev_labels[cat_WHO])) %>% 
    dlply(.(cat_WHO)) %>% 
    c(list(Cohort = mutate(eda_long$analysis_tbl, cat_WHO = 'Cohort'))) %>% 
    map(as_tibble) %>% 
    map(mutate, cat_WHO = unname(factor(cat_WHO, c(globals$sev_labels))))
  
# normality and EOV checks, time points as splitting factor -----
  
  insert_msg('Normality and EOV')
  
  eda_long$normality <- eda_long$analysis_tbl %>% 
    map(~explore(data = .x, 
                 variables = eda_long$num_variables, 
                 what = 'normality', 
                 pub_styled = TRUE))
  
  eda_long$eov <- eda_long$analysis_tbl %>% 
    map(~compare_variables(.x, 
                           variables = eda_long$num_variables, 
                           split_factor = 'time', 
                           what = 'variance', 
                           pub_styled = TRUE))
  
  ## distribution histograms
  
  eda_long$hist_plots <- eda_long$analysis_tbl %>% 
    map(function(sub_group) eda_long$num_variables %>% 
          map(~plot_variable(sub_group, variable = .x, 
                             split_factor = 'time', 
                             type = 'hist', 
                             cust_theme = globals$common_theme, 
                             bins = 10, 
                             facet_hist = 'vertical')) %>% 
          set_names(eda_long$num_variables))
  
# descriptive stats -----
 
  insert_msg('Descriptive stats')
  
  plan('multisession')
  
  eda_long$desc_stats <- eda_long$analysis_tbl %>% 
    future_map(~explore(data = .x, 
                        split_factor = 'time', 
                        variables = eda_long$variables$variable, 
                        what = 'table', 
                        pub_styled = TRUE) %>% 
                 reduce(left_join, by = 'variable') %>% 
                 set_names(c('variable', paste0('fup_', 1:4))), 
               .options = furrr_options(seed = TRUE))
  
# testing -----
  
  insert_msg('Testing')
  
  eda_long$test_results <- eda_long$analysis_tbl %>% 
    future_map(~compare_variables(filter(.x), 
                                  variables = eda_long$variables$variable, 
                                  split_factor = 'time', 
                                  what = 'test', 
                                  types = eda_long$variables$test, 
                                  pub_styled = TRUE, 
                                  adj_method = 'BH', 
                                  ci = FALSE), 
               .options = furrr_options(seed = TRUE))
  

  plan('sequential')
  
# Plotting the numeric variables in violin plots -----
  
  insert_msg('Plotting the numeric variables in violin plots')
  
  eda_long$plots <- list(data = eda_long$analysis_tbl, 
                         sub_group = names(eda_long$analysis_tbl), 
                         test_res = map(eda_long$test_results, 
                                        filter, variable %in% eda_long$num_variables)) %>% 
    pmap(function(data, sub_group, test_res) list(variable = eda_long$num_variables, 
                                                  plot_title = paste(sub_group, 
                                                                     translate_var(eda_long$num_variables), 
                                                                     sep = ': '), 
                                                  y_lab = translate_var(eda_long$num_variables, out_value = 'axis_lab'), 
                                                  plot_subtitle = test_res$significance) %>% 
           pmap(plot_variable, 
                data, 
                split_factor = 'time', 
                type = 'violin', 
                cust_theme = globals$common_theme) %>% 
           set_names(eda_long$num_variables))
  
# END ------
  
  insert_tail()