# This script models the values of numeric longitudinal features
# As many of them are strongly non-normally distributed, Friedman test
# with Wilcoxon paired tests are used for the analysis.

  insert_head()
  
# data container -----
  
  long_num <- list()
  
# globals: variables and analysis table ------
  
  insert_msg('Globals setup')
  
  ## variables and group definition
  
  long_num$variables <- globals$var_lexicon %>% 
    filter(var_type == 'numeric', 
           var_collection == 'longitudinal') %>% 
    .$variable
  
  long_num$acute_vars <- c('mmrc', 'ECOG', 'sympt_number') ## variables recorded during acute COVID
  
  long_num$post_vars <- long_num$variables[!long_num$variables %in% long_num$acute_vars]

  ## analysis tables: the whole cohort and the severity groups
  
  long_num$analysis_tbl <- cov_data$data_tbl %>% 
    select(ID, time, cat_WHO, all_of(long_num$variables)) %>% 
    mutate(time = car::recode(time, "1 = 2; 2 = 3; 3 = 6; 4 = 12"), 
           time_cat = factor(time), 
           cat_WHO = unname(globals$sev_labels[cat_WHO]))
  
  long_num$analysis_tbl <- long_num$analysis_tbl %>% 
    dlply(.(cat_WHO)) %>% 
    c(list(Cohort = mutate(long_num$analysis_tbl, cat_WHO = 'Cohort'))) %>% 
    map(as_tibble)
  
  long_num$analysis_tbl_acute <- long_num$analysis_tbl  %>% 
    map(select, ID, time, time_cat, long_num$acute_vars)
  
  long_num$analysis_tbl <- long_num$analysis_tbl %>% 
    map(~.x[!names(.x) %in% long_num$acute_vars]) %>% 
    map(filter, time != 0)
  
# descriptive stats ------
  
  insert_msg('Descriptive stats')
  
  ## acute variables
  
  long_num$desc_stats_acute <- long_num$analysis_tbl_acute %>% 
    map(function(sub_set) long_num$acute_vars %>% 
          map_dfr(~explore(data = complete_cases(sub_set[c('ID', 'time_cat', .x)]), 
                           split_factor = 'time_cat', 
                           variables = .x, 
                           what = 'table', 
                           pub_styled = TRUE) %>% 
                    reduce(left_join, by = 'variable') %>% 
                    set_names(c('variable', paste0('fup_', 0:4)))))
  
  ## post-acute variables
  
  long_num$desc_stats <- long_num$analysis_tbl %>% 
    map(function(sub_set) long_num$post_vars %>% 
          map_dfr(~explore(data = complete_cases(sub_set[c('ID', 'time_cat', .x)]), 
                           split_factor = 'time_cat', 
                           variables = .x, 
                           what = 'table', 
                           pub_styled = TRUE) %>% 
                    reduce(left_join, by = 'variable') %>% 
                    set_names(c('variable', paste0('fup_', 1:4)))))
  
  ## common tables
  
  long_num$desc_stats <- map2(long_num$desc_stats_acute, 
                              long_num$desc_stats, 
                              outer_rbind)

  long_num$desc_stats_acute <- NULL
  
# Testing with Friedman test ------
  
  insert_msg('Testing: Friedman test')
  
  ## acute variables
  
  long_num$test_results_acute <- long_num$analysis_tbl_acute %>% 
    map(function(sub_set) long_num$acute_vars %>% 
          map_dfr(~compare_variables(data = complete_cases(sub_set[c('ID', 'time_cat', .x)]), 
                                     split_factor = 'time_cat', 
                                     variables = .x, 
                                     what = 'test',
                                     types = 'friedman_test', 
                                     pub_styled = TRUE, 
                                     ci = FALSE)))
  
  ## post-acute variables
  
  long_num$test_results <- long_num$analysis_tbl %>% 
    map(function(sub_set) long_num$post_vars %>% 
          map_dfr(~compare_variables(data = complete_cases(sub_set[c('ID', 'time_cat', .x)]), 
                                     split_factor = 'time_cat', 
                                     variables = .x, 
                                     what = 'test',
                                     types = 'friedman_test', 
                                     pub_styled = TRUE, 
                                     ci = FALSE)))
  
  ## common tables and p value adjustment with Benjamini-Hochberg method

  long_num$test_results <- map2(long_num$test_results_acute, 
                                long_num$test_results, 
                                rbind) %>% 
    map(mutate, 
        p_adjusted = p.adjust(p_value, 'BH'), 
        significance = ifelse(p_adjusted < 0.001, 
                              'p < 0.001', 
                              ifelse(p_adjusted >= 0.05, 
                                     paste0('ns (p = ', signif(p_adjusted, 2), ')'), 
                                     paste('p =', signif(p_adjusted, 2)))))
  
  long_num$test_results <- long_num$test_results %>% 
    map(mutate, 
        plot_cap = paste(eff_size, significance, sep = ', '))
  
  long_num$test_results_acute <- NULL
  
# Plotting -----
  
  insert_msg('Plotting')
  
  ## acute variables
  
  long_num$plots_acute <- list(sub_set = long_num$analysis_tbl_acute, 
                               subset_name = names(long_num$analysis_tbl_acute), 
                               fill_colors = globals$sev_colors) %>% 
    pmap(function(sub_set, subset_name, fill_colors) list(plot_var = long_num$acute_vars, 
                                                          plot_title = paste(subset_name, 
                                                                             translate_var(long_num$acute_vars), 
                                                                             sep = ': '), 
                                                          y_lab = translate_var(long_num$acute_vars, out_value = 'axis_lab')) %>% 
           pmap(function(plot_var, plot_title, y_lab) plot_kinet(data = complete_cases(sub_set[c('ID', 'time', plot_var)]), 
                                                                 plot_var = plot_var, 
                                                                 time_var = 'time', 
                                                                 fill_color = fill_colors, 
                                                                 plot_title = plot_title, 
                                                                 y_lab = y_lab)) %>% 
           set_names(long_num$acute_vars))
  
  ## post-acute variables
  
  long_num$plots <- list(sub_set = long_num$analysis_tbl, 
                         subset_name = names(long_num$analysis_tbl), 
                         fill_colors = globals$sev_colors) %>% 
    pmap(function(sub_set, subset_name, fill_colors) list(plot_var = long_num$post_vars, 
                                                          plot_title = subset_name, 
                                                          y_lab = translate_var(long_num$post_vars, out_value = 'axis_lab')) %>% 
           pmap(function(plot_var, plot_title, y_lab) plot_kinet(data = complete_cases(sub_set[c('ID', 'time', plot_var)]), 
                                                                 plot_var = plot_var, 
                                                                 time_var = 'time', 
                                                                 fill_color = fill_colors, 
                                                                 plot_title = plot_title, 
                                                                 y_lab = y_lab)) %>% 
           set_names(long_num$post_vars))
  
  ## presenting the significance and effect size statistic values in the plot captions
  
  long_num$plots <- map2(long_num$plots_acute, 
                         long_num$plots, 
                         c)
  
  long_num$plots <- list(plot = long_num$plots, 
                         tst_results = long_num$test_results) %>% 
    pmap(function(plot, tst_results) map2(plot, tst_results$plot_cap, 
                                          ~.x + 
                                            labs(subtitle = .y) + 
                                            theme(panel.grid.major = element_blank())))
  
  ## moving the tag content (n numbers) to the plot captions
  
  long_num$plots <- long_num$plots %>% 
    map(~map(.x, ~.x + 
               labs(subtitle = paste(.x$labels$subtitle, .x$labels$tag, sep = ', ')) + 
               theme(plot.tag = element_blank())))

# END -----
  
  long_num <- compact(long_num)
  
  insert_tail()