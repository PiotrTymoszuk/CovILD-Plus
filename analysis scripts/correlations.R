# This script determines correlations:
# between symptom burden, dyspnoe, fatigue, sleep problems, impaired lung function, cardiac dysfunction, CT severity score, 
# SMWD, EQ5DL_p, BRCS, SSD12, Stress, SES, KW_IPQ and SOCL9
# The tool of choice is Kendall correlation coefficient. Time point - the last visit

  insert_head()
  
# data container ----
  
  correl<- list()
  
# globals: outcome variables and the analysis table ----
  
  insert_msg('Globals setup')
  
  ## variable and variable pairs

  correl$pairs <- combn(globals$corr_variables, 2, 
                        simplify = FALSE) %>%
    map(as.list) %>% 
    transpose %>% 
    set_names(c('var1', 'var2')) %>% 
    map(unlist) %>% 
    as_tibble
  
  ## variables for interest for the detailed plots
  
  correl$detail_vars <- c('Chalder_FS', 'EQ5DL_p', 'EQ5DL_activities', 'Stress')
  
  ## analysis table with the cohort and severity groups
  
  correl$analysis_tbl <- cov_data$data_tbl %>% 
    filter(time == 4) %>% 
    select(ID, cat_WHO, all_of(globals$corr_variables)) %>% 
    filter(complete.cases(.))
  
  correl$analysis_tbl <- correl$analysis_tbl %>% 
    dlply(.(cat_WHO), as_tibble) %>% 
    c(list(cohort = correl$analysis_tbl)) %>% 
    map(select, -ID, -cat_WHO) %>% 
    map(~map_dfc(.x, as.numeric))

# Serial correlation analysis -----
  
  insert_msg('Correlations')
  
  correl$test_results <- map(correl$analysis_tbl, 
                             function(sev) map2(correl$pairs$var1, 
                                                correl$pairs$var2, 
                                                ~safely(correlate_variables)(sev, 
                                                                             variables = c(.x, .y), 
                                                                             what = 'correlation', 
                                                                             type = 'kendall', 
                                                                             pub_styled = FALSE, 
                                                                             adj_method = 'none')))
  
  
  ## getting the results, correlation analysis impossible for anosmia in the 
  ## severe groups: no patient anosmia there
  
  correl$test_results <- correl$test_results %>% 
    map(~map_dfr(.x, ~.x$result) %>% 
          mutate(p_adjusted = p.adjust(p_value, 'BH'), 
                 var1_label = translate_var(variable1), 
                 var2_label = translate_var(variable2), 
                 correlation = ifelse(p_adjusted >= 0.05, 'ns', 
                                      ifelse(estimate > 0, 'positive', 'negative')), 
                 plot_cap = paste0('\u03C4 = ', signif(estimate, 2), ', ', significance), 
                 var_pair = paste(variable1, variable2, sep = '_')))

# convenience table with the significant correlations in the whole cohort ----
  
  insert_msg('Significant correlations')
  
  correl$signifcant_cohort <- correl$test_results$cohort %>% 
    filter(p_adjusted < 0.05) %>% 
    .$var_pair
  
  ## a table with the factors pairs significantly correlating in the entire cohort
  
  correl$signifcant <- correl$test_results %>% 
    map(filter, var_pair %in% correl$signifcant_cohort)
  
# presenting the results as a bubble plot -----
  
  insert_msg('Bubble plot with significant correlations')
  
  correl$bubble_plots <- list(data = correl$signifcant, 
                              plot_title = globals$sev_labels[names(correl$signifcant)], 
                              plot_tag = map_dbl(correl$analysis_tbl, nrow) %>% 
                                paste('n =', .)) %>% 
    pmap(plot_corr_buble)

# correlation plots of the symptom number and mmrc with the SMWD, stress, chalder and eq5dl scales ------  
  
  insert_msg('Plotting correlations of mMRC and symptom number')
  
  ## symptom number
  
  correl$sympt_no_plots <- list(x = correl$detail_vars, 
                                y = paste(translate_var(correl$detail_vars), 
                                          translate_var('sympt_number'), 
                                          sep = ' vs '), 
                                z = translate_var(correl$detail_vars, 
                                                  out_value = 'axis_lab'), 
                                cap = filter(correl$test_results$cohort, 
                                             variable1 == 'sympt_number', 
                                             variable2 %in% correl$detail_vars)[['plot_cap']]) %>% 
    pmap(function(x, y, z, cap) plot_correlation(correl$analysis_tbl$cohort, 
                                                 variables = c('sympt_number', x), 
                                                 type = 'correlation', 
                                                 cust_theme = globals$common_theme, 
                                                 plot_title = y, 
                                                 plot_subtitle = cap, 
                                                 x_lab = '# symptoms', 
                                                 y_lab = z, 
                                                 show_trend = FALSE)) %>% 
    map(~.x + 
          scale_x_continuous(breaks = 0:9) + 
          geom_smooth(method = 'lm', 
                      se = FALSE, 
                      span = 0.8) + 
          labs(subtitle = paste(.x$labels$subtitle, .x$labels$tag, sep = ', ')) + 
          theme(plot.tag = element_blank())) %>% 
    set_names(correl$detail_vars)
  
  
  ## mMRC
  
  correl$mmrc_plots <- list(x = correl$detail_vars, 
                            y = paste(translate_var(correl$detail_vars), 
                                      translate_var('mmrc'), 
                                      sep = ' vs '), 
                            z = translate_var(correl$detail_vars, 
                                              out_value = 'axis_lab'), 
                            cap = filter(correl$test_results$cohort, 
                                         variable1 == 'mmrc', 
                                         variable2 %in% correl$detail_vars)[['plot_cap']]) %>% 
    pmap(function(x, y, z, cap) plot_correlation(correl$analysis_tbl$cohort, 
                                                 variables = c('mmrc', x), 
                                                 type = 'correlation', 
                                                 cust_theme = globals$common_theme, 
                                                 plot_title = y, 
                                                 plot_subtitle = cap, 
                                                 x_lab = 'mMRC', 
                                                 y_lab = z, 
                                                 show_trend = FALSE)) %>% 
    map(~.x + 
          geom_smooth(method = 'lm', 
                      se = FALSE, 
                      span = 0.8) + 
          labs(subtitle = paste(.x$labels$subtitle, .x$labels$tag, sep = ', ')) + 
          theme(plot.tag = element_blank())) %>% 
    set_names(correl$detail_vars)

# END -----
  
  insert_tail()