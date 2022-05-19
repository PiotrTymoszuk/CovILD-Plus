# Checks the distribution and of the constant and fixed time point (1-year variables)
# Generates a cohort characteristic table at the one-year visit

  insert_head()
  
# container list -----
  
  eda_fix <- list()
  
# globals: variables of interest and analysis table -----
  
  insert_msg('Globals setup')
  
  ## variables of interest
  
  eda_fix$variables <- globals$var_lexicon %>% 
    filter(var_collection != 'index', 
           !variable %in% c('SarsCov2_IgG', 'pain_sympt', 'ab_quant')) %>% ## variables not recorded at the last visit
    rbind(globals$var_lexicon %>% 
            filter(variable == 'rehabilitation')) %>% 
    mutate(test = ifelse(var_type == 'numeric', 'kruskal_test', 'chisq_test')) %>% 
    select(-ref_time)

  ## numeric variables
  
  eda_fix$num_variables <- eda_fix$variables %>% 
    filter(var_type == 'numeric') %>% 
    .$variable
  
  ## score and count variables
  
  eda_fix$score_count_variables <- c('sympt_number', 
                                     'BRCS', 
                                     'EQ5DL_p', 
                                     'EQ5DL_mobility', 
                                     'EQ5DL_selfcare', 
                                     'EQ5DL_activities', 
                                     'EQ5DL_pain', 
                                     'EQ5DL_anxiety', 
                                     'Chalder_FS', 
                                     'SSD12', 
                                     'Stress', 
                                     'SES', 
                                     'KW_IPQ', 
                                     'SOCL9')
  
  ## analysis table with the 1-year data
  
  eda_fix$analysis_tbl <- cov_data$data_tbl %>% 
    filter(time == 4) %>% 
    select(ID, cat_WHO, all_of(eda_fix$variables$variable)) %>% 
    mutate(cat_WHO = factor(unname(globals$sev_labels[cat_WHO])))
  
# Normality check and equality of variance (splitting by the severity) ------

  insert_msg('Normality and EOV check')

  eda_fix$normality <- explore(eda_fix$analysis_tbl, 
                               variables = eda_fix$num_variables,
                               what = 'normality', 
                               pub_styled = TRUE) ## multiple score and inflammation variables non-normal
  
  eda_fix$eov <- compare_variables(eda_fix$analysis_tbl, 
                                   variables = eda_fix$num_variables, 
                                   split_factor = 'cat_WHO', 
                                   what = 'variance', 
                                   pub_styled = TRUE)
  
  ## distribution plots
  
  eda_fix$hist_plots <- eda_fix$num_variables %>% 
    map(~plot_variable(eda_fix$analysis_tbl, 
                       variable = .x, 
                       split_factor = 'cat_WHO', 
                       type = 'hist', 
                       bins = 10, 
                       facet_hist = 'vertical', 
                       cust_theme = globals$common_theme)) %>% 
    set_names(eda_fix$num_variables)
  
# Checking the compatibility with the Poisson distribution for the score and count variables -----
  
  insert_msg('Checking the Poisson distribution assumption for the score and count variables')
  
  ## mean == variance assumption
  
  eda_fix$score_mean_var <- explore(eda_fix$analysis_tbl, 
                                    variables = eda_fix$score_count_variables, 
                                    what = 'list', 
                                    pub_styled = FALSE) %>% 
    map(~.x$statistic) %>% 
    map2_dfr(., names(.), ~tibble(variable = .y, 
                                  mean = .x$value[1], 
                                  varaince = .x$value[2]^2))
  
  ## Kolmorgov-Smirnov test with the random Poisson distribution values
  
  set.seed(1234)
  
  eda_fix$rpois_tbl <- map2_dfc(eda_fix$score_count_variables, 
                                eda_fix$score_mean_var$mean, 
                                ~tibble(!!.x := rpois(n = nrow(eda_fix$analysis_tbl), 
                                                      lambda = .y)))
  
  eda_fix$score_ks_test <- compare_variables(eda_fix$analysis_tbl, 
                                             eda_fix$rpois_tbl, 
                                             variables = eda_fix$score_count_variables, 
                                             what = 'distribution', 
                                             pub_styled = TRUE)
  
# Descriptive stats in the severity groups -----
  
  insert_msg('Descriptive stats in the severity groups')
  
  eda_fix$desc_stats <- explore(eda_fix$analysis_tbl, 
                                split_factor = 'cat_WHO', 
                                variables = eda_fix$variables$variable, 
                                what = 'table', 
                                pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', levels(eda_fix$analysis_tbl$cat_WHO)))
  
# Checking for the differences between the severity groups with Kruskal-Wallis or chi-squared test ----
  
  insert_msg('Testing')
  
  eda_fix$test_results <- compare_variables(eda_fix$analysis_tbl, 
                                            variables = eda_fix$variables$variable, 
                                            split_factor = 'cat_WHO', what = 'test', 
                                            types = eda_fix$variables$test, 
                                            pub_styled = TRUE, 
                                            ci = FALSE, 
                                            adj_method = 'BH')

# Plotting the numeric variables as violin plots ------
  
  insert_msg('Violin plots with the numeric variables')
  
  eda_fix$plots <- list(variable = eda_fix$num_variables, 
                        y_lab = translate_var(eda_fix$num_variables, out_value = 'axis_lab'), 
                        plot_title = translate_var(eda_fix$num_variables, out_value = 'label'), 
                        plot_subtitle = filter(eda_fix$test_results, 
                                               variable %in% eda_fix$num_variables)$significance) %>% 
    pmap(plot_variable, 
         eda_fix$analysis_tbl,
         split_factor = 'cat_WHO', 
         type = 'violin', 
         point_alpha = 0.8, 
         cust_theme = globals$common_theme) %>% 
    set_names(eda_fix$num_variables) %>% 
    map(~.x + scale_fill_manual(values = unname(globals$sev_colors)))
  
# END ----
  
  insert_tail()