# Characterizes the clusters in respect to the variable set used in 
# explorative data analysis.

  insert_head()
  
# container list -----
  
  clust_chara <- list()
  
# globals -----
  
  insert_msg('Globals setup')
  
  clust_chara$variables <- globals$var_lexicon %>% 
    filter(variable == 'cat_WHO') %>% 
    mutate(var_type = 'factor', 
           test = 'chisq_test') %>% 
    select(-ref_time)
  
  clust_chara$variables <- rbind(clust_chara$variables, 
                                 eda_fix$variables) %>% 
    mutate(plot_type = ifelse(var_type == 'numeric', 'violin', 'stack'))
  
  clust_chara$analysis_tbl <- eda_fix$analysis_tbl %>% 
    left_join(set_names(part_clust$clust_obj$clust_assignment, 
                        c('ID', 'clust_id')), 
              by = 'ID') %>% 
    filter(!is.na(clust_id))
  
  clust_chara$num_vars <- clust_chara$variables %>% 
    filter(var_type == 'numeric') %>% 
    .$variable
  
  clust_chara$n_numbers <- ngroups(part_clust$clust_ob)
  
# descriptive statistics ------
  
  insert_msg('Descriptive stats')
  
  clust_chara$desc_stats <- explore(clust_chara$analysis_tbl, 
                                    split_factor = 'clust_id', 
                                    variables = clust_chara$variables$variable, 
                                    what = 'table', 
                                    pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', 'clust_1', 'clust_2', 'clust_3'))
  
# testing: numeric values are tested by Kruskal-Wallis test, categorical ones by Chi-squared -----
  
  insert_msg('Testing')
  
  clust_chara$test_results <- compare_variables(clust_chara$analysis_tbl, 
                                                split_factor = 'clust_id', 
                                                variables = clust_chara$variables$variable, 
                                                what = 'test', 
                                                types = clust_chara$variables$test, 
                                                ci = FALSE, 
                                                adj_method = 'BH', 
                                                pub_styled = TRUE) %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '))
  
# Single violin and stacked bar plots ------
  
  insert_msg('Violin plots for particular variables')
  
  clust_chara$plots <- list(variable = clust_chara$variables$variable, 
                            type = clust_chara$variables$plot_type, 
                            plot_title = translate_var(clust_chara$variables$variable, out_value = 'label_long'), 
                            plot_subtitle = clust_chara$test_results$plot_cap) %>% 
    pmap(plot_variable, 
         clust_chara$analysis_tbl, 
         split_factor = 'clust_id', 
         cust_theme = globals$common_theme, 
         scale = 'percent') %>% 
    set_names(clust_chara$variables$variable)
  
  clust_chara$plots[clust_chara$num_vars] <- map2(clust_chara$plots[clust_chara$num_vars], 
                                                  translate_var(clust_chara$num_vars, out_value = 'axis_lab_long'), 
                                                  ~.x + 
                                                    scale_fill_manual(values = globals$clust_colors) + 
                                                    labs(y = .y))

# Summary scatter plot with the effect sizes and significance -----
  
  insert_msg('Summary scatter plot')
  
  ## plotting object
  
  clust_chara$summary_obj <- compare_variables(clust_chara$analysis_tbl, 
                                               split_factor = 'clust_id', 
                                               variables = clust_chara$variables$variable, 
                                               what = 'test', 
                                               types = clust_chara$variables$test, 
                                               ci = FALSE, 
                                               adj_method = 'BH', 
                                               pub_styled = FALSE) %>% 
    filter(!is.infinite(estimate))
  
  ## plots: separate ones for the numeric and factor features, top 10 factors labeled
  
  clust_chara$summary_obj <- c(factor = 'Chi-squared test', 
                               numeric = 'Kruskal-Wallis test') %>% 
    map(~filter(clust_chara$summary_obj, test == .x)) %>% 
    map(mutate, 
        variable = translate_var(variable, out_value = 'label_long')) %>% 
    map(~mutate(.x, 
                plot_lab = ifelse(.x$variable %in% top_n(.x, 10, estimate)$variable, 
                                  variable, 
                                  NA)))
  
  clust_chara$summary_plots <- list(x = clust_chara$summary_obj, 
                                    plot_subtitle = c('Differences in categorical features', 
                                                      'Differences in numeric features')) %>% 
    pmap(plot, 
         cust_theme = globals$common_theme, 
         plot_title = 'Recovery clusters', 
         show_labels = 'none') %>% 
    map(~.x + 
          geom_text_repel(aes(label = plot_lab), 
                          size = 2.3, 
                          force = 2, 
                          pull = 0.2, 
                          max.overlaps = 25))
  
  clust_chara$summary_plots <- list(plot = clust_chara$summary_plots , 
                                    x_lab = list('Effect size, Cramer V', 
                                                 expression('Effect size, '*eta^2)), 
                                    y_lab = list(expression(chi^2*' test, -log'[10]*' p'), 
                                                 expression('Kruskal-Wallis test, -log'[10]*' p'))) %>% 
    pmap(function(plot, x_lab, y_lab) plot + 
           labs(x = x_lab, 
                y = y_lab, 
                tag = paste0('\n', plot$labels$tag)) + 
           geom_hline(yintercept = -log10(0.05), 
                      linetype = 'dashed'))
  
# END ----
  
  insert_tail()