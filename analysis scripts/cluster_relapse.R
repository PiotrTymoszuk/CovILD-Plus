# Symptom relapse, 12 vs 6 months in the recovery clusters. 
# Individuals with the complete symptom record are included

  insert_head()
  
# container list -----
  
  clust_relapse <- list()
  
# Globals -----
  
  insert_msg('Globals setup')
  
  clust_relapse$variables <- c('fatigue_sympt', 
                               'sleep_sympt', 
                               'dyspnoe_sympt', 
                               'cough_sympt', 
                               'night_sweat_sympt', 
                               'gastro_sympt', 
                               'anosmia_sympt')
  
  clust_relapse$var_labs <- clust_relapse$variables %>% 
    translate_var %>% 
    stri_replace(fixed = ' (', replacement = '\n(') %>% 
    set_names(clust_relapse$variables)

  clust_relapse$analysis_tbl <- cov_data$data_tbl %>% 
    filter(time %in% c(3, 4)) %>% 
    select(time, ID, all_of(clust_relapse$variables)) %>% 
    complete_cases %>% 
    left_join(part_clust$clust_obj$clust_assignment %>% 
                set_names(c('ID', 'clust_id')), 
              by = 'ID') %>% 
    filter(!is.na(clust_id))
  
  ## n numbers
  
  clust_relapse$n_numbers <- clust_relapse$analysis_tbl %>% 
    filter(time == 4) %>% 
    count(clust_id)
  
  clust_relapse$n_tag <- map2_chr(clust_relapse$n_numbers$clust_id, 
                                  clust_relapse$n_numbers$n, 
                                  paste, sep = ': n = ') %>% 
    paste(collapse = ', ') %>% 
    paste0('\n', .)
  
# Determining the late (12 vs 6 month relapse rates) -----
  
  insert_msg('Relapse rates in the clusters')
  
  ## determining if there's a relapse
  
  clust_relapse$relapse_present <- clust_relapse$analysis_tbl %>% 
    dlply('ID', arrange, time) %>% 
    map(select, all_of(clust_relapse$variables)) %>% 
    map(~map_dfc(.x, as.numeric)) %>% 
    map(~map_dfc(.x, ~ifelse(.x[2] > .x[1], 'yes', 'no'))) %>% 
    map(~map_dfc(.x, factor, levels = c('no', 'yes'))) %>% 
    map2_dfr(., names(.), ~mutate(.x, ID = .y)) %>% 
    left_join(part_clust$clust_obj$clust_assignment %>% 
                set_names(c('ID', 'clust_id')), 
              by = 'ID') %>% 
    filter(!is.na(clust_id))
  
# descriptive stats -----
  
  insert_msg('Descriptive stats')
  
  clust_relapse$desc_stats <- explore(clust_relapse$relapse_present, 
                                      split_factor = 'clust_id', 
                                      variables = clust_relapse$variables, 
                                      what = 'table', 
                                      pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', paste0('clust_', 1:3))) %>% 
    map_dfc(stri_replace_all, 
            regex = 'no:.*\\nyes:\\s{1}', 
            replacement = '')

# testing for the differences between the clusters -----
  
  insert_msg('Testing')
  
  clust_relapse$test_results <- compare_variables(clust_relapse$relapse_present, 
                                                  split_factor = 'clust_id', 
                                                  variables = clust_relapse$variables, 
                                                  what = 'test', 
                                                  types = 'chisq_test', 
                                                  ci = FALSE, 
                                                  pub_styled = TRUE, 
                                                  adj_method = 'BH') %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = '\n'), 
           plot_cap = ifelse(p_adjusted < 0.05, plot_cap, NA))
  
# Plotting the late (12 vs 6 month relapse rates) ----
  
  insert_msg('Relapse rate plot')
  
  ## percents relapsing individuals
  
  clust_relapse$relapse_plot <- explore(clust_relapse$relapse_present, 
                                        split_factor = 'clust_id', 
                                        variables = clust_relapse$variables, 
                                        what = 'list', 
                                        pub_styled = FALSE) %>% 
    map(~map(.x, ~.x$statistic[2, ])) %>% 
    map(~map2_dfr(.x, names(.x), ~mutate(.x, variable = .y))) %>% 
    map2_dfr(., names(.), ~mutate(.x, clust_id = factor(.y)))
  
  ## bar plot
  
  clust_relapse$relapse_plot <- clust_relapse$relapse_plot %>%
    ggplot(aes(x = percent, 
               y = reorder(variable, percent), 
               fill = clust_id)) + 
    geom_bar(stat = 'identity', 
             color = 'black', 
             position = position_dodge(width = 0.9)) + 
    geom_text(data = clust_relapse$test_results %>% 
                mutate(percent = 33.5, 
                       clust_id = '#2'), 
              aes(label = plot_cap), 
              size = 2.75, 
              vjust = 0.5, 
              hjust = -0.4) + 
    scale_fill_manual(values = globals$clust_colors, 
                      name = 'Recovery\ncluster') + 
    scale_y_discrete(labels = clust_relapse$var_labs) + 
    expand_limits(x = 40) + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Late symptom relapse rate', 
         subtitle = '12- vs 6-month follow-up', 
         x = '% of cluster', 
         tag = clust_relapse$n_tag) + 
    annotate('segment', 
             x = 34, 
             xend = 34, 
             y = 6 - 0.35, 
             yend = 6 + 0.35)
  
# END -----
  
  insert_tail()