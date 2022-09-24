# Sub- and post-acute treatment with steroid and cluster assignment 
# Frequency of CT abnormalities in the moderate ans severe CoV patients 
# in each cluster.

  insert_head()
  
# container -----
  
  clust_rev <- list()
  
# globals -----
  
  insert_msg('Globals')
  
  clust_rev$analysis_tbl <- part_clust$clust_obj$clust_assignment %>% 
    set_names(c('ID', 'clust_id')) %>% 
    left_join(cov_data$data_tbl %>% 
                filter(time == 4) %>% 
                select(ID, treat_steroids, cat_WHO, ct_severity_any), 
              by = 'ID') %>% 
    mutate(cat_WHO = car::recode(cat_WHO, 
                                 "'A' = 'Ambulatory'; 
                                 'HM' = 'Moderate'; 
                                 'HS' = 'Severe'"), 
           cat_WHO = factor(cat_WHO, c('Ambulatory', 'Moderate', 'Severe')))
  
# cluster assignment and steroid therapy -----
  
  insert_msg('Cluster assignment and steroid therapy')
  
  ## descriptive stats
  
  clust_rev$stats_steroids <- explore(clust_rev$analysis_tbl, 
                                      split_factor = 'clust_id', 
                                      variables = 'treat_steroids', 
                                      what = 'table',
                                      pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', levels(clust_rev$analysis_tbl$clust_id)))
  
  ## testing
  
  clust_rev$test_steroids <- compare_variables(clust_rev$analysis_tbl, 
                                               split_factor = 'clust_id', 
                                               variables = 'treat_steroids', 
                                               what = 'eff_size', 
                                               types = 'cramer_v', 
                                               ci = FALSE, 
                                               pub_styled = TRUE) %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '))
  
  ## plotting
  
  clust_rev$plot_steroids <- 
    plot_variable(clust_rev$analysis_tbl, 
                  variable = 'treat_steroids', 
                  split_factor = 'clust_id', 
                  scale = 'percent', 
                  type = 'stack', 
                  plot_title = translate_var('treat_steroids'), 
                  plot_subtitle = clust_rev$test_steroids$plot_cap[1], 
                  cust_theme = globals$common_theme)
  
  clust_rev$plot_steroids <- 
    clust_rev$plot_steroids + 
    scale_fill_manual(values = c(no = 'steelblue', 
                                 yes = 'coral3'), 
                      labels = c(no = 'no steroids', 
                                 yes = 'steroids')) + 
    labs(tag = clust_rev$plot_steroids$labels$tag %>% 
           stri_replace_all(fixed = '\n', replacement = ', ') %>% 
           paste0('\n', .))
  
# CoV severity in the clusters, severity and CT abnormalities -------
  
  insert_msg('Cov severity, CT abnormality and cluster assignment')
  
  ## frequency calculation
  
  clust_rev$stats_ct <- clust_rev$analysis_tbl %>% 
    group_by(cat_WHO, clust_id) %>% 
    count(ct_severity_any, .drop = FALSE) %>% 
    mutate(percent = n/sum(n) * 100, 
           n_complete = sum(n), 
           axis_lab = paste(clust_id, n_complete, sep = ': n = '),
           axis_lab_exp = paste0(clust_id, '\n(n = ', n_complete, ')'), 
           plot_lab = signif(percent, 2)) %>% 
    filter(ct_severity_any == 'yes') %>% 
    ungroup
  
  ## plotting: figure for the reviewer
  
  clust_rev$plots_ct <- clust_rev$stats_ct %>% 
    dlply('cat_WHO') %>% 
    list(data = ., 
         data_names = names(.), 
         fill_color = globals$sev_colors[1:3]) %>% 
    pmap(function(data, data_names, fill_color) ggplot(data, 
                                                       aes(x = percent, 
                                                           y = reorder(axis_lab, - as.numeric(clust_id)))) + 
           geom_bar(stat = 'identity', 
                    color = 'black', 
                    fill = fill_color) + 
           geom_text(aes(label = plot_lab), 
                     size = 2.75, 
                     hjust = -0.3) + 
           scale_x_continuous(limits = c(0, 105), 
                              breaks = seq(0, 100, by = 25)) + 
           globals$common_theme + 
           labs(title = paste('CT abnormality,', 
                              tolower(data_names), 
                              'COVID-19'), 
                subtitle = 'Comparison of the recovery clusters', 
                x = '% of cluster', 
                y = 'Recovery cluster'))
  
  ## plotting graphs for the supplement
  
  clust_rev$plots_ct_suppl <- clust_rev$stats_ct %>% 
    dlply('cat_WHO') %>% 
    list(data = ., 
         data_names = names(.), 
         fill_color = globals$sev_colors[1:3]) %>% 
    pmap(function(data, data_names, fill_color) ggplot(data, 
                                                       aes(x = reorder(axis_lab_exp, 
                                                                       as.numeric(clust_id)), 
                                                           y = percent)) + 
           geom_bar(stat = 'identity', 
                    color = 'black', 
                    fill = fill_color) + 
           geom_text(aes(label = plot_lab), 
                     size = 2.75, 
                     vjust = -0.4) + 
           scale_y_continuous(limits = c(0, 105), 
                              breaks = seq(0, 100, by = 25)) + 
           globals$common_theme + 
           labs(title = paste('CT abnormality,', 
                              tolower(data_names), 
                              'COVID-19'), 
                y = '% of cluster', 
                x = 'Recovery cluster'))
  
# END ------
  
  insert_tail()