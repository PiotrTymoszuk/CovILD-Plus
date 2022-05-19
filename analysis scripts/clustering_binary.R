# Clusters the cohort data with the optimal algorithm. The input clustering data 
# is a min/max normalized table with the clustering variables specified in the 
# globals. Prior to clustering, the data was subjected to UMAP analysis.

  insert_head()
  
# container list -----
  
  part_clust <- list()
  
# globals: the analysis table, UMAP object and the clusetring analysis object ------
  
  insert_msg('Globals setup')
  
  ## analysis objects

  part_clust$clust_obj <- cl_devel$test_algos$pam.smc
  
  ## cluster renaming
  
  part_clust$clust_obj$clust_assignment <- part_clust$clust_obj$clust_assignment %>% 
    mutate(clust_id = paste0('#', clust_id), 
           clust_id = factor(clust_id)) %>% 
    map_dfc(function(x) if(is.numeric(x)) factor(x) else x)
  
  ## clustering feature table
  
  part_clust$analysis_tbl <- model.frame(part_clust$clust_obj) %>% 
    rownames_to_column('ID') %>% 
    left_join(set_names(part_clust$clust_obj$clust_assignment, 
                        c('ID', 'clust_id')), 
              by = 'ID') %>% 
    as_tibble

# Characteristic of the clustering object: diagnostic plots, PCA score and UMAP plots -----
  
  insert_msg('Characteristic of the clustering object')

  ## n numbers in the clusters
  
  part_clust$n_numbers <- ngroups(part_clust$clust_obj)
  
  part_clust$n_tag <- map2_chr(part_clust$n_numbers$clust_id,
                           part_clust$n_numbers$n, 
                           ~paste0(.x, ': n = ', .y)) %>% 
    paste(collapse = '\n') %>% 
    paste0('\n', .)
  
  ## diagnostic plots: WSS, silhouette and distance heat map
  
  part_clust$diagnostic_plots <- plot(part_clust$clust_obj , 
                                      type = 'diagnostic', 
                                      cust_theme = globals$common_theme)
  
  part_clust$diagnostic_plots$heat_map <- plot(part_clust$clust_obj, 
                                               type = 'heat_map', 
                                               cust_theme = globals$common_theme) + 
    labs(fill = 'Manhattan\ndistance', 
         tag = part_clust$n_tag)
  
  ## MDS score plot
  
  part_clust$mds_plot <- plot(part_clust$clust_obj, 
                              type = 'components', 
                              cust_theme = globals$common_theme, 
                              red_fun = 'mds', 
                              with = 'data', 
                              kdim = 3) + 
    scale_fill_manual(values = globals$clust_colors, 
                      name = 'Participant\ncluster')

# Comparing the clustering features between the clusters: Kruskal-Wallis test ------
  
  insert_msg('Comparing the clustering feature levels between the clusters')
  
  ## descriptive stats
  
  part_clust$clust_desc_stats <- explore(part_clust$analysis_tbl, 
                                         split_factor = 'clust_id', 
                                         variables = globals$clust_variables, 
                                         what = 'table', 
                                         pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', 'clust_1', 'clust_2', 'clust_3'))
  
  ## testing for the differences
  
  part_clust$clust_test_results <- compare_variables(part_clust$analysis_tbl, 
                                                     split_factor = 'clust_id', 
                                                     variables = globals$clust_variables, 
                                                     what = 'test', 
                                                     types = 'chisq_test', 
                                                     ci = FALSE, 
                                                     pub_styled = TRUE, 
                                                     adj_method = 'BH')
  
  ## significant items
  
  part_clust$clust_signif_vars <- part_clust$clust_test_results %>% 
    filter(p_adjusted < 0.05) %>% 
    .$variable

# Heat map with the features ------
  
  insert_msg('Heat map with the features')
  
  ## dummy clust_analysis objects
  
  part_clust$plotting_clust <- list(data = part_clust$clust_obj$data,
                                    dist_mtx = calculate_dist(model.frame(part_clust$clust_obj), 
                                                              'smc'), 
                                    dist_method = 'smc', 
                                    clust_obj = NULL, 
                                    clust_fun = 'prediction', 
                                    clust_assignment = part_clust$clust_obj$clust_assignment, 
                                    dots = list2()) %>% 
    clust_analysis
  
  part_clust$ft_clust <- list(data = part_clust$clust_obj$data,
                              dist_mtx = calculate_dist(model.frame(part_clust$clust_obj), 
                                                        'smc'), 
                              dist_method = 'smc', 
                              clust_obj = NULL, 
                              clust_fun = 'prediction', 
                              clust_assignment = tibble(observation = globals$clust_variables, 
                                                        clust_id = factor(c(rep('Clinical', 9), 
                                                                            rep('CP', 3), 
                                                                            rep('Psychosocial', 7)), 
                                                                          c('CP', 'Clinical', 'Psychosocial'))), 
                              dots = list2()) %>% 
    clust_analysis
  
  ## heat map with the variables significantly differing between the clusters
  
  part_clust$heat_map_ft <- plot_clust_hm(x_object = part_clust$plotting_clust, 
                                          y_object = part_clust$ft_clust, 
                                          plot_title = 'Participant clusters', 
                                          plot_subtitle = 'Clustering feature levels', 
                                          x_lab = 'Participant', 
                                          cust_theme = globals$common_theme, 
                                          discrete_fill = TRUE) + 
    scale_y_discrete(labels = translate_var(globals$clust_variables)) + 
    scale_fill_manual(values = c('steelblue', 'coral3'), 
                      labels = c('absent', 'present'), 
                      name = '') + 
    labs(tag = part_clust$n_tag)
  
# Visualization of the clustering features in ribbon plots (mean with 2SD) -----
  
  insert_msg('Ribbon plots of the clustering features')

  part_clust$ribbon_plots <- list(variables = dlply(part_clust$ft_clust$clust_assignment, 
                                                    'clust_id', 
                                                    function(x) x$observation), 
                                  plot_title = c('Cardiopulmonary recovery', 
                                                 'Clinical recovery', 
                                                 'Psychosocial recovery')) %>% 
    pmap(draw_stat_panel, 
         data = part_clust$analysis_tbl, 
         split_factor = 'clust_id', 
         stat = 'mean', 
         err_stat = '2se', 
         form = 'line', 
         alpha = 0.25, 
         x_lab = expression('% of cluster, '*''%+-%''*'2SE'), 
         cust_theme = globals$common_theme) %>% 
    map(~.x + 
          theme(axis.title.y = element_blank()) + 
          scale_fill_manual(values = globals$clust_colors, 
                            name = 'Recovery cluster') + 
          scale_color_manual(values = globals$clust_colors, 
                             name = 'Recovery cluster') + 
          scale_x_continuous(labels = function(x) scales::percent(x, suffix = '')))
  
  ## X axis order
  
  part_clust$ribbon_plots$Psychosocial <- 
    part_clust$ribbon_plots$Psychosocial + 
    scale_y_discrete(limits = rev(c('Stress_hi', 
                                    'EQ5DL_low', 
                                    'EQ5DL_activities_bi', 
                                    'EQ5DL_anxiety_bi', 
                                    'EQ5DL_pain_bi', 
                                    'EQ5DL_mobility_bi', 
                                    'EQ5DL_selfcare_bi')), 
                     labels = translate_var(globals$clust_variables))
  
  part_clust$ribbon_plots$CP <- 
    part_clust$ribbon_plots$CP + 
    scale_y_discrete(limits = rev(c('ct_severity_any', 
                                    'diastolic_dysf', 
                                    'lufo_red')), 
                     labels = translate_var(globals$clust_variables))
  
  part_clust$ribbon_plots$Clinical <- 
    part_clust$ribbon_plots$Clinical + 
    scale_y_discrete(limits = rev(c('smwd_low', 
                                    'sympt_present', 
                                    'dyspnoe_sympt', 
                                    'cough_sympt', 
                                    'fatigue_sympt', 
                                    'Chalder_FS_bimodal', 
                                    'sleep_sympt', 
                                    'night_sweat_sympt', 
                                    'anosmia_sympt')), 
                     labels = translate_var(globals$clust_variables))
  
# END -----
  
  insert_tail()