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
  
  ## numbers of observations in the clusters
  
  part_clust$clust_n <- ngroups(part_clust$clust_obj)
  
  part_clust$clust_n <- map2_chr(part_clust$clust_n[[1]], 
                                 part_clust$clust_n[[2]], 
                                 paste, sep = ': n = ') %>% 
    set_names(part_clust$clust_n[[1]])

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

# Comparing the clustering features between the clusters: chi-squared test ------
  
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
  
  part_clust$clust_test_results <- 
    compare_variables(part_clust$analysis_tbl, 
                      split_factor = 'clust_id', 
                      variables = globals$clust_variables, 
                      what = 'test', 
                      types = 'chisq_test', 
                      ci = FALSE, 
                      pub_styled = TRUE, 
                      adj_method = 'BH')
  
  ## labels for the ribbon plots with the significance
  
  part_clust$ribbon_labs <- part_clust$clust_test_results %>% 
    mutate(plot_lab = translate_var(variable, out_value = 'label_long'), 
           plot_lab = stri_replace(plot_lab, fixed = ' (', replacement = '\n('), 
           plot_lab = paste(plot_lab, significance, sep = '\n'))
  
  part_clust$ribbon_labs <- set_names(part_clust$ribbon_labs$plot_lab, 
                                      part_clust$ribbon_labs$variable)
  
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
  
  ## heat map with all clustering variables
  
  part_clust$heat_map_ft <- plot_clust_hm(x_object = part_clust$plotting_clust, 
                                          y_object = part_clust$ft_clust, 
                                          plot_title = 'Participant clusters', 
                                          plot_subtitle = 'Clustering feature levels', 
                                          x_lab = 'Participant', 
                                          cust_theme = globals$common_theme, 
                                          discrete_fill = TRUE) + 
    scale_y_discrete(labels = translate_var(globals$clust_variables, 
                                            out_value = 'label_long')) + 
    scale_fill_manual(values = c('steelblue', 'coral3'), 
                      labels = c('absent', 'present'), 
                      name = '') + 
    labs(tag = part_clust$n_tag)
  
# Visualization of the clustering features in ribbon plots (mean with 2SD) -----
  
  insert_msg('Ribbon plots of the clustering features')

  part_clust$ribbon_plots <- 
    list(variables = dlply(part_clust$ft_clust$clust_assignment, 
                           'clust_id', 
                           function(x) x$observation), 
         plot_title = c('Cardiopulmonary findings', 
                        'Symptoms and physical performance', 
                        'QoL, mental health and usual activity')) %>% 
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
          theme(axis.title.y = element_blank(), 
                plot.title.position = 'plot') + 
          scale_fill_manual(values = globals$clust_colors, 
                            labels = part_clust$clust_n, 
                            name = 'Recovery cluster') + 
          scale_color_manual(values = globals$clust_colors, 
                             labels = part_clust$clust_n, 
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
                     labels = part_clust$ribbon_labs)
  
  part_clust$ribbon_plots$CP <- 
    part_clust$ribbon_plots$CP + 
    scale_y_discrete(limits = rev(c('ct_severity_any', 
                                    'diastolic_dysf', 
                                    'lufo_red')), 
                     labels = part_clust$ribbon_labs)
  
  part_clust$ribbon_plots$Clinical <- 
    part_clust$ribbon_plots$Clinical + 
    scale_y_discrete(limits = rev(c('smwd_low', 
                                    'sympt_present', 
                                    'fatigue_sympt', 
                                    'Chalder_FS_bimodal', 
                                    'sleep_sympt', 
                                    'dyspnoe_sympt',
                                    'cough_sympt', 
                                    'night_sweat_sympt', 
                                    'anosmia_sympt')), 
                     labels = part_clust$ribbon_labs)
  
# Permutation importance of the clustering factors -----
  
  insert_msg('Permutation importance of clustering factors')
  
  set.seed(1234)
  
  ## importance table
  
  plan('multisession')
  
  part_clust$importance_tbl <- sample(1:1000, size = 100, replace = FALSE) %>% 
    set_names(paste0('run_', 1:100)) %>% 
    future_map(~impact(part_clust$clust_obj, seed = .x), 
               .options = furrr_options(seed = TRUE)) %>% 
    map2_dfr(., names(.), ~mutate(.x, run = .y))
  
  plan('sequential')
  
  ## plotting
  
  part_clust$importance_plot <- part_clust$importance_tbl %>% 
    filter(variable != 'data') %>% 
    mutate(variable = translate_var(variable)) %>% 
    ggplot(aes(x = frac_diff, 
               y = reorder(variable, frac_diff))) +
    geom_vline(xintercept = 0, 
               linetype = 'dashed') + 
    geom_boxplot(fill = 'steelblue', 
                 outlier.color = NA, 
                 alpha = 0.25) + 
    geom_point(shape = 16, 
               color = 'black', 
               alpha = 0.15, 
               position = position_jitter(height = 0.1, width = 0)) + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Permutation importance of clustering variables',
         subtitle = '100 runs', 
         x = expression(Delta * ' clustering variance'))
  
# END -----
  
  insert_tail()