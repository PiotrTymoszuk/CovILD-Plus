# Finding the optimal clustering procedure. The input is a UMAP object.

  insert_head()
  
# container list ------
  
  cl_devel <- list()
  
# globals: min/max normalized analysis table ------
  
  insert_msg('Globals setup')
  
  cl_devel$analysis_tbl <- cov_data$clust_tbl %>% 
    map_dfc(function(x) if(is.factor(x)) as.numeric(x) else x) %>% 
    column_to_rownames('ID') %>% 
    min_max(complete_cases = TRUE)
  
  cl_devel$dist_stats <- c('smc', 'tanimoto')

# Candidate clustering objects ------
  
  insert_msg('Candidate clustering solutions')

  ## hierarchical clustering

  cl_devel$test_algos$hcl[cl_devel$dist_stats] <- cl_devel$dist_stats %>% 
    map(~hcluster(cl_devel$analysis_tbl , 
                  distance_method = .x, 
                  k = 3, 
                  hc_method = 'ward.D2', 
                  seed = 1234))

  ## PAM
  
  cl_devel$test_algos$pam[cl_devel$dist_stats] <- cl_devel$dist_stats %>% 
    map(~kcluster(cl_devel$analysis_tbl, 
                  distance_method = .x, 
                  k = 3, 
                  clust_fun = 'pam', 
                  seed = 1234))
  
  ## SOM + HCL
  
  cl_devel$test_algos[['SOM+HCL']][cl_devel$dist_stats] <- cl_devel$dist_stats %>% 
    map(~combi_cluster(cl_devel$analysis_tbl, 
                       distance_som = .x, 
                       xdim = 5, 
                       ydim = 4, 
                       topo = 'hexagonal', 
                       neighbourhood.fct = 'gaussian', 
                       toroidal = FALSE, 
                       rlen = 1000, 
                       node_clust_fun = hcluster, 
                       k = 3, 
                       distance_nodes = 'euclidean', 
                       seed = 1234))
  
  ## SOM + PAM
  
  cl_devel$test_algos[['SOM+PAM']][cl_devel$dist_stats] <- cl_devel$dist_stats %>% 
    map(~combi_cluster(cl_devel$analysis_tbl, 
                       distance_som = .x, 
                       xdim = 5, 
                       ydim = 4, 
                       topo = 'hexagonal', 
                       neighbourhood.fct = 'gaussian', 
                       toroidal = FALSE, 
                       rlen = 1000, 
                       node_clust_fun = kcluster,
                       clust_fun = 'pam', 
                       k = 3, 
                       distance_nodes = 'euclidean', 
                       seed = 1234))

  cl_devel$test_algos <- unlist(cl_devel$test_algos, recursive = FALSE)
  
# Checking the clustering variances and cross-validation errors ------
  
  insert_msg('Clustering varinces and cross-validation errors')
  
  cl_devel$variances <- cl_devel$test_algos %>% 
    map(var) %>% 
    map(~.x[c('total_wss', 'total_ss', 'between_ss', 'frac_var')]) %>% 
    map_dfr(as_tibble) %>% 
    mutate(algorithm = names(cl_devel$test_algos))
  
  cl_devel$cv <- cl_devel$test_algos %>% 
    map(cv, nfolds = 10) %>% 
    map_dfr(~.x$summary) %>% 
    mutate(algorithm = names(cl_devel$test_algos))

# Plotting the comparison results ------
  
  insert_msg('Plotting the comparison results')
  
  cl_devel$test_results <- left_join(cl_devel$variances[c('algorithm', 'frac_var')], 
                                     cl_devel$cv[c('algorithm', 'mean_error')], 
                                     by = 'algorithm') %>% 
    mutate(correct_rate = 1 - mean_error)
  
  cl_devel$test_plot <- cl_devel$test_results %>% 
    select(- mean_error) %>% 
    gather(key = 'stat', 
           value = 'value', 
           frac_var, 
           correct_rate) %>% 
    mutate(algorithm = paste(toupper(stri_split_fixed(algorithm, 
                                                      pattern = '.', 
                                                      simplify = TRUE)[, 1]), 
                             stri_split_fixed(algorithm, 
                                              pattern = '.', 
                                              simplify = TRUE)[, 2], 
                             sep = ', '), 
           algorithm = stri_replace(algorithm, 
                                    fixed = '.', 
                                    replacement = ', '), 
           algorithm = stri_replace(algorithm, 
                                    fixed = 'smc', 
                                    replacement = 'SMD'), 
           algorithm = stri_replace(algorithm, 
                                    fixed = 'tanimoto', 
                                    replacement = 'Tanimoto')) %>% 
    ggplot(aes(x = value, 
               y = reorder(algorithm, value), 
               fill = stat)) + 
    geom_bar(stat = 'identity', 
             color = 'black', 
             position = position_dodge(0.9)) + 
    scale_fill_manual(values = c('correct_rate' = 'steelblue', 
                                 'frac_var' = 'coral3'), 
                      labels = c('correct_rate' = 'CV, correct rate', 
                                 'frac_var' = 'Clustering variance'), 
                      name = '') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Clustering algorithm comparison', 
         subtitle = 'Clustering variance and 10-fold CV', 
         x = 'Statistic value')
  
# END ----
  
  insert_tail()