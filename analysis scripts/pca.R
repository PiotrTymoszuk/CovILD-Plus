# PCA and investigation of the clustering tendency in respect to the clustering 
# variables specified in globals. The one-year time point

  insert_head()
  
# container list -----
  
  pca <- list()
  
# globals: analysis table with min/max-normalized variables -----
  
  insert_msg('Globals setup')
  
  pca$analysis_tbl <- cov_data$clust_tbl %>% 
    map_dfc(function(x) if(is.factor(x)) as.numeric(x) else x) %>% 
    column_to_rownames('ID') %>% 
    min_max(complete_cases = TRUE)
  
# Determining the number of dimensions -----
  
  insert_msg('Dimension number')
  
  pca$dim_num_pca <- reduce_data(data = pca$analysis_tbl, 
                                 kdim = 8, 
                                 red_fun = 'pca')
  
  pca$dim_num_scree <- plot(pca$dim_num_pca, 
                            type = 'scree', 
                            cust_theme = globals$common_theme)
  
  ## it looks that 5 components explain > 80% of the variance
  
# 5d PCA -----
  
  insert_msg('PCA')
  
  pca$pca_obj <- reduce_data(data = pca$analysis_tbl, 
                             kdim = 5, 
                             red_fun = 'pca')
  
  pca$pca_obj$loadings <- pca$pca_obj$loadings %>% 
    mutate(variable = translate_var(variable))
  
  ## score and loadings plots
  
  pca$pca_score_plot <- plot(pca$pca_obj, 
                         type = 'scores', 
                         cust_theme = globals$common_theme)
  
  pca$pca_loadings_plot <- plot(pca$pca_obj, 
                            type = 'loadings',
                            cust_theme = globals$common_theme)

# UMAP -----
  
  insert_msg('2d UMAP')
  
  pca$umap_obj <- reduce_data(data = pca$analysis_tbl, 
                              distance_method = 'euclidean', 
                              kdim = 2, 
                              red_fun = 'umap')
  
  ## score plot
  
  pca$umap_score_plot <- plot(pca$umap_obj, 
                              type = 'scores', 
                              cust_theme = globals$common_theme)
  
# clustering tendencies of the data set, PCA and UMAP scores -----
  
  insert_msg('Clustering tendency')
  
  pca$clust_tend <- list(data = pca$analysis_tbl, 
                         pca = pca$pca_obj$component_tbl, 
                         umap = pca$umap_obj$component_tbl)
  
  pca$clust_tend[c('pca', 'umap')] <- pca$clust_tend[c('pca', 'umap')] %>% 
    map(column_to_rownames, 'observation')
  
  pca$clust_tend <- pca$clust_tend %>% 
    map(get_clust_tendency, 
        n = 35)

# END -----
  
  insert_tail()