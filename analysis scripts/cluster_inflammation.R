# Differences in inflammatory markers in the recovery clusters at the early and  
# 12 month follow-up

  insert_head()
  
# container list ------
  
  clust_infl <- list()
  
# globals -----
  
  insert_msg('Globals setup')
  
  clust_infl$variables <- c('IL6', 'CRP', 'DDimer', 'anemia', 'FT_elv')
  
  ## analysis table
  
  clust_infl$analysis_tbl <- cov_data$data_tbl %>% 
    filter(time %in% c(1, 4)) %>% 
    select(ID, time, all_of(clust_infl$variables)) %>% 
    left_join(part_clust$clust_obj$clust_assignment %>% 
                set_names(c('ID', 'clust_id')), 
              by = 'ID') %>% 
    filter(!is.na(clust_id)) %>% 
    dlply('time', as_tibble) %>% 
    set_names(c('fup_2', 'fup_5'))
  
# descriptive stats -----
  
  insert_msg('Descriptive stats')
  
  clust_infl$desc_stats <- clust_infl$analysis_tbl %>% 
    map(explore, 
        split_factor = 'clust_id', 
        variables = clust_infl$variables, 
        what = 'table', 
        pub_styled = TRUE) %>% 
    map(reduce, left_join, by = 'variable') %>% 
    map(set_names, c('variable', paste0('clust_', 1:3)))
  
# testing: non-parametric (inflammatory markers are skewed) -----
  
  insert_msg('Testing')
  
  clust_infl$test_results <- clust_infl$analysis_tbl %>% 
    map(compare_variables, 
        split_factor = 'clust_id', 
        variables = clust_infl$variables, 
        what = 'test', 
        types = c(rep('kruskal_test', 3), 
                  rep('chisq_test', 2)), 
        ci = FALSE, 
        adj_method = 'BH', 
        pub_styled = TRUE) %>% 
    map(mutate, plot_cap = paste(eff_size, significance, sep = ', '))
  
# Plotting ------
  
  insert_msg('Plotting')
  
  clust_infl$plots <- list(x = clust_infl$analysis_tbl, 
                           y = clust_infl$test_results, 
                           z = c('2 months', '12 months')) %>% 
    pmap(function(x, y, z) list(variable = clust_infl$variables, 
                                plot_title = paste(translate_var(clust_infl$variables), 
                                                   z, sep = ', '), 
                                plot_subtitle = y$plot_cap, 
                                type = c(rep('violin', 3), 
                                         rep('stack', 2))) %>% 
           pmap(plot_variable, 
                x, 
                split_factor = 'clust_id', 
                scale = 'percent', 
                cust_theme = globals$common_theme, 
                x_lab = 'Recovery cluster') %>% 
           map(~.x + 
                 labs(tag = .x$labels$tag %>% 
                        stri_replace_all(fixed = '\n', replacement = ', ') %>% 
                        paste0('\n', .))) %>% 
           set_names(clust_infl$variables)) %>% 
    unlist(recursive = FALSE)
  
  ## adjustments
  
  clust_infl$plots[c('fup_2.IL6', 'fup_2.CRP', 'fup_2.DDimer', 
                     'fup_5.IL6', 'fup_5.CRP', 'fup_5.DDimer')] <- 
    clust_infl$plots[c('fup_2.IL6', 'fup_2.CRP', 'fup_2.DDimer', 
                       'fup_5.IL6', 'fup_5.CRP', 'fup_5.DDimer')] %>% 
    map(~.x + 
          scale_fill_manual(values = globals$clust_colors, 
                            name = 'Recovery\ncluster'))
  
  clust_infl$plots[c('fup_2.anemia', 'fup_2.FT_elv', 
                     'fup_5.anemia', 'fup_5.FT_elv')] <- 
    clust_infl$plots[c('fup_2.anemia', 'fup_2.FT_elv', 
                       'fup_5.anemia', 'fup_5.FT_elv')] %>% 
    map(~.x + 
          scale_fill_manual(values = c(no = 'steelblue', 
                                       yes = 'coral3'),
                            name = ''))
  
# panels of the plots, for the author information only ------
  
  insert_msg('Plot panels')
  
  clust_infl$panels <- list(fup2 = c('fup_2.IL6', 'fup_2.CRP', 'fup_2.DDimer', 
                                     'fup_2.anemia', 'fup_2.FT_elv'), 
                            fup5 = c('fup_5.IL6', 'fup_5.CRP', 'fup_5.DDimer', 
                                     'fup_5.anemia', 'fup_5.FT_elv')) %>% 
    map(~clust_infl$plots[.x]) %>% 
    map(~map(.x, ~.x + theme(legend.position = 'none'))) %>% 
    map(~c(.x, list(get_legend(clust_infl$plots[[4]])))) %>% 
    map(~plot_grid(plotlist = .x, 
                   ncol = 3, 
                   align = 'hv', 
                   axis = 'tblr')) %>% 
    map2(., c('inflammation_2_months_clusters', 
              'inflammation_12_months_clusters'), 
         ~as_figure(.x, 
                    label = .y, 
                    w = 180, 
                    h = 150))
  
  clust_infl$panels %>% 
    walk(save_figure, 
         path = './for authors/', 
         format = 'pdf')
    
# END ----
  
  insert_tail()