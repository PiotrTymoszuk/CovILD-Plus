# Tables and figures for the reviewer -------

  insert_head()

# containers ------

  rev_tables <- list()
  rev_figures <- list()
  
# Table 1: baseline features of the steroid treatment strata ------
  
  insert_msg('Table 1: characteristic of the steroid treatment strata')
  
  rev_tables$steroid_base <- 
    right_join(steroid_base$desc_stats, 
               steroid_base$test_results %>% 
                 filter(p_adjusted < 0.1) %>% 
                 select(variable, significance, eff_size), 
               by = 'variable') %>% 
    mutate(variable = factor(variable, 
                             c('age', 'sex', 'weight_class', 
                               'comorb_present', 'no_comorb', 
                               'cardiovascular_comorb', 
                               'hypertension_comorb', 
                               'cat_WHO'))) %>% 
    arrange(variable) %>% 
    format_summ_tbl %>% 
    rbind(tibble(variable = 'N, participants', 
                 no_steroids = count(steroid_base$analysis_tbl, 
                                     treat_steroids)$n[1], 
                 steroids = count(steroid_base$analysis_tbl, 
                                  treat_steroids)$n[2], 
                 significance = NA, 
                 eff_size = NA), .) %>% 
    mutate(variable = stri_replace(variable, 
                                   regex = ',\\s{1}%$', 
                                   replacement = '')) %>% 
    set_names(c('Variable', 'No steroids', 
                'Steroids', 'Significance', 'Effect size')) %>% 
    mdtable(label = 'rev_table_1_steroid_base', 
            ref_name = 'steroid_base', 
            caption = 'Significant and near-significant differences in baseline demographic and clinical characteristic of participants with and without post-acute steroid therapy.')
  
# Figure 1: clustering variable importance and CT abnormalities in severity groups and clusters -----
  
  insert_msg('Figure 1: clustering variable importance, CT abnormalities')
  
  rev_figures$clust <- clust_rev$plots_ct %>% 
    map(~.x + theme(plot.title.position = 'plot')) %>% 
    plot_grid(plotlist = ., 
              nrow = 3, 
              align = 'hv') %>% 
    plot_grid(ggdraw(), 
              ggdraw(), 
              part_clust$importance_plot + 
                theme(plot.title.position = 'plot'), 
              ., 
              ncol = 2, 
              rel_heights = c(0.025, 0.975), 
              rel_widths = c(0.6, 0.4), 
              labels = c('A', 'B', '', ''), 
              label_size = 10) %>% 
    as_figure(label = 'figure_1_clustering', 
              ref_name = 'clust', 
              caption = 'Clustering factor importance and frequency of lung CT abnormalities in ambulatory, moderate and severe COVID-19 patients in the recovery clusters.', 
              w = 180, 
              h = 180)
  
# Figure for reviewers 2 - 4: steroids, symptoms and cardiopulmonary abnormalities at one-year FUP -----
  
  insert_msg('Figure 2 - 4: steroids, symptoms and CP abnormalities at one-year FUP.')
  
  rev_figures$steroid_symptoms <- steroid_fup$plots_year %>% 
    map(~.x$symptoms + 
          theme(strip.text = element_text(size = 6), 
                legend.position = 'bottom')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_2_steroids_symptoms', 
              ref_name = 'steroid_symptoms', 
              caption = 'Post-acute steroid therapy of moderate and severe COVID-19 and persistent symptoms at the one-year follow-up.', 
              w = 180, 
              h = 240)
  
  rev_figures$steroid_lufo <- steroid_fup$plots_year %>% 
    map(~.x$pulmo + 
          theme(legend.position = 'bottom')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_3_steroids_lufo', 
              ref_name = 'steroid_lufo', 
              caption = 'Post-acute steroid therapy of moderate and severe COVID-19 and LFT abnormalities at the one-year follow-up.', 
              w = 180, 
              h = 160)
  
  rev_figures$steroid_ct <- steroid_fup$plots_year %>% 
    map(~.x$ct + 
          theme(legend.position = 'bottom', 
                strip.text = element_text(size = 6))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_4_steroids_ct', 
              ref_name = 'steroid_ct', 
              caption = 'Post-acute steroid therapy of moderate and severe COVID-19 and lung CT abnormalities at the one-year follow-up.', 
              w = 180, 
              h = 100)
  
# Figure for reviewers 5: steroids and cluster assignment ------
  
  insert_msg('Figure 5: steroids and cluster assignment')
  
  rev_figures$steroid_cluster <- clust_rev$plot_steroids %>% 
    as_figure(label = 'figure_5_steroids_clusters', 
              ref_name = 'steroid_cluster', 
              caption = 'Frequency of post-acute steroid therapy in the COVID-19 recovery clusters.', 
              w = 90, 
              h = 70)
  
# Saving the figures on the disc ------
  
  insert_msg('Saving the figures')
  
  rev_figures %>% 
    walk(pickle, 
         format = 'pdf', 
         device = cairo_pdf, 
         path = './paper/figures reviewer')
  
# END -----
  
  insert_head()