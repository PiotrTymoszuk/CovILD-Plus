# Supplementary Figures -----

  insert_head()

# container list -----

  suppl_figures <- list()
  
# Figure S1: selected symptoms -----
  
  insert_msg('Figure S1: recovery from selected symptoms')

  suppl_figures$symptoms1$left_panel <- long_prev$plots[c('Cohort', 
                                                          'Ambulatory', 
                                                          'Moderate', 
                                                          'Severe')] %>% 
    map(~.x$fatigue_sympt) %>% 
    combine_plots(common_legend = 'hide', 
                  y_cust_range = c(0, 110), 
                  ncol = 1)
  
  suppl_figures$symptoms1$middle_panel <- long_prev$plots[c('Cohort', 
                                                            'Ambulatory', 
                                                            'Moderate', 
                                                            'Severe')] %>% 
    map(~.x$sleep_sympt) %>% 
    combine_plots(common_legend = 'hide', 
                  y_cust_range = c(0, 50), 
                  ncol = 1)
  
  suppl_figures$symptoms1$right_panel <- long_prev$plots[c('Cohort', 
                                                           'Ambulatory', 
                                                           'Moderate', 
                                                           'Severe')] %>% 
    map(~.x$dyspnoe_sympt) %>% 
    combine_plots(common_legend = 'hide', 
                  y_cust_range = c(0, 100), 
                  ncol = 1)
  
  suppl_figures$symptoms1 <- plot_grid(suppl_figures$symptoms1$left_panel, 
                                       suppl_figures$symptoms1$middle_panel, 
                                       suppl_figures$symptoms1$right_panel, 
                                       ncol = 3, 
                                       labels = LETTERS, 
                                       label_size = 10) %>% 
    as_figure(label = 'figure_s1_fatigue_sleep_dyspnoe', 
              w = 180, 
              h = 210, 
              ref_name = 'symptoms1', 
              caption = 'Recovery of fatigue, sleep problems and dyspnea.')
  
# Figure S2 - S3: symptoms, cardiopulmonary findings, physical and mental recovery -----
  
  insert_msg('Figures S2 - S3')
  
  suppl_figures[c('phys_mental_recovery1', 
                  'phys_mental_recovery2')] <-
    list(overlap$ribbon_panels[c(1, 3)], 
         overlap$ribbon_panels[c(2, 4)]) %>% 
    map(~map(.x, ~.x + theme(legend.position = 'none'))) %>% 
    map(~plot_grid(plotlist = .x, 
                   nrow = 2, 
                   align = 'hv', 
                   labels = LETTERS, 
                   label_size = 10))
  
  suppl_figures$phys_mental_recovery1 <- 
    plot_grid(suppl_figures$phys_mental_recovery1, 
              plot_grid(get_legend(overlap$ribbon_panels[[1]]), 
                        get_legend(overlap$ribbon_panels[[3]]), 
                        nrow = 2, 
                        align = 'hv', 
                        axis = 'tblr'), 
              ncol = 2, 
              rel_widths = c(0.85, 0.15))
  
  suppl_figures$phys_mental_recovery2 <- 
    plot_grid(suppl_figures$phys_mental_recovery2, 
              plot_grid(get_legend(overlap$ribbon_panels[[2]]), 
                        get_legend(overlap$ribbon_panels[[4]]), 
                        nrow = 2, 
                        align = 'hv', 
                        axis = 'tblr'), 
              ncol = 2, 
              rel_widths = c(0.85, 0.15))
  
  
  suppl_figures[c('phys_mental_recovery1', 
                  'phys_mental_recovery2')] <- 
    list(x = suppl_figures[c('phys_mental_recovery1', 
                             'phys_mental_recovery2')], 
         label = c('figure_s2_sympt_lft_recovery', 
                   'figure_s3_ct_cardio_recovery'), 
         ref_name = c('overlap1', 
                      'overlap2'), 
         caption = c('Impact of persistent symptoms and LFT abnormality on physical and mental recovery from COVID-19.', 
                     'Impact of lung CT and cardiological abnormality on physical and mental recovery from COVID-19.')) %>% 
    pmap(as_figure, 
         w = 180, 
         h = 220)
  
# Figure S4: clustering QC and importance------
  
  insert_msg('Figure S4: Clustering QC')
  
  ## top panel: clustering goodness and WSS curve

  suppl_figures$clust_qc$top_panel <- 
    plot_grid(cl_devel$test_plot + 
                theme(plot.subtitle = element_blank(), 
                      legend.position = 'bottom'), 
              part_clust$diagnostic_plots$wss + 
                theme(plot.tag = element_blank(), 
                      plot.subtitle = element_blank()), 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = LETTERS, 
              label_size = 10)
  
  ## complete figure
  
  suppl_figures$clust_qc <- 
    plot_grid(suppl_figures$clust_qc$top_panel, 
              part_clust$importance_plot + 
                coord_flip() + 
                theme(axis.text.x = element_text(angle = 90, 
                                                 hjust = 1, 
                                                 vjust = 0.5), 
                      axis.title.x = element_blank(), 
                      axis.title.y = element_text(size = 8, 
                                                  color = 'black', 
                                                  angle = 90)), 
              nrow = 2, 
              rel_heights = c(1, 2), 
              labels = c('', 'C'), 
              label_size = 10) %>% 
    as_figure(label = 'figure_s4_cluster_qc', 
              w = 180, 
              h = 210, 
              ref_name = 'clust_qc', 
              caption = 'Development of COVID-19 recovery clusters.')
  
# Figure S5: cluster heat map, CT finding rates and relapse rates in the clusters -----
  
  insert_msg('Figure S5: heath map, CT findings and  relapse rates in the clusters')
  
  ## upper panel: heat map
  
  suppl_figures$clust_relapse$upper_panel <- 
    plot_grid(part_clust$heat_map_ft + 
                labs(title = 'Recovery clusters') + 
                theme(legend.position = 'none', 
                      plot.tag = element_blank(), 
                      plot.subtitle = element_blank()), 
              plot_grid(get_legend(part_clust$heat_map_ft), 
                        ggdraw() + 
                          draw_text(part_clust$heat_map_ft$labels$tag, 
                                    size = 8, 
                                    hjust = 0), 
                        nrow = 2), 
              ncol = 2, 
              rel_widths = c(0.72, 0.25))
  
  ## middle panel: CT abnormality rate
  
  suppl_figures$clust_relapse$middle_panel <- 
    clust_rev$plots_ct_suppl %>% 
    map(~.x + theme(plot.title.position = 'plot')) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv')
  
  ## entire figure
  
  suppl_figures$clust_relapse <- 
    plot_grid(suppl_figures$clust_relapse$upper_panel,
              ggdraw(), 
              suppl_figures$clust_relapse$middle_panel, 
              clust_relapse$relapse_plot, 
              nrow = 4, 
              rel_heights = c(1.3, 0.1, 1, 1.3), 
              labels = c('A', 'B', '', 'C'), 
              label_size = 10) %>% 
    as_figure(label = 'figure_s5_ct_relapse_clusters', 
              w = 180, 
              h = 230, 
              ref_name = 'clust_relapse', 
              caption = 'Heat map of clustering features, frequency of CT abnormalities and rates of symptom relapse at the 12-month follow-up in the COVID-19 recovery clusters.')
    
  
# Figure S6: other clustering features ------
  
  insert_msg('Figure S6: other clustering features')
  
  suppl_figures$clust_extra <- 
    clust_chara$plots[c('age', 'sex', 'weight_class', 
                        'cat_WHO', 'smwd_dref', 'ECOG')] %>% 
    map(~.x + 
          theme(plot.title.position = 'plot') + 
          labs(tag = .x$labels$tag %>% 
                 stri_replace_all(fixed = '\n', replacement = ', ') %>% 
                 paste0('\n', .), 
               x = 'Recovery cluster'))
  
  suppl_figures$clust_extra[c('age', 'ECOG', 'smwd_dref')] <- 
    suppl_figures$clust_extra[c('age', 'ECOG', 'smwd_dref')] %>% 
    map(~.x + theme(legend.position = 'none'))
  
  suppl_figures$clust_extra$cat_WHO <- suppl_figures$clust_extra$cat_WHO + 
    scale_fill_manual(values = unname(globals$sev_colors))
  
  suppl_figures$clust_extra$sex <- suppl_figures$clust_extra$sex + 
    scale_fill_manual(values = c('steelblue', 'coral3'))
  
  suppl_figures$clust_extra$weight_class <- suppl_figures$clust_extra$weight_class +
    scale_fill_manual(values = c('steelblue', 'cornsilk2', 'coral3'))
  
  suppl_figures$clust_extra <- plot_grid(plotlist = suppl_figures$clust_extra, 
                                         ncol = 2, 
                                         align = 'hv', 
                                         axis = 'tblr') %>% 
    as_figure(label = 'figure_s6_extra_clust_features', 
              w = 180, 
              h = 210, 
              ref_name = 'clust_extra', 
              caption = 'Demographic features, COVID-19 severity, physical performance and mobility in the COVID-19 recovery clusters.')
  
# Figure S7 - S10: multivariate modeling -------
  
  insert_msg('Figure S7 - 10: multi-modeling')
  
  suppl_figures[c('sympt_risk', 
                  'lft_risk', 
                  'ct_risk', 
                  'dysf_risk')] <- 
    list(roc_plot_list = multi_mod$roc_plots[c('sympt_present', 
                                               'lufo_red', 
                                               'ct_severity_any', 
                                               'diastolic_dysf')], 
         lasso_forest_plot = multi_mod$forest_plots[c('sympt_present', 
                                                      'lufo_red', 
                                                      'ct_severity_any', 
                                                      'diastolic_dysf')], 
         lasso_rel_h = list(c(0.8, 0.2), 
                            c(0.6, 0.4), 
                            c(1, 0), 
                            c(1, 0)), 
         roc_rel_h = list(c(1, 0), 
                          c(1, 0), 
                          c(0.9, 0.1), 
                          c(0.7, 0.3))) %>% 
    pmap(make_mod_panel)
  
  suppl_figures[c('sympt_risk', 
                  'lft_risk', 
                  'ct_risk', 
                  'dysf_risk')] <- 
    list(x = suppl_figures[c('sympt_risk', 
                             'lft_risk', 
                             'ct_risk', 
                             'dysf_risk')], 
         label = c('figure_s7_symptom_risk', 
                   'figure_s8_lft_risk', 
                   'figure_s9_ct_risk', 
                   'figure_s10_dysf_risk'), 
         ref_name = c('sympt_risk', 
                      'lft_risk', 
                      'ct_risk', 
                      'dysf_risk'), 
         caption = c('Modeling of the persistent symptom risk at the 1-year post-COVID-19 follow-up.',
                     'Modeling of the persistent functional lung abnormality at the 1-year post-COVID-19 follow-up.', 
                     'Modeling of the persistent radiological lung abnormality at the 1-year post-COVID-19 follow-up.', 
                     'Modeling of the persistent diastolic dysfunction at the 1-year post-COVID-19 follow-up.'), 
         h = c(170, 
               170, 
               180, 
               220)) %>% 
    pmap(as_figure, 
         w = 180)

# Saving the figures on the disc ----
  
  insert_msg('Saving the figures')
  
  suppl_figures %>% 
    walk(pickle, 
         path = './paper/supplementary figures/', 
         device = cairo_pdf)
  
# END -----
  
  insert_tail()