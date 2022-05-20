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
              h = 210)
  
# Figure S2: recovery from selected symptoms ------
  
  insert_msg('Figure S2: recovery from selected symptoms')
  
  suppl_figures$symptoms2$left_panel <- long_prev$plots[c('Cohort', 
                                                          'Ambulatory', 
                                                          'Moderate', 
                                                          'Severe')] %>% 
    map(~.x$night_sweat_sympt) %>% 
    combine_plots(common_legend = 'hide', 
                  y_cust_range = c(0, 90), 
                  ncol = 1)
  
  suppl_figures$symptoms2$middle_panel <- long_prev$plots[c('Cohort', 
                                                            'Ambulatory', 
                                                            'Moderate', 
                                                            'Severe')] %>% 
    map(~.x$cough_sympt) %>% 
    combine_plots(common_legend = 'hide', 
                  y_cust_range = c(0, 90), 
                  ncol = 1)
  
  suppl_figures$symptoms2$right_panel <- long_prev$plots[c('Cohort', 
                                                           'Ambulatory', 
                                                           'Moderate', 
                                                           'Severe')] %>% 
    map(~.x$anosmia_sympt) %>% 
    combine_plots(common_legend = 'hide', 
                  y_cust_range = c(0, 65), 
                  ncol = 1)
  
  suppl_figures$symptoms2 <- plot_grid(suppl_figures$symptoms2$left_panel, 
                                       suppl_figures$symptoms2$middle_panel, 
                                       suppl_figures$symptoms2$right_panel, 
                                       ncol = 3, 
                                       labels = LETTERS, 
                                       label_size = 10) %>% 
    as_figure(label = 'figure_s2_sweat_cough_anosmia', 
              w = 180, 
              h = 210)  
  
# Figure S3: recovery of the LFT parameters ----
  
  insert_msg('Figure S3: LFT recovery')
  
  suppl_figures$lft$left_panel <- long_num$plots[c('Cohort', 
                                                   'Ambulatory', 
                                                   'Moderate', 
                                                   'Severe')] %>% 
    map(~.x$FEV1_p) %>% 
    combine_plots(common_legend = 'hide', 
                  ncol = 1)
  
  suppl_figures$lft$middle_panel <- long_num$plots[c('Cohort', 
                                                     'Ambulatory', 
                                                     'Moderate', 
                                                     'Severe')] %>% 
    map(~.x$FVC_p) %>% 
    combine_plots(common_legend = 'hide', 
                  ncol = 1)
  
  suppl_figures$lft$right_panel <- long_num$plots[c('Cohort', 
                                                    'Ambulatory', 
                                                    'Moderate', 
                                                    'Severe')] %>% 
    map(~.x$DLCO_p) %>% 
    combine_plots(common_legend = 'hide', 
                  ncol = 1)
  
  suppl_figures$lft <- plot_grid(suppl_figures$lft$left_panel, 
                                 suppl_figures$lft$middle_panel, 
                                 suppl_figures$lft$right_panel, 
                                 ncol = 3, 
                                 labels = LETTERS, 
                                 label_size = 10) %>% 
    as_figure(label = 'figure_s3_lft_recovery', 
              w = 180, 
              h = 210)
  
# Figure S4: clustering QC ------
  
  insert_msg('Figure S4: Clustering QC')

  suppl_figures$clust_qc <- plot_grid(cl_devel$test_plot + 
                                        theme(plot.subtitle = element_blank(), 
                                              legend.position = 'bottom'), 
                                      part_clust$diagnostic_plots$wss + 
                                        theme(plot.tag = element_blank(), 
                                              plot.subtitle = element_blank()), 
                                      ncol = 2, 
                                      align = 'hv', 
                                      axis = 'tblr', 
                                      labels = LETTERS, 
                                      label_size = 10) %>% 
    as_figure(label = 'figure_s4_cluster_qc', 
              w = 180, 
              h = 100)
  
# Figure S5: other clustering features ------
  
  insert_msg('Figure S5: other clustering features')
  
  suppl_figures$clust_extra <- clust_chara$plots[c('cat_WHO', 'sex', 
                                                   'weight_class', 'age', 
                                                   'ECOG', 'smwd_dref')] %>% 
    map(~.x + 
          theme(plot.title.position = 'plot') + 
          labs(tag = .x$labels$tag %>% 
                 stri_replace_all(fixed = '\n', replacement = ', ') %>% 
                 paste0('\n', .), 
               x = 'Cluster'))
  
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
    as_figure(label = 'figure_s5_extra_clust_features', 
              w = 180, 
              h = 210)

# Figure S6 - S9: multivariate modeling -------
  
  insert_msg('Figure S6 - 9: multi-modeling')
  
  suppl_figures[c('sympt_risk', 
                  'lft_risk', 
                  'ct_risk', 
                  'dysf_risk')] <- list(roc_plot_list = multi_mod$roc_plots[c('sympt_present', 
                                                                              'lufo_red', 
                                                                              'ct_severity_any', 
                                                                              'diastolic_dysf')], 
                                        lasso_forest_plot = multi_mod$forest_plots[c('sympt_present', 
                                                                                     'lufo_red', 
                                                                                     'ct_severity_any', 
                                                                                     'diastolic_dysf')], 
                                        lasso_rel_h = list(c(0.75, 0.25), 
                                                           c(0.6, 0.4), 
                                                           c(0.95, 0.05), 
                                                           c(0.98, 0.02))) %>% 
    pmap(make_mod_panel)
  
  suppl_figures[c('sympt_risk', 
                  'lft_risk', 
                  'ct_risk', 
                  'dysf_risk')] <- map2(suppl_figures[c('sympt_risk', 
                                                        'lft_risk', 
                                                        'ct_risk', 
                                                        'dysf_risk')], 
                                        c('figure_s6_symptom_risk', 
                                          'figure_s7_lft_risk', 
                                          'figure_s8_ct_risk', 
                                          'figure_s9_dysf_risk'), 
                                        as_figure, 
                                        w = 180, 
                                        h = 200)
  
# Saving the figures on the disc ----
  
  insert_msg('Saving the figures')
  
  suppl_figures %>% 
    walk(save_figure, 
         path = './paper/supplementary figures/', 
         device = cairo_pdf)
  
# END -----
  
  insert_tail()