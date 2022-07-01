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
              h = 210, 
              ref_name = 'symptoms2', 
              caption = 'Recovery from night sweating, cough and smell disorders.')  
  
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
              h = 210, 
              ref_name = 'lft', 
              caption = 'Changes in FEV1, FVC and DLCO during COVID-19 convalescence.')
  
# Figure S4 - S5: symptoms, cardiopulmonary findings,physical and mental recovery -----
  
  insert_msg('Figures S4 - S5')
  
  suppl_figures[c('phys_mental_recovery1', 
                  'phys_mental_recovery2')] <- list(overlap$ribbon_panels[1:2], 
                                                    overlap$ribbon_panels[3:4]) %>% 
    map(~map(.x, ~.x + theme(legend.position = 'none'))) %>% 
    map(~plot_grid(plotlist = .x, 
                   nrow = 2, 
                   align = 'hv', 
                   labels = LETTERS, 
                   label_size = 10) %>% 
          plot_grid(get_legend(overlap$ribbon_panels[[1]]), 
                    ncol = 2, 
                    rel_widths = c(0.85, 0.15))) %>% 
    list(x = ., 
         label = c('figure_s4_sympt_lft_recovery', 
                   'figure_s5_ct_cardio_recovery'), 
         ref_name = c('overlap1', 
                      'overlap2'), 
         caption = c('Impact of persistent symptoms and LFT abnormality on physical and mental recovery from COVID-19.', 
                     'Impact of lung CT and cardiological abnormality on physical and mental recovery from COVID-19.')) %>% 
    pmap(as_figure, 
         w = 180, 
         h = 220)
  
# Figure S6: clustering QC ------
  
  insert_msg('Figure S6: Clustering QC')

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
    as_figure(label = 'figure_s6_cluster_qc', 
              w = 180, 
              h = 100, 
              ref_name = 'clust_qc', 
              caption = 'Development of COVID-19 recovery clusters.')
  
# Figure S7: symptom relapse rates in the clusters -----
  
  insert_msg('Figure S7: Symptom relapse rates in the clusters')
  
  suppl_figures$clust_relapse <- plot_grid(clust_relapse$relapse_plot) %>% 
    as_figure(label = 'figure_s7_late_relapse_clusters', 
              w = 180, 
              h = 120, 
              ref_name = 'clust_relapse', 
              caption = 'Rates of symptom relapse at the 12-month follow-up in the COVID-19 recovery clusters.')
  
# Figure S8: other clustering features ------
  
  insert_msg('Figure S8: other clustering features')
  
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
    as_figure(label = 'figure_s8_extra_clust_features', 
              w = 180, 
              h = 210, 
              ref_name = 'clust_extra', 
              caption = 'COVID-19 severity, demographic features, physical performance and mobility in the COVID-19 recovery clusters.')

# Figure S9 - S10 : R-squares for univariate models ------
  
  insert_msg('Figure S9 and S10: R-squared for univariate models')
  
  suppl_figures[c('uni_mod_rsq1', 
                  'uni_mod_rsq2')] <- 
    list(uni_mod$rsq_plots[c('sympt_present', 
                             'lufo_red')], 
         uni_mod$rsq_plots[c('ct_severity_any', 
                             'diastolic_dysf')]) %>% 
    map(~map(.x, ~.x + theme(legend.position = 'none'))) %>% 
    map(~plot_grid(plotlist = .x,
                   ncol = 2, 
                   align = 'hv', 
                   labels = LETTERS, 
                   label_size = 10) %>% 
          plot_grid(get_legend(uni_mod$rsq_plots$diastolic_dysf + 
                                 theme(legend.position = 'bottom')), 
                    nrow = 2, 
                    rel_heights = c(0.9, 0.1))) %>% 
    list(x = ., 
         label = c('figure_s9_uni_modeling_rsq_sympt_ltf', 
                   'figure_s10_uni_modeling_rsq_ct_cardio'), 
         ref_name = c('uni_mod_rsq1', 
                      'uni_mod_rsq2'), 
         caption = c('Fractions of persistent symptom and LFT abnormality risk variance explained by independent variables.', 
                     'Fractions of CT abnormality and diastolic dysfunction risk variance explained by independent variables.')) %>% 
    pmap(as_figure, 
         w = 180, 
         h = 220)

# Figure S11 - S14: multivariate modeling -------
  
  insert_msg('Figure S10 - 13: multi-modeling')
  
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
                  'dysf_risk')] <- 
    list(x = suppl_figures[c('sympt_risk', 
                             'lft_risk', 
                             'ct_risk', 
                             'dysf_risk')], 
         label = c('figure_s11_symptom_risk', 
                   'figure_s12_lft_risk', 
                   'figure_s13_ct_risk', 
                   'figure_s14_dysf_risk'), 
         ref_name = c('sympt_risk', 
                      'lft_risk', 
                      'ct_risk', 
                      'dysf_risk'), 
         caption = c('Modeling of the persistent symptom risk at the 1-year post-COVID-19 follow-up.',
                     'Modeling of the persistent functional lung abnormality at the 1-year post-COVID-19 follow-up.', 
                     'Modeling of the persistent radiological lung abnormality at the 1-year post-COVID-19 follow-up.', 
                     'Modeling of the persistent diastolic dysfunction at the 1-year post-COVID-19 follow-up.')) %>% 
    pmap(as_figure, 
         w = 180, 
         h = 200)

# Saving the figures on the disc ----
  
  insert_msg('Saving the figures')
  
  suppl_figures %>% 
    walk(pickle, 
         path = './paper/supplementary figures/', 
         device = cairo_pdf)
  
# END -----
  
  insert_tail()