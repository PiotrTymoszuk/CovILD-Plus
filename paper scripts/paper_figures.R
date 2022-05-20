# This script generates paper figures. Figure 1 is the CONSORT plot.

  insert_head()
  
# data containers -----
  
  paper_figures <- list()
  
# Figure 1: CONSORT plot ------
  
  insert_msg('Figure 1: consort plot')
  
  paper_figures$consort <- plot_grid(ggdraw() + 
                                       draw_image('./input data/Consort.png')) %>% 
    as_figure('figure_1_consort', 
              w = 90, 
              h = 110)
  
# Figure 2: symptom presence kinetic and particular symptoms at the 1-year FUP ------
  
  insert_msg('Figure 2: symptoms')
  
  paper_figures$symptoms <- long_prev$plots[c('Cohort', 
                                              'Ambulatory', 
                                              'Moderate', 
                                              'Severe')] %>% 
    map(~.x$sympt_present) %>% 
    combine_plots(y_cust_range = c(0, 107), 
                  common_legend = 'hide', 
                  ncol = 1) %>% 
    plot_grid(plot_grid(prev_sum$plots$symptoms + 
                          guides(fill = FALSE), 
                        ggdraw(), 
                        nrow = 2, 
                        rel_heights = c(0.85, 0.15)), 
              ., 
              ncol = 2, 
              rel_widths = c(0.55, 0.45), 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_2_symptoms', 
              w = 180, 
              h = 210)
  
# Figure 3: LFT abnormalities kinetic and particular LFT features at the 1-year FUP ------
  
  insert_msg('Figure 3: LFT')
  
  paper_figures$lft <- long_prev$plots[c('Cohort', 
                                         'Ambulatory', 
                                         'Moderate', 
                                         'Severe')] %>% 
    map(~.x$lufo_red) %>% 
    combine_plots(y_cust_range = c(0, 70), 
                  common_legend = 'hide', 
                  ncol = 1) %>%
    plot_grid(plot_grid(prev_sum$plots$pulmo + 
                          guides(fill = FALSE), 
                        ggdraw(), 
                        nrow = 2, 
                        rel_heights = c(0.5, 0.5)), 
              ., 
              ncol = 2, 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_3_lft', 
              w = 180, 
              h = 210)
  
# Figure 4: CT recovery-------
  
  insert_msg('Figure 4: CT recovery')
  
  paper_figures$ct <- long_prev$plots[c('Cohort', 
                                        'Ambulatory', 
                                        'Moderate', 
                                        'Severe')] %>% 
    map(~.x$ct_severity_any) %>% 
    combine_plots(y_cust_range = c(0, 110), 
                  common_legend = 'hide', 
                  ncol = 2) %>% 
    plot_grid(plot_grid(prev_sum$plots$ct +
                          guides(fill = FALSE), 
                        ggdraw(), 
                        ncol = 2, 
                        rel_widths = c(0.7, 0.3)), 
              ., 
              nrow = 2, 
              rel_heights = c(0.35, 0.65), 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_4_ct', 
              w = 180, 
              h = 180)
  
# Figure 5: Cardio recovery -------
  
  insert_msg('Figure 4: Cardio recovery')
  
  paper_figures$cardio <- long_prev$plots[c('Cohort', 
                                            'Ambulatory', 
                                            'Moderate', 
                                            'Severe')] %>% 
    map(~.x$diastolic_dysf) %>% 
    combine_plots(y_cust_range = c(0, 100), 
                  common_legend = 'hide', 
                  ncol = 2) %>% 
    plot_grid(plot_grid(prev_sum$plots$cardio + 
                          guides(fill = FALSE), 
                        ggdraw(), 
                        ncol = 2), 
              ., 
              nrow = 2, 
              rel_heights = c(0.35, 0.65), 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_5_cardio', 
              w = 180, 
              h = 180)
  
# Figure 6: correlations ------  
  
  insert_msg('Figure 6: correlations')
  
  paper_figures$correlations <- correl$bubble_plot$cohort + 
    theme(plot.subtitle = element_blank(), 
          legend.position = 'none') + 
    labs(title = 'Symptoms, respiratory and psychosocial features')
  
  paper_figures$correlations <- paper_figures$correlations %>% 
    as_figure(label = 'figure_6_correlations', 
              w = 180, 
              h = 195)
  
# Figure 7: recovery clusters --------
  
  insert_msg('Figure 7: Recovery clusters')
  
  paper_figures$clusters$upper_panel <- plot_grid(part_clust$heat_map_ft + 
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
  
  paper_figures$clusters$signif_fct <- clust_chara$test_results %>% 
    filter(p_adjusted < 0.05, 
           variable %in% globals$clust_variables) %>% 
    .$variable
  
  paper_figures$clusters$lower_panel <- plot_grid(part_clust$ribbon_plots$CP + 
                                                    scale_y_discrete(labels = function(x) embolden_scale(x, 
                                                                                             highlight = paper_figures$clusters$signif_fct, 
                                                                                             translate = TRUE), 
                                                                     limits = rev(c('ct_severity_any', 
                                                                                    'diastolic_dysf', 
                                                                                    'lufo_red'))) + 
                                                    theme(legend.position = 'none', 
                                                          axis.text.y = element_markdown()), 
                                                  get_legend(part_clust$ribbon_plots[[1]]), 
                                                  part_clust$ribbon_plots$Clinical + 
                                                    scale_y_discrete(labels = function(x) embolden_scale(x, 
                                                                                                         highlight = paper_figures$clusters$signif_fct, 
                                                                                                         translate = TRUE), 
                                                                     limits = rev(c('smwd_low', 
                                                                                   'sympt_present', 
                                                                                   'dyspnoe_sympt', 
                                                                                   'cough_sympt', 
                                                                                   'fatigue_sympt', 
                                                                                   'Chalder_FS_bimodal', 
                                                                                   'sleep_sympt', 
                                                                                   'night_sweat_sympt', 
                                                                                   'anosmia_sympt'))) + 
                                                    theme(legend.position = 'none', 
                                                          axis.text.y = element_markdown()), 
                                                  part_clust$ribbon_plots$Psychosocial + 
                                                    scale_y_discrete(labels = function(x) embolden_scale(x, 
                                                                                                         highlight = paper_figures$clusters$signif_fct, 
                                                                                                         translate = TRUE), 
                                                                     limits = rev(c('Stress_hi', 
                                                                                    'EQ5DL_low', 
                                                                                    'EQ5DL_activities_bi', 
                                                                                    'EQ5DL_anxiety_bi', 
                                                                                    'EQ5DL_pain_bi', 
                                                                                    'EQ5DL_mobility_bi', 
                                                                                    'EQ5DL_selfcare_bi'))) + 
                                                    theme(legend.position = 'none', 
                                                          axis.text.y = element_markdown()), 
                                                  nrow = 2, 
                                                  rel_heights = c(1, 2), 
                                                  align = 'hv', 
                                                  axis = 'tblr')

  paper_figures$clusters <- plot_grid(paper_figures$clusters$upper_panel, 
                                      paper_figures$clusters$lower_panel, 
                                      nrow = 2, 
                                      labels = LETTERS, 
                                      label_size = 10, 
                                      rel_heights = c(0.4, 0.6)) %>% 
    as_figure(label = 'figure_7_clusters', 
              w = 180, 
              h = 220)
  
# Figure 8: psychosocial recovery in the clusters -----
  
  insert_msg('Figure 8: psychosocial recovery in the clusters')
  
  paper_figures$clust_psych <- clust_chara$plots[c('EQ5DL_p', 
                                                   'EQ5DL_activities', 
                                                   'EQ5DL_anxiety', 
                                                   'EQ5DL_pain', 
                                                   'Chalder_FS', 
                                                   'Stress',
                                                   'SSD12', 
                                                   'BRCS')] %>% 
    map(~.x + 
          theme(legend.position = 'none', 
                plot.title.position = 'plot', 
                plot.subtitle = element_text(hjust = 0.2)) + 
          labs(tag = .x$labels$tag %>% 
                 stri_replace_all(fixed = '\n', replacement = ', ') %>% 
                 paste0('\n', .), 
               title = stri_replace(.x$labels$title, 
                                    regex = '\\(.*\\)', 
                                    replacement = ''))) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_8_clust_psychosoc', 
              w = 180, 
              h = 210)
  
# Saving the figures on the disc -----
  
  insert_msg('Saving the figures on the disc')
  
  paper_figures %>% 
    walk(save_figure, 
         path = './paper/figures', 
         device = cairo_pdf)
  
# END -----
  
  insert_tail()