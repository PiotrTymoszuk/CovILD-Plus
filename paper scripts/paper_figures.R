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
              h = 90 * 2241/1488, 
              ref_name = 'consort', 
              caption = 'Flow diagram of the study analysis inclusion.')
  
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
                          guides(fill = 'none'), 
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
              h = 210, 
              ref_name = 'symptoms', 
              caption = 'COVID-19 symptom recovery.')
  
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
                          guides(fill = 'none'), 
                        ggdraw(), 
                        nrow = 2, 
                        rel_heights = c(0.5, 0.5)), 
              ., 
              ncol = 2, 
              labels = LETTERS, 
              label_size = 10, 
              rel_widths = c(0.53, 0.47)) %>% 
    as_figure(label = 'figure_3_lft', 
              w = 180, 
              h = 210, 
              ref_name = 'lft', 
              caption = 'Functional lung recovery.')
  
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
                          guides(fill = 'none'), 
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
              h = 180, 
              ref_name = 'ct', 
              caption = 'Radiological lung recovery.')
  
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
                          guides(fill = 'none'), 
                        ggdraw(), 
                        ncol = 2), 
              ., 
              nrow = 2, 
              rel_heights = c(0.35, 0.65), 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_5_cardio', 
              w = 180, 
              h = 180, 
              ref_name = 'cardio', 
              caption = 'Cardiological recovery.')
  
# Figure 6: correlations ------  
  
  insert_msg('Figure 6: correlations')
  
  paper_figures$correlations <- correl$bubble_plot$cohort + 
    theme(plot.tag = element_blank(), 
          legend.position = 'none') + 
    labs(title = 'Symptoms, respiratory and psychosocial features', 
         subtitle = paste("Kendall's \u03C4 B,", 
                          correl$bubble_plot$cohort$labels$tag))
  
  paper_figures$correlations <- paper_figures$correlations %>% 
    as_figure(label = 'figure_6_correlations', 
              w = 180, 
              h = 195, 
              ref_name = 'correlations', 
              caption = 'Correlation of symptoms, physical performance, cardiopulmonary findings, mental health and quality of life at the one-year follow-up.')
  
# Figure 7: recovery clusters --------
  
  insert_msg('Figure 7: Recovery clusters')
  
  paper_figures$clusters <- 
    plot_grid(part_clust$ribbon_plots$CP + 
                theme(legend.position = 'none'), 
              get_legend(part_clust$ribbon_plots[[1]]), 
              ggdraw(), 
              ggdraw(), 
              part_clust$ribbon_plots$Clinical + 
                theme(legend.position = 'none'), 
              part_clust$ribbon_plots$Psychosocial + 
                theme(legend.position = 'none'), 
              nrow = 3, 
              rel_heights = c(1, 0.1, 2), 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('A', '', 'B', 'C', ''), 
              label_size = 10) %>% 
    as_figure(label = 'figure_7_clusters', 
              w = 180, 
              h = 200, 
              ref_name = 'clusters', 
              caption = 'COVID-19 recovery clusters.')
  
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
               x = 'Recovery cluster', 
               title = stri_replace(.x$labels$title, 
                                    regex = '\\(.*\\)', 
                                    replacement = ''))) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_8_clust_psychosoc', 
              w = 180, 
              h = 210, 
              ref_name = 'clust_psych', 
              caption = 'Quality of life, fatigue and mental health rating in the COVID-19 recovery clusters.')
  
# Saving the figures on the disc -----
  
  insert_msg('Saving the figures on the disc')
  
  paper_figures %>% 
    walk(pickle, 
         path = './paper/figures', 
         format = 'pdf', 
         device = cairo_pdf)
  
  paper_figures %>% 
    walk(pickle, 
         path = './paper/figures eps', 
         format = 'eps', 
         device = cairo_ps)
  
# END -----
  
  insert_tail()