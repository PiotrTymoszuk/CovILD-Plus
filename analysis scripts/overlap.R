# Overlap between the ongoing symptoms, cardiovascular abnormality, fatigue, 
# low SMWD and mental health features

  insert_head()
  
# container list -------
  
  overlap <- list()
  
# globals -------
  
  insert_msg('Globals setup')
  
  ## numeric performance and mental health responses
  
  overlap$responses <- c('smwd_dref', 
                         'Chalder_FS', 
                         'EQ5DL_p', 
                         'EQ5DL_mobility', 
                         'EQ5DL_selfcare', 
                         'EQ5DL_activities', 
                         'EQ5DL_pain', 
                         'EQ5DL_anxiety', 
                         'Stress', 
                         'BRCS', 
                         'SSD12')
  
  ## explanatory variables
  
  overlap$variables <- c('sympt_present', 
                         'ct_severity_any', 
                         'lufo_red', 
                         'diastolic_dysf')
  
  ## analysis table
  
  overlap$analysis_tbl <- cov_data$data_tbl %>% 
    filter(time == 4) %>% 
    select(all_of(overlap$responses), 
           all_of(overlap$variables))
  
  ## plotting table with Z score values
  
  overlap$norm_data <- overlap$analysis_tbl %>% 
    map_dfc(function(x) if(is.numeric(x)) scale(x)[, 1] else x)

  plan('multisession')
  
# descriptive stats ------
  
  insert_msg('Descriptive stats')
  
  overlap$desc_stats <- set_names(overlap$variables, 
                                  overlap$variables) %>% 
    map(~explore(overlap$analysis_tbl %>% 
                   filter(!is.na(.data[[.x]])), 
                 split_factor = .x, 
                 variables = overlap$responses, 
                 what = 'table', 
                 pub_styled = TRUE)) %>% 
    map(reduce, left_join, by = 'variable') %>% 
    map(set_names, c('variable', 'absent', 'present'))
  
# testing: Mann-Whitney U test with r effect size statistic -----
  
  insert_msg('Testing')
  
  overlap$test_results <- set_names(overlap$variables, 
                                    overlap$variables) %>% 
    future_map(~compare_variables(overlap$analysis_tbl, 
                           split_factor = .x, 
                           variables = overlap$responses, 
                           what = 'eff_size', 
                           types = 'wilcoxon_r', 
                           ci = FALSE, 
                           pub_styled = TRUE, 
                           adj_method = 'BH'), 
               .options = furrr_options(seed = TRUE))
  
# significant factors -----
  
  insert_msg('Significant factors')
  
  overlap$signif_factors <- overlap$test_results %>% 
    map(filter, p_adjusted < 0.05) %>% 
    map(~.x$variable)
  
  
# Summary plots of effect sizes and significance -------
  
  insert_msg('Significance - effect size plots')
  
  overlap$etest_obj <- set_names(overlap$variables, 
                                 overlap$variables) %>% 
    future_map(~compare_variables(overlap$analysis_tbl, 
                                  split_factor = .x, 
                                  variables = overlap$responses, 
                                  what = 'eff_size', 
                                  types = 'wilcoxon_r', 
                                  ci = FALSE, 
                                  pub_styled = FALSE, 
                                  adj_method = 'BH'), 
               .options = furrr_options(seed = TRUE)) %>% 
    map(mutate, variable = translate_var(variable))
  
  overlap$summary_plots <- list(x = overlap$etest_obj, 
                                plot_title = c('Effects of persistent symptoms on physical and mental recovery', 
                                               'Effects of CT abnormality on physical and mental recovery', 
                                               'Effects of LFT abnormality on physical and mental recovery', 
                                               'Effects of diastolic dysfunction on physical and mental recovery')) %>% 
    pmap(plot, 
         cust_theme = globals$common_theme, 
         show_labels = 'signif') %>% 
    map(~.x + 
          geom_hline(yintercept = -log10(0.05), 
                     linetype = 'dashed') + 
          labs(x = 'Effect size, Wilcoxon r', 
               y = expression(-log[10]*' pFDR'), 
               tag = .x$labels$tag %>% 
                 paste0('\n', .)))
  
# Ribbon summary plots ------
  
  insert_msg('Ribbon panels')
  
  ## variable lexicon
  
  overlap$var_lexicons <- globals$var_lexicon %>% 
    mutate(label = stri_replace(label, fixed = ' (', replacement = '\n('), 
           label = stri_replace(label, fixed = 'Somatic symptom disorder', replacement = 'SSD'), 
           label = stri_replace(label, fixed = 'impairment', replacement = 'imp.'))
  
  overlap$var_lexicons <- overlap$test_results %>% 
    map(~.x[c('variable', 'significance')]) %>% 
    map(left_join, overlap$var_lexicons, by = 'variable') %>% 
    map(mutate, label = paste(label, significance, sep = '\n'))
  
  ## plots
  
  overlap$ribbon_panels <- list(split_factor = set_names(overlap$variables, 
                                                         overlap$variables), 
                                data = map(overlap$variables, 
                                           ~filter(overlap$norm_data, !is.na(.data[[.x]]))), 
                                plot_title = c('Persistent symptoms', 
                                               'CT abnormality', 
                                               'LFT abnormality', 
                                               'Diastolic dysfunction'), 
                                plot_tag = map(overlap$summary_plots, 
                                               ~.x$labels$tag)) %>% 
    pmap(draw_stat_panel, 
         variables = overlap$responses, 
         stat = 'mean', 
         err_stat = '2se', 
         form = 'line', 
         cust_theme = globals$common_theme, 
         scale = 'width', 
         point_alpha = 0.05, 
         x_lab = 'Z score') %>% 
    map2(., 
         c('Persistent\nsymptoms', 
           'LFT\nabnormality', 
           'CT\nabnormality', 
           'Diastolic\ndysfunction'), 
         ~.x + 
           scale_fill_manual(values = c('steelblue', 'coral3'), 
                             labels = c('absent', 'present'), 
                             name = .y) + 
           scale_color_manual(values = c('steelblue', 'coral3'), 
                              labels = c('absent', 'present'),  
                              name = .y)) %>% 
    map2(., 
         overlap$var_lexicons, 
         ~.x + 
           scale_y_discrete(labels = translate_var(overlap$responses, 
                                                   dict = .y), 
                            limits = overlap$responses))
  
  ## converting to polar plots
  
  overlap$ribbon_panels <- overlap$ribbon_panels %>% 
    map(~.x + 
          coord_polar(theta = 'y', clip = 'off') + 
          scale_x_continuous(limits = c(-0.8, 0.8), 
                             breaks = seq(-0.6, 0.6, by = 0.2)) + 
          theme(axis.title = element_blank(), 
                axis.text.y = element_blank(), 
                axis.line = element_blank(), 
                axis.ticks = element_blank()) + 
          labs(subtitle = 'Z score, mean \u00B1 2\u00D7SEM'))
  
  ## adding the scale labs
  
  for(i in seq(-0.6, 0.6, by = 0.2)) {
    
    overlap$ribbon_panels <- overlap$ribbon_panels %>% 
      map(~.x + 
            annotate('text', 
                     label = round(i, 1), 
                     size = 2.75, 
                     y = 0.5, 
                     x = i, 
                     color = 'gray60'))
    
  }

# END ----
  
  plan('sequential')
  
  rm(i)
  
  insert_tail()
  