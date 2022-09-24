# Compares course of symptoms, CT, LFT and TTE abnormalities between 
# the participants included and excluded in the analysis 
# (one-year visit completed) till the 6-month follow-up
# Because the data of the excluded individuals are per se incomplete
# standard hypothesis testing (chi-squared test for trend) is used instead of
# mixed-effect modeling

  insert_head()
  
# container list ------
  
  drop_course <- list()
  
# globals -----
  
  insert_msg('Globals setup')
  
  drop_course$variables <- c('sympt_present', 
                             'ct_severity_any', 
                             'lufo_red', 
                             'diastolic_dysf')
  
  drop_course$analysis_tbl <- cov_data$raw_data %>% 
    filter(time %in% 1:3) %>% 
    mutate(included = ifelse(ID %in% cov_data$year_complete, 
                             'included', 'excluded'), 
           included = factor(included, c('excluded', 'included')), 
           time = car::recode(time, 
                              "1 = '2 months'; 
                              2 = '3 months'; 
                              3 = '6 months'"), 
           time = factor(time)) %>% 
    select(ID, time, included, all_of(drop_course$variables))
  
# descriptive stats -----
  
  insert_msg('Descriptive stats')
  
  drop_course$desc_stats <- drop_course$analysis_tbl %>% 
    dlply('included') %>% 
    map(explore, 
        split_factor = 'time', 
        variables = drop_course$variables, 
        what = 'table', 
        pub_styled = TRUE) %>% 
    map(reduce, left_join, by = 'variable') %>% 
    map(set_names, c('variable', paste(c(2, 3, 6), 'months')))
  
# Testing for differences in time for the included and excluded participants -----
  
  insert_msg('Testing for the time effect')
  
  drop_course$test_time <- drop_course$analysis_tbl %>% 
    dlply('included') %>% 
    map(compare_variables, 
        split_factor = 'time', 
        variables = drop_course$variables, 
        what = 'eff_size', 
        types = 'cramer_v',
        ci = FALSE, 
        pub_styled = TRUE, 
        adj_method = 'BH') %>% 
    map(mutate, plot_cap = paste(eff_size, significance, sep = ', '))

# Testing for differences between the included/excluded participants for single time points -----
  
  insert_msg('Testing for the analysis inclusion effects')
  
  drop_course$test_inclusion <- drop_course$analysis_tbl %>% 
    dlply('time') %>% 
    map(compare_variables, 
        split_factor = 'included', 
        variables = drop_course$variables, 
        what = 'eff_size', 
        types = 'cramer_v', 
        ci = FALSE, 
        pub_styled = TRUE, 
        adj_method = 'BH') %>% 
    map(mutate, plot_cap = paste(eff_size, significance, sep = ', '))
  
# Plotting tables with the frequences per time point and inclusion status ------
  
  insert_msg('Plotting tables')
  
  drop_course$plot_tbl <- set_names(drop_course$variables, 
                                    drop_course$variables) %>% 
    map(function(var) drop_course$analysis_tbl %>% 
          group_by(included, time) %>% 
          count(.data[[var]]) %>% 
          filter(!is.na(.data[[var]])) %>% 
          mutate(percent = n/sum(n) * 100, 
                 n_complete = sum(n), 
                 plot_lab = signif(percent, 2), 
                 axis_lab = paste0(included, ', n = ', n_complete)) %>% 
          filter(.data[[var]] == 'yes') %>% 
          ungroup)
  
# Bar plots with the percentages of individuals with symptoms or CP findings ----
  
  insert_msg('Plotting')

  drop_course$plots <- 
    map2(drop_course$plot_tbl, 
         translate_var(names(drop_course$plot_tbl)), 
         ~ggplot(.x, 
                 aes(x = percent, 
                     y = reorder(axis_lab, -as.numeric(time)), 
                     fill = included)) + 
           geom_bar(stat = 'identity', 
                    color = 'black') + 
           geom_text(aes(label = plot_lab), 
                     color = 'white', 
                     hjust = 1.5, 
                     size = 2.75) + 
           scale_fill_manual(values = c(excluded = 'steelblue', 
                                        included = 'darkolivegreen4'), 
                             name = 'Analysis') + 
           facet_grid(time ~ .,
                      scales = 'free', 
                      space = 'free') + 
           globals$common_theme + 
           theme(axis.title.y = element_blank()) + 
           labs(x = '% of strata', 
                title = .y, 
                subtitle = 'Consecutive follow-ups after COVID-19 diagnosis'))

# END -----
  
  insert_tail()