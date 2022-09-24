# Univariate modeling of the key responses at the 1-year visit

  insert_head()
  
# container list ------
  
  uni_mod <- list()
  
# globals: modeling list ------
  
  insert_msg('Globals setup')
  
  uni_mod$responses <- globals$mod_features %>% 
    filter(response == 'yes') %>% 
    select(variable, type)
  
  uni_mod$variables <- globals$mod_features %>% 
    filter(response == 'no') %>% 
    .$variable
  
  uni_mod$plot_titles <- c('CT abnormality, one-year follow-up', 
                           'LFT abnormality, one-year follow-up', 
                           'Diastolic dysfunction, one-year follow-up', 
                           'Symptoms present, one-year follow-up')

# serial modeling -------
  
  insert_msg('Serial modeling')
  
  uni_mod$models <- uni_mod$responses %>% 
    pmap(function(variable, type) uni_mod$variables %>% 
           map(~make_lm(data = cov_data$mod_table, 
                        response = variable, 
                        indep_variable = .x, 
                        mod_fun = switch(type, 
                                         logistic = glm, 
                                         gaussian = lm, 
                                         ordinal = polr), 
                        family = switch(type, 
                                        logistic = 'binomial', 
                                        gaussian = NULL, 
                                        ordinal = NULL))) %>% 
           set_names(uni_mod$variables)) %>% 
    set_names(uni_mod$responses$variable)
  
# Inference, assumptions and fit stats -----
  
  insert_msg('Inference summary')
  
  ## inference
  
  uni_mod$summary <- list(model = uni_mod$models, 
                          trans = uni_mod$responses$type) %>% 
    pmap(function(model, trans) model %>% 
           map_dfr(summary, 
                   type = 'inference', 
                   transf_fun = switch(trans, 
                                       logistic = exp, 
                                       gaussian = identity, 
                                       ordinal = exp))) %>% 
    map(mutate, p_adjusted = p.adjust(p_value, 'BH'))
  
  ## assumptions
  
  uni_mod$assumptions <- uni_mod$models %>% 
    map(~map2_dfr(.x, names(.x), 
                  ~summary(.x, type = 'assumptions') %>% 
                    mutate(variable = .y))) %>% 
    map2(., names(.), ~mutate(.x, response = .y))
  
  ## fit stats
  
  uni_mod$fit_stats <- uni_mod$models %>% 
    map(~map_dfr(.x, summary, type = 'fit') %>% 
          mutate(variable = uni_mod$variables, 
                 rsq_size = cut(raw_rsq, 
                                c(-Inf, 0.01, 0.09, 0.25, Inf), 
                                c('none', 'small', 'moderate', 'large'))))
  
# Identification of the significant factors ------
  
  insert_msg('Significant factors')
  
  uni_mod$signif_fct <- uni_mod$summary %>% 
    map(filter, 
        p_adjusted < 0.05, 
        parameter != '(Intercept)') %>% 
    map(~.x$variable) %>% 
    map(unique)

# plotting: Forest plots -------
  
  insert_msg('Forest plots with the significantly regulated factors')
  
  uni_mod$forest_plots <- map2(uni_mod$summary, 
                               uni_mod$signif_fct,
                               ~filter(.x, variable %in% .y)) %>% 
    map(mutate, 
        variable = translate_var(variable, dict = globals$mod_features), 
        level = ifelse(level == 'yes', 'present', level))
  
  uni_mod$forest_plots <- list(x = uni_mod$forest_plots, 
                               plot_title = uni_mod$plot_titles) %>% 
    pmap(safely(plot_forest), 
         cust_theme = globals$common_theme, 
         x_lab = 'OR, 95% CI', 
         cutpoint = 1) %>% 
    map(~.x$result)
  
# plotting of R-squares ------
  
  insert_msg('R squared plots')
  
  uni_mod$rsq_plots <- map2(map(uni_mod$fit_stats, 
                                top_n, n = 20, raw_rsq), 
                            uni_mod$plot_titles, 
                            ~ggplot(.x, 
                                    aes(x = raw_rsq, 
                                        y = reorder(variable, raw_rsq), 
                                        fill = rsq_size)) + 
                              geom_bar(stat = 'identity', 
                                       color = 'black') + 
                              geom_vline(xintercept = 0.01, 
                                         linetype = 'dashed') + 
                              geom_vline(xintercept = 0.09, 
                                         linetype = 'dashed') + 
                              geom_vline(xintercept = 0.25, 
                                         linetype = 'dashed') + 
                              scale_fill_manual(values = c(none = 'gray60', 
                                                           small = 'cornsilk', 
                                                           moderate = 'coral3', 
                                                           large = 'firebrick4'), 
                                                name = 'Effect size') + 
                              scale_y_discrete(labels = translate_var(uni_mod$variables, 
                                                                      dict = globals$mod_features)) + 
                              expand_limits(x = 0.45) + 
                              globals$common_theme + 
                              theme(axis.title.y = element_blank()) + 
                              labs(title = .y, 
                                   subtitle = 'top 20 highest R\u00B2', 
                                   x = expression('unadjusted R'^2)))
  
# END -----
  
  insert_tail()