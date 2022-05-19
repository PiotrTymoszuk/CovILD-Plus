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
    map(~map_dfr(.x, summary, type = 'assumptions'))
  
  ## fit stats
  
  uni_mod$fit_stats <- uni_mod$models %>% 
    map(~map_dfr(.x, summary, type = 'fit') %>% 
          mutate(variable = uni_mod$variables))
  
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
                               plot_title = c('CT abnormality, 1 year', 
                                              'LFT abnormality, 1 year', 
                                              'Diastolic dysfunction, 1 year', 
                                              'Symptoms present, 1 year')) %>% 
    pmap(safely(plot_forest), 
         cust_theme = globals$common_theme, 
         x_lab = 'OR, 95% CI') %>% 
    map(~.x$result)
  
# END -----
  
  insert_tail()