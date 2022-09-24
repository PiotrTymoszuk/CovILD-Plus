# Multivariate modeling, LASSO/elastic net.

  insert_head()
  
# container list ------
  
  multi_mod <- list()
  
# globals: model formulas -------
  
  insert_msg('Globals setup')
  
  multi_mod$responses <- globals$mod_features %>% 
    filter(response == 'yes') %>% 
    mutate(metric = ifelse(type == 'gaussian', 'RMSE', 'Kappa'), 
           family = ifelse(type == 'gaussian', 'gaussian', 'binomial'))

  multi_mod$variables <- globals$mod_features %>% 
    filter(response == 'no') %>% 
    .$variable
  
  ## formulas
  
  multi_mod$formulas <- multi_mod$responses$variable %>% 
    paste0('~', paste(multi_mod$variables, collapse = '+')) %>% 
    map(as.formula) %>% 
    set_names(multi_mod$responses$variable)
  
  ## analysis data without NA's
  
  multi_mod$analysis_tbl <- multi_mod$responses$variable %>% 
    map(~cov_data$mod_table[c(.x, multi_mod$variables)]) %>% 
    map(~filter(.x, complete.cases(.x)))
  
  ## X and Y vectors
  
  multi_mod$analysis_x <- multi_mod$analysis_tbl %>% 
    map(~model.matrix(~., .x[multi_mod$variables])) %>% 
    set_names(multi_mod$responses$variable)
  
  multi_mod$analysis_y <- map2(multi_mod$responses$variable, 
                               multi_mod$analysis_tbl, 
                               ~.y[[.x]]) %>% 
    set_names(multi_mod$responses$variable)
  
  ## n numbers for the factor levels
  
  multi_mod$level_n <- multi_mod$analysis_x %>% 
    map(as.data.frame) %>% 
    map(select, - `(Intercept)`) %>% 
    map(~map_dbl(.x, sum))
  
  multi_mod$level_n <- multi_mod$level_n %>% 
    map(~tibble(parameter = names(.x), 
                n = .x))
  
# finding the optimal lambda values by repeated 10-fold CV ------
  
  insert_msg('Lambda finding')
  
  set.seed(1234)
  
  plan('multisession')
  
  multi_mod$lambdas <- 1:100 %>% 
    future_map(function(rep) list(x = multi_mod$analysis_x, 
                                  y = multi_mod$analysis_y, 
                                  family = multi_mod$responses$family) %>% 
                 pmap(cv.glmnet, 
                      nfolds = 10, 
                      alpha = 1), 
               .options = furrr_options(seed = TRUE)) %>% 
    set_names(paste0('rep_', 1:100))
  
  plan('sequential')
  
  multi_mod$lambdas <- multi_mod$lambdas %>% 
    transpose %>% 
    map(~map_dfr(.x, ~tibble(lambda = .x$lambda.min, 
                             deviance = .x$cvm[.x$index[1, 1]])))
  
  multi_mod$opt_lambda <- multi_mod$lambdas %>% 
    map(~filter(.x, deviance == min(deviance))) %>% 
    map(~.x$lambda)
  
# construction of the multi-parameter models ------
  
  insert_msg('Multi-parameter model construction and cross-validation')
  
  registerDoParallel(cores = 7)
  
  set.seed(1234)
  
  multi_mod$models <- list(form = multi_mod$formulas, 
                           data = multi_mod$analysis_tbl, 
                           metric = multi_mod$responses$metric, 
                           family = multi_mod$responses$family, 
                           lambda = multi_mod$opt_lambda) %>% 
    pmap(function(form, 
                  data, 
                  metric, 
                  family, 
                  lambda) train(form = form, 
                                data = data, 
                                family = family, 
                                method = 'glmnet', 
                                trControl = trainControl(method = 'cv', 
                                                         number = 10, 
                                                         returnData = TRUE, 
                                                         savePredictions = 'final', 
                                                         returnResamp = 'final', 
                                                         classProbs = TRUE), 
                                tuneGrid = data.frame(alpha = 1, 
                                                      lambda = lambda)))

  stopImplicitCluster()
  
  multi_mod$models <- multi_mod$models %>% 
    map(as_caretx)
  
# Characteristic of the models: fit stats and assumption check ------
  
  insert_msg('Model characteristic')
  
  ### fit stats
  
  multi_mod$fit_stats <- multi_mod$models %>% 
    map(summary)
  
  ## model assumptions
  
  multi_mod$diagn_plots <- multi_mod$models %>% 
    map(plot, type = 'diagnostic')
  
# Model estimates -------
  
  insert_msg('Model estimates')
  
  multi_mod$summary <- map2(multi_mod$models, 
                            multi_mod$opt_lambda, 
                            ~coef(.x$finalModel, s = .y)) %>% 
    map(as.matrix) %>% 
    map(as.data.frame) %>% 
    map(rownames_to_column, 'parameter') %>% 
    map(filter, s1 != 0) %>% 
    map(mutate, 
        variable = stri_extract(parameter, 
                                regex = paste(multi_mod$variables, 
                                              collapse = '|')), 
        level = stri_replace(parameter, 
                             regex = paste(multi_mod$variables, 
                                           collapse = '|'), 
                             replacement = ''), 
        var_lab = translate_var(variable), 
        var_lab = ifelse(level %in% c('yes', ''), 
                         var_lab, 
                         paste(var_lab, level, sep = ': ')), 
        var_lab = ifelse(level == '', 
                         paste0(var_lab, '/item'), 
                         var_lab), 
        var_lab = paste(var_lab, 
                        translate_var(variable, 
                                      out_value = 'ref_code', 
                                      dict = globals$mod_features), 
                        sep = '\n'), 
        regulation = ifelse(s1 < 0, 'negative', 'positive'))
  
  multi_mod$summary <- map2(multi_mod$summary, 
                            multi_mod$responses$family, 
                            ~mutate(.x, 
                                    estimate = if(.y == 'binomial') exp(s1) else s1)) %>% 
    map(as_tibble)
  
  ## appending the summary table with the n numbers per levels
  
  multi_mod$summary <- map2(multi_mod$summary, 
                            multi_mod$level_n, 
                            left_join, by = 'parameter') %>% 
    map(mutate, 
        var_lab = ifelse(level != '', 
                         paste(var_lab, n, sep = ', n = '), 
                         var_lab))
  
# Forest plots with the non-zero estimates ------

  insert_msg('Forest plots with the non-tero estimates')
  
  multi_mod$forest_plots <- multi_mod$summary %>% 
    map(filter, parameter != '(Intercept)') %>% 
    map(~ggplot(.x, 
                aes(x = estimate, 
                    y = reorder(var_lab, estimate), 
                    fill = regulation, 
                    size = abs(s1))) + 
          geom_point(shape = 21) + 
          geom_text(aes(label = signif(estimate, 2)), 
                    size = 2.75, 
                    hjust = 0.5, 
                    vjust = -1.4) + 
          scale_fill_manual(values = c('negative' = 'steelblue', 
                                       'positive' = 'indianred3')) + 
          guides(size = 'none', 
                 fill = 'none') + 
          globals$common_theme + 
          theme(axis.title.y = element_blank()))
  
  ## plot adjustment
  
  multi_mod$forest_plots <- 
    list(plot = multi_mod$forest_plots, 
         plot_title = c('CT abnormality, one-year follow-up', 
                        'LFT abnormality, one-year follow-up', 
                        'Diastolic dysfunction, one-year follow-up', 
                        'Symptoms present, one-year follow-up'), 
         n = map(multi_mod$analysis_tbl, nrow)) %>% 
    pmap(function(plot, plot_title, n) plot + 
           labs(title = plot_title, 
                x = expression('OR'[LASSO]), 
                tag = paste('n =', n)) + 
           geom_vline(xintercept = 1, 
                      linetype = 'dashed'))
  
# ROC plots -------
  
  insert_msg('ROC plots')

  multi_mod$roc_plots <- 
    list(x = multi_mod$models, 
         line_color = c('firebrick4', 
                        'coral3', 
                        'steelblue3', 
                        'darkolivegreen4'), 
         plot_title = c('CT abnormality, one-year follow-up', 
                        'LFT abnormality, one-year follow-up', 
                        'Diastolic dysfunction, one-year follow-up', 
                        'Symptoms present, one-year follow-up')) %>% 
    pmap(plot, 
         type = 'roc', 
         cust_theme = globals$common_theme, 
         labels = FALSE, 
         point_size = 0, 
         annotation_x = 0.45) %>% 
    map(~map(.x, 
             ~.x + 
               labs(subtitle = .x$labels$subtitle %>% 
                      stri_replace(fixed = 'sq', replacement = '\u00B2') %>% 
                      stri_replace(fixed = 'Kappa', replacement = '\u03BA'))))
  
# END -----
  
  insert_tail()