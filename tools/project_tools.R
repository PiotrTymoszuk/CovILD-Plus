# A medley of functional project tools

  require(plyr)
  require(tidyverse)
  require(caret)
  require(doParallel)
  require(randomForest)
  require(grDevices)
  require(pcaPP)
  require(rlang)
  require(cowplot)
  require(glue)
  require(ggtext)
  
# data import and transformation -----
  
  recode_var <- function(data, 
                         old_var, 
                         new_var, 
                         ID_var = 'ID', 
                         time_var = NULL, 
                         trans_fun = 'my_identity', 
                         args = NULL) {
    
    ## renames a variable, if time and id variable names provided, they're included
    ## in  the output as well
    ## the transformation function enables
    ## args specifies the arguments to the transforming function
    
    trans_call <- call2(trans_fun, 
                        data[[old_var]], 
                        !!!args)

    if(!is.null(time_var)) {
      
      data %>% 
        transmute(!!ID_var := .data[[ID_var]], 
                  !!new_var := eval(trans_call), 
                  !!time_var := .data[[time_var]])
      
    } else {
      
      data %>% 
        transmute(!!ID_var := .data[[ID_var]], 
                  !!new_var := eval(trans_call))
      
    }
    
  }
  
  recode_vec <- function(vector, recodes, ...) {
    
    return(car::recode(vector, 
                       recodes = recodes, 
                       ...))
    
  }
  
  recode_yn <- function(vector, reverse = FALSE, as.factor = TRUE, ...) {
    
    ## recodes a 0/1 vector no/yes or the other way round
    
    if(reverse) {
      
      new_vec <- recode_vec(vector, 
                            "'yes' = 1;
                             'no' = 0",
                            as.factor = as.factor)
      
      #if(!as_factor) {
        
       # new_vec <- as.numeric(new_vec)
        
      #}
      
    } else {
      
      new_vec <- recode_vec(vector, 
                            "1 = 'yes';
                             0 = 'no'", 
                            as.factor = as.factor)
      
    }
    
    #if(as_factor) {
      
     # new_vec <- factor(new_vec)
      
    #}
    
    return(new_vec)
    
  }
  
  binarize <- function(vector, cutoff, labels = "'yes';'no'") {
    
    return(cut(vector, 
               c(-Inf, cutoff, Inf), 
               unlist(stri_split_fixed(labels, ';'))))
    
    
  }
  
  my_identity <- function(x, ...) {
    
    identity(x)
    
  }
  
  my_factor <- function(x, levels, ...) {
    
    factor(x, levels =  unlist(stri_split_fixed(levels, ';')))
    
  }
  
  outer_rbind <- function(tbl1, tbl2) {
    
    ## binds two data frames by rows, missing variables are filled with NA
    
    ## missing variables
    
    miss1 <- names(tbl2)[!names(tbl2) %in% names(tbl1)]
    miss2 <- names(tbl1)[!names(tbl1) %in% names(tbl2)]
    
    ## filling the tables
    
    for(i in miss1){
      
      tbl1 <- tbl1 %>% 
        mutate(!!sym(i) := NA)
      
    }
    
    for(i in miss2){
      
      tbl2 <- tbl2 %>% 
        mutate(!!sym(i) := NA)
      
    }
    
    return(rbind(tbl1, tbl2))
    
  }
  
  set_constant <- function(data, variable, reference, ID_var = 'ID', time_var = 'time') {
    
    ## takes the variable values from the reference time point and sets them accordingly 
    ## for the remaining time points
    
    ref_tbl <- data %>% ## table with the reference tables and IDs
      filter(.data[[time_var]] == reference) %>% 
      select(all_of(c(ID_var, variable)))
    
    remain_tbl <- data %>% 
      select(- !!ensym(variable))
    
    updated_tbl <- left_join(remain_tbl, 
                             ref_tbl, 
                             by = ID_var)
    
    return(updated_tbl)
    
  }
  
# displaying and formatting kinetic modeling results ----

  combine_plots <- function(plotlist, 
                            common_legend = c('no', 'yes', 'hide'), 
                            y_cust_range = NULL, ...) {
    
    ## combines plots sets a common scale
    
    ## common scale ranges
    
    plot_data <- plotlist %>% 
      map(~.x$data)

    if(is.null(y_cust_range)) {
      
      y_quo <- plotlist %>% 
        map(~.x$mapping$y)
      
      y_scale_range <- map2(y_quo, 
                            plot_data, 
                            eval_tidy) %>% 
        range
      
    } else {
      
      y_scale_range <- y_cust_range
      
    }
    
    ## plot panel
    
    plot_list <- plotlist %>% 
      map(function(x) x + 
            scale_y_continuous(limits = y_scale_range))
    
    common_legend <- match.arg(common_legend, 
                               choices = c('no', 'yes', 'hide'))
    
    if(common_legend == 'no') {
      
      return(plot_grid(plotlist = plot_list, ...))
      
    } else if(common_legend == 'yes') {
      
      return(plot_list %>% 
               map(function(x) x + theme(legend.position = 'none')) %>% 
               plot_grid(plotlist = ., ...) %>% 
               plot_grid(., 
                         get_legend(plot_list[[1]]), 
                         ncol = 2, 
                         rel_widths = c(0.9, 0.1)))
      
    } else {
      
      return(plot_list %>% 
               map(function(x) x + theme(legend.position = 'none')) %>% 
               plot_grid(plotlist = ., ...))
      
    }
    
  }

# variable:label translation, color setup -----
  
  translate_var <- function(variable, 
                            key = 'variable', 
                            out_value = 'label', 
                            dict = globals$var_lexicon, 
                            unit = FALSE) {
    
    naming_vec <- dict[[out_value]]

    if(unit) {
      
      naming_vec <- ifelse(is.na(dict[['unit']]), 
                           naming_vec, 
                           paste(naming_vec, dict[['unit']], sep = ', '))
      
    }
    
    naming_vec <- set_names(naming_vec, 
                            dict[[key]])
    
    return(naming_vec[variable])
    
  }
  
  set_colors_ <- function(color_no, seed = 123) {
    
    ## picks n colors at random from the standard palette
    
    set.seed(seed)
    
    return(colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)] %>% 
             sample(size = color_no))
    
  }
  
# kinetic plotting and modeling ------
  
  plot_kinet <- function(data, 
                         plot_var = 'DLCO_p', 
                         time_var = 'time_numeric', 
                         y_lab = 'DLCO, %', 
                         plot_title = NULL, 
                         plot_subtitle = NULL, 
                         fill_color = 'firebrick', 
                         x_lab = 'Months post COVID-19') {
    
    
    ## kinetic line plot
    
    ## median table
    
    med_tbl <- data %>% 
      dlply(time_var) %>% 
      map(explore, 
          variables = plot_var) %>% 
      map(~.x[[1]][['statistic']]) %>% 
      map2_dfr(., names(.), 
               ~tibble(!!time_var := as.numeric(.y), 
                       median = .x[['value']][3], 
                       perc_25 = .x[['value']][4],
                       perc_75 = .x[['value']][5], 
                       ID = '1'))
    
    n_numbers <- data %>% 
      filter(.data[[time_var]] == min(.data[[time_var]])) %>% 
      nrow
    
    ## plot
    
    data %>% 
      ggplot(aes(x = .data[[time_var]], 
                 y = .data[[plot_var]])) + 
      geom_line(aes(group = ID), 
                color = 'gray75') + 
      geom_ribbon(data = med_tbl, 
                  aes(y = median, 
                      ymin = perc_25, 
                      ymax = perc_75), 
                  fill = fill_color, 
                  alpha = 0.4) + 
      geom_line(data = med_tbl, 
                aes(y = median, 
                    group = ID), 
                size = 1.5, 
                color = fill_color) + 
      scale_x_continuous(breaks = med_tbl[[time_var]]) + 
      globals$common_theme + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = paste('n =', n_numbers), 
           x = x_lab, 
           y = y_lab)
    
  }
  
# prevalence plotting -------
  
  plot_prev_bubble <- function(data, 
                               variables = prev_sum$var_groups$symptoms, 
                               plot_title = NULL, 
                               plot_subtitle = NULL, 
                               plot_tag = NULL) {
    
    
    ## plots the feature prevalence in the severity groups and in the cohort
    ## as a bubble plot
    
    data <- data %>% 
      select(cat_WHO, 
             all_of(variables)) %>% 
      filter(complete.cases(.))
    
    data <- rbind(data, 
                  mutate(data, 
                         cat_WHO = 'cohort')) %>% 
      mutate(cat_WHO = globals$sev_labels[cat_WHO], 
             cat_WHO = factor(cat_WHO, 
                              c('Cohort', 'Ambulatory', 'Moderate', 'Severe')))
    
    ## counting, % of complete answers
    
    count_tbl <- variables %>% 
      map(~count(data, cat_WHO, .data[[.x]], .drop = FALSE)) %>% 
      map(group_by, cat_WHO) %>% 
      map(mutate, percent = n/sum(n) * 100) %>% 
      map(ungroup)
    
    count_tbl <- map2(count_tbl, 
                      variables, 
                      ~mutate(.x, variable = .y)) %>% 
      map2(variables, ~filter(.x, .data[[.y]] == 'yes')) %>% 
      map_dfr(~.x[c('variable', 'cat_WHO', 'percent')])
    
    ## meta
    
    n_numbers <- count(data, cat_WHO)
    
    ax_labs <- map2(c('Cohort', 'Ambulatory', 'Moderate', 'Severe'), 
                    n_numbers$n, 
                    ~paste(.x, .y, sep = '\nn = ')) %>% 
      set_names(c('Cohort', 'Ambulatory', 'Moderate', 'Severe'))
    
    mid_point <- mean(count_tbl$percent)
    
    ## plotting
    
    count_tbl %>% 
      ggplot(aes(x = cat_WHO, 
                 y = reorder(variable, percent), 
                 size = percent, 
                 fill = percent)) + 
      geom_point(shape = 21) + 
      geom_text(aes(label = paste0(signif(percent, 2), '%')), 
                size = 2.75, 
                hjust = 0.5, 
                vjust = -1.4) + 
      guides(size = FALSE) + 
      scale_x_discrete(labels = ax_labs) + 
      scale_fill_gradient2(low = 'steelblue3', 
                           mid = 'white', 
                           high = 'firebrick3', 
                           midpoint = mid_point, 
                           name = '%') + 
      scale_y_discrete(labels = translate_var(variables, out_value = 'label_long')) + 
      globals$common_theme + 
      theme(axis.title = element_blank()) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag)
    
  }

# correlations -----
  
  plot_corr_buble <- function(data, 
                              plot_title = NULL, 
                              plot_subtitle = NULL, 
                              plot_tag = NULL) {
    
    ## plots results of a correlation analysis as a bubble plots.
    ## Point size and color codes for the estimate value.
    
    data %>% 
      ggplot(aes(x = var1_label, 
                 y = var2_label, 
                 fill = estimate, 
                 size = abs(estimate))) + 
      geom_point(shape = 21) + 
      geom_text(aes(label = signif(estimate, 2)), 
                size = 2.2, 
                hjust = 0.5,
                vjust = -1.45) + 
      scale_x_discrete(limits = translate_var(globals$corr_variables, out_value = 'label_long')) + 
      scale_y_discrete(limits = translate_var(globals$corr_variables, out_value = 'label_long')) + 
      scale_fill_gradient2(low = 'steelblue3', 
                           mid = 'white', 
                           high = 'firebrick3', 
                           midpoint = 0) +
      guides(size = FALSE) + 
      globals$common_theme + 
      theme(axis.title = element_blank(),
            axis.text.x = element_text(angle = 90, 
                                       hjust = 1, 
                                       vjust = 0.5), 
            panel.grid.major = element_line(color = 'gray90')) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           fill = expression(tau))
    
  }
  
# figures -------
  
  make_mod_panel <- function(roc_plot_list, 
                             lasso_forest_plot, 
                             lasso_rel_h = c(0.9, 0.1)) {
    
    ## makes a combined panel with the plot of LASSO modeling estimates
    ## and ROC curves in the training and CV data sets.
    
    left_panel <- map2(roc_plot_list, 
                       c('Training data set', 
                         'Cross-validation'), 
                       ~.x + 
                         labs(title = .y) + 
                         theme(plot.tag = element_blank())) %>% 
      plot_grid(plotlist = ., 
                nrow = 2, 
                align = 'hv') %>% 
      plot_grid(ggdraw(), 
                nrow = 2, 
                rel_heights = c(0.85, 0.15))
    
    right_panel <- plot_grid(lasso_forest_plot + 
                               labs(tag = paste0('\n', 
                                                 roc_plot_list[[1]]$labels$tag)), 
                             ggdraw(),
                             nrow = 2,
                             rel_heights = lasso_rel_h)
    
    plot_grid(right_panel, 
              ggdraw(), 
              left_panel, 
              ncol = 3, 
              rel_widths = c(1.1, 0.07, 0.83), 
              labels = c('A', '', 'B'), 
              label_size = 10)
    
  }
  
  
# varia -----
  
  vec_sum <- function(vec_list, na.rm = T) {
    
    transpose(as.list(vec_list)) %>% 
      map(reduce, c) %>% 
      map_dbl(sum, na.rm = na.rm)
    
  }
  
  complete_cases <- function(data, id_var = 'ID') {
    
    ### selects the individuals with the complete variable record
    
    dlply(data, id_var) %>% 
      map_dfr(function(x) if(any(!complete.cases(x))) NULL else x)
    
    
  }
  
  format_summ_tbl <- function(data, 
                              rm_n = TRUE, 
                              rm_mean = TRUE, 
                              rm_complete = TRUE, 
                              out_value = 'axis_lab_long') {
    
    ## formats a summary table with descriptive stats
    
    data <- data %>% 
      map_dfc(stri_replace, regex = 'no:.*\\nyes:\\s{1}', replacement = '') %>% 
      map_dfc(stri_replace, regex = '\\nno:.*$', replacement = '') %>% 
      map_dfc(stri_replace_all, fixed = '% (', replacement = '% (n = ') %>% 
      map_dfc(stri_replace, fixed = 'Median =', replacement = 'median:') %>% 
      map_dfc(stri_replace, fixed = 'Mean =', replacement = 'mean:') %>% 
      map_dfc(stri_replace, fixed = 'Range', replacement = 'range') %>% 
      map_dfc(stri_replace, fixed = 'Complete', replacement = 'complete') %>% 
      mutate(variable = translate_var(variable, out_value = out_value))
    
    if(rm_n) {
      
      data <- data %>% 
        map_dfc(stri_replace, regex = '\\ncomplete.*$', replacement = '')
        
    }
    
    if(rm_mean) {
      
      data <- data %>% 
        map_dfc(stri_replace, regex = 'mean.*\\n', replacement = '')
      
    }
    
    if(rm_complete) {
      
      data <- data %>% 
        map_dfc(stri_replace, fixed = 'complete: ', replacement = '')
      
    }
    
    data
    
  }
  
  re_adjust <- function(data, method = 'BH') {
    
    ## adjusts for multiple testing e.g. with the Benjamini-Hochberg method
    
    if(method != 'none') {
      
      data <- data %>% 
        mutate(p_adjusted = p.adjust(p_value, method = method))
      
    }
    
    data %>% 
      mutate(significance = ifelse(p_adjusted < 0.001, 
                                   'p < 0.001', 
                                   ifelse(p_adjusted >= 0.05, 
                                          paste0('ns (p = ', signif(p_adjusted, 2), ')'), 
                                          paste('p =', signif(p_adjusted, 2)))))
    
  }
  
  mm_inch <- function(x) 0.0393700787 * x
  
  embolden_scale <- function(x, 
                             highlight,  
                             color = 'black', 
                             family = '', 
                             translate = FALSE, 
                             dict = globals$var_lexicon, ...) {
    
    if(!translate) {
      
      return(ifelse(x %in% highlight, 
                    glue("<b style='color:{color}'>{x}</b>"), 
                    x))
      
    } else {
      
      labels <- translate_var(x, dict = dict, ...)
      
      return(ifelse(x %in% highlight, 
                    glue("<b style='color:{color}'>{labels[x]}</b>"), 
                    labels[x]))
      
      
    }
    
  }

# END -----