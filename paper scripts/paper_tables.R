# This script generates supplementary tables 

  insert_head()
  
# data containers -----
  
  paper_tables <- list()
  suppl_tables <- list()
  
# globals -----
  
  insert_msg('Globals setup')
  
  paper_tables$chara_vars <- c('sex', 'age', 'weight_class', 'smoking', 
                               'comorb_present', 'endometabolic_comorb', 
                               'diabetes_comorb', 'hyperchol_comorb', 
                               'cardiovascular_comorb', 'pulmonary_comorb', 
                               'malingancy_comorb', 'immdef_comorb', 
                               'ckd_comorb', 'gastro_comorb', 
                               'treat_steroids', 
                               'rehabilitation')
  
  paper_tables$clin_vars <- c('sympt_present', 
                              'sympt_number', 
                              'sleep_sympt', 
                              'dyspnoe_sympt', 
                              'cough_sympt', 
                              'night_sweat_sympt', 
                              'gastro_sympt', 
                              'anosmia_sympt', 
                              'fatigue_sympt', 
                              'hair_loss_sympt', 
                              'derma_sympt', 
                              'lufo_red', 
                              'ct_severity_any', 
                              'diastolic_dysf')
  
  paper_tables$reco_vars <- c('smwd', 'smwd_dref', 'smwd_low', 
                              'Chalder_FS', 'Chalder_FS_bimodal', 
                              'EQ5DL_p', 'EQ5DL_low', 
                              'EQ5DL_mobility', 'EQ5DL_mobility_bi', 
                              'EQ5DL_selfcare', 'EQ5DL_selfcare_bi', 
                              'EQ5DL_activities', 'EQ5DL_activities_bi', 
                              'EQ5DL_pain', 'EQ5DL_pain_bi', 
                              'EQ5DL_anxiety', 'EQ5DL_anxiety_bi', 
                              'Stress', 'Stress_hi', 'SSD12', 
                              'BRCS', 'BRCS_class')
  
  paper_tables$sev_n <- count(cov_data$data_tbl %>% 
                                filter(time == 0) %>% 
                                mutate(cat_WHO = globals$sev_labels[cat_WHO]), 
                              cat_WHO)
  
  paper_tables$clust_n <- ngroups(part_clust$clust_obj)
  
  paper_tables$mod_references <- uni_mod$variables %>% 
    map_dfr(~tibble(variable = .x, 
                    reference = if(is.factor(cov_data$mod_table[[.x]])) levels(cov_data$mod_table[[.x]])[1] else NA)) %>% 
    left_join(globals$mod_features[c('variable', 'ref_code')], by = 'variable')
  
# Table 1: cohort characteristic -------
  
  insert_msg('Table 1, cohort characteristic')
  
  paper_tables$cohort_chara <- explore(cov_data$data_tbl %>% 
                                         filter(time == 0), 
                                       variables = paper_tables$chara_vars, 
                                       what = 'table', 
                                       pub_styled = TRUE)
  
  paper_tables$cohort_chara <- eda_fix[c('desc_stats', 'test_results')] %>% 
    map(filter, 
        variable %in% paper_tables$chara_vars) %>% 
    reduce(left_join, by = 'variable') %>% 
    select(variable, Ambulatory, Moderate, Severe, significance, eff_size) %>% 
    left_join(paper_tables$cohort_chara, ., by = 'variable') %>% 
    format_summ_tbl(rm_n = FALSE) %>% 
    rbind(tibble(variable = 'N number', 
                 statistic = length(cov_data$year_complete), 
                 Ambulatory = paper_tables$sev_n$n[1], 
                 Moderate = paper_tables$sev_n$n[2], 
                 Severe = paper_tables$sev_n$n[3], 
                 significance = NA, 
                 eff_size = NA), .) %>% 
    mutate(variable = stri_replace(variable, 
                                   fixed = ', %', 
                                   replacement = '')) %>% 
    set_names(c('Variable', 
                'CovILD cohort', 
                'Ambulatory COVID-19', 
                'Moderate COVID-19', 
                'Severe COVID-19', 
                'Significance', 
                'Effect size'))
  
  ## removing the redundant N number entry for all records except
  ## the rehabilitation status (NA's present)

  paper_tables$cohort_chara <- paper_tables$cohort_chara %>% 
    mutate(`CovILD cohort` = ifelse(Variable != 'Rehabilitation', 
                                    stri_replace_all(`CovILD cohort`, 
                                                     regex = '\\nn.*$', 
                                                     replacement = ''), 
                                    `CovILD cohort`), 
           `Ambulatory COVID-19` = ifelse(Variable != 'Rehabilitation', 
                                          stri_replace_all(`Ambulatory COVID-19`, 
                                                           regex = '\\nn.*$', 
                                                           replacement = ''), 
                                          `Ambulatory COVID-19`), 
           `Moderate COVID-19` = ifelse(Variable != 'Rehabilitation', 
                                        stri_replace_all(`Moderate COVID-19`, 
                                                         regex = '\\nn.*$', 
                                                         replacement = ''), 
                                        `Moderate COVID-19`), 
           `Severe COVID-19` = ifelse(Variable != 'Rehabilitation', 
                                      stri_replace_all(`Severe COVID-19`, 
                                                       regex = '\\nn.*$', 
                                                       replacement = ''), 
                                      `Severe COVID-19`)) %>% 
    as_mdtable(label = 'cohort_characteristic', 
               ref_name = 'cohort', 
               caption = 'Baseline characteristics, post-acute steroid therapy and rehabilitation status of the study cohort and the COVID-19 severity groups.')
  
# Table 2: study end points ------
  
  insert_msg('Table 2: study end points')
  
  paper_tables$endpoints <- explore(cov_data$data_tbl %>% 
                                      filter(time == 4), 
                                    variables = c('sympt_present', 
                                                  'lufo_red', 
                                                  'ct_severity_any', 
                                                  'diastolic_dysf'), 
                                    what = 'table', 
                                    pub_styled = TRUE)

  paper_tables$endpoints <- eda_fix[c('desc_stats', 'test_results')] %>% 
    map(filter, 
        variable %in% c('sympt_present', 
                        'lufo_red', 
                        'ct_severity_any', 
                        'diastolic_dysf')) %>% 
    reduce(left_join, by = 'variable') %>% 
    select(variable, Ambulatory, Moderate, Severe, significance, eff_size) %>% 
    left_join(paper_tables$endpoints, ., by = 'variable') %>% 
    format_summ_tbl(rm_n = FALSE) %>% 
    mutate(variable = stri_replace(variable, 
                                   fixed = ', %', 
                                   replacement = '')) %>% 
    set_names(c('Variable', 
                'CovILD cohort', 
                'Ambulatory COVID-19', 
                'Moderate COVID-19', 
                'Severe COVID-19', 
                'Significance', 
                'Effect size')) %>% 
    as_mdtable(label = 'endpoints', 
               ref_name = 'enpoints', 
               caption = 'Key outcome measures at the one-year follow-up in the study cohort and the COVID-19 severity groups.')
  
# Table 3: psychosocial recovery in the cohort -----
  
  insert_msg('Table 3 Psychosocial recovery')
  
  paper_tables$psych_reco <- explore(cov_data$data_tbl %>% 
                                       filter(time == 4), 
                                     variables = paper_tables$reco_vars, 
                                     what = 'table', 
                                     pub_styled = TRUE)
  
  paper_tables$psych_reco <- eda_fix[c('desc_stats', 'test_results')] %>% 
    map(filter, variable %in% paper_tables$reco_vars) %>% 
    reduce(left_join, by = 'variable') %>% 
    select(variable, Ambulatory, Moderate, Severe, significance, eff_size) %>% 
    left_join(paper_tables$psych_reco, ., by = 'variable') %>% 
    mutate(variable = factor(variable, levels = paper_tables$reco_vars)) %>% 
    arrange(variable) %>%
    format_summ_tbl(rm_n = FALSE) %>% 
    mutate(variable = stri_replace(variable, 
                                   fixed = ', %', 
                                   replacement = '')) %>% 
    set_names(c('Variable', 
                'CovILD cohort', 
                'Ambulatory', 
                'Moderate', 
                'Severe', 
                'Significance', 
                'Effect size')) %>% 
    as_mdtable(label = 'psych_recovery', 
               ref_name = 'psych-reco', 
               caption = 'Physical performance, fatigue, self-perceived general health, quality of life and mental health readouts at the one-year follow-up in the study cohort and the COVID-19 severity groups.')

# Supplementary Table S1: study variables -----
  
  insert_msg('Table S1: study variables')
  
  suppl_tables$study_vars <- globals$var_lexicon %>% 
    filter(variable %in% c(long_num$variables, 
                           long_prev$variables, 
                           globals$mod_features$variable, 
                           globals$clust_variables, 
                           eda_fix$variables$variable)) %>% 
    filter(!duplicated(variable)) %>% 
    mutate(time = ifelse(stri_detect(variable, fixed = 'sympt'), 
                         'acute COVID-19 till 1-year FUP', 
                         ifelse(var_collection == 'longitudinal', 
                                '60- till 1 year FUP', 
                                car::recode(ref_time, 
                                            "0 = 'acute COVID-19'; 1 = '60-day FUP'; 4 = '1-year FUP'"))), 
           modeling_var = ifelse(variable %in% uni_mod$variables, 'yes', 'no'), 
           corr_var = ifelse(variable %in% globals$corr_variables, 'yes', 'no'), 
           clustering_var = ifelse(variable %in% globals$clust_variables, 'yes', 'no')) %>% 
    select(variable, axis_lab, time, modeling_var, corr_var, clustering_var) %>% 
    set_names(c('R variable', 
                'Label', 
                'Collection time points', 
                'Independent modeling variable', 
                'Correlation variable', 
                'Clustering variable'))
  
  suppl_tables$study_vars <- suppl_tables$study_vars %>% 
    arrange(`Collection time points`) %>% 
    as_mdtable(label = 'study_vars', 
               ref_name = 'study_vars', 
               caption = 'Study variables.')

# Supplementary Table S2: baseline characteristic of the clusters ------
  
  insert_msg('Table S2: cluster comparison')
  
  ## redundant features: age, sex, severity and weight class are removed
  ## since they were presented already in the figures
  
  suppl_tables$clust_chara <- clust_chara[c('desc_stats', 'test_results')] %>% 
    map(filter, 
        variable %in% c(paper_tables$chara_vars), 
        !variable %in% c('cat_WHO', 'sex', 'age', 'weight_class')) %>% 
    reduce(left_join, by = 'variable') %>% 
    select(variable, starts_with('clust'), significance, eff_size) %>% 
    mutate(variable = factor(variable, 
                             levels = c(paper_tables$chara_vars, 'cat_WHO'))) %>% 
    arrange(variable) %>% 
    format_summ_tbl %>% 
    mutate(variable = stri_replace(variable, 
                                   fixed = ', %', 
                                   replacement = '')) %>% 
    rbind(tibble(variable = 'N number', 
                 clust_1 = paper_tables$clust_n$n[1], 
                 clust_2 = paper_tables$clust_n$n[2], 
                 clust_3 = paper_tables$clust_n$n[3], 
                 significance = NA, 
                 eff_size = NA), .) %>% 
    set_names(c('Variable', 
                'Cluster #1', 
                'Cluster #2', 
                'Cluster #3', 
                'Significance', 
                'Effect size')) %>% 
    mutate(Variable = stri_replace(Variable, fixed = ', %', replacement = '')) %>% 
    as_mdtable(label = 'clust_chara', 
               ref_name = 'clust_base',
               caption = 'Extended baseline characteristics, post-acute steroid therapy and rehabilitation status of the COVID-19 recovery clusters.')
  
# Supplementary Table S3: one-year characteristic of the clusters -----
  
  insert_msg('Table S3: one-year characteristic of the clusters')
  
  suppl_tables$clust_fup <- 
    clust_chara[c('desc_stats', 'test_results')] %>% 
    map(filter, 
        variable %in% c('sympt_number', 
                        'hair_loss_sympt', 
                        'derma_sympt', 
                        'ct_severity_score', 
                        'smwd', 
                        'EQ5DL_mobility', 
                        'EQ5DL_selfcare')) %>% 
    reduce(left_join, by = 'variable') %>% 
    select(variable, starts_with('clust'), significance, eff_size) %>% 
    mutate(variable = factor(variable, 
                             levels = c('sympt_number', 
                                        'hair_loss_sympt', 
                                        'derma_sympt', 
                                        'ct_severity_score', 
                                        'smwd', 
                                        'smwd_dref', 
                                        'EQ5DL_mobility', 
                                        'EQ5DL_selfcare'))) %>% 
    arrange(variable) %>% 
    format_summ_tbl %>% 
    rbind(tibble(variable = 'N number', 
                 clust_1 = paper_tables$clust_n$n[1], 
                 clust_2 = paper_tables$clust_n$n[2], 
                 clust_3 = paper_tables$clust_n$n[3], 
                 significance = NA, 
                 eff_size = NA), .) %>% 
    mutate(variable = stri_replace(variable, 
                                   fixed = ', %', 
                                   replacement = '')) %>%
    set_names(c('Variable', 
                'Cluster #1', 
                'Cluster #2', 
                'Cluster #3', 
                'Significance', 
                'Effect size')) %>% 
    as_mdtable(label = 'clust_endpoints', 
               ref_name = 'clust_fup', 
               caption = 'Extended characteristic of the COVID-19 recovery clusters at the one-year follow-up.')

# saving the tables on the disc ----
  
  insert_msg('Saving the tables on the disc')
  
  suppl_tables %>% 
    set_names(paste0('Table S', 
                     1:length(suppl_tables))) %>% 
    write_xlsx(path = './paper/supplementary_tables.xlsx')
  
  paper_tables <- paper_tables[c('cohort_chara', 'endpoints', 'psych_reco')]
  
  paper_tables %>% 
    set_names(paste0('Table ', 
                     1:length(paper_tables))) %>% 
    write_xlsx(path = './paper/tables.xlsx')
  
  suppl_tables$study_vars %>% 
    write_xlsx('./paper/supplementary_table_s1.xlsx')
    
  
# END -----
  
  insert_tail()