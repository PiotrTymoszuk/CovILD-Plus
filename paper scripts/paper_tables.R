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
               caption = 'Baseline characteristics of the study cohort and the COVID-19 severity groups.')
  
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
               caption = 'Key outcome measures of participants according to COVID-19 severity groups.')
  
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
               caption = '12-month sub-maximal exercise performance and mental health across COVID-19 severity groups.')

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
               ref_name = 'study-vars', 
               caption = 'Study variables.')

# Supplementary Table S2: assessment battery ------
  
  insert_msg('Table S2: assessment battery')
  
  suppl_tables$battery <- read_excel('./input data/psychosoc_ass_battery.xlsx') %>% 
    as_mdtable(label = 'battery', 
               ref_name = 'battery', 
               caption = 'Physical performance, fatigue, quality of life, psychosocial and mental health assessment battery.')
  
# Supplementary Table S3: characteristic of the clusters ------
  
  insert_msg('Table S3: cluster comparison')
  
  suppl_tables$clust_chara <- clust_chara[c('desc_stats', 'test_results')] %>% 
    map(filter, variable %in% c(paper_tables$chara_vars, 'cat_WHO')) %>% 
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
               ref_name = 'clust-clinics',
               caption = 'Demographic and clinical characteristics of the COVID-19 recovery clusters.')
  
# Supplementary Table S4: clinical and cardiopulmonary recovery in the clusters -----
  
  insert_msg('Table S4: cariopulmonary recovery in the clusters')
  
  suppl_tables$clust_endpoints <- clust_chara[c('desc_stats', 'test_results')] %>% 
    map(filter, 
        variable %in% paper_tables$clin_vars) %>% 
    reduce(left_join, by = 'variable') %>% 
    select(variable, starts_with('clust'), significance, eff_size) %>% 
    mutate(variable = factor(variable, levels = paper_tables$clin_vars)) %>% 
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
               ref_name = 'clust-clin-reco', 
               caption = 'Symptoms and cardiopulmonary abnormalities at the 1-year follow-up in the COVID-19 recovery clusters.')
  
# Supplementary Table S5: psychosocial and performance recovery in the clusters -----
  
  insert_msg('Table S5: psychosocial and performance recovery in the clusters')
  
  suppl_tables$psych_reco <- clust_chara[c('desc_stats', 'test_results')] %>% 
    map(filter, variable %in% paper_tables$reco_vars) %>% 
    reduce(left_join, by = 'variable') %>% 
    select(variable, starts_with('clust'), significance, eff_size) %>% 
    mutate(variable = factor(variable, levels = paper_tables$reco_vars)) %>% 
    arrange(variable) %>% 
    format_summ_tbl(rm_n = FALSE) %>% 
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
                'Effect size'))
  
  ## removing the redundant n numbers, only for BRCS and SSD12, there are some
  ## NA's
  
  suppl_tables$psych_reco <- suppl_tables$psych_reco %>% 
    mutate(`Cluster #1` = ifelse(!Variable %in% c('Somatic symptom disorder (SSD-12)', 
                                                  'Resilience (BRCS)'), 
                                 stri_replace_all(`Cluster #1`, 
                                                  regex = '\\nn.*$', 
                                                  replacement = ''), 
                                 `Cluster #1`), 
           `Cluster #2` = ifelse(!Variable %in% c('Somatic symptom disorder (SSD-12)', 
                                                  'Resilience (BRCS)'), 
                                 stri_replace_all(`Cluster #2`, 
                                                  regex = '\\nn.*$', 
                                                  replacement = ''), 
                                 `Cluster #2`), 
           `Cluster #3` = ifelse(!Variable %in% c('Somatic symptom disorder (SSD-12)', 
                                                  'Resilience (BRCS)'), 
                                 stri_replace_all(`Cluster #3`, 
                                                  regex = '\\nn.*$', 
                                                  replacement = ''), 
                                 `Cluster #3`)) %>% 
    as_mdtable(label = 'psych_reco', 
               ref_name = 'clust-psych-reco', 
               caption = 'Mobility, physical performance and psychosocial rating at the 1-year follow-up in the COVID-19 recovery clusters.')

# Supplementary Table S6: univariate modeling results ------
  
  insert_msg('Table S6: Significant univariate modeling results')
  
  suppl_tables$uni_mod <- map2_dfr(uni_mod$summary, 
                                   uni_mod$signif_fct, 
                                   ~filter(.x, variable %in% .y)) %>% 
    left_join(paper_tables$mod_references, by = 'variable') %>% 
    mutate(method = ifelse(family == 'binomial', 'logistic', 'linear'), 
           response = paste0(translate_var(response), ', 1 year FUP'), 
           variable = translate_var(variable), 
           estimate = paste0(signif(estimate, 2), ' [', signif(lower_ci, 2), ' - ', signif(upper_ci, 2), ']'), 
           level = ifelse(is.na(level), 'per item', level), 
           level = ifelse(level %in% c('HM', 'HS', 'A'), globals$sev_labels[level], level), 
           reference = ifelse(reference %in% c('HM', 'HS', 'A'), globals$sev_labels[reference], reference)) %>% 
    re_adjust('none') %>% 
    mutate(variable = paste(variable, ref_code, sep = ', ')) %>% 
    select(response, variable, reference, level, n, n_complete, estimate, significance) %>% 
    set_names(c('Response', 'Independent variable', 'Reference', 
                'Level', 'N level', 'N total', 
                'OR, 95% CI', 'Significance')) %>% 
    as_mdtable(label = 'uni_mod', 
               ref_name = 'uni-mod', 
               caption = 'Significant results of univariable modeling of the risk of symptom presence, lung function abnormalities, radiological chest abnormalities and diastolic dysfunction at the 1-year follow-up.')

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
    
  
# END -----
  
  insert_tail()