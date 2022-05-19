# This script imports the data of the CovILD study

# toolbox ----

  library(readxl)
  library(foreign)
  library(soucer)
  library(stringi)
  library(plyr)
  library(tidyverse)
  library(rlang)

  source_all('./tools/project_tools.R', message = TRUE, crash = TRUE)

  insert_head()
  
# Data containers ----
  
  prev_visits <- list()
  year_visit <- list()
  
  cov_data <- list()
  globals <- list()
  
# globals ----

  ## graphics
  
  globals$corr_colors <- c('negative' = 'steelblue4', 
                           'positive' = 'firebrick4', 
                           'ns' = 'gray60')
  
  globals$common_text <- element_text(size = 8, face = 'plain', color = 'black')
  
  globals$common_margin <- ggplot2::margin(t = 4, l = 3, r = 2, unit = 'mm')
  
  globals$common_theme <- theme_classic() + theme(axis.text = globals$common_text, 
                                                  axis.title = globals$common_text, 
                                                  plot.title = element_text(size = 8, 
                                                                            face = 'bold', 
                                                                            color = 'black', 
                                                                            hjust = 0), 
                                                  plot.subtitle = globals$common_text, 
                                                  plot.tag = element_text(size = 8, 
                                                                          face = 'plain', 
                                                                          color = 'black', 
                                                                          hjust = 0), 
                                                  plot.tag.position = 'bottom', 
                                                  legend.text = globals$common_text, 
                                                  legend.title = globals$common_text, 
                                                  strip.text = globals$common_text,
                                                  strip.background = element_rect(fill = 'gray95', color = 'gray80'), 
                                                  plot.margin = globals$common_margin, 
                                                  panel.grid.major = element_line(color = 'gray90'))
  
  ## patient severity group colors and labels
  
  globals$sev_colors <- c(A = 'steelblue', 
                          HM = 'gray50', 
                          HS = 'coral4', 
                          cohort = 'darkolivegreen4')
  
  globals$sev_labels <- c(A = 'Ambulatory', 
                          HM = 'Moderate', 
                          HS = 'Severe', 
                          cohort = 'Cohort')
  
  ## cluster colors
  
  globals$clust_colors <- c('#1' = 'steelblue', 
                            '#2' = 'darkolivegreen4', 
                            '#3' = 'coral3')

# reading the recoding scheme -----
  
  globals$var_coding[c('prev_visits', 
                       'year_visit')] <- c('prev_visits', 
                                           'year_visit') %>% 
    map(read_excel, 
        path = './input data/var_recoding.xlsx') %>% 
    map(select, 
        old_var, new_var, trans_fun, args1, args2) %>% 
    map(mutate, 
        args1 = stri_replace_all(args1, regex = '“|”', replacement = ''), 
        args1 = stri_replace_all(args1, regex = '‘|’', replacement = "'"), 
        args1 = map(args1, function(x) if(any(is.na(x))) NULL else x)) %>% 
    map(mutate, 
        args2 = stri_replace_all(args2, regex = '“|”', replacement = ''), 
        args2 = stri_replace_all(args2, regex = '‘|’', replacement = "'"), 
        args2 = map(args2, function(x) if(any(is.na(x))) NULL else x)) %>% 
    map(mutate, 
        args = map2(args1, args2, list)) %>% 
    map(select, 
        - args1, 
        - args2)

# reading the variable lexicon -----
  
  insert_msg('Reading the variable lexicon')
  
  globals$var_lexicon <- read_excel('./input data/var_lexicon.xlsx') %>% 
    mutate(axis_lab = ifelse(is.na(unit), label, paste(label, unit, sep = ', ')))

# setup of the modeling and clustering variables -----
  
  insert_msg('Modeling variables')
  
  ## correlation variables
  
  globals$corr_variables <- c('sympt_number', 
                              'mmrc', 
                              'fatigue_sympt', 
                              'Chalder_FS', 
                              'sleep_sympt', 
                              'cough_sympt', 
                              'night_sweat_sympt', 
                              'anosmia_sympt', 
                              'smwd_dref', 
                              'lufo_red', 
                              'DLCO_p',
                              'ct_severity_score', 
                              'diastolic_dysf', 
                              'EQ5DL_p', 
                              'EQ5DL_mobility', 
                              'EQ5DL_selfcare', 
                              'EQ5DL_activities', 
                              'EQ5DL_pain', 
                              'EQ5DL_anxiety', 
                              'Stress')
  
  ## binary clustering variables
  
  globals$clust_variables <- c('sympt_present', 
                               'dyspnoe_sympt', 
                               'fatigue_sympt', 
                               'Chalder_FS_bimodal', 
                               'sleep_sympt', 
                               'cough_sympt', 
                               'night_sweat_sympt', 
                               'anosmia_sympt', 
                               'smwd_low', 
                               'lufo_red', 
                               'ct_severity_any', 
                               'diastolic_dysf', 
                               'EQ5DL_low', 
                               'EQ5DL_mobility_bi', 
                               'EQ5DL_selfcare_bi', 
                               'EQ5DL_activities_bi', 
                               'EQ5DL_pain_bi', 
                               'EQ5DL_anxiety_bi', 
                               'Stress_hi')
  
  ## modeling
  
  globals$mod_features <- read_excel('./input data/modeling_features.xlsx') %>% 
    mutate(label = translate_var(variable), 
           ref_code = car::recode(reference, 
                                  "0 = 'acute COVID-19'; 1 = '60-day FUP'; 4 = '1-year FUP'"), 
           label = paste(label, ref_code, sep = '\n'))
  
# data till visit 6 -----
  
  insert_msg('Reading and cleaning the data from the previous visits')
  
  ## reading the excel data
  
  prev_visits$prev_visits <- read_excel('./input data/Covid_V0bisV2_tc_271220.xlsx')
  
  ## new patient classification
  
  prev_visits$sev_class <- read_excel('./input data/CovILD V3.xlsx', sheet = 'demographics') %>% 
    select(ID, WHO, cat_WHO, Reha) %>% 
    mutate(cat_WHO = car::recode(cat_WHO, 
                                 "'hospitalized,mild' = 'HM'; 
                                 'ambulatory' = 'A'; 
                                 'hospitalized, severe' = 'HS'"))
  
  prev_visits$prev_visits <- left_join(prev_visits$prev_visits, 
                                       prev_visits$sev_class, 
                                       by = 'ID')
  
  ## antibody level stratification
  
  prev_visits$ab_quant <- cut(prev_visits$prev_visits$SarsCov2_IgG, 
                             quantile(prev_visits$prev_visits$SarsCov2_IgG, 
                                      c(0, 0.25, 0.5, 0.75, 1), 
                                      na.rm = TRUE), 
                             c('Q1', 'Q2', 'Q3', 'Q4'), 
                             include.lowest = TRUE)
  
  ## manual re-coding
  
  prev_visits$prev_visits <- prev_visits$prev_visits %>% 
    mutate(smoking = ifelse(current_smoker == 1,
                            'active', 
                            ifelse(smoking_ex == 0, 
                                   'never', 'ex')), 
           TSAT_red = ifelse((gender == 0 & TSAT < 15)|(gender == 1 & TSAT < 20), 
                             'yes', 'no'), 
           anemia = ifelse((gender == 0 & Hb < 120)|(gender == 1 & Hb < 140), 
                           'yes', 'no'), 
           FT_elv = ifelse((gender == 0 & ferritin >= 150)|(gender == 1 & ferritin >= 300), 
                           'yes', 'no'), 
           sympt_number = sleep_disorder + dyspnoe + cough + fever + night_sweat + GI_sympt  + anosmia + ECOG_imp  + pain, 
           sympt_present = ifelse(time == 0, 
                                  ifelse(sympt_number > 0, 
                                         'yes', 
                                         'no'), 
                                  recode_yn(persistent_symptoms, F, F)), 
           ab_quant = prev_visits$ab_quant)
  
  ## automated cleaning
  
  prev_visits <- globals$var_coding$prev_visits %>% 
    pmap(recode_var, 
         data = prev_visits$prev_visits, 
         time_var = 'time') %>% 
    reduce(left_join, 
         by = c('ID', 'time'))

# one year data ----
  
  insert_msg('Reading and cleaning the one-year data')
  
  ## reading the excel files
  
  year_visit <- c('demographics', 
                  'persistent symptoms', 
                  'spirometry', 
                  'echocardiography') %>% 
    map(read_excel, 
        path = './input data/CovILD V3.xlsx')
  
  year_visit[c('ct', 
               'biochemistry')] <- c('./input data/CT severity score V4.xlsx', 
                                     './input data/CovILD V3 Labor.xlsx') %>% 
    map(read_excel)
  
  year_visit[c('smwd', 
               'psychosoc')] <- c('6MWD (filtered)', 
                                  'Questionnaires') %>% 
    map(~read_excel(path = './input data/CovILD_subset_EQoL_6MWT.xlsx', 
                    sheet = .x))
  
  year_visit$psychosoc2 <- read_excel('./input data/CovILD V3.xlsx', 
                                      sheet = 'life quality') %>% 
    select(ID, SSD12, Stress, `SES (1-4)`, `KW (IPQ)`, SOCL9)

  year_visit <- year_visit %>%
    reduce(left_join, 
           by = 'ID')
  
  ## manual re-coding
  ## defining the reduced lung function
  ## filling the missing/not surveyed variables that were queried in the previous visits with NA
  
  year_visit <- year_visit %>% 
    mutate(time = 4) %>% 
    mutate(FEV1_FVC_p = FEV1_L/FVC_L * 100, 
           lufo_red = (FVC_p < 80) +  (FEV1_p < 80) +  (FEV1_FVC_p < 70) + (TLC_p < 80) + (DLOC_p < 80)) %>% 
    mutate(BRCS_class = cut(BRCS, 
                            c(-Inf, 13, 16, Inf), 
                            c('low', 'medium', 'high'))) %>% 
    mutate(`D-Dimer` = ifelse(`D-Dimer` == '< 171', 0, `D-Dimer`) %>% 
             as.numeric, 
           `Interleukin-6 (Roche)` = ifelse(`Interleukin-6 (Roche)` == '< 1,5', 
                                            0, 
                                            `Interleukin-6 (Roche)`) %>% 
             as.numeric, 
           Procalcitonin = ifelse(Procalcitonin == '< 0,06', 0, Procalcitonin) %>% 
             as.numeric, 
           `C-reaktives Prot. (CRP)` = ifelse(`C-reaktives Prot. (CRP)` == '< 0,06', 
                                              0, 
                                              `C-reaktives Prot. (CRP)`) %>% 
             as.numeric, 
           `NT-pro-BNP` = ifelse(`NT-pro-BNP` == '< 50', 0, `NT-pro-BNP`) %>% 
             as.numeric, 
           `Hepcidin-25` = ifelse(`Hepcidin-25` == '< 1,1', 0, `Hepcidin-25`) %>% 
             as.numeric, 
           TSAT_red = ifelse((gender == 'female' & Transferrinsaettigung < 15)|(gender == 'male' & Transferrinsaettigung < 20), 
                             'yes', 'no'), 
           anemia = ifelse((gender == 'female' & Haemoglobin < 120)|(gender == 'male' & Haemoglobin < 140), 
                           'yes', 'no'), 
           FT_elv = ifelse((gender == 'female' & Ferritin >= 150)|(gender == 'male' & Ferritin >= 300), 
                           'yes', 'no'), 
           Meter = as.numeric(Meter), 
           catCFS = as.numeric(catCFS))
  
  ## identification of the individuals who came to the year-followup
  
  cov_data$year_complete <- year_visit %>% 
    filter(V3yn == 'yes') %>% 
    .$ID
  
  ## automated cleaning
  
  year_visit <- globals$var_coding$year_visit %>% 
    pmap(recode_var, 
         data = year_visit) %>% 
    reduce(left_join, 
           by = 'ID')
  
# joining the data sets, constraining to the set with the complete year follow-up -----
  
  insert_msg('Joining the data sets')
  
  cov_data$data_tbl <- outer_rbind(prev_visits, year_visit) %>% 
    filter(ID %in% cov_data$year_complete)
  
# additional wrangling tasks: time re-coding, setting up the constant variables ------
  
  insert_msg('Additional wrangling - time recoding, constant setup')
  
  cov_data$data_tbl <- cov_data$data_tbl %>% 
    mutate(visit = paste0('V', time) %>% 
             factor, 
           cat_WHO = factor(cat_WHO, c('A', 'HM', 'HS')), 
           time_numeric = recode_vec(time, 
                                     "0 = 0; 1 = 60; 2 = 100; 3 = 180; 4 = 360"), 
           Ddimer_elv = droplevels(Ddimer_elv), 
           smwd_dref = smwd - smwd_ref, 
           smwd_dlower = smwd - smwd_lower, 
           hair_loss_sympt = factor(hair_loss_sympt, c('no', 'yes')), 
           derma_sympt = factor(derma_sympt, c('no', 'yes')), 
           BRCS_class = factor(BRCS_class, c('low', 'medium', 'high')), 
           EQ5DL_low  = factor(EQ5DL_low, c('no', 'yes')), 
           Stress_hi = factor(Stress_hi, c('no', 'yes')), 
           SES_low = factor(SES_low, c('no', 'yes')), 
           TSAT_red = factor(TSAT_red, c('no', 'yes')), 
           RV_red = factor(RV_red, c('no', 'yes')), 
           Chalder_FS_bimodal = recode_yn(Chalder_FS_bimodal), 
           smwd_low = ifelse(smwd_dref < 0, 'yes', 'no'), 
           smwd_low = factor(smwd_low, c('no', 'yes')), 
           weight_class = factor(weight_class, c('normal', 'overweight', 'obesity')))
  
  ## binarization of the EQ5DL components (>= 2)
  
  for(i in c('EQ5DL_mobility', 
             'EQ5DL_selfcare', 
             'EQ5DL_activities', 
             'EQ5DL_pain', 
             'EQ5DL_anxiety')) {
    
    cov_data$data_tbl <- cov_data$data_tbl %>% 
      mutate(!!paste0(i, '_bi') := factor(ifelse(.data[[i]] > 1, 'yes', 'no')))
    
  }
  
  ## constants
  
  constants <- globals$var_lexicon %>% 
    filter(var_collection == 'constant')
  
  for(i in 1:nrow(constants)) {
    
    cov_data$data_tbl <- set_constant(data = cov_data$data_tbl, 
                                      variable = constants$variable[i], 
                                      reference = constants$ref_time[i], 
                                      ID_var = 'ID', 
                                      time_var = 'time')
    
    
  }

# generating a table used for modeling ----
  
  insert_msg('Generating a modeling table')
  
  cov_data$mod_table <- cov_data$data_tbl %>% 
    select(all_of(globals$mod_features$variable), 
           time, 
           ID)
  
  for(i in 1:nrow(globals$mod_features)) {
    
    cov_data$mod_table <- set_constant(data = cov_data$mod_table, 
                                       variable = globals$mod_features$variable[i], 
                                       reference = globals$mod_features$reference[i], 
                                       ID_var = 'ID', 
                                       time_var = 'time')
    
  }
  
  ## filtering
  ## stratification of the age variable
  
  cov_data$mod_table <- cov_data$mod_table %>% 
    filter(time == 1) %>% 
    select( - time) %>% 
    mutate(age = cut(age, 
                     c(-Inf, 50, 65, Inf), 
                     c('up to 50', 
                       '51 - 65', 
                       'over 65')))

# Generating a table used for correlation, PCA and clustering analysis of the key 1-year features ------
  
  insert_msg('Clustering analysis table')
  
  cov_data$clust_tbl <- cov_data$data_tbl %>% 
    filter(visit == 'V4') %>% 
    select(ID, all_of(globals$clust_variables)) %>% 
    filter(complete.cases(.))
  
# END -----
  
  rm(prev_visits, 
     year_visit, 
     i, 
     constants)
  
  insert_tail()