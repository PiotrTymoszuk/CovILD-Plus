# Descriptive stats on the vists times

  insert_head()
  
# container ------
  
  visits <- list()
  
# calculating medians and IQRs -----
  
  insert_msg('Medians and IQRs')
  
  visits$post_diagnosis <- cov_data$dates %>% 
    filter(ID %in% cov_data$year_complete) %>% 
    explore(variables = c('mo2_fup_delta_diagno', 
                          'mo3_fup_delta_diagno', 
                          'mo6_fup_delta_diagno', 
                          'year_fup_delta_diagno'), 
            what = 'table', 
            pub_styled = TRUE)
  
  visits$post_symptoms <- cov_data$dates %>% 
    filter(ID %in% cov_data$year_complete) %>% 
    explore(variables = c('mo2_fup_delta_sympt', 
                          'mo3_fup_delta_sympt', 
                          'mo6_fup_delta_sympt', 
                          'year_fup_delta_sympt'), 
            what = 'table', 
            pub_styled = TRUE)
  
# END ------
  
  insert_tail()