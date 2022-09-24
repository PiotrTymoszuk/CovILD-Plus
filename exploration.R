# Performs explorative data analysis

# tools ------

  library(exda)
  library(soucer)
  library(plyr)
  library(tidyverse)
  library(rlang)
  library(furrr)
  
  insert_head()
  
  source_all('./tools/project_tools.R', message = TRUE, crash = TRUE)
  
# exploration scripts ------
  
  insert_msg('Sourcing the exploration scripts')
  
  c('./exploration scripts/visit_dataes.R', 
    './exploration scripts/fixed_variables.R', 
    './exploration scripts/long_variables.R', 
    './exploration scripts/drop_out_base.R', 
    './exploration scripts/drop_out_course.R', 
    './exploration scripts/steroid_base.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END -----
  
  insert_tail()