# This script performs data analyses

  library(soucer)
  library(exda)
  library(lme4)
  library(lmerTest)
  library(lmqc)
  library(kinet)
  library(dplyr)
  library(tidyverse)
  library(rlang)
  library(clustTools)
  library(somKernels)
  library(ggrepel)
  
  c('./tools/project_tools.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
  insert_head()
  
# analysis scripts -----
  
  insert_msg('Data analysis')
  
  c('./analysis scripts/longitudinal_numeric.R', 
    './analysis scripts/longitudinal_prevalence.R', 
    './analysis scripts/prevalence_summary.R', 
    './analysis scripts/correlations.R', 
    './analysis scripts/pca.R', 
    './analysis scripts/cluster_development_binary.R', 
    './analysis scripts/clustering_binary.R', 
    './analysis scripts/cluster_characteristic.R') %>% 
    source_all(message = TRUE, crash = TRUE)

  
# END ----
  
  insert_tail()