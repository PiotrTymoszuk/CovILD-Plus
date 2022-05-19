# Modeling tasks

  library(lmqc)
  library(tidyverse)
  library(rlang)
  library(caret)
  library(caretExtra)
  library(soucer)
  library(MASS)
  library(doParallel)
  library(glmnet)
  
  select <- dplyr::select
  
# Modeling scripts ------
  
  c('./modeling scripts/univariate.R', 
    './modeling scripts/multivariate.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END -----