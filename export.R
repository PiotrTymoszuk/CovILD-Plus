# Exports the cleared covild data till the 1-year visit

  library(soucer)
  library(tidyverse)

  insert_head()
  
  covild <- list(data = cov_data$data_tbl, 
                 dict = globals$var_lexicon)
  
  save(covild, file = './export/covild.RDa')

  insert_tail()  