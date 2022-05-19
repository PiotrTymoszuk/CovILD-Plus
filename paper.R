# This script generates figures, tables and a report file

# Tools ------

  library(exda)
  library(soucer)
  library(tidyverse)
  library(figur)
  library(cowplot)
  library(knitr)
  library(rmarkdown)
  library(bookdown)
  library(flextable)
  library(writexl)

  source_all('./tools/project_tools.R', message = TRUE, crash = TRUE)

  insert_head()
  
# Rendering scripts -----
  
  insert_msg('Executing the paper scripts')
  
  c('./paper scripts/paper_tables.R',
    './paper scripts/paper_figures.R', 
    './paper scripts/suppl_figures.R', 
    './paper scripts/deploy_paper.R') %>% 
    source_all(message = TRUE, crash = TRUE)

# END ----
  
  insert_tail()