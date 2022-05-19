# This program executes the scripts of the CovILD 1-year project

# libraries -----

  library(soucer)

# sourcing analysis scripts ----

  print(source_all(c('import.R', 
                     'exploration.R', 
                     'analysis.R', 
                     'modeling.R', 
                     'paper.R'), 
                   message = TRUE, crash = TRUE))
  
  save.image()
  
# END ----