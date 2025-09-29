#!/usr/bin/env Rscript

# Install required packages for enhanced NFL model
required_packages <- c(
  'nflreadr', 'rstan', 'shiny', 'shinydashboard', 
  'DT', 'plotly', 'dplyr', 'tidyr', 'readr', 
  'zoo', 'purrr', 'stringr'
)

installed_packages <- installed.packages()[, 'Package']
new_packages <- required_packages[!(required_packages %in% installed_packages)]

if(length(new_packages)) {
  cat('Installing packages:', paste(new_packages, collapse=', '), '\n')
  install.packages(new_packages, repos='https://cran.rstudio.com/')
  cat('Package installation complete!\n')
} else {
  cat('All required packages already installed\n')
}

# Test loading key packages
cat('Testing package loading...\n')
library(nflreadr)
library(dplyr)
cat('✓ Key packages loaded successfully\n')