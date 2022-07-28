## ---------------------------
##
## Script name: set_up_enviroment
##
## Purpose of script: functions for installing packages necessary for modeling
##
## Author: Zach Tallevast
##
## Date Created: 2021-12-13
##
## Email: zetvzb@gmail.com
## ---------------------------
##
## Notes:
##   
##
## ---------------------------



# Specify Required Packages 
.packages = c("tidyverse","knitr","scales","english", "rmarkdown", "RODBC", "gt", "janitor")


# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()[,'Package']
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)




