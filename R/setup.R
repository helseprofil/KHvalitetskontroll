# Load packages 
library(utils)
source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")
kh_load(dplyr, tidyr, stringr, purrr, data.table)

# Remove redundant functions
rm(list = ls()[grepl("^kh_", ls())])
rm(list = ls()[grepl("^pkg_", ls())])
rm(list = ls()[grepl("^git_", ls())])
