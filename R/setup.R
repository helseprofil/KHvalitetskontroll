# Switch to main branch and pull any changes (add new files etc)
gert::git_pull()

# Load packages 
source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")
kh_load(dplyr, tidyr, stringr, purrr, data.table)

# Remove redundant functions
rm(list = ls()[grepl("^kh_", ls())])
rm(list = ls()[grepl("^pkg_", ls())])
rm(list = ls()[grepl("^git_", ls())])
