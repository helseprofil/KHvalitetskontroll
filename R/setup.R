# Switch to main branch and pull any changes (add new files etc)
cat("\nSer etter endringer\n")
gert::git_branch_checkout("main")
gert::git_pull()
cat("\nAlle filer oppdatert\n")

# Load packages 
source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")
kh_load(dplyr, tidyr, stringr, purrr, data.table)

# Remove redundant functions
rm(list = ls()[grepl("^kh_", ls())])
rm(list = ls()[grepl("^pkg_", ls())])
rm(list = ls()[grepl("^git_", ls())])

# Source internal scripts
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/misc.R")
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/functions_step1.R")
