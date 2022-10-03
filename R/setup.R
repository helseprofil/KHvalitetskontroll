# Load packages 
# source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")
# rm(list = ls()[grepl("^pkg_", ls())])

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(data.table)
library(gert)
#kh_load(dplyr, tidyr, stringr, purrr, data.table, gert)

source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/misc.R")
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/functions_step1.R")

# Remove redundant functions
# rm(list = ls()[grepl("^kh_", ls())])
# rm(list = ls()[grepl("^pkg_", ls())])
# rm(list = ls()[grepl("^is_not_", ls())])
