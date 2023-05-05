# Switch to main branch, look for updates:
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/updateproject.R")

# Make sure missing packages are installed, and versions are compatible with renv.lock
renv::restore()

# Load packages
cat("\n...Loading packages and solving conflicts...")
library(conflicted)
library(RODBC)
library(dplyr)
library(forcats)
library(stringr)
library(purrr)
library(DT)
library(ggplot2)
library(ggh4x)
library(norgeo)
library(collapse)
library(data.table)

# Solve conflicts
conflicted::conflict_prefer("filter", "dplyr", quiet = T)
conflicted::conflict_prefer("lag", "dplyr", quiet = T)
conflicted::conflict_prefer("D", "collapse", quiet = T)
conflicted::conflict_prefer("between", "data.table", quiet = T)
conflicted::conflict_prefer("first", "data.table", quiet = T)
conflicted::conflict_prefer("last", "data.table", quiet = T)
conflicted::conflict_prefer("transpose", "data.table", quiet = T)

# Load project functions
cat("\n...Loading project functions...")
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/misc.R")
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/functions_step1.R")
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/functions_step2.R")
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/functions_friskvik.R")
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/functions_interactive.R")
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/globals.R")
cat("\n...done!")

# Source welcome messages
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/welcome.R")
