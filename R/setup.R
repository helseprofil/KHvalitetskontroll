# Switch to main branch, look for updates:
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/updateproject.R")

# Activate renv, read current lockfile
source("renv/activate.R")

# Load packages
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

# Load internal functions
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/dev/R/misc.R")
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/dev/R/functions_step1.R")
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/dev/R/functions_step2.R")
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/dev/R/functions_friskvik.R")
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/dev/R/functions_interactive.R")
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/dev/R/globals.R")

# Source welcome messages
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/dev/R/welcome.R")
