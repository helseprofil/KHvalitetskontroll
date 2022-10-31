#Load packages
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(data.table)
library(DT)
library(gert)
library(rmarkdown)
library(tools)

# Load internal functions
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/misc.R")
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/functions_step1.R")
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/functions_step2.R")

# Checks folder structure
if(!dir.exists("USER/Output")){
  dir.create("USER/Output")
}

if(!dir.exists("USER/Filedumps")){
  dir.create("USER/Filedumps")
}

if(!dir.exists("USER/Lokale filer, overskrives ikke")){
  dir.create("USER/Lokale filer, overskrives ikke")
}
