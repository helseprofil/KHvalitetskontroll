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
library(ggplot2)

# Load internal functions
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/misc.R")
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/functions_step1.R")
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/functions_step2.R")

# Checks folder structure and create a folder for local files

projectroot <- rprojroot::find_root("KHvalitetskontroll.Rproj")

if(!dir.exists(file.path(projectroot, "USER/Lokale filer, overskrives ikke"))){
  dir.create(file.path(projectroot, "USER/Lokale filer, overskrives ikke"))
}

rm(projectroot)

# Set ggplot theme and color palette

theme_set(theme_bw())
theme_update(legend.position = "bottom", 
             panel.grid.minor = element_blank(),
             text = element_text(color = "black"))