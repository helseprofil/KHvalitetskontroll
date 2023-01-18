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
library(ggh4x)

# Load internal functions
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/misc.R")
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/functions_step1.R")
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/functions_step2.R")
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/functions_interactive.R")

# Load list of all dimensions, and make hidden object
source("https://raw.githubusercontent.com/helseprofil/misc/main/alldimensions.R") 
.ALL_DIMENSIONS <- ALL_DIMENSIONS
rm(ALL_DIMENSIONS)

# Checks folder structure and create a folder for local files
if(!dir.exists("USER/Lokale filer, overskrives ikke")){
  dir.create("USER/Lokale filer, overskrives ikke")
}

# Set ggplot theme and color palette
theme_set(theme_bw())
theme_update(legend.position = "top", 
             panel.grid.minor = element_blank(),
             text = element_text(color = "black"),
             plot.margin = margin(t = 1, b = 1, r = 1, unit = "cm"))


# Set global options
PROFILEYEAR <- 2023  # For saving in correct folder
DUMPS <- c("dfnew_flag", "dfold_flag", "compareKUBE") # Default is to create all file dumps

      