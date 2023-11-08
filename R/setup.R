# Set encoding for R>=4.2
if(version$major >= 4 & version$minor >= 2){
  Sys.setlocale("LC_ALL", "nb-NO.UTF-8")
}

# Switch to main branch, look for updates:
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/updateproject.R")

# Load packages
cat("\n...Loading packages and solving conflicts...\n")

## Make sure all packages in the lockfile is installed and package version correct
renv::restore(prompt = FALSE)
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/load_packages_functions.R")
cat("\n...done!")

# Source welcome messages
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/welcome.R")
