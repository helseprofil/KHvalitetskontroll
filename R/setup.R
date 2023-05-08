# Switch to main branch, look for updates:
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/updateproject.R")

# Make sure missing packages are installed, and versions are compatible with renv.lock
renv::restore()

# Load packages

cat("\n...Loading packages and solving conflicts...")
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/load_packages_functions.R")
cat("\n...done!")

# Source welcome messages
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/welcome.R")
