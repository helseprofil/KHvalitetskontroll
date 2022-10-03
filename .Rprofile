source("renv/activate.R")
library(stats)

# Switch to main branch, look for updates:
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/updateproject.R")

# Source setup file, loading functions:
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/setup.R")

# Just for fun
cat("\nRandom Quote just for fun...:\n")

if (interactive())
  try(fortunes::fortune(), silent = TRUE)

cat("\nWelcome to KHvalitetskontroll\n", 
    "\n---\n", 
    updatemsg, 
    "\n---\n", 
    "\nAvailable user files:\n",
    "- Kvalitetskontroll.Rmd\n",
    "- Interactive.Rmd")

rm(updatemsg)
