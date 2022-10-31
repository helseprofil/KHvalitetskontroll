source("renv/activate.R")
library(stats)

# Switch to main branch, look for updates:
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/updateproject.R")

# Source setup file to load functions:
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/setup.R")

# Just for fun
cat("\nRandom Quote just for fun...:\n")

if (interactive())
  try(fortunes::fortune(), silent = TRUE)

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

# Syncronize project library with lockfile

cat("\nSynchronize project library\n")
renv::restore()

cat("testbeskjed")

# Welcome messages

cat("\nWelcome to KHvalitetskontroll\n", 
    "\n---\n", 
    "\nAvailable user files:\n",
    "- Kvalitetskontroll_del1.Rmd\n",
    "- Kvalitetskontroll_del2.Rmd\n",
    "- Interactive.Rmd (not active)",
    "\n---\n",
    updatemsg, 
    "\n---\n")

rm(updatemsg)
