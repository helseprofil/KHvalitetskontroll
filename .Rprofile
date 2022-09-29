source("renv/activate.R")
library(stats)

cat("\nSer etter endringer\n")
gert::git_branch_checkout("main")
gert::git_pull()
cat("\nAlle filer oppdatert\n")

# Source setup file
source("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/R/setup.R")

# Just for fun
cat("\nRandom Quote for the funz...:\n")

if (interactive())
  try(fortunes::fortune(), silent = TRUE)

cat("\nWelcome to KHvalitetskontroll\n\nOpen 'Kvalitetskontroll.rmd' in USER folder to start")
