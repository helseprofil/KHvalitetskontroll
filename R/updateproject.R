.lastupdate <- "12.05.2023"

ask <- utils::askYesNo(paste0("SISTE OPPDATERING AV BRUKERFILER: ", .lastupdate, "\n\nVil du oppdatere?"))

if(ask) {
  cat("\nLooks for updates\n",
      "\nIf this fails, run to following:\n",
      '\n source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")\n',
      "\n kh_install(KHvalitetskontroll)\n\n")
  gert::git_branch_checkout("main")
  gert::git_reset_hard()
  gert::git_pull()
  
  # Activate renv, read current lockfile after updating
  renv::activate()
  
  .updatemsg <- "\nEverything is up to date!\n"
} else {
  .updatemsg <- paste0("\nUser files not updated, new versions may be available.\n\nMost recent update: ", .lastupdate)
}

rm(ask)