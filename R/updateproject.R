.lastupdate <- "12.01.2022"

ask <- utils::askYesNo(paste0("SISTE OPPDATERING AV BRUKERFILER: ", .lastupdate, "\n\nVil du oppdatere?"))

if(ask) {
  cat("\nSer etter endringer\n",
      "\nDersom dette feiler:\n",
      '\n source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")\n',
      "\n kh_install(KHvalitetskontroll)\n\n")
  gert::git_branch_checkout("main")
  gert::git_reset_hard()
  gert::git_pull()
  .updatemsg <- "\nAlt er oppdatert!\n"
} else {
  .updatemsg <- paste0("\nBrukerfiler ikke oppdatert, nye versjoner kan være tilgjengelige.\n\nSist oppdatert: ", .lastupdate)
}

rm(ask)