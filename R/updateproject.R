ask <- utils::askYesNo("SISTE OPPDATERING AV BRUKERFILER 08.12.2022\n\nVil du oppdatere?")

if(ask) {
  cat("\nSer etter endringer\n",
      "\nDersom dette feiler:\n",
      '\n source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")\n',
      "\n kh_install(KHvalitetskontroll)\n\n")
  gert::git_branch_checkout("main")
  gert::git_reset_hard()
  gert::git_pull()
  updatemsg <- "\nEverything up to date!\n"
} else {
  updatemsg <- "\nUser files not updated, new versions may be available.\nCopy the files you want to keep and restart the project (Ctrl + Shift + F10) to get the latest updates.\n"
}

rm(ask)