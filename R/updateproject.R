ask <- utils::askYesNo("Vil du oppdatere brukerfilene til siste versjon?\n-Dette vil overskrive eksisterende filer.\n-Dersom du har gjort endringer du vil beholde, velg nei og kopier filene til lokal mappe")


if(ask) {
  cat("\nSer etter endringer\n")
  gert::git_branch_checkout("main")
  gert::git_reset_hard()
  gert::git_pull()
  updatemsg <- "\nEverything up to date!\n"
} else {
  updatemsg <- "\nUser files not updated, new versions may be available.\nCopy the files you want to keep and restart the project (Ctrl + Shift + F10) to get the latest updates.\n"
}

rm(ask)
