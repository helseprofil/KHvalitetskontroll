ask <- utils::askYesNo("Vil du oppdatere brukerfilene til siste versjon? Dette vil overskrive eksisterende filer. Dersom du har gjort endringer du vil beholde, velg nei og kopier filene til lokal mappe")


if(ask) {
  cat("\nSer etter endringer\n")
  gert::git_branch_checkout("main")
  gert::git_reset_hard()
  gert::git_pull()
  updatemsg <- "\nAlle filer oppdatert\n"
} else {
  updatemsg <- "\nBrukerfiler ikke oppdatert. Kopier filer du vil beholde og restart prosjektet (Ctrl + Shift + F10) for å hente siste versjon\n"
}

rm(ask)
