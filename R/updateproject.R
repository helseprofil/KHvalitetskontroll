cat("\nSer etter endringer\n")
gert::git_branch_checkout("main")
gert::git_pull()
cat("\nAlle filer oppdatert\n")
