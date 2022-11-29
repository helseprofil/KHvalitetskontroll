# Welcome messages

cat("\nWelcome to KHvalitetskontroll\n", 
    "\n---\n", 
    "\nAvailable user files:\n",
    "- Kvalitetskontroll_del1.Rmd\n",
    "- Kvalitetskontroll_del2.Rmd\n",
    "- Interactive.Rmd (not active)",
    "\n---\n",
    if(exists("updatemsg")) {
      updatemsg
    },
    "\n---\n")

if(exists("updatemsg")){
  rm(updatemsg)
}


# Check package versions
cat("\nControlling package versions:\n")

renv::status()