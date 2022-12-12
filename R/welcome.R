# Welcome messages

cat("\nWelcome to KHvalitetskontroll\n", 
    "\n---\n", 
    "\nAvailable user files:\n",
    "- Kvalitetskontroll_del1.Rmd\n",
    "  - Manuell sjekk\n",
    "- Kvalitetskontroll_del2.Rmd\n",
    "  - Compare\n",
    "- Interaktiv.Rmd",
    "  - Functions for interactive quality control",
    "\n---\n",
    if(exists(".updatemsg")) {
      .updatemsg
    },
    "\n---\n")

if(exists(".updatemsg")){
  rm(.updatemsg)
}

if(exists(".lastupdate")){
  rm(.lastupdate)
}



# Check package versions
cat("\nControlling package versions:\n")

renv::status()