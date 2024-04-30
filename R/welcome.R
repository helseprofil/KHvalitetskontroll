# Welcome messages

cat("\n\n---\n",
    "\nWelcome to KHvalitetskontroll!\n", 
    "\n---\n", 
    "\nAvailable USER files:\n",
    "- Kvalitetskontroll_del1.Rmd\n",
    "  - Manuell sjekk\n",
    "- Kvalitetskontroll_del2.Rmd\n",
    "  - Compare\n",
    "- Interaktiv.Rmd\n",
    "  - Functions for interactive quality control\n",
    "- Friskviksjekk.Rmd\n",
    "  - Functions to compare FRISKVIK vs KUBE\n",
    "\n---\n",
    "\n- Valid GEO for outlier detection and recoding: 2024\n",
    "\n---\n",
    if(exists(".updatemsg")) {
      .updatemsg
    },
    "\n---\n")


# Remove messages and unneccessary objects
rmobj <- c(".updatemsg", ".lastupdate", ".localupdate")
for(i in rmobj){
  if(exists(i)){
    rm(list = i)
  }
}