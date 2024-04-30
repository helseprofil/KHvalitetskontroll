# Change according to when the user files or packages were last updated
.lastupdate <- "30.04.2024b"

# Find latest local update date
.localupdate <- grep(".lastupdate <-", readLines("R/updateproject.R", n = 3), value = T)
.localupdate <- sub(".*(\\d{2}.\\d{2}.\\d{4}).*", "\\1", .localupdate)

if(length(.localupdate) == 0){
  .localupdate <- "unknown"
}

.updateproject <- function(lu, loc){
  
  # Check if main branch is active, if not return and let the user keep on dev work. 
  b <- system("git branch --show-current", intern = TRUE)
  
  if(b != "main"){
    message("\nYou are on the branch '", b, 
            "'.\nKeep on the good dev work or switch to the main branch `system(git checkout main)` to continue with quality control!")
    return(invisible(NULL))
  } 
  
  message("\nYou are on the main branch")
  
  if(isTRUE(lu == loc)){
    message("\nPackages and user files are up to date, you are ready to go!")
    return(invisible(NULL))
  }
  
  # Update all files if on master branch and updates available
  if(lu != loc){
    choice <- menu(choices = c("Yes", "No"),
                   title = paste0("\nUpdates available for packages or USER files!!",
                                  "\n\nLast version on GitHub: ", lu,
                                  "\nYour local version: ", loc,
                                  "\n\nUpdate files (recommended)?"))
    
    if(choice == 1){
      message("\nFetching updates...")
      invisible(system("git fetch origin main"))
      invisible(system("git reset --hard origin/main"))
      invisible(system("git pull"))
    } else {
      message("\nSkipping updates, your packages and USER files might be outdated.")
    }
  }
}

.updateproject(lu = .lastupdate,
               loc = .localupdate)
