# Change according to when the user files were last updated
.lastupdate <- "10.04.2024"

.updateproject <- function(lu){
  
  # Check if main branch is active, if not return and let the user keep on dev work. 
  b <- system("git branch --show-current", intern = TRUE)
  if(b != "main"){
    message("\nYou are on the branch '", b, "'.\nKeep on the good dev work or switch to the main branch `system(git checkout main)` to continue with quality control!")
    return(invisible(NULL))
  } else {
    message("\nYou are on the main branch, ready for some good old quality control, enjoy!")
  }
  
  # Update all files if on master branch
  
  if(b == "main"){
    choice <- menu(choices = c("Yes", "No"),
                   title = paste0("\nUpdate package version and user files (recommended)?",
                                  "\nLast updated on: ", lu))
    
    if(choice == 1){
      system("git checkout main")
      system("git reset --hard")
      system("git pull")
      renv::activate()
    } else {
      message("\nSkipping update, your packages and user files might be outdated")
    }
  }
}

.updateproject(lu = .lastupdate)