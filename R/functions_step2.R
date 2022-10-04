#' Find new year
#' 
#' Extracts unique values of year in the new file (dfnew) which do not exist in the old file (ref).
#'
#' @param data1 new data file (new)
#' @param data2 old data file (ref)
#'
#' @return
#' @export
#'
#' @examples
FindNewyear <- function(data1 = dfnew,
                        data2 = dfold) {
  
  if (any(!unique(data1$AAR) %in% unique(data2$AAR))) {
    res <- data1 %>% 
      filter(!AAR %in% data2$AAR) %>% 
      select(AAR) %>% 
      unique()
    return(as.vector(res$AAR)
    )
    
  } else {
    return("no exclude year")
  }
}

new_year <- find_newyear()
print(paste0("New year:", new_year))


#' Find_newcols
#' 
#' Extracts names of columns found in the new file, but not in the old (ref) file
#'
#' @param data1 
#' @param data2
#'
#' @return
#' @export
#'
#' @examples
find_newcols <- function(data1 = dfnew, data2 = dfold) {
  
  names(data1 %>% 
          select(!any_of(as.character(names(data2))))) 
}

#' Print_newcols
#' 
#' Prints the results from find_newcols
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
print_newcols <- function(x){
  ifelse(length(x) > 0,
         paste0("Newcols detected: ", str_c(x, collapse = ", ")),
         paste0("No new columns in kube1, not found in kube2"))
}

find_newcols(newfile, oldfile) %>% print_newcols()