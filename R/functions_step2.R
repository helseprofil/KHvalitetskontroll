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
NewYear <- function(data1 = dfnew,
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

#' CompareCols
#'
#' Compare the columns across two KUBE files
#' Report whether any new (not in old), or expired (not in new) columns are present, 
#' 
#' @param data1 New file, defaults to dfnew
#' @param data2 Old file, defaults to dfold
#'
#' @return
#' @export
#'
#' @examples
CompareCols <- function(data1 = dfnew,
                         data2 = dfold){
  
  new <- names(data1 %>% 
                 select(!any_of(as.character(names(data2))))) 
  
  exp <- names(data2 %>% 
                 select(!any_of(as.character(names(data1))))) 
  
  msgnew <- case_when(length(new) == 0 ~ "\nNo new columns.",
                      TRUE ~ paste0("\nNew columns found: ", str_c(new, collapse = ", ")))
  
  msgold <- case_when(length(exp) == 0 ~ "\nNo expired columns.",
                      TRUE ~ paste0("\nExpired columns found: ", str_c(exp, collapse = ", ")))
  
  cat(msgnew)
  cat(msgold)
  
}


CompareRows <- function(data1 = dfnew,
                        data2 = dfold){
  
  
}






