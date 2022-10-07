



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



#' NewRows
#' 
#' Flags rows in new KUBE which does not exist in old KUBE
#'
#' @param data1 defaults to dfnew
#' @param data2 defaults to dfold
#' @param dim dimensions to compare for flagging
#'
#' @return
#' @export
#'
#' @examples
NewRows <- function(data1 = dfnew,
                   data2 = dfold,
                   dim = NULL){
  
  data1[, newrow := 0]
  
}

#' ExpRows
#' 
#' Flags rows in old KUBE which no longer exists in new KUBE
#'
#' @param data1 defaults to dfnew
#' @param data2 defaults to dfold
#' @param dim dimensions to compare for flagging
#'
#' @return
#' @export
#'
#' @examples
ExpRows <- function(data1 = dfnew,
                   data2 = dfold,
                   dim = NULL){
  
  data2[, exprow := 0]
  
}


  
  dimold <- unique(data2[[dim]])
  
  data1 %>% 
    mutate(newrow = case_when(!(.data[[dim]] %in% dimold) ~ 1,
                              TRUE ~ 0))
}


FlagNewRow <- function()

