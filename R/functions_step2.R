


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


FlagNewRows <- function(data1 = dfnew,
                        data2 = dfold,
                        dim = DIMENSIONS){
  
  # Use NewRows and Exprows to flag both new rows in new kube and expired rows in old cube
  # Map over DIMENSIONS to update newrow/exprow, can use case_when for this.
  
}

