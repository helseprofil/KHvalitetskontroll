#' flag_new
#' 
#' Initiate flagged version of dfnew (dfnew_flag), saved to global environment
#' Identifies common and new dimensions compared to old KUBE
#' For common dimensions, all new rows are flagged (sets newrow = 1)
#' For new dimensions, all rows != 0 (total numbers) are flagged as new
#' 
#'
#' @param data1 new KUBE, defaults to dfnew
#' @param data2 old KUBE, defaults to dfold
#' @param dims character vector of dimensions columns, defaults to DIMENSIONS set in INPUT
#'
#' @return
#' @export
#'
#' @examples
FlagNew <- function(data1 = dfnew, 
                    data2 = dfold, 
                    dims = DIMENSIONS){
  
  # Identify dimension columns present in new and old KUBE
  # Separate into new and same dimensions for flagging
  dimnew <- names(data1[, names(data1) %in% dims, with = F])
  dimold <- names(data2[, names(data2) %in% dims, with = F])
  newdims <- dimnew[!dimnew %in% dimold]
  commondims <- dimnew[dimnew %in% dimold]
  
  # Initiate flagged version of new KUBE (sets newrow = 0), saves to global env
  # Sorts KUBE according to common and new dims
  dfnew_flag <<- copy(data1)[, newrow := 0L]
  setkeyv(dfnew_flag, c(commondims, newdims))
  
  # For same dimensions, flag all rows with new levels 
  # Loops over common dimensions. Flags previously unflagged rows for new levels 
  walk(commondims, 
       ~dfnew_flag[!dfnew_flag[[.x]] %in% unique(data2[[.x]]) & newrow == 0,
                   newrow := 1L])
  
  # For new dimensions, flag any rows != 0 (i.e. not total numbers)
  walk(newdims,
       ~dfnew_flag[dfnew_flag[[.x]] != 0 & newrow == 0,
                   newrow := 1L])
}


###########
# -----
###########


