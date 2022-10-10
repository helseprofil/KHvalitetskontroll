#' FlagNew
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
.FlagNew <- function(data1, 
                    data2, 
                    dims){
  
  # Identify dimension columns present in new and old KUBE
  # Separate into common and new dimensions for flagging
  dimnew <- names(data1[, names(data1) %in% dims, with = F])
  dimold <- names(data2[, names(data2) %in% dims, with = F])
  commondims <- dimnew[dimnew %in% dimold]
  newdims <- dimnew[!dimnew %in% dimold]
  
  msgcommon <- case_when(length(commondims) == 0 ~ "\n-No common dimensions found",
                      TRUE ~ paste0("\n-Common columns found: ", str_c(commondims, collapse = ", ")))
  
  msgnew <- case_when(length(newdims) == 0 ~ "\n-No new dimensions.",
                      TRUE ~ paste0("\n-New dimensions found: ", str_c(newdims, collapse = ", ")))
  
  cat(msgcommon)
  cat(msgnew)
  
  # Initiate flagged version of new KUBE (sets newrow = 0), saves to global env
  # Sorts KUBE according to common and new dims
  dfnew_flag <<- copy(data1)[, newrow := 0L]
  setkeyv(dfnew_flag, c(commondims, newdims))
  
  # For common dimensions, flag all rows with new levels 
  # Loops over common dimensions. Flags previously unflagged rows for new levels 
  walk(commondims, 
       ~dfnew_flag[!dfnew_flag[[.x]] %in% unique(data2[[.x]]) & newrow == 0,
                   newrow := 1L])
  
  cat("\n-For common dimensions, flagged all rows with new levels")
  
  # For new dimensions, flag any rows != 0 (i.e. not total numbers)
  if(length(newdims) != 0) {
    walk(newdims,
         ~dfnew_flag[dfnew_flag[[.x]] != 0 & newrow == 0,
                     newrow := 1L])
    cat("\n-For new dimensions, flagged all rows not representing total numbers")
  } 
  
  cat("\n-Flagged version of new KUBE created: dfnew_flag")
}

#' FlagOld
#' 
#' Initiate flagged version of dfold (dfold_flag), saved to global environment
#' Identifies common and expired dimensions compared to new KUBE
#' For common dimensions, all expired rows are flagged (sets exprow = 1)
#' For expired dimensions, all rows != 0 (total numbers) are flagged as expired
#' 
#' @param data1 new KUBE, defaults to dfnew
#' @param data2 old KUBE, defaults to dfold
#' @param dims character vector of dimensions columns, defaults to DIMENSIONS set in INPUT
#'
#' @return
#' @export
#'
#' @examples
.FlagOld <- function(data1,
                    data2,
                    dims){
  
  # Identify dimension columns present in old and new KUBE
  # Separate into common and expired dimensions for flagging
  dimnew <- names(data1[, names(data1) %in% dims, with = F])
  dimold <- names(data2[, names(data2) %in% dims, with = F])
  commondims <- dimold[dimold %in% dimnew]
  expdims <- dimold[!dimold %in% dimnew]
  
  msgcommon <- case_when(length(commondims) == 0 ~ "\n-No common dimensions found",
                         TRUE ~ paste0("\n-Common columns found: ", str_c(commondims, collapse = ", ")))
  
  msgexp <- case_when(length(expdims) == 0 ~ "\n-No expired dimensions.",
                      TRUE ~ paste0("\n-Expired dimensions found: ", str_c(expdims, collapse = ", ")))
  
  cat(msgcommon)
  cat(msgexp)
  
  # Initiate flagged version of old KUBE (sets newrow = 0), saves to global env
  # Sorts KUBE according to common and new dims
  dfold_flag <<- copy(data2)[, exprow := 0L]
  setkeyv(dfold_flag, c(commondims, expdims))
  
  # For common dimensions, flag all rows with expired levels 
  # Loops over common dimensions. Flags previously unflagged rows for expired levels 
  walk(commondims, 
       ~dfold_flag[!dfold_flag[[.x]] %in% unique(data1[[.x]]) & exprow == 0,
                   exprow := 1L])
  
  cat("\n-For common dimensions, flagged all rows with expired levels")
  
  # For expired dimensions, flag any rows != 0 (i.e. not total numbers)
  if(length(expdims) != 0) {
    walk(expdims,
         ~dfold_flag[dfold_flag[[.x]] != 0 & exprow == 0,
                     exprow := 1L])
    cat("\n-For expired dimensions, flagged all rows not representing total numbers")
  } 
  
  cat("\n-Flagged version of old KUBE created: dfold_flag")
}

FlagKubes <- function(data1 = dfnew,
                      data2 = dfold,
                      dims = DIMENSIONS,
                      dumps = DUMPS,
                      dumpname = DUMPname){
  
  cat("STARTS flagging new kube")
  .FlagNew(data1 = data1,
          data2 = data2,
          dims = dims)
  cat("\n\nSTARTS flagging old kube")
  .FlagOld(data1 = data1,
          data2 = data2,
          dims = dims)
  cat("\n\nCOMPLETED flagging\n")
  
  if("dfnew_flag" %in% dumps){
    fwrite(dfnew_flag, 
           file = paste0(dumpname, "_flagged.csv"),
           sep = ";")
    cat("\nFILEDUMP: Flagged new KUBE")
  }
  
  if("dfold_flag" %in% dumps){
    fwrite(dfold_flag, 
           file = paste0(dumpname, "_old_flagged.csv"),
           sep = ";")
    cat("\nFILEDUMP: Flagged old KUBE")
  }

}


