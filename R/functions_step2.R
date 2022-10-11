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
                     commondims,
                     newdims){
  
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
                     commondims,
                     expdims){

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


#' CreateCompare
#' 
#' Create combined KUBE for comparison of value columns. 
#' 
#' -Removes new or expired rows
#' -Merges on common dimensions
#' -Select common value columns, with suffixes _new and _old
#'
#' @param data1 new flagged KUBE
#' @param data2 old flagged KUBE
#' @param commondims common dimensions
#' @param commonvals common values
#'
#' @return
#' @export
#'
#' @examples
.CreateCompare <- function(data1,
                           data2,
                           commondims,
                           commonvals){
  
  # Format new KUBE
  cat("\n-Formats new KUBE")
  cat("\n  -Remove new rows, select common dimensions and values")
  comparenew <- copy(data1)
  # Remove new rows, select common columns and values
  comparenew <- comparenew[newrow == 0,
                           c(commondims, commonvals), with = F]
  # Add suffix to value columns
  commonvals_new <- paste0(commonvals, "_new")
  setnames(comparenew, commonvals, commonvals_new)
  
  # Format old KUBE
  cat("\n-Formats old KUBE")
  cat("\n  -Remove expired rows, select common dimensions and values")
  compareold <- copy(data2)
  # Remove new rows, select common columns and values
  compareold <- compareold[exprow == 0,
                           c(commondims, commonvals), with = F]
  # Add suffix to value columns
  commonvals_old <- paste0(commonvals, "_old")
  setnames(compareold, commonvals, commonvals_old)
  
  # Create comparedata
  compareKUBE <<- comparenew[compareold, on = commondims]
  
}

#' Format data
#' 
#' Flags new rows in new KUBE, exports flagged KUBE
#' 
#' Flags expired rows in old KUBE, exports flagged KUBE
#' 
#' Create a combined CompareKUBE for comparison of value columns, merged on common 
#' dimensions. All common value columns are given suffixes _new, and _old. 
#' 
#' The two flagged KUBEs and the CompareKUBE can be saved as .csv-files through 
#' the dumps and dumpname parameter
#' 
#'
#' @param data1 New KUBE, defaults to dfnew
#' @param data2 Old KUBE, defaults to dfold
#' @param dims Character vector of dimension columns, defaults to DIMENSIONS
#' @param vals Character vector of value volumns, defaults to VALUES
#' @param dumps List of dump points, defaults to DUMPS
#' @param dumpname Name of KUBE, for naming of filedumps, defaults to DUMPname
#'
#' @return
#' @export
#'
#' @examples
FormatData <- function(data1 = dfnew,
                       data2 = dfold,
                       dims = DIMENSIONS,
                       vals = VALUES,
                       dumps = DUMPS,
                       dumpname = DUMPname){
  
  # Identify dimension columns present in new and old KUBE
  # Separate into common, new, and expired dimensions for flagging, write standard msg
  dimnew <- names(data1[, names(data1) %in% dims, with = F])
  dimold <- names(data2[, names(data2) %in% dims, with = F])
  commondims <- dimnew[dimnew %in% dimold]
  newdims <- dimnew[!dimnew %in% dimold]
  expdims <- dimold[!dimold %in% dimnew]
  
  msg_commondims <- case_when(length(commondims) == 0 ~ "\n-No common dimensions found",
                             TRUE ~ paste0("\n-Common columns found: ", str_c(commondims, collapse = ", ")))
  
  msg_newdims <- case_when(length(newdims) == 0 ~ "\n-No new dimensions.",
                          TRUE ~ paste0("\n-New dimensions found: ", str_c(newdims, collapse = ", ")))
  
  msg_expdims <- case_when(length(expdims) == 0 ~ "\n-No expired dimensions.",
                      TRUE ~ paste0("\n-Expired dimensions found: ", str_c(expdims, collapse = ", ")))
  
  # Identify value columns present in new and old KUBE
  # Separate into common, new, and expired values, write standard msg
  valnew <- names(data1[, names(data1) %in% vals, with = F])
  valold <- names(data2[, names(data2) %in% vals, with = F])
  commonvals <- valnew[valnew %in% valold]
  newvals <- valnew[!valnew %in% valold]
  expvals <- valold[!valold %in% valnew]
  
  msg_commonvals <- case_when(length(commonvals) == 0 ~ "\n-No common value columns found",
                         TRUE ~ paste0("\n-Common value columns found: ", str_c(commonvals, collapse = ", ")))
  
  msg_newvals <- case_when(length(newvals) == 0 ~ "\n-No new value columns.",
                      TRUE ~ paste0("\n-New value columns found: ", str_c(newvals, collapse = ", ")))
  
  msg_expvals <- case_when(length(expvals) == 0 ~ "\n-No expired value columns.",
                      TRUE ~ paste0("\n-Expired value columns found: ", str_c(expvals, collapse = ", ")))
  
  
  # Flag new KUBE
  cat("STARTS flagging new kube:")
  cat(msg_commondims)
  cat(msg_newdims)
  .FlagNew(data1 = data1, 
           data2 = data2, 
           commondims = commondims,
           newdims = newdims)

  cat("\n\nSTARTS flagging old kube:")
  cat(msg_commondims)
  cat(msg_expdims)
  .FlagOld(data1 = data1,
           data2 = data2,
           commondims = commondims,
           expdims = expdims)
  cat("\n\nCOMPLETED flagging!\n")
  
  if("dfnew_flag" %in% dumps){
    fwrite(dfnew_flag, 
           file = paste0("Filedumps/", dumpname, "_new_flagged.csv"),
           sep = ";")
    cat(paste0("\nFILEDUMP: ", dumpname, "_flagged.csv"))
  }
  
  if("dfold_flag" %in% dumps){
    fwrite(dfold_flag, 
           file = paste0("Filedumps/", dumpname, "_old_flagged.csv"),
           sep = ";")
    cat(paste0("\nFILEDUMP: ", dumpname, "_old_flagged.csv"))
  }
  
  cat("\n\nSTARTS create compareKUBE:")
  
  .CreateCompare(data1 = dfnew_flag,
                 data2 = dfold_flag,
                 commondims = commondims,
                 commonvals = commonvals)
  
  cat("\n\n-COMPLETED creating compareKUBE")
  
  if("compareKUBE" %in% dumps){
    fwrite(compareKUBE, 
           file = paste0("Filedumps/", dumpname, "_compareKUBE.csv"),
           sep = ";")
    cat(paste0("\nFILEDUMP: ", dumpname, "_compareKUBE.csv"))
  }

}


