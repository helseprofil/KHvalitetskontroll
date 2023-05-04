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
#' @param commondims common dimensions for dfnew and old, for flagging of new rows
#' @param newdims new dimensions, for flagging of new rows (not totals)
#' @param vals value columns in dfnew, for outlier detection
#'
#' @return
#' @export
#'
#' @examples
.FlagNew <- function(data1, 
                     data2, 
                     commondims,
                     newdims,
                     dims,
                     vals){
  
  # Initiate flagged version of new KUBE (sets newrow = 0), saves to global env
  # Sorts KUBE according to common and new dimensions
  dfnew_flag <<- data.table::copy(data1)[, `:=` (newrow = 0L)]
  data.table::setkeyv(dfnew_flag, c(commondims, newdims))
  
  if(is.null(data2)){
    dfnew_flag[, newrow := 1]
    cat("\n- No old file, all rows flagged as new")
  } else {
  # For common dimensions, flag all rows with new levels 
  # Loops over common dimensions. Flags previously unflagged rows for new levels 
  purrr::walk(commondims, 
              \(x){
                dfnew_flag[!get(x) %in% data2[, unique(get(x))] & newrow == 0,
                           newrow := 1L]
                })
  
  cat("\n- For common dimensions, flagged all rows with new levels as new rows")
  
  # For new dimensions, flag any rows != 0 (i.e. not total numbers)
  if(length(newdims) != 0) {
    purrr::walk(newdims,
                \(x){
                dfnew_flag[get(x) != 0 & newrow == 0,
                           newrow := 1L]
                  })
    cat("\n- For new dimensions, flagged all rows not representing total numbers as new rows")
  }
  
  }
  
  # Flag outliers
  dfnew_flag <<- .FlagOutlier(data = dfnew_flag,
                              dims = dims,
                              vals = vals)[]
  
  cat("\n\n- Flagged version of new KUBE created: dfnew_flag\n")
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
                     expdims,
                     dims,
                     vals){

  # Initiate flagged version of old KUBE (sets newrow = 0), saves to global env
  # Sorts KUBE according to common and new dims
  dfold_flag <<- data.table::copy(data2)[, exprow := 0L]
  data.table::setkeyv(dfold_flag, c(commondims, expdims))
  
  # For common dimensions, flag all rows with expired levels 
  # Loops over common dimensions. Flags previously unflagged rows for expired levels 
  purrr::walk(commondims, 
              \(x){
              dfold_flag[!get(x) %in% data1[, unique(get(x))] & exprow == 0,
                          exprow := 1L]
                })
  
  cat("\n- For common dimensions, flagged all rows with expired levels")
  
  # For expired dimensions, flag any rows != 0 (i.e. not total numbers)
  if(length(expdims) != 0) {
    purrr::walk(expdims,
                \(x){
                dfold_flag[get(x) != 0 & exprow == 0,
                            exprow := 1L]
                  })
    cat("\n- For expired dimensions, flagged all rows not representing total numbers")
  } 
  
  # Flag outliers
  dfold_flag <<- .FlagOutlier(data = dfold_flag,
                              dims = dims,
                              vals = vals)[]
  
  cat("\n\n- Flagged version of old KUBE created: dfold_flag\n")
}

#' FixDecimals
#' 
#' Helper function to round value columns according to type
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
.FixDecimals <- function(data,
                         commonvals){
  
  reldiffcols <- stringr::str_subset(names(data), "_reldiff")
  valcols <- stringr::str_subset(names(data), stringr::str_c(commonvals, collapse = "|")) |> 
    stringr::str_subset("_reldiff", negate = TRUE)
  
  round0 <- stringr::str_subset(valcols, "^RATE.n_new$|^RATE.n_old$|SPVFLAGG")
  round1 <- stringr::str_subset(valcols, "TELLER|NEVNER")
  round2 <- c(stringr::str_subset(valcols, "^RATE_|SMR|MEIS"), reldiffcols)
  
  .myround <- function(data, val, round){
    set(data, j = val, value = round(data[[val]], round))
  }
  
  purrr::walk(round0, \(x) .myround(data, x, 0))
  purrr::walk(round1, \(x) .myround(data, x, 1))
  purrr::walk(round2, \(x) .myround(data, x, 2))
  
  data[]
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
  cat("\n- Formats new KUBE")
  cat("\n  - Remove new rows, select common dimensions and values")
  comparenew <- data.table::copy(data1)
  
  # Add TELLER and NEVNER, set equal to TELLER/NEVNER_uprikk if not present in dfnew and present in dfold
  # For rows where SPVFLAGG != 0, the generated column is set to NA_real_
  # Add the value to commonvals to generate _new/_old/_diff/_reldiff columns
  for(i in c("TELLER", "NEVNER")){
    if(i %in% names(data2) & isFALSE(i %in% names(comparenew)) & paste0(i, "_uprikk") %in% names(comparenew)){
      comparenew[, (i) := get(paste0(i, "_uprikk"))]
      comparenew[SPVFLAGG != 0, (i) := NA_real_]
      commonvals <- c(commonvals, i)
    }
  }

  # Remove new rows, select common columns and values
  comparenew <- comparenew[newrow == 0, c(..commondims, ..commonvals)]
  # Add suffix to value columns
  commonvals_new <- paste0(commonvals, "_new")
  data.table::setnames(comparenew, commonvals, commonvals_new)
  
  # Format old KUBE
  cat("\n- Formats old KUBE")
  cat("\n  - Remove expired rows, select common dimensions and values")
  compareold <- data.table::copy(data2)
  # Remove new rows, select common columns and values
  compareold <- compareold[exprow == 0, c(..commondims, ..commonvals)]
  # Add suffix to value columns
  commonvals_old <- paste0(commonvals, "_old")
  data.table::setnames(compareold, commonvals, commonvals_old)
  
  # Create comparedata
  compareKUBE <- comparenew[compareold, on = commondims] 
  
  colorder <- commondims
  for(i in commonvals){
    colorder <- c(colorder, paste0(i, c("_new", "_old")))
  }
  data.table::setcolorder(compareKUBE, colorder)
  
  # Create diff columns
  
  for(i in commonvals){
    
    new <- paste0(i, "_new")
    old <- paste0(i, "_old")
    diff <- paste0(i, "_diff")
    reldiff <- paste0(i, "_reldiff")
    
    # Initiate _diff (new - old) and _reldiff (new/old) columns 
    compareKUBE[, (diff) := get(new) - get(old)]
    compareKUBE[, (reldiff) := get(new) / get(old)]
    # For rows with missing new or old values, set _diff and _reldiff to NA
    compareKUBE[is.na(compareKUBE[[new]]) & !is.na(compareKUBE[[old]]), (diff) := NA_real_]
    compareKUBE[is.na(compareKUBE[[new]]) & !is.na(compareKUBE[[old]]), (reldiff) := NA_real_]
    # For rows with missing old AND new, set _diff = 0, and _reldiff = 1
    compareKUBE[is.na(compareKUBE[[new]]) & is.na(compareKUBE[[old]]), (diff) := 0]
    compareKUBE[is.na(compareKUBE[[new]]) & is.na(compareKUBE[[old]]), (reldiff) := 1]
  }
  
  # Remove SPVFLAGG_reldiff
  compareKUBE[, SPVFLAGG_reldiff := NULL]
  
  compareKUBE <- .FixDecimals(compareKUBE, commonvals)
  # Export compareKUBE to global environment
  # compareKUBE <<- .FixDecimals(compareKUBE)
  compareKUBE <<- compareKUBE
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
#' the dumps 
#' 
#'
#' @param data1 New KUBE, defaults to dfnew
#' @param data2 Old KUBE, defaults to dfold
#' @param dumps List of dump points, defaults to .DUMPS (NULL)
#' @param profileyear To save output in the correct folder
#' @param dfnew_flag_name optional name of filedumps, defaults to NA
#' @param dfold_flag_name optional name of filedumps, defaults to NA
#' @param compareKUBE_name optional name of filedumps, defaults to NA
FormatData <- function(data1 = dfnew,
                       data2 = dfold,
                       dumps = DUMPS,
                       profileyear = PROFILEYEAR,
                       dfnew_flag_name = NA,
                       dfold_flag_name = NA,
                       compareKUBE_name = NA){
  
  # Create folder structure, if not existing, and set file path for file dumps
  kubename <- .GetKubename(data1)
  
  if(isFALSE(is.null(dumps))){
    .CreateFolders(profileyear = profileyear,
                   kubename = kubename)
  }
  
  dumppath <- file.path("F:", 
                        "Forskningsprosjekter", 
                        "PDB 2455 - Helseprofiler og til_",
                        "PRODUKSJON", 
                        "VALIDERING", 
                        "NESSTAR_KUBER",
                        profileyear,
                        "KVALITETSKONTROLL",
                        kubename,
                        "FILDUMPER",
                        "/")

  # Identify dimension and value columns
  if(is.null(data2)){
    .IdentifyColumns(data1)
  } else {
    .IdentifyColumns(data1, data2)
  }
  
  # Summary of dimensions and values, if data2 provided
  if(isFALSE(is.null(data2))){
  msg_commondims <- dplyr::case_when(length(.commondims) == 0 ~ "\n- No common dimensions found",
                              TRUE ~ paste0("\n- Common dimensions found: ", stringr::str_c(.commondims, collapse = ", ")))
  
  msg_newdims <- dplyr::case_when(length(.newdims) == 0 ~ "\n- No new dimensions.",
                          TRUE ~ paste0("\n- New dimensions found: ", stringr::str_c(.newdims, collapse = ", ")))
  
  msg_expdims <- dplyr::case_when(length(.expdims) == 0 ~ "\n- No expired dimensions.",
                      TRUE ~ paste0("\n- Expired dimensions found: ", stringr::str_c(.expdims, collapse = ", ")))
  
  msg_commonvals <- dplyr::case_when(length(.commonvals) == 0 ~ "\n- No common value columns found",
                         TRUE ~ paste0("\n- Common value columns found: ", stringr::str_c(.commonvals, collapse = ", ")))
  
  msg_newvals <- dplyr::case_when(length(.newvals) == 0 ~ "\n- No new value columns.",
                      TRUE ~ paste0("\n- New value columns found: ", stringr::str_c(.newvals, collapse = ", ")))
  
  msg_expvals <- dplyr::case_when(length(.expvals) == 0 ~ "\n- No expired value columns.",
                      TRUE ~ paste0("\n- Expired value columns found: ", stringr::str_c(.expvals, collapse = ", ")))
  }
  
  # Flag new KUBE (create dfnew_flag)
  cat("STARTS flagging new kube:")
  if(isFALSE(is.null(data2))){
    cat(msg_commondims)
    cat(msg_newdims)
    cat(msg_commonvals)
    cat(msg_newvals)
  }
  .FlagNew(data1 = data1, 
           data2 = data2, 
           commondims = .commondims,
           newdims = .newdims,
           dims = .dims1,
           vals = .vals1)
  
  if(isFALSE(is.null(data2))){
  # Flag old KUBE (if data2 provided, create dfold_flag)
  cat("\nSTARTS flagging old kube:")
  cat(msg_commondims)
  cat(msg_expdims)
  cat(msg_commonvals)
  cat(msg_expvals)
  .FlagOld(data1 = data1,
           data2 = data2,
           commondims = .commondims,
           expdims = .expdims,
           dims = .dims2,
           vals = .vals2)
  
  # Add PREV_OUTLIER and NEW_OUTLIER to dfnew_flag
  cat("\nAdding PREV_OUTLIER to dfnew_flag:")
  
  dfnew_flag <<- .AddPrevOutlier(data1 = dfnew_flag,
                                 data2 = dfold_flag,
                                 commondims = .commondims)
  
  cat("\nCOMPLETED flagging and outlier detection!\n")
  
  cat("\nSTARTS create compareKUBE:")
  
  .CreateCompare(data1 = dfnew_flag,
                 data2 = dfold_flag,
                 commondims = .commondims,
                 commonvals = .commonvals)
    
  cat("\n\n-COMPLETED creating compareKUBE\n")
  } else {
    cat("\n\n-No old KUBE to be flagged, compareKUBE not created ")
  }
  
  # File dumps
  
  datetagnew <- .GetKubedatetag(data1)
  datetagold <- data.table::fcase(isFALSE(is.null(data2)), .GetKubedatetag(data2),
                                  default = "")
  
  ## Create list of required dumps and savenames
  reqdumpfiles <- data.table::data.table(dumpfiles = c("dfnew_flag", "dfold_flag", "compareKUBE"), 
                                         savenames = c(dfnew_flag_name, dfold_flag_name, compareKUBE_name))
  reqdumpfiles <- reqdumpfiles[dumpfiles %in% dumps]
  
  ## Make sure only valid dumps are required
  if("dfold_flag" %in% dumps & is.null(data2)){
    cat("FILEDUMP dfold_flag required, but dfold not provided. No filedump written.")
    reqdumpfiles <- reqdumpfiles[dumpfiles != "dfold_flag"]
  }
  if("compareKUBE" %in% dumps & is.null(data2)){
    cat("FILEDUMP compareKUBE required, but dfold not provided. No filedump written.")
    reqdumpfiles <- reqdumpfiles[dumpfiles != "compareKUBE"]
  }
  
  purrr::walk2(reqdumpfiles$dumpfiles,
               reqdumpfiles$savenames,
               \(x,y) {
                 .savefiledump(filedump = x,
                               savename = y,
                               dumppath = dumppath,
                               kubename = kubename,
                               datetagnew = datetagnew,
                               datetagold = datetagold)
                 })
  
  cat("\nDONE!")

}

#' .savefiledump
#'
#' @param filedump "dfnew_flag", "dfold_flag", or "compareKUBE"
#' @param savename dfnew_flag_name, dfold_flag_name, or compareKUBE_name
#' @param dumps vector of required filedumps
#' @param dumppath where to write files
#' @param kubename name of kube
#' @param datetagnew 
#' @param datetagold 
#' @param outdata 
#'
#' @return
#' @export
#'
#' @examples
.savefiledump <- function(filedump,
                          savename,
                          dumppath = dumppath,
                          kubename = kubename,
                          datetagnew = datetagnew,
                          datetagold = datetagold){
  
  if(filedump == "dfnew_flag"){
    outdata <- copy(dfnew_flag)
    datetag <- datetagnew
    type <- "(new)_FLAGGED.csv"
  } else if(filedump == "dfold_flag"){
    outdata <- copy(dfold_flag)
    datetag <- datetagold
    type <- "(old)_FLAGGED.csv"
  } else if(filedump == "compareKUBE"){
    outdata <- copy(compareKUBE)
    datetag <- data.table::fcase(.GetKubename(dfnew_flag) == .GetKubename(dfold_flag), paste0(datetagnew, 
                                                                                              "_vs_", 
                                                                                              datetagold),
                                 .GetKubename(dfnew_flag) != .GetKubename(dfold_flag), paste0(datetagnew, 
                                                                                              "_vs_", 
                                                                                              .GetKubename(dfold_flag), 
                                                                                              "_", 
                                                                                              datetagold))
    type <- "COMPARE.csv"
  }
  
  # Set filename
  if(isFALSE(is.na(savename))){
    filename <- paste0(stringr::str_remove(savename, ".csv"), ".csv")
  } else {
    filename <- paste0(kubename, "_", datetag, "_", type)
  }
  
  file <- paste0(dumppath, filename)
  
  # Write file if it doesn't exist
  if(!file.exists(file)) {
    data.table::fwrite(outdata,
                       file = file,
                       sep = ";")
    cat(paste0("\nFILEDUMP ", filedump, ": ", filename, "\n"))
  } else {
    cat(paste0("\nFILEDUMP ", filedump, " already exists: ", filename, "\n"))
  }

}

#' .GetKubedatetag
#'
.GetKubedatetag <- function(data){
  stringr::str_extract(attributes(data)$Filename, "\\d{4}-\\d{2}-\\d{2}-\\d{2}")
}

#' How many rows differs for each value column
#' Calculate the mean difference
#' 
#' For each geographical level
#'
#' @param data defaults to compareKUBE
#'
#' @return
#' @export
#'
#' @examples
CompareDiffRows <- function(data = compareKUBE) {
  vals <- gsub("_diff", "", names(data)[stringr::str_detect(names(data), "_diff")])
  geoniv <- c("TOTAL", "LAND", "FYLKE", "KOMMUNE", "BYDEL")
  
  .RowDiff <- function(data,
                       val,
                       geoniv) {
    diff <- paste0(val, "_diff")
    reldiff <- paste0(val, "_reldiff")
    new <- paste0(val, "_new")
    old <- paste0(val, "_old")
    
    # Subset data based on selected geographical level
    if (geoniv == "LAND") {
      data <- data[GEO == 0]
    } else if (geoniv == "FYLKE") {
      data <- data[GEO > 0 & GEO < 80]
    } else if (geoniv == "KOMMUNE") {
      data <- data[GEO > 999 & GEO < 10000]
    } else if (geoniv == "BYDEL") {
      data <- data[GEO >= 10000]
    }
    
    # Calculate n rows diff,
    # If nrowdiff > 0, and both new and old value exists, calculate mean/min/max diff within selected geographical strata
    # If nrowdiff == 0, set mean/min/max = NA
    nidentical <- nrow(data[get(diff) == 0])
    nprikknew <- nrow(data[is.na(get(new)) & !is.na(get(old))])
    nprikkexp <- nrow(data[!is.na(get(new)) & is.na(get(old))])
    ndifferent <- nrow(data[get(diff) != 0])
    
    #Initiate output table values as NA
    meandiff <- NA_real_
    mindiff <- NA_real_
    maxdiff <- NA_real_
    meanratio <- NA_real_
    minratio <- NA_real_
    maxratio <- NA_real_
    
    if (ndifferent > 0) {
      # Create subset of data for rows that differ and where new and old value exists
      diffdata <- data[get(diff) != 0 & !is.na(get(new)) & !is.na(get(old))]
      
      # If diff column exists, calculate mean, min, and max, overwrite output table values
      if(diff %in% names(diffdata)){
      meandiff <- round(mean(diffdata[[diff]], na.rm = T), 3)
      mindiff <- round(min(diffdata[[diff]], na.rm = T), 3)
      maxdiff <- round(max(diffdata[[diff]], na.rm = T), 3)
      }
    
      # If reldiff columns exists, calculate mean, min, and max, overwrite output table values
      if(reldiff %in% names(diffdata)){
      meanratio <- round(mean(diffdata[[reldiff]], na.rm = T), 3)
      minratio <- round(min(diffdata[[reldiff]], na.rm = T), 3)
      maxratio <- round(max(diffdata[[reldiff]], na.rm = T), 3)
      } 
    } 
    
    # Create summary table
    dplyr::tibble(
      GEOniv = geoniv,
      Value = val,
      `N identical` = nidentical,
      `N new prikk` = nprikknew,
      `N exp prikk` = nprikkexp,
      `N different` = ndifferent,
      `Mean diff` = meandiff,
      `Min diff` = mindiff,
      `Max diff` = maxdiff,
      `Mean ratio` = meanratio,
      `Min ratio` = minratio,
      `Max ratio` = maxratio
    )
  }
  
  # Create summary table
  # Map over geographical levels, and within each level map over values to create rowdiff table
  difftable <- purrr::map_df(geoniv, \(geoniv) {
    purrr::map_df(vals, ~ .RowDiff(
      data = data,
      val = .x,
      geoniv = geoniv
    )) 
  })
  
  # Convert to data.table and convert GEOniv and Value to factor for filtering
  data.table::setDT(difftable)
  filtercols <- c("GEOniv", "Value")
  difftable[, (filtercols) := lapply(.SD, as.factor), .SDcols = filtercols]
  
  # Create output table
  DT::datatable(difftable,
                filter = "top",
                rownames = F,
                options = list(
                  columnDefs = list(list(targets = 2:(ncol(difftable)-1),
                                         searchable = FALSE)),
                  # Show length menu, table, pagination, and information
                  dom = 'ltpi', 
                  scrollX = TRUE
                  )
                )
}

#' Plot differences between new and old file across time
#' 
#' 
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
PlotTimeDiff <- function(data = compareKUBE){
  
  # Identify value column to plot
  if("MEIS_diff" %in% names(data)){
    val <- "MEIS"
  } else if ("RATE_diff" %in% names(data)){
    val <- "RATE"
  } else if ("SMR_diff" %in% names(data)){
    val <- "SMR"
  } else {
    cat("\n- None of MEIS, RATE, or SMR available for plot")
    return(invisible(NULL))
  }
  
  # Get diff columns
  diff <- paste0(val, "_diff")
  reldiff <- paste0(val, "_reldiff")
  
  # Create plotdata, add geoniv
  plotdata <- data[SPVFLAGG_new == 0 & SPVFLAGG_old == 0 & get(diff) != 0][, geoniv := character()]
  plotdata[GEO == 0, geoniv := "Land"]
  plotdata[GEO > 0 & GEO < 100, ':=' (geoniv = "Fylke")]
  plotdata[GEO > 100 & GEO < 10000, ':=' (geoniv = "Kommune")]
  plotdata[GEO > 10000, ':=' (geoniv = "Bydel")]
  
  data.table::setnames(plotdata, old = c(diff, reldiff), new = c("Absolute", "Ratio"))
  
  # Reshape data before plotting
  plotdata <- data.table::melt(plotdata,
                               measure.vars = c("Absolute", "Ratio"))
  
  # Plotting function to create one plot per geoniv
  .plotgeoniv <- function(geofilter){
    
    d <- plotdata[geoniv == geofilter]
    
    ggplot(d,
           mapping = aes(x = AAR, 
                         y = value)) +
      geom_boxplot() + 
      facet_wrap(vars(variable), 
                 scales = "free") + 
      labs(title = geofilter,
           x = "",
           y = val) + 
      theme(axis.text.x = element_text(angle = 30, vjust = 0.5))
  }
  
  # Create plots
  plots <- purrr::map(unique(plotdata$geoniv),
                      ~.plotgeoniv(geofilter = .x))
  
  # Print plots
  purrr::walk(plots, print)
}

#' Compare new and old value
#' 
#' Prints one table per value column, highlighting the absolute and relative 
#' difference between the new and the old file. 
#' 
#' Produces an R object per value column for differing rows
#'
#' @param data defaults to compareKUBE
#' @param profileyear 
#'
#' @return
#' @export
#'
#' @examples
CompareNewOld <- function(data = compareKUBE,
                          profileyear = PROFILEYEAR){
  
  if(!exists(".ALL_DIMENSIONS")) {
    source("https://raw.githubusercontent.com/helseprofil/misc/main/alldimensions.R")
    .ALL_DIMENSIONS <- ALL_DIMENSIONS
    rm(ALL_DIMENSIONS)
  }
  
  # Identify existing dimensions
  dims <- names(data)[names(data) %in% .ALL_DIMENSIONS]
  vals <- gsub("_new", "", names(data)[str_detect(names(data), "_new")])
  
  # Get filepath for filedumps
  kubename <- .GetKubename(data1)
  .CreateFolders(profileyear = profileyear,
                 kubename = kubename)
  
  dumppath <- file.path("F:", 
                        "Forskningsprosjekter", 
                        "PDB 2455 - Helseprofiler og til_",
                        "PRODUKSJON", 
                        "VALIDERING", 
                        "NESSTAR_KUBER",
                        profileyear,
                        "KVALITETSKONTROLL",
                        kubename,
                        "FILDUMPER",
                        "/")
  
  .CompareValue <- function(data,
                            dims,
                            val,
                            dumppath = dumppath){
    
    new <- paste0(val, "_new")
    old <- paste0(val, "_old")
    
    # Filter out rows where *_new != *_old
    data <- data[data[[new]] != data[[old]]]
    
    # Create Absolute and Relative difference
    
    data <- data[, ':=' (Absolute = data[[new]]-data[[old]],
                         Relative = round(data[[new]]/data[[old]], 3))]
    data.table::setcolorder(data, c(dims, new, old, "Absolute", "Relative"))
    
  }
  
  tables <- purrr::map(vals, 
                       ~.CompareValue(data = data,
                                      dims = dims,
                                      val = .x))
  
  purrr::walk(tables, print)
}

#' Flag outliers in dfnew_flag
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
.FlagOutlier <- function(data,
                         dims,
                         vals){
  
  # Check if weights and lists of small and large kommune exists, and create if not. 
  if(!all(exists(".weightsdata"), exists(".smallkommune"), exists(".largekommune"))){
    .SmallLargeKommune()
    .weightsdata <- data.table::data.table(GEO = .allgeos, WEIGHTS = .allweights)
  }
  
  # Add GEONIV column
  collapse::settransform(data, 
                         GEONIV = collapse::qF(data.table::fcase(GEO == 0, "L",
                                                                 GEO %in% 81:84, "H",
                                                                 GEO < 99, "F",
                                                                 GEO %in% .largekommune, "K",
                                                                 GEO %in% .smallkommune, "k",
                                                                 GEO > 9999, "B"), sort = FALSE))
  
  .val <- data.table::fcase("MEIS" %in% vals, "MEIS",
                            "RATE" %in% vals, "RATE",
                            "SMR" %in% vals, "SMR",
                            default = NA)
  
  # If MEIS, RATE or SMR is present, estimate grouped, weighted quantiles and detect outliers
  if(!is.na(.val)){
  
    cat(paste0("\n - Outlier detection based on ", .val, "\n"))
  
    # Set bycols (geoniv and all dims except GEO/AAR), and create collapse grouping object
    bycols <- c("GEONIV", stringr::str_subset(dims, "GEO|AAR", negate = T))
    g <- collapse::GRP(data, bycols)
    
    # Add WEIGHTS, and set to NULL if all values are missing in a strata
    data[.weightsdata, WEIGHTS := i.WEIGHTS, on = "GEO"]
    data[, WEIGHTS := if(all(is.na(get(.val)))){ NA_real_ }, by = bycols]
    
    # Set weights for helseregion = 0, otherwise fnth fails when weights is missing and data is present
    # Alternative is to filter out HELSEREGION, but if present it should be kept. 
    data[GEONIV == "H", WEIGHTS := 0]
    w <- data$WEIGHTS
    
    # Estimate weighted quantiles, and low and high cutoffs
    cutoffs <- collapse::fmutate(
      g[["groups"]],
      MIN = collapse::fmin(data[, get(.val)], g = g),
      wq25 = collapse::fnth(data[, get(.val)], n = 0.25, g = g, w = w, ties = 1),
      wq50 = collapse::fnth(data[, get(.val)], n = 0.50, g = g, w = w, ties = 1),
      wq75 = collapse::fnth(data[, get(.val)], n = 0.75, g = g, w = w, ties = 1),
      MAX = collapse::fmax(data[, get(.val)], g = g),
      LOW = wq25 - 1.5*(wq75-wq25),
      HIGH = wq75 + 1.5*(wq75-wq25)
      )
    
    # Merge cutoffs onto data 
    data[cutoffs, `:=` (MIN = i.MIN,
                        wq25 = i.wq25,
                        wq50 = i.wq50,
                        wq75 = i.wq75,
                        MAX = i.MAX,
                        LOW = i.LOW,
                        HIGH = i.HIGH),
               on = bycols]
  
    # Flag outliers and define HIGHLOW
    data[, `:=` (OUTLIER = data.table::fcase(get(.val) < LOW | get(.val) > HIGH, 1, 
                                 get(.val) >= LOW & get(.val) <= HIGH, 0,
                                 default = NA),
                 HIGHLOW = data.table::fcase(get(.val) < LOW, "Low",
                                 get(.val) > HIGH, "High",
                                 default = NA))] 
  } else {
    cat("\n- Neither MEIS, RATE, nor SMR available for outlier detection")
  }
  
}

#' .AddPrevOutlier
#' 
#' Creates a data frame to be used for outlier plotting. 
#' Adds helper columns outlier_old and newoutlier to filter out new outliers.
#'
#' @param data1 
#' @param data2 
#' @param commondims 
.AddPrevOutlier <- function(data1 = dfnew_flag,
                            data2 = dfold_flag,
                            commondims){
  d <- copy(data1)
  d[data2, PREV_OUTLIER := i.OUTLIER, on = commondims]
  d[, NEW_OUTLIER := 0]
  d[OUTLIER == 1 & (is.na(PREV_OUTLIER) | PREV_OUTLIER == 0), NEW_OUTLIER := 1]
  d[]
  
}

#' PlotOutlier
#' 
#' Creates boxplots to visualize outliers, 
#' and for each boxplot time series plots of all outliers are also produced
#' 
#' 
#' @param data Dataset flagged for outliers
#'
#' @return
#' @export
#'
#' @examples
PlotOutlier <- function(data){

  # Identify dimension and value columns  
  .IdentifyColumns(data)
  
  if("MEIS" %in% .vals1){
    val <- "MEIS"
    cat("\n- MEIS plotted")
  } else if ("RATE" %in% .vals1){
    val <- "RATE"
    cat("\n- RATE plotted")
  } else if ("SMR" %in% .vals1){
    val <- "SMR"
    cat("\n- SMR plotted")
  } else {
    cat("\n- None of MEIS, RATE, or SMR available for boxplot")
    return(invisible(NULL))
  }
  
  bycols <- c("GEONIV", stringr::str_subset(.dims1, "GEO|AAR", negate = T))
  
  # Estimate N observations per strata, and maximum and minimum non-outlier for boxplot whiskers
  data[, `:=` (N_obs = sum(!is.na(get(val))),
               MINABOVELOW = collapse::fmin(get(val)[get(val) >= LOW]),
               MAXBELOWHIGH = collapse::fmax(get(val)[get(val) <= HIGH])), 
       keyby = bycols]
  
  # Extract data to construct boxplots
  baseplotdata <- collapse::GRP(data, c(bycols, 
                                        "MIN", 
                                        "wq25", "wq50", "wq75", 
                                        "MAX", 
                                        "LOW", "HIGH", "N_obs", "MINABOVELOW", "MAXBELOWHIGH"))[["groups"]]
  
  # Extract data containing only outliers
  outlierdata <- data[OUTLIER == 1][, label := paste0(GEO, "'", stringr::str_sub(AAR, -2L, -1L))]
  
  # Create vector of boxplot filenames
  namecols <- stringr::str_subset(bycols, "GEONIV", negate = TRUE)
  boxplot_names <- collapse::GRP(baseplotdata, namecols)[["groups"]][, do.call(paste, c(.SD, sep = ","))]
  
  
  baseplotdata[!is.na(wq50) & KJONN == 0 & ALDER == "0_44"] %>% 
    ggplot(aes(x = forcats::fct_rev(GEONIV),
               ymin = MINABOVELOW,
               lower = wq25,
               middle = wq50,
               upper = wq75,
               ymax = MAXBELOWHIGH)) + 
    scale_x_discrete(drop = F) + 
    coord_flip() + 
    facet_wrap(namecols, labeller = labeller(.multi_line = F),scales = "free_x") + 
    geom_errorbar(width = 0.5) + 
    geom_boxplot(stat = "identity") 
    geom_text(data = outlierdata[ALDER == "0_44" & KODEGRUPPE == "Pas_med_hjerneslag" & KJONN == 2],
               aes(y = get(val), label = label), angle = 45, size = 12/.pt) + 
    coord_flip()
  
    
    # En bildefil per plott. Tittel = namecols, inneholder boxplot + tidsserier for alle nye uteliggere
  
}

