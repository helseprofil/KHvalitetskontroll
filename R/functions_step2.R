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
                     vals){
  
  # Initiate flagged version of new KUBE (sets newrow = 0), saves to global env
  # Sorts KUBE according to common and new dimensions
  dfnew_flag <<- copy(data1)[, `:=` (newrow = 0L)]
  setkeyv(dfnew_flag, c(commondims, newdims))
  
  # For common dimensions, flag all rows with new levels 
  # Loops over common dimensions. Flags previously unflagged rows for new levels 
  walk(commondims, 
       ~dfnew_flag[!dfnew_flag[[.x]] %in% unique(data2[[.x]]) & newrow == 0,
                   newrow := 1L])
  
  cat("\n-For common dimensions, flagged all rows with new levels as new rows")
  
  # For new dimensions, flag any rows != 0 (i.e. not total numbers)
  if(length(newdims) != 0) {
    walk(newdims,
         ~dfnew_flag[dfnew_flag[[.x]] != 0 & newrow == 0,
                     newrow := 1L])
    cat("\n-For new dimensions, flagged all rows not representing total numbers as new rows")
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
.FixDecimals <- function(data){
  
  round0 <- c("RATE.n", "SPVFLAGG")
  round1 <- c("TELLER_", "NEVNER_", "sumTELLER", "sumNEVNER")
  round2 <- c("RATE_", "MEIS_", "SMR_")
  
  
  if(any(str_detect(names(data), 
                    str_c(round0, collapse = "|")))) { 
    data <- data %>% 
      mutate(across(starts_with(round0), round, 0))
  }
  
  if(any(str_detect(names(data), 
                    str_c(round1, collapse = "|")))) { 
    data <- data %>% 
      mutate(across(starts_with(round1), round, 1))
  }
  
  if(any(str_detect(names(data), 
                    str_c(round2, collapse = "|")))) {
    data <- data %>%
      mutate(across(starts_with(round2), round, 2))
  }
  
  data
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
  compareKUBE <- comparenew[compareold, on = commondims]  %>% 
    select(all_of(commondims),
           starts_with(paste0(commonvals, "_")),
           everything()) 
  setattr(compareKUBE, "Filename_new", attributes(data1)$Filename)  
  
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
  
  # Export compareKUBE to global environment
  compareKUBE <<- .FixDecimals(compareKUBE)
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
#' @param dims Character vector of dimension columns, defaults to DIMENSIONS
#' @param vals Character vector of value volumns, defaults to VALUES
#' @param dumps List of dump points, defaults to .DUMPS (NULL)
#' @param profileyear To save output in the correct folder
#'
#' @return
#' @export
#'
#' @examples
FormatData <- function(data1 = dfnew,
                       data2 = dfold,
                       dumps = DUMPS,
                       profileyear = PROFILEYEAR){
  
  if(!exists(".ALL_DIMENSIONS")) {
    source("https://raw.githubusercontent.com/helseprofil/misc/main/alldimensions.R")
    .ALL_DIMENSIONS <- ALL_DIMENSIONS
    rm(ALL_DIMENSIONS)
  }

  # Create folder structure, if not existing, and set file path for file dumps
  kubename <- .GetKubename(data1)
  
  if(!is.null(dumps)){
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

  # Identify dimension columns present in new and old KUBE
  # Separate into common, new, and expired dimensions for flagging, write standard msg
  dimnew <- names(data1)[names(data1) %in% .ALL_DIMENSIONS]
  dimold <- names(data2)[names(data2) %in% .ALL_DIMENSIONS]
  commondims <- dimnew[dimnew %in% dimold]
  newdims <- dimnew[!dimnew %in% dimold]
  expdims <- dimold[!dimold %in% dimnew]
  
  msg_commondims <- case_when(length(commondims) == 0 ~ "\n- No common dimensions found",
                             TRUE ~ paste0("\n- Common columns found: ", str_c(commondims, collapse = ", ")))
  
  msg_newdims <- case_when(length(newdims) == 0 ~ "\n- No new dimensions.",
                          TRUE ~ paste0("\n- New dimensions found: ", str_c(newdims, collapse = ", ")))
  
  msg_expdims <- case_when(length(expdims) == 0 ~ "\n- No expired dimensions.",
                      TRUE ~ paste0("\n- Expired dimensions found: ", str_c(expdims, collapse = ", ")))
  
  # Identify value columns present in new and old KUBE
  # Separate into common, new, and expired values, write standard msg
  valnew <- names(data1)[!names(data1) %in% .ALL_DIMENSIONS]
  valold <- names(data2)[!names(data2) %in% .ALL_DIMENSIONS]
  commonvals <- valnew[valnew %in% valold]
  newvals <- valnew[!valnew %in% valold]
  expvals <- valold[!valold %in% valnew]
  
  msg_commonvals <- case_when(length(commonvals) == 0 ~ "\n- No common value columns found",
                         TRUE ~ paste0("\n- Common value columns found: ", str_c(commonvals, collapse = ", ")))
  
  msg_newvals <- case_when(length(newvals) == 0 ~ "\n- No new value columns.",
                      TRUE ~ paste0("\n- New value columns found: ", str_c(newvals, collapse = ", ")))
  
  msg_expvals <- case_when(length(expvals) == 0 ~ "\n- No expired value columns.",
                      TRUE ~ paste0("\n- Expired value columns found: ", str_c(expvals, collapse = ", ")))
  
  # Flag new KUBE
  cat("STARTS flagging new kube:")
  cat(msg_commondims)
  cat(msg_newdims)
  cat(msg_commonvals)
  cat(msg_newvals)
  .FlagNew(data1 = data1, 
           data2 = data2, 
           commondims = commondims,
           newdims = newdims,
           vals = valnew)
  
  # Detect outliers...
  cat("STARTS outlier detection:")
  # .FlagOutlier()
  
  # Filedump new KUBE
  if("dfnew_flag" %in% dumps){
    filename <- str_remove(attributes(dfnew)$Filename, ".csv")
    fwrite(dfnew_flag, 
           file = paste0(dumppath, filename, "_(new)_FLAGGED.csv"),
           sep = ";")
    cat(paste0("\nFILEDUMP: ", filename, "_(new)_FLAGGED.csv\n"))
  }
  
  # Flag old KUBE
  cat("\n\nSTARTS flagging old kube:")
  cat(msg_commondims)
  cat(msg_expdims)
  cat(msg_commonvals)
  cat(msg_expvals)
  .FlagOld(data1 = data1,
           data2 = data2,
           commondims = commondims,
           expdims = expdims)
  cat("\n\nCOMPLETED flagging!\n")
  
  # File dump old KUBE
  
  if("dfold_flag" %in% dumps){
    filename <- str_remove(attributes(dfold)$Filename, ".csv")
    fwrite(dfold_flag, 
           file = paste0(dumppath, filename, "_(old)_FLAGGED.csv"),
           sep = ";")
    cat(paste0("\nFILEDUMP: ", filename, "_(old)_FLAGGED.csv\n"))
  }
  
  cat("\n\nSTARTS create compareKUBE:")
  
  .CreateCompare(data1 = dfnew_flag,
                 data2 = dfold_flag,
                 commondims = commondims,
                 commonvals = commonvals)
  
  cat("\n\n-COMPLETED creating compareKUBE")
  
  if("compareKUBE" %in% dumps){
    filenamenew <- str_remove(attributes(dfnew)$Filename, ".csv")
    filenameold <- str_remove(attributes(dfold)$Filename, ".csv")
    fwrite(compareKUBE, 
           file = paste0(dumppath, filenamenew, "_vs_", filenameold,"_COMPARE.csv"),
           sep = ";")
    cat(paste0("\nFILEDUMP: ", filenamenew, "_vs_", filenameold, "_COMPARE.csv\n"))
  }
  
  cat("\nDONE!")

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
  vals <- gsub("_diff", "", names(data)[str_detect(names(data), "_diff")])
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
    tibble(
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
  difftable <- map_df(geoniv, function(geoniv) {
    map_df(vals, ~ .RowDiff(
      data = data,
      val = .x,
      geoniv = geoniv
    )) 
  })
  
  # Convert to data.table and convert GEOniv and Value to factor for filtering
  setDT(difftable)
  filtercols <- c("GEOniv", "Value")
  difftable[, (filtercols) := lapply(.SD, as.factor), .SDcols = filtercols]
  
  # Create output table
  DT::datatable(difftable,
                filter = "top",
                rownames = F,
                options = list(
                  columnDefs = list(list(targets = 2:(ncol(difftable)-1),
                                         searchable = FALSE)),
                  # Show length menu, table and pagination
                  dom = 'ltp', 
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
  } else if ("RATE" %in% names(data)){
    val <- "RATE_diff"
  } else if ("SMR" %in% names(data)){
    val <- "SMR_diff"
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
  
  setnames(plotdata, old = c(diff, reldiff), new = c("Absolute", "Ratio"))
  
  # Reshape data before plotting
  plotdata <- melt(plotdata,
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
  plots <- map(unique(plotdata$geoniv),
               ~.plotgeoniv(geofilter = .x))
  
  # Print plots
  walk(plots, print)
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
    setcolorder(data, c(dims, new, old, "Absolute", "Relative"))
    
  }
  
  tables <- map(vals, 
                ~.CompareValue(data = data,
                              dims = dims,
                              val = .x))
  
  walk(tables, print)
}

#' Flag outliers
#' 
#' *_outlier: Within all strata (total GEO and AAR)
#' *_outlierTS: Within time series (all strata, total AAR)
#' 
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
# .FlagOutlier <- function(data = dfnew_flag,
#                          dims = dimnew){
#   
#   d <- copy(data)
#   
#   # Init required columns for outlier detection
#   d[, ':=' (geoniv = NA_character_,
#             low = NA_real_,
#             high = NA_real_)]
#   
#   d[GEO == 0, geoniv := "L"]
#   d[GEO > 0 & GEO < 100, ':=' (geoniv = "F")]
#   d[GEO > 100 & GEO < 10000, ':=' (geoniv = "K")]
#   d[GEO > 10000, ':=' (geoniv = "B")]
#   
#   
#   # Detect strata for outlier detection
#   groupdims <- str_subset(dims, "GEO|AAR", negate = TRUE)
# 
#   # Identify value columns to detect outlier
#   if("MEIS" %in% names(d)){
#     outlierval <- "MEIS"
#     cat("\n- Outliers detected based on MEIS")
#   } else if ("RATE" %in% names(d)){
#     outlierval <- "RATE"
#     cat("\n- Outliers detected based on RATE")
#   } else if ("SMR" %in% names(d)){
#     outlierval <- "SMR"
#     cat("\n- Outliers detected based on SMR")
#   } else {
#     cat("\n- None of MEIS, RATE, or SMR available for outlier detection")
#   }
#   
  # if()
  # 
  # 
  # for(i in outliervals){
  # 
  #   dfnew_flag[, ':=' (low = quantile(.SD, 0.25, na.rm = T) - 1.5*IQR(subset[[i]], na.rm = T),
  #                      high = quantile(.SD, 0.75, na.rm = T) + 1.5*IQR(subset[[i]], na.rm = T)),
  #              by = c("geoniv", groupdims),
  #              .SDcols = i]
  # 
  #   dfnew_flag[, paste0(i, "_outlier") := NA_real_]
  #   dfnew_flag[, c("low", "high") := list(NULL)]
  # }

  
# }

# PlotDiffTime <- function(data = compareKUBE){
#  
# }

  