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
                     newdims){
  
  # Initiate flagged version of new KUBE (sets newrow = 0), saves to global env
  # Sorts KUBE according to common and new dimensions
  dfnew_flag <<- copy(data1)[, `:=` (newrow = 0L)]
  setkeyv(dfnew_flag, c(commondims, newdims))
  
  if(is.null(data2)){
    dfnew_flag[, newrow := 1]
    cat("\n- No old file, all rows flagged as new")
  } else {
  # For common dimensions, flag all rows with new levels 
  # Loops over common dimensions. Flags previously unflagged rows for new levels 
  walk(commondims, 
       ~dfnew_flag[!dfnew_flag[[.x]] %in% unique(data2[[.x]]) & newrow == 0,
                   newrow := 1L])
  
  cat("\n- For common dimensions, flagged all rows with new levels as new rows")
  
  # For new dimensions, flag any rows != 0 (i.e. not total numbers)
  if(length(newdims) != 0) {
    walk(newdims,
         ~dfnew_flag[dfnew_flag[[.x]] != 0 & newrow == 0,
                     newrow := 1L])
    cat("\n- For new dimensions, flagged all rows not representing total numbers as new rows")
  }
  
  }
  
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
  
  cat("\n- For common dimensions, flagged all rows with expired levels")
  
  # For expired dimensions, flag any rows != 0 (i.e. not total numbers)
  if(length(expdims) != 0) {
    walk(expdims,
         ~dfold_flag[dfold_flag[[.x]] != 0 & exprow == 0,
                     exprow := 1L])
    cat("\n- For expired dimensions, flagged all rows not representing total numbers")
  } 
  
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
  cat("\n- Formats new KUBE")
  cat("\n  - Remove new rows, select common dimensions and values")
  comparenew <- copy(data1)
  # Remove new rows, select common columns and values
  comparenew <- comparenew[newrow == 0,
                           c(commondims, commonvals), with = F]
  # Add suffix to value columns
  commonvals_new <- paste0(commonvals, "_new")
  setnames(comparenew, commonvals, commonvals_new)
  
  # Format old KUBE
  cat("\n- Formats old KUBE")
  cat("\n  - Remove expired rows, select common dimensions and values")
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
                       profileyear = PROFILEYEAR,
                       dfnew_flag_name = NULL,
                       dfold_flag_name = NULL,
                       compareKUBE_name = NULL){
  
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

  # Identify dimension and value columns
  if(is.null(data2)){
    .IdentifyColumns(data1)
  } else {
    .IdentifyColumns(data1, data2)
  }
  
  # Summary of dimensions and values, if data2 provided
  if(!is.null(data2)){
  msg_commondims <- case_when(length(.commondims) == 0 ~ "\n- No common dimensions found",
                              TRUE ~ paste0("\n- Common columns found: ", str_c(.commondims, collapse = ", ")))
  
  msg_newdims <- case_when(length(.newdims) == 0 ~ "\n- No new dimensions.",
                          TRUE ~ paste0("\n- New dimensions found: ", str_c(.newdims, collapse = ", ")))
  
  msg_expdims <- case_when(length(.expdims) == 0 ~ "\n- No expired dimensions.",
                      TRUE ~ paste0("\n- Expired dimensions found: ", str_c(.expdims, collapse = ", ")))
  
  msg_commonvals <- case_when(length(.commonvals) == 0 ~ "\n- No common value columns found",
                         TRUE ~ paste0("\n- Common value columns found: ", str_c(.commonvals, collapse = ", ")))
  
  msg_newvals <- case_when(length(.newvals) == 0 ~ "\n- No new value columns.",
                      TRUE ~ paste0("\n- New value columns found: ", str_c(.newvals, collapse = ", ")))
  
  msg_expvals <- case_when(length(.expvals) == 0 ~ "\n- No expired value columns.",
                      TRUE ~ paste0("\n- Expired value columns found: ", str_c(.expvals, collapse = ", ")))
  }
  
  # Flag new KUBE
  cat("STARTS flagging new kube:")
  if(!is.null(data2)){
    cat(msg_commondims)
    cat(msg_newdims)
    cat(msg_commonvals)
    cat(msg_newvals)
  }
  .FlagNew(data1 = data1, 
           data2 = data2, 
           commondims = .commondims,
           newdims = .newdims)
  
  # Detect outliers...
  # cat("STARTS outlier detection:")
  # .FlagOutlier()
  
  # Filedump new KUBE
  if("dfnew_flag" %in% dumps){
    
    # Set filename
    if(!is.null(dfnew_flag_name)){
      filename <- paste0(str_remove(dfnew_flag_name, ".csv"), ".csv")
    } else {
      filename <- paste0(str_remove(attributes(dfnew)$Filename, ".csv"), "_(new)_FLAGGED.csv")
    }
    
    file <- paste0(dumppath, filename)
    
    # Write file if it doesn't exist
    if(!file.exists(file)) {
      fwrite(dfnew_flag,
             file = file,
             sep = ";")
      cat(paste0("\nFILEDUMP: ", filename, "\n"))
    } else {
      cat(paste0("\nFILEDUMP already exists: ", filename, "\n"))
    }
  }
  
  # Flag old KUBE (if data2 provided)
  cat("\nSTARTS flagging old kube:")
  if(!is.null(data2)){
    cat(msg_commondims)
    cat(msg_expdims)
    cat(msg_commonvals)
    cat(msg_expvals)
    .FlagOld(data1 = data1,
             data2 = data2,
             commondims = .commondims,
             expdims = .expdims)
  
    # File dump old KUBE
  
    if("dfold_flag" %in% dumps){
      # Set filename
      if(!is.null(dfold_flag_name)){
        filename <- paste0(str_remove(dfold_flag_name, ".csv"), ".csv")
        } else {
          filename <- paste0(str_remove(attributes(dfold)$Filename, ".csv"), "_(old)_FLAGGED.csv")
          }
    
      file <- paste0(dumppath, filename)
      
      # Write file if it doesn't exist
      if(!file.exists(file)){
        fwrite(dfold_flag,
               file = paste0(dumppath, filename),
               sep = ";")
        cat(paste0("\nFILEDUMP: ", filename, "\n"))
        } else {
          cat(paste0("\nFILEDUMP already exists: ", filename, "\n"))
        }
    }
  } else {
    cat("\n- No old file to be flagged\n")
  }
  
  cat("\nCOMPLETED flagging!\n")
  
  cat("\nSTARTS create compareKUBE:")
  
  if(!is.null(data2)){
    .CreateCompare(data1 = dfnew_flag,
                   data2 = dfold_flag,
                   commondims = .commondims,
                   commonvals = .commonvals)
    
    cat("\n\n-COMPLETED creating compareKUBE\n")
    
    if("compareKUBE" %in% dumps){
      filenamenew <- str_remove(attributes(dfnew)$Filename, ".csv")
      filenameold <- str_remove(attributes(dfold)$Filename, ".csv")
      
      # Set filename
      if(!is.null(compareKUBE_name)){
        filename <- paste0(str_remove(compareKUBE_name, ".csv"), ".csv")
      } else {
        filename <- paste0(filenamenew, "_vs_", filenameold, "_COMPARE.csv")
      }
      
      file <- paste0(dumppath, filename)
      
      # Write file if it doesn't exist
      if(!file.exists(file)){
        fwrite(compareKUBE,
               file = paste0(dumppath, filename),
               sep = ";")
        cat(paste0("\nFILEDUMP: ", filename, "\n"))
      } else {
        cat(paste0("\nFILEDUMP already exists: ", filename, "\n"))
      }   
    }
  } else {
    cat("\n- No old file, compareKUBE not created\n")
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
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
.FlagOutlier <- function(data){
  
  # Identify dimension and value columns
  .IdentifyColumns(data)
  
  # Select value column for outlier detection
  if("MEIS" %in% .vals1){
    val <- "MEIS"
    cat("\nOutliers detection based on MEIS")
  } else if ("RATE" %in% .vals1){
    val <- "RATE"
    cat("\nOutliers detection based on RATE")
  } else if ("SMR" %in% .vals1){
    val <- "SMR"
    cat("\nOutliers detection based on SMR")
  } else {
    cat("\n- None of MEIS, RATE, or SMR available for outlier detection")
    return(invisible(NULL))
  }
  
  # Add geoniv, low, high, outlier, and highlow column
  
  data[, GEONIV := NA_character_, keyby = GEO]
  data[GEO == 0, GEONIV := "Land"]
  data[between(GEO, 1, 99), GEONIV := "Fylke"]
  data[between(GEO, 100, 9999), GEONIV := "Kommune"]
  data[GEO > 9999, GEONIV := "Bydel"]
  data[, LOW := NA_real_]
  data[, HIGH := NA_real_]
  data[, OUTLIER := 0]
  data[, HIGHLOW := NA_character_]
  
  # Set bycols (geoniv and all dims except GEO/AAR,)
  bycols <- c("GEONIV", str_subset(.dims1, "GEO|AAR", negate = T))
  
  # Estimate low and high cutoff for outlier detection, 
  
  data[, LOW := quantile(get(val), 0.25, na.rm = T) - 1.5*IQR(get(val), na.rm = T), by = bycols]
  data[, HIGH := quantile(get(val), 0.75, na.rm = T) + 1.5*IQR(get(val), na.rm = T), by = bycols]
  
  # Flag outliers
  data[get(val) < LOW, `:=` (OUTLIER = 1,
                             HIGHLOW = "low")]
  data[get(val) > HIGH, `:=` (OUTLIER = 1,
                              HIGHLOW = "high")]
}
  