#' CompareCols
#'
#' Compare the columns across two KUBE files
#' Report whether any new (not in old), or expired (not in new) columns are present.
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
  
  # Identify dimension and value columns
  .IdentifyColumns(data1, data2)
  
  msgnew <- case_when(length(.newdims) == 0 ~ "No new columns.",
                      TRUE ~ paste0("New columns found: ", str_c(.newdims, collapse = ", ")))
  
  msgexp <- case_when(length(.expdims) == 0 ~ "\nNo expired columns.",
                      TRUE ~ paste0("\nExpired columns found: ", str_c(.expdims, collapse = ", ")))
  
  cat(msgnew)
  cat(msgexp)
  
}

#' CompareDims
#' 
#' Compare unique levels of selected dimension columns.
#' Prints the number of levels, new levels (not in old KUBE), expired levels (not in new KUBE)
#'
#' @param data1 new KUBE, defaults to dfnew
#' @param data2 old KUBE, defaults to dfold
#'
#' @return table with 4 columns: Dimension name, N levels, New levels, Expired levels, with one row per input dimension
#' @export
#'
#' @examples
CompareDims <- function(data1 = dfnew, 
                        data2 = dfold){
  
  # Identify dimension and value columns
  .IdentifyColumns(data1, data2)
  
  # Check if any new or expired dimensions are present
  if(length(.expdims) != 0 || length(.newdims) != 0){
  cat(c("The following dimensions are not present in both files: ", print_dim(c(.newdims, .expdims)), "\n"))
  }
  
  CompareDim <- function(data1, 
                         data2, 
                         dim = NULL){
    
    # Identify unique levels of dim, 
    levels1 <- unique(data1[[dim]])
    levels2 <- unique(data2[[dim]])
    
    # Identify new or expired levels
    newlevels <- str_subset(levels1, str_c(levels2, collapse = "|"), negate = TRUE)
    explevels <- str_subset(levels2, str_c(levels1, collapse = "|"), negate = TRUE)
    
    # Replace with "none" if 0 new/expired levels
    if(length(newlevels) == 0){
      newlevels <- "none"
    } 
    
    if(length(explevels) == 0){
      explevels <- "none"
    }
  
    # Create output
    tibble("Dimension" = dim,
           "N levels (new)" = length(levels1),
           "New levels" = str_c(newlevels, collapse = ", "),
           "Expired levels" = str_c(explevels, collapse = ", "))
  }
  
  map_df(.commondims, ~CompareDim(data1, data2, dim = .x))
}

#' ComparePrikk
#' 
#' Calculate the number of censored observations in the new and old KUBE, and calculate the absolute and relative difference. Results can be further grouped by an additional dimension. 
#'
#' @param data1 new KUBE, defaults to dfnew set in INPUT
#' @param data2 old KUBE, defaults to dfold set in INPUT
#' @param groupdim dimension to group output by, in addition to SPVFLAGG
#' 
#' @return a table containing the number of flagged rows in the new and old KUBE, and the absolute and relative difference, grouped by type of SPVFLAGG and an additional dimension (optional)
#' @export
#'
#' @examples
#' ComparePrikk(groupdim = ALDER)
ComparePrikk <- function(data1 = dfnew, 
                         data2 = dfold, 
                         groupdim = GROUPdims){
  
  # Calculate number of observations per strata of SPV-flagg + groupdim
  new <- data1[, .("N (new)" = .N), keyby = c("SPVFLAGG", groupdim)]
  old <- data2[, .("N (old)" = .N), keyby = c("SPVFLAGG", groupdim)]
  
  # merge tables
  output <- new[old]
  
  # Calculate absolute and relative difference
  output[, `:=` (Absolute = `N (new)`-`N (old)`,
                 Relative = round(`N (new)`/`N (old)`, 3))]
  
  # Convert dimensions to factor
  convert <- names(output)[!names(output) %in% c("N (new)", "N (old)", "Absolute", "Relative")]
  output[, (convert) := lapply(.SD, as.factor), .SDcols = c(convert)]
  
  DT::datatable(output, 
                filter = "top",
                rownames = FALSE,
                options = list(
                  columnDefs = list(list(targets = c("N (new)", "N (old)", "Absolute", "Relative"),
                                         searchable = FALSE)),
                  # Show length menu, table, pagination, and information
                  dom = 'ltpi', 
                  scrollX = TRUE)
                )
}

#' Compare censored observations across strata
#'
#' @param data1 
#' @param data2 
#'
#' @return
#' @export
#'
#' @examples
ComparePrikkTS <- function(data1 = dfnew,
                           data2 = dfold){
  
  # Identify dimension and value columns
  .IdentifyColumns(data1, data2)
  
  # Combine data
  data <- rbindlist(list(copy(data1)[, .SD, .SDcols = .commoncols][, KUBE := "New"], 
                         copy(data2)[, .SD, .SDcols = .commoncols][, KUBE := "Old"]))
  
  # Identify grouping dimensions
  groupdims <- str_subset(c(.commondims, "KUBE"), "AAR", negate = TRUE)
  
  # Calculate n censored observations, 
  data[, FLAGG := 0][SPVFLAGG != 0, FLAGG := 1]
  data <- data[, .(N_PRIKK = sum(FLAGG, na.rm = TRUE)), by = groupdims]
  
  # Aggregate to N prikk per strata
  data <- data[, .(PRIKK = .N), by = .(KUBE, N_PRIKK)]
  
  # Calculate proportions of time series with n prikk
  data[, ANDEL := round(PRIKK/sum(PRIKK), 3), by = KUBE]

  # Create output table
  out <- dcast(data, N_PRIKK~KUBE, value.var = c("PRIKK", "ANDEL"))
  setcolorder(out, c("N_PRIKK",
                     str_subset(names(out), "New"),
                     str_subset(names(out), "Old")))
  out[, "N_PRIKK" := as.factor(N_PRIKK)]
  setnames(out, names(out), str_replace(names(out), "_", " "))
  
  DT::datatable(out, 
                filter = "top", 
                rownames = F,
                options = list(
                  columnDefs = list(list(targets = 2:ncol(out)-1,
                                         searchable = FALSE)),
                  # Show length menu, table, pagination, and information
                  dom = 'ltpi', 
                  scrollX = TRUE)
                )
}

#' CheckPrikk
#' 
#' Check if all values below the censoring limit has been removed. If ok, the function returns a confirmation. If any number below the limit is detected, all rows containing unacceptable values are returned for inspection. 
#'
#' @param data1 New KUBE, defaults to dfnew 
#' @param dim Dimension you want to check, defaults to sumTELLER
#' @param limit Censor limit, the highest unacceptable value of dim. Defaults to `PRIKKlimit`, defined in input section of the Rmarkdown file. 
#'
#' @return
#' @export
#'
#' @examples
CheckPrikk <- function(data1 = dfnew,
                       val = PRIKKval, 
                       limit = PRIKKlimit){
  
  cat(paste0("Controlled column: ", val))
  cat(paste0("\nLimit: ", limit))
  
  filtered <- data1[get(val) <= limit]
  
  if(nrow(filtered) == 0) {
    cat("\nNo values < limit")
  } else {
    cat(paste0("\nN values <= limit: ", nrow(filtered)))
    cat(paste0("\nView all rows with ", val, " <= ", limit, " with View(notcensored)"))
    
    num <- which(sapply(filtered, is.numeric))
    filtered[, (num) := lapply(.SD, round, 2), .SDcols = num]
    notcensored <<- filtered
    View(notcensored)
  }
}

#' CompareFylkeLand
#' 
#'
#' @param data1 new KUBE, defaults to dfnew set in INPUT
#' @param groupdim 
#' @param compare 
#'
#' @return
#' @export
#'
#' @examples
CompareFylkeLand <- function(data = dfnew, 
                             groupdim = GROUPdims, 
                             compare = COMPAREval){
  
  # Create subset, remove helseregion
  data <- copy(data)[GEO<100 & !GEO %in% 81:84] 
  data[, geolevel := "Fylke"]
  data[GEO == 0, geolevel := "Land"]
  
  cat("GEOcodes included: ", str_c(unique(data$GEO), collapse = ", "), "\n")
  
  # Sum compare value per strata of geolevel and grouping dims
  data <- data[, .("sum" = sum(get(compare), na.rm = T)), keyby = c("geolevel", groupdim)]
  
  # Format output
  data <- dcast(data, ... ~ geolevel, value.var = "sum")
  
  # Estimate absolute and relative difference, format digits
  data[, `:=` (Absolute = Land - Fylke,
               Relative = round(Land/Fylke, 3))]
  format <- c("Land", "Fylke", "Absolute")
  data[, (format) := lapply(.SD, round, 0), .SDcols = format]
  
  if(nrow(data[Relative < 1]) == 0) {
    cat("\nLAND is always larger than FYLKE")
  } else {
    cat("\nIn some rows, FYLKE is larger than LAND.\n See rows with Absolute < 1")
  }
   
  # Convert groupdim to factor and set column order
  data[, (groupdim) := lapply(.SD, as.factor), .SDcols = groupdim]
  setcolorder(data, c(groupdim, "Land", "Fylke", "Absolute", "Relative"))
  
  DT::datatable(data[order(-Relative)], 
                filter = "top",
                rownames = F,
                options = list(
                  columnDefs = list(list(targets = c("Land", "Fylke", "Absolute", "Relative"),
                                         searchable = FALSE)),
                  # Show length menu, table, pagination, and information
                  dom = 'ltpi')
                )
}

#' CompareKommuneFylke
#' 
#'
#' @param data1 new KUBE, defaults to dfnew set in INPUT
#' @param groupdim 
#' @param compare 
#'
#' @return
#' @export
#'
#' @examples
CompareKommuneFylke <- function(data = dfnew, 
                                groupdim = GROUPdims, 
                                compare = COMPAREval){
  
  # Create subset, remove helseregion
  data <- copy(data)[GEO > 0 & GEO < 10000 & !GEO %in% 81:84]
  data[, geolevel := "Kommune"]
  data[GEO < 100, geolevel := "Fylke"]
  
  cat("GEOcodes included: ", str_c(unique(data$GEO), collapse = ", "), "\n")
  
  # Sum compare value per strata of geolevel and grouping dims
  data <- data[, .("sum" = sum(get(compare), na.rm = T)), keyby = c("geolevel", groupdim)]
  
  # Format output
  data <- dcast(data, ... ~ geolevel, value.var = "sum")
  
  # Estimate absolute and relative difference, format digits
  data[, `:=` (Absolute = Fylke - Kommune,
               Relative = round(Fylke/Kommune, 3))]
  format <- c("Fylke", "Kommune", "Absolute")
  data[, (format) := lapply(.SD, round, 0), .SDcols = format]
  
  if(nrow(data[Relative < 1]) == 0) {
    cat("\nFYLKE is always larger than KOMMUNE")
  } else {
    cat("\nIn some rows, KOMMUNE is larger than FYLKE.\n See rows with Absolute < 1")
  }
  
  # Convert groupdim to factor and set column order
  data[, (groupdim) := lapply(.SD, as.factor), .SDcols = groupdim]
  setcolorder(data, c(groupdim, "Fylke", "Kommune", "Absolute", "Relative"))
  
  DT::datatable(data[order(-Relative)], 
                filter = "top",
                rownames = F,
                options = list(
                  columnDefs = list(list(targets = c("Fylke", "Kommune", "Absolute", "Relative"),
                                         searchable = FALSE)),
                  # Show length menu, table, pagination, and information
                  dom = 'ltpi')
                )
}

#' CompareBydelKommune
#'
#' @param data1 
#' @param groupdim 
#' @param compare 
#'
#' @return
#' @export
#'
#' @examples
CompareBydelKommune <- function(data = dfnew, 
                                groupdim = GROUPdims, 
                                compare = COMPAREval) {
  
  kommunegeo <- c(301, 1103, 4601, 5001)
  bydelsgeo <- unique(data[GEO>9999]$GEO)
  
  # If no data on bydel, stop and return NULL
  if(length(bydelsgeo) < 1){
    cat("No geo-codes corresponding to bydel, not possible to estimate unspecified bydel.\n")
    return(invisible(NULL))
  } 
  
  # Create subset, create geolevel and kommune variable
  data <- copy(data)[GEO %in% c(kommunegeo, bydelsgeo)]
  data[, `:=` (geolevel = "Bydel",
               KOMMUNE = character())]
  data[grep("^301", GEO), KOMMUNE := "Oslo"]
  data[grep("^1103", GEO), KOMMUNE := "Stavanger"]
  data[grep("^4601", GEO), KOMMUNE := "Bergen"]
  data[grep("^5001", GEO), KOMMUNE := "Trondheim"]
  data[GEO < 9999, geolevel := "Kommune"]
  
  cat("GEOcodes included: ", str_c(unique(data$GEO), collapse = ", "), "\n")
  
  # Sum compare value per strata of geolevel and grouping dims
  data <- data[, .("sum" = sum(get(compare), na.rm = T)), keyby = c("KOMMUNE", "geolevel", groupdim)]
  
  # Format output
  data <- dcast(data, ... ~ geolevel, value.var = "sum")
  
  # Estimate absolute and relative difference, format digits
  data[, `:=` (Absolute = Kommune - Bydel,
               Relative = round(Kommune/Bydel, 3))]
  format <- c("Kommune", "Bydel", "Absolute")
  data[, (format) := lapply(.SD, round, 0), .SDcols = format]
  
  if(nrow(data[Relative < 1]) == 0) {
    cat("\nKOMMUNE is always larger than BYDEL") 
  } else {
    cat("\nIn some rows, BYDEL is larger than KOMMUNE.\nSee rows with Absolute < 1")
  }
  
  # Convert KOMMUNE & groupdim to factor and set column order
  fct_col <- c("KOMMUNE", groupdim)
  data[, (fct_col) := lapply(.SD, as.factor), .SDcols = fct_col]
  setcolorder(data, c("KOMMUNE", groupdim, "Kommune", "Bydel", "Absolute", "Relative"))
  
  DT::datatable(data[order(-Relative)], 
                filter = "top",
                rownames = F,
                options = list(
                  columnDefs = list(list(targets = c("Kommune", "Bydel", "Absolute", "Relative"),
                                         searchable = FALSE)),
                  # Show length menu, table, pagination, and information
                  dom = 'ltpi')
                )
}



#' CompareOslo
#'
#' @param data1 
#' @param groupdim 
#' @param compare 
#'
#' @return
#' @export
#'
#' @examples
CompareOslo <- function(data = dfnew, 
                        groupdim = GROUPdims, 
                        compare = COMPAREval){
  
  # Create subset, remove helseregion
  data <- copy(data)[GEO %in% c(3, 301)]
  data[, geolevel := "Oslo Kommune"]
  data[GEO == 3, geolevel := "Oslo Fylke"]
  
  # Sum compare value per strata of geolevel and grouping dims
  data <- data[, .("sum" = sum(get(compare), na.rm = T)), keyby = c("geolevel", groupdim)]
  
  # Format output
  data <- dcast(data, ... ~ geolevel, value.var = "sum")
  
  # Estimate absolute and relative difference, format digits
  data[, `:=` (Absolute = `Oslo Fylke`- `Oslo Kommune`,
               Relative = round(`Oslo Fylke`/`Oslo Kommune`, 3))]
  format <- c("Oslo Fylke", "Oslo Kommune", "Absolute")
  data[, (format) := lapply(.SD, round, 0), .SDcols = format]
  
  if(nrow(data[Relative < 1]) == 0) {
    cat("Oslo kommune is identical to Oslo fylke!") 
  } else {
    cat("Oslo fylke is not identical to Oslo fylke.\nSee rows where Absolute does not = 0")
  }
  
  # Convert groupdim to factor and set column order
  data[, (groupdim) := lapply(.SD, as.factor), .SDcols = groupdim]
  setcolorder(data, c(groupdim, "Oslo Fylke", "Oslo Kommune", "Absolute", "Relative"))
  
  DT::datatable(data[order(-Relative)], 
                filter = "top",
                rownames = F,
                options = list(
                  columnDefs = list(list(targets = c("Oslo Fylke", "Oslo Kommune", "Absolute", "Relative"),
                                         searchable = FALSE)),
                  # Show length menu, table, pagination, and information
                  dom = 'ltpi', 
                  scrollX = TRUE)
                )
}

#' Plot country-level time series across selected dimensions
#'
#' @param data 
#' @param plotdims 
#' @param plotvals 
#'
#' @return
#' @export
#'
#' @examples
PlotTimeseries <- function(data = dfnew){
  
  plotdata <- copy(data)
  
  # Identify dimension and value columns
  .IdentifyColumns(plotdata)
  
  # Identify all dimensions in the file, plotdims (- GEO and AAR) and plotvals (- RATE.n and SPVFLAGG)
  .TSplotdims <<- str_subset(.dims1, "GEO|AAR", negate = TRUE)
  .TSplotvals <<- str_subset(names(plotdata),
                            str_c(c(.dims1, "RATE.n", "SPVFLAGG"), collapse = "|"),
                            negate = TRUE)
  
  # Find plotheight for HTML report
  .PlotHeight <<- 2 + 2*ceiling(length(.TSplotvals)/2)
  
  # Extract only country level data,
  plotdata <- plotdata[GEO == 0]
  # Keep all dimensions, but only value columns included in plotval
  plotdata <- plotdata[, .SD, .SDcols = c(.dims1, .TSplotvals)]
  
  # Organize plotdata according to all dimensions
  setkeyv(plotdata, .dims1)
  
  # Create AARx for plotting on x-axis
  plotdata[, AARx := as.numeric(str_extract(AAR, "[:digit:]*(?=_)"))]
  
  # Identify dimensions to aggregate or keep total (if containing 0), excluding dim to be plotted
  aggdims <- .TSplotdims
  containtotal <- character()
  for(i in aggdims){
    if(0 %in% unique(plotdata[[i]])){
      # add to containtotal
      containtotal <- c(containtotal, i)
      # remove from aggdims
      aggdims <- str_subset(aggdims, i, negate = T) 
    }
  }
  
  # Find total age group, if existing
  ALDERtot_tf <- FALSE
  if("ALDER" %in% names(plotdata)){
  ALDERl <- min(as.numeric(str_extract(plotdata$ALDER, "[:digit:]*(?=_)")))
  ALDERh <- max(as.numeric(str_extract(plotdata$ALDER, "(?<=_)[:digit:]*")))
  ALDERtot <- paste0(ALDERl, "_", ALDERh)
  ALDERtot_tf <- ALDERtot %in% unique(plotdata$ALDER)
  
    if(ALDERtot_tf){
      containtotal <- c(containtotal, "ALDER")
      aggdims <- str_subset(aggdims, "ALDER", negate = TRUE)
    } 
  }
  
  # Identify value columns to average or sum
  sumcols <- .TSplotvals[str_detect(.TSplotvals, c("TELLER"))]
  avgcols <- .TSplotvals[!.TSplotvals %in% sumcols]
  
  # Feedback messages on aggregation
  cat("Aggregation summary:")
  cat("\n -Dimensions are aggregated to totals when not plotted")
  
  cat(paste0("\nTotal group present and kept for: ", print_dim(containtotal)))
  if(ALDERtot_tf){
    cat(paste0("\n - Total age group identified: ", ALDERtot))
  }
  cat(paste0("\nData aggregated for: ", print_dim(aggdims)))
  cat(paste0("\n - Value columns aggregated to sum: ", print_dim(sumcols)))
  cat(paste0("\n - Value columns aggregated to average: ", print_dim(avgcols)))
  
  # Loop through plotdims to generate the plots
  .TS <<- map(.TSplotdims, ~.plot_ts(dim = .x,
                                     plotdata = plotdata,
                                     plotvals = .TSplotvals,
                                     aggdims = aggdims,
                                     containtotal = containtotal,
                                     sumcols = sumcols,
                                     avgcols = avgcols,
                                     dimexist = .dims1,
                                     ALDERtot = ALDERtot))
  
} 

# Plotting function
.plot_ts <- function(dim,
                     plotdata,
                     plotvals,
                     aggdims,
                     containtotal,
                     sumcols,
                     avgcols,
                     dimexist,
                     ALDERtot){
  
  # Exclude dim from containtotal/aggdims
  aggdims <- str_subset(aggdims, dim, negate = TRUE)
  containtotal <- str_subset(containtotal, dim, negate = TRUE)
  
  # Copy plotdata
  d <- copy(plotdata)
  
  # Find n rows for plot legend
  nrow_legend <- ceiling(length(unique(d[[dim]]))/3)
  
  # Keep totals for columns with total present (containtotal). 
  # For ALDER, rows = aldertot is kept, for other dimensions rows == 0 is kept. 
  for(i in containtotal){
    if(i == "ALDER"){
      d <- d[get(i) == ALDERtot]
    } else {
      d <- d[get(i) == 0]
    }
  }
  
  # Aggregate value columns for all dimensions without total (aggdims)
  for(i in aggdims){
    
    # Group by all existing standard dimensions
    groupcols <- str_subset(dimexist, i, negate = TRUE)
    d[, (avgcols) := lapply(.SD, mean, na.rm = T), .SDcols = avgcols, by = groupcols]
    d[, (sumcols) := lapply(.SD, sum, na.rm = T), .SDcols = sumcols, by = groupcols]
    d[, (i) := "Aggregated"]
    
    # Remove duplicates and return data
    d <- unique(d)
  }
  
  # Convert dim to factor and .TSplotvals to double for plotting
  d[, (dim) := lapply(.SD, as.factor), .SDcols = dim]
  d[, (plotvals) := lapply(.SD, as.numeric), .SDcols = plotvals]
  
  # convert to long format
  d <- melt(d, 
            measure.vars = plotvals, 
            variable.name = "targetnumber", 
            value.name = "yval")
  
  # Plot time series
  d %>%
    ggplot(aes(x = AARx,
               y = yval,
               color = .data[[dim]],
               group = .data[[dim]])) +
    geom_point() +
    geom_line() +
    facet_wrap(~targetnumber,
               ncol = 2,
               scales = "free_y") +
    labs(x = "Year",
         y = NULL,
         title = NULL) +
    scale_x_continuous(breaks = seq(min(d$AARx),
                                    max(d$AARx),
                                    by = 1), 
                       labels = sort(unique(d$AAR)), 
                       expand = expansion(add = 0.2)) +
    guides(color = guide_legend(title = NULL, 
                                nrow = nrow_legend, 
                                byrow = TRUE)) +
    theme(axis.text.x = element_text(angle = 30, vjust = 0.5),
          panel.spacing = unit(0.5, "cm"))  +
    force_panelsizes(rows = unit(4, "cm"))
}

PrintTimeseries <- function(dims = .TSplotdims,
                            plots = .TS){
  
  for(i in 1:length(dims)){
    
    header <-  paste0("\n\n## Across ", dims[i], "\n")
    cat(header)
    
    print(plots[[i]])
    
    cat("\n")
  }
}

# .AggregateExtradim <- function(data, dimextra, dimexist, plotvals){
#   # Group by all existing standard dimensions
#   groupcols <- str_subset(dimexist, dimextra, negate = TRUE)
#   # Identify value columns to average or sum
#   sumcols <- plotvals[str_detect(plotvals, c("TELLER"))]
#   avgcols <- plotvals[!plotvals %in% sumcols]
#   # Aggregate plotvals, remove dimextra column, remove duplicated rows
#   data[, (avgcols) := lapply(.SD, mean, na.rm = T), .SDcols = avgcols, by = groupcols]
#   data[, (sumcols) := lapply(.SD, sum, na.rm = T), .SDcols = sumcols, by = groupcols]
#   data[, (dimextra) := "TOTAL"]
#   data <- unique(data)
#   data
# }
# 
# .AggregateAge <- function(data){
#   ALDERl <- min(as.numeric(str_extract(data$ALDER, "[:digit:]*(?=_)")))
#   ALDERh <- max(as.numeric(str_extract(data$ALDER, "(?<=_)[:digit:]*")))
#   ALDERtot <- paste0(ALDERl, "_", ALDERh)
#   data[ALDER == ALDERtot]
# }




#' UnspecifiedBydel
#'
#' Calculates the proportion of unspecified BYDEL, for evaluation of the quality of data on this geographical level. 
#' 
#' The proportion is calculated for all strata containing complete (no censoring) data on BYDEL, for all value columns whose name contains "TELLER" or "NEVNER"
#' 
#' Outputs a sortable and searchable table ordered according to proportion unspecified 
#' 
#' @param data defaults to dfnew
#' @param bydelstart defaults to BYDELSTART
#'
#' @return
#' @export
#'
#' @examples
UnspecifiedBydel <- function(data = dfnew){
  
  kommunegeo <- c(301, 1103, 4601, 5001)
  bydelsgeo <- unique(data[GEO>9999]$GEO)
  
  # If no data on bydel, stop and return NULL
  if(length(bydelsgeo) < 1){
    cat("No geo-codes corresponding to bydel, not possible to estimate unspecified bydel.\n")
    return(invisible(NULL))
  } 
  
  d <- copy(data)
  
  # Identify dimension and value columns
  .IdentifyColumns(d)
  
  # Remove years with no data on bydel
  
  bydelaar <- d[GEO %in% bydelsgeo & SPVFLAGG == 0, .N, by = AAR]$AAR
  d <- d[AAR %in% bydelaar]
  
  # If no rows left after filtering years, stop and return NULL.
  if(nrow(d) < 1){
      cat("No rows left in data after removing years < bydel start.\n")
      return(invisible(NULL))
    } 
  
  # Identify dimemsion and value columns
  vals <- str_subset(.vals1, "TELLER|NEVNER")
        
  # Create subset, create geolevel and kommune variable
  d <- d[GEO %in% c(kommunegeo, bydelsgeo), .SD, .SDcols = c(.dims1, vals, "SPVFLAGG")]
  d[, ':=' (KOMMUNE = character(),
            GEONIV = "Bydel")]
  d[grep("^301", GEO), KOMMUNE := "Oslo"]
  d[grep("^1103", GEO), KOMMUNE := "Stavanger"]
  d[grep("^4601", GEO), KOMMUNE := "Bergen"]
  d[grep("^5001", GEO), KOMMUNE := "Trondheim"]
  d[GEO < 9999, GEONIV := "Kommune"]
  
  # Identify complete strata within kommune and all dims except GEO
  d[, sumSPV := sum(SPVFLAGG), by = c("KOMMUNE", 
                                      str_subset(.dims1, "GEO", negate = TRUE))]
  # Only keep complete strata
  d <- d[sumSPV == 0]
  if(nrow(d) < 1){
    cat("No complete strata, not possible to estimate unspecified bydel. Was bydelstart set to the correct year?\n")
    return(invisible(NULL))
  } 
  
  # sum `vals` columns for kommune and bydel
  d <- d[, lapply(.SD, sum, na.rm = T), .SDcols = vals, by = c("KOMMUNE", "GEONIV", 
                                                               str_subset(.dims1, "GEO", negate = TRUE))]
  
  # Format table
  d[, (vals) := lapply(.SD, as.numeric, na.rm = T), .SDcols = vals]
  d <- melt(d, measure.vars = c(vals), variable.name = "MALTALL")
  d <- dcast(d, ... ~ GEONIV, value.var = "value")
  
  # Estimate unknown bydel
  d[, `UOPPGITT, %` := 100*(1 - Bydel/Kommune)]
  
  # Convert all dimensions to factor for search function
  convert <- str_subset(names(d), str_c(c(.dims1, "KOMMUNE", "MALTALL"), collapse = "|"))
  d[, (convert) := lapply(.SD, as.factor), .SDcols = convert]
  round <- which(sapply(d, is.numeric))
  d[, (round) := lapply(.SD, round, 1), .SDcols = round]
  
  # Make datatable output
  DT::datatable(d[order(-`UOPPGITT, %`)], 
                filter = "top",
                rownames = F,
                options = list(
                  columnDefs = list(list(targets = c("Bydel", "Kommune", "UOPPGITT, %"),
                                         searchable = FALSE)),
                  # Show length menu, table, pagination, and information
                  dom = 'ltpi')
                )
  
}
