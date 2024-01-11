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
  
  if(is.null(data2)){
  
    cat(paste0("Columns in file: ", stringr::str_c(names(data1), collapse = ", ")))
    
  } else {
  
  newcols <- stringr::str_subset(names(data1), str_c("^", names(data2), "$", collapse = "|"), negate = TRUE)  
  newcols <- stringr::str_subset(newcols, "_uprikk", negate = TRUE)  
  msgnew <- data.table::fcase(length(newcols) == 0, "-No new columns.",
                             default = paste0("-New columns found: ", stringr::str_c(newcols, collapse = ", ")))
  
  newuprikk <- stringr::str_subset(names(data1), "_uprikk")  
  msguprikk <- data.table::fcase(length(newuprikk) == 0, "\n  -No '_uprikk'-columns in new file",
                                  default = paste0("\n-dfnew _uprikk columns: ", stringr::str_c(newuprikk, collapse = ", ")))
  
  expcols <- stringr::str_subset(names(data2), str_c("^", names(data1), "$", collapse = "|"), negate = TRUE)  
  expcols <- stringr::str_subset(expcols, "origgeo", negate = TRUE)  
  msgexp <- data.table::fcase(length(expcols) == 0, "\n-No expired columns.",
                              default = paste0("\n-Expired columns found: ", stringr::str_c(expcols, collapse = ", ")))
  
  cat(msgnew)
  cat(msgexp)
  cat(msguprikk)
  }
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
  
  if(is.null(data2)){
    
    .IdentifyColumns(data1)
    out <- purrr::map_df(.dims1, 
                         \(x)
                         dplyr::tibble("Dimension" = x,
                                       "N (levels)" = length(data1[, unique(get(x))])))
    return(out)
  } 
  
    # Identify dimension and value columns
  .IdentifyColumns(data1, data2)
  
  # Check if any new or expired dimensions are present
  if(length(.expdims) != 0 || length(.newdims) != 0){
  cat(c("The following dimensions are only present in one file: ", 
        "\n- dfnew: ", stringr::str_c(.newdims, collapse = ", "), 
        "\n- dfold: ", stringr::str_c(.expdims, collapse = ", ")), "\n")
  }
  
  if("origgeo" %in% names(data2) & exists("recodings_dfold")){
    invalid <- recodings_dfold[grepl("99$", current)]
    if(nrow(invalid > 0)){
    cat("\n Due to geo recoding, the following GEO-codes are no longer valid: ", stringr::str_c(invalid$old, collapse = ", "))
    }
  }
  
  purrr::map_df(.commondims, ~.CompareDim(data1, data2, dim = .x))
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
CheckPrikk <- function(data = dfnew,
                       val = PRIKKval, 
                       limit = PRIKKlimit){
  
  if(is.na(val) || is.na(limit)){
    cat("PRIKKval and/or PRIKKlimit = NA, no check performed")
    return(invisible(NULL))
  }
  
  d = copy(data)
  
  # Always use _uprikk columns if col not present
  val <- data.table::fcase(val == "TELLER" & !val %in% names(data) & "TELLER_uprikk" %in% names(data), "TELLER_uprikk",
                           val == "sumTELLER" & !val %in% names(data) & "sumTELLER_uprikk" %in% names(data), "sumTELLER_uprikk",
                           val == "NEVNER" & !val %in% names(data) & "NEVNER_uprikk" %in% names(data), "NEVNER_uprikk",
                           val == "sumNEVNER" & !val %in% names(data) & "sumNEVNER_uprikk" %in% names(data), "sumNEVNER_uprikk",
                           default = val)
  
  cat(paste0("Controlled column: ", val))
  cat(paste0("\nLimit: ", limit))
  
  # filter out rows where SPVFLAGG == 0, and any val <= limit
  uncensored <- d[SPVFLAGG == 0 & get(val) <= limit]
  
  if(nrow(uncensored) == 0) {
      cat("\nNo values < limit")
  } else if (nrow(uncensored) > 0){
      cat(paste0("\nN values <= limit: ", nrow(uncensored)))
      cat(paste0("\nView all rows with ", val, " <= ", limit, " with View(notcensored)"))
      notcensored <<- uncensored
      View(notcensored)
  }
}

#' ComparePrikk
#' 
#' Calculate the number of censored observations in the new and old KUBE, and calculate the absolute and relative difference. Results can be further grouped by an additional dimension. 
#'
#' @param data1 new KUBE, defaults to dfnew set in INPUT
#' @param data2 old KUBE, defaults to dfold set in INPUT
#' @param groupdim dimension to group output by, in addition to SPVFLAGG
#' 
#' @return a table containing the number of censored rows in the new and old KUBE, and the absolute and relative difference, grouped by type of SPVFLAGG and an additional dimension (optional)
#' @export
#'
#' @examples
#' ComparePrikk(groupdim = ALDER)
ComparePrikk <- function(data1 = dfnew, 
                         data2 = dfold, 
                         groupdim = GROUPdims){
  
  bycols <- c("SPVFLAGG", groupdim)
  
  # Calculate number of observations per strata of SPV-flagg + groupdim
  new <- data1[, .("N (new)" = .N), keyby = bycols]
  
  # If only new file available (new indicator), return table of new file
  if (is.null(data2)) {
    
    output <- new

  } else {
    old <- data2[, .("N (old)" = .N), keyby = bycols]
    
    # merge tables
    output <- old[new, on = bycols]
    
    # Rectangularize output to get all combinations of SPVFLAGG and groupdims
    allcomb <- output[, do.call(CJ, c(.SD, unique = TRUE)), .SDcols = bycols]
    output <- output[allcomb, on = bycols]
    
    # Set N new and old = 0 if missing
    purrr::walk(c("N (new)", "N (old)"), \(x) output[is.na(get(x)), (x) := 0])
    
    # Calculate absolute and relative difference
    output[, `:=` (Absolute = `N (new)` - `N (old)`,
                   Relative = round(`N (new)` / `N (old)`, 3))]
  }
    
  # Convert bycols to factor
  output[, (bycols) := lapply(.SD, as.factor), .SDcols = bycols]
  
  # Print output table  
  DT::datatable(
      output,
      filter = "top",
      rownames = FALSE,
      options = list(
        columnDefs = list(list(
          targets =  stringr::str_subset(names(output), 
                                         stringr::str_c(bycols, collapse = "|"), 
                                         negate = TRUE),
          searchable = FALSE
        )),
        # Show length menu, table, pagination, and information
        dom = 'ltpi',
        scrollX = TRUE
      )
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
  
  if(is.null(data2)){
    
    # Identify dimension and value columns
    .IdentifyColumns(data1)
    
    data <- data.table::copy(data1)
    
    # Identify grouping dimensions
    groupdims <- stringr::str_subset(.dims1, "AAR", negate = TRUE)
    
    # Calculate n censored observations, 
    data[, FLAGG := 0][SPVFLAGG != 0, FLAGG := 1]
    data <- data[, .(N_PRIKK = sum(FLAGG, na.rm = TRUE)), by = groupdims]
    
    # Aggregate to N prikk per strata
    out <- data[, .(`N STRATA` = .N), by = .(N_PRIKK)]
    
    # Calculate proportions of time series with n prikk
    out[, ANDEL := round(`N STRATA`/sum(`N STRATA`), 3)]
    
  } else {
  # Identify dimension and value columns
  .IdentifyColumns(data1, data2)
  
  # Combine data
    data <- data.table::rbindlist(list(data.table::copy(data1)[, .SD, .SDcols = .commoncols][, KUBE := "New"],
                                       data.table::copy(data2)[, .SD, .SDcols = .commoncols][, KUBE := "Old"]))
  
  # Identify grouping dimensions
  groupdims <- stringr::str_subset(c(.commondims, "KUBE"), "AAR", negate = TRUE)
  
  # Calculate n censored observations, 
  data[, FLAGG := 0][SPVFLAGG != 0, FLAGG := 1]
  data <- data[, .(N_PRIKK = sum(FLAGG, na.rm = TRUE)), by = groupdims]
  
  # Aggregate to N prikk per strata
  data <- data[, .(`N STRATA` = .N), by = .(KUBE, N_PRIKK)]
  
  # Calculate proportions of time series with n prikk
  data[, ANDEL := round(`N STRATA`/sum(`N STRATA`), 3), by = KUBE]

  # Create output table
  out <- data.table::dcast(data, N_PRIKK~KUBE, value.var = c("N STRATA", "ANDEL"))
  data.table::setcolorder(out, c(
    "N_PRIKK",
    stringr::str_subset(names(out), "New"),
    stringr::str_subset(names(out), "Old")
  ))
  }
  
  # Format and print output table
  out <- out[, "N_PRIKK" := as.factor(N_PRIKK)][order(N_PRIKK)]
  data.table::setnames(out, names(out), stringr::str_replace(names(out), "_", " "))
  
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
                             run = CompareGEO){
  
  if(isFALSE(run)){
    cat("CompareGEO = FALSE, function inactive")
    return(invisible(NULL))
  }
  
  compare <- .find_compare(data, "TELLER")
  
  if(is.na(compare)){
    cat("\nNo sumTELLER(_uprikk) or TELLER present in data, no output generated\n")
    return(invisible(NULL))
  } else {
    cat("\nGeoLevels compared on", compare, "\n")
  }
  
  # Create subset, remove helseregion
  data <- data.table::copy(data)[GEO<100 & !GEO %in% 81:84] 
  data[, geolevel := "Fylke"]
  data[GEO == 0, geolevel := "Land"]
  
  cat("GEOcodes included: ", stringr::str_c(unique(data$GEO), collapse = ", "), "\n")
  
  # Check if both Land and Fylke is present
  if(base::isFALSE("Land" %in% data$geolevel)){
    cat("No rows corresponding to Land, comparison not possible")
    return(invisible(NULL))
  }
  if(base::isFALSE("Fylke" %in% data$geolevel)){
    cat("No rows corresponding to Fylke, comparison not possible")
    return(invisible(NULL))
  }
  
  # Sum compare value per strata of geolevel and grouping dims
  data <- data[, .("sum" = sum(get(compare), na.rm = T)), keyby = c("geolevel", groupdim)]
  
  # Format output
  data <- data.table::dcast(data, ... ~ geolevel, value.var = "sum")
  
  # If groupdim == NULL, remove column named "."
  if(is.null(groupdim)){
  data[, . := NULL]
  }
  
  # Estimate absolute and relative difference, format digits
  data[, `:=` (Absolute = Land - Fylke,
               Relative = round(Land/Fylke, 3))]
  format <- c("Land", "Fylke", "Absolute")
  data[, (format) := lapply(.SD, round, 0), .SDcols = format]
  
  if(nrow(data[Absolute != 0]) == 0) {
    cat("\nLAND and FYLKE are identical")
  } else if(nrow(data[Relative < 1]) == 0) {
    cat("\nLAND is always equal to or larger than FYLKE")
  } else {
    cat("\nIn some rows, FYLKE is larger than LAND.\n See rows with Absolute < 1")
  }
   
  # If any groupdims present, convert to factor and set column order
  if(!is.null(groupdim)){
  data[, (groupdim) := lapply(.SD, as.factor), .SDcols = groupdim]
  data.table::setcolorder(data, c(groupdim, "Land", "Fylke", "Absolute", "Relative"))
  }
  
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
                                run = CompareGEO){
  
  if(isFALSE(run)){
    cat("CompareGEO = FALSE, function inactive")
    return(invisible(NULL))
  }
  
  compare <- .find_compare(data, "TELLER")
  
  if(is.na(compare)){
    cat("\nNo sumTELLER(_uprikk) or TELLER present in data, no output generated\n")
    return(invisible(NULL))
  } else {
    cat("\nGeoLevels compared on", compare, "\n")
  }
  
  # Create subset, remove helseregion
  data <- data.table::copy(data)[GEO > 0 & GEO < 10000 & !GEO %in% 81:84]
  data[, geolevel := "Kommune"]
  data[GEO < 100, geolevel := "Fylke"]
  
  cat("GEOcodes included: ", stringr::str_c(unique(data$GEO), collapse = ", "), "\n")
  
  # Check if both Fylke and Kommune is present
  if(!"Fylke" %in% data$geolevel){
    cat("No rows corresponding to Fylke, comparison not possible")
    return(invisible(NULL))
  }
  if(!"Kommune" %in% data$geolevel){
    cat("No rows corresponding to Kommune, comparison not possible")
    return(invisible(NULL))
  }
  
  # Sum compare value per strata of geolevel and grouping dims
  data <- data[, .("sum" = sum(get(compare), na.rm = T)), keyby = c("geolevel", groupdim)]
  
  # Format output
  data <- data.table::dcast(data, ... ~ geolevel, value.var = "sum")
  
  # If groupdim == NULL, remove column named "."
  if(is.null(groupdim)){
    data[, . := NULL]
  }
  
  # Estimate absolute and relative difference, format digits
  data[, `:=` (Absolute = Fylke - Kommune,
               Relative = round(Fylke/Kommune, 3))]
  format <- c("Fylke", "Kommune", "Absolute")
  data[, (format) := lapply(.SD, round, 0), .SDcols = format]
  
  if (nrow(data[Absolute != 0]) == 0){
    cat("\nFYLKE and KOMMUNE are identical")
  } else if (nrow(data[Relative < 1]) == 0) {
    cat("\nFYLKE is always equal to or larger than KOMMUNE")
  } else {
    cat("\nIn some rows, KOMMUNE is larger than FYLKE.\n See rows with Absolute < 1")
  }
  
  # If any groupdims present, convert to factor and set column order
  if(!is.null(groupdim)){
  data[, (groupdim) := lapply(.SD, as.factor), .SDcols = groupdim]
  data.table::setcolorder(data, c(groupdim, "Fylke", "Kommune", "Absolute", "Relative"))
  }
  
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
                                run = CompareGEO) {
  
  if(isFALSE(run)){
    cat("CompareGEO = FALSE, function inactive")
    return(invisible(NULL))
  }
  
  compare <- .find_compare(data, "TELLER")
  
  if(is.na(compare)){
    cat("\nNo sumTELLER(_uprikk) or TELLER present in data, no output generated\n")
    return(invisible(NULL))
  } else {
    cat("\nGeoLevels compared on", compare, "\n")
  }
  
  kommunegeo <- c(301, 1103, 4601, 5001)
  bydelsgeo <- unique(data[GEO>9999]$GEO)
  
  # If no data on bydel, stop and return NULL
  if(length(bydelsgeo) < 1){
    cat("No geo-codes corresponding to bydel, comparison not possible.\n")
    return(invisible(NULL))
  } 
  
  # Create subset, create geolevel and kommune variable
  data <- data.table::copy(data)[GEO %in% c(kommunegeo, bydelsgeo)]
  data[, `:=` (geolevel = "Bydel",
               KOMMUNE = character())]
  data[grep("^301", GEO), KOMMUNE := "Oslo"]
  data[grep("^1103", GEO), KOMMUNE := "Stavanger"]
  data[grep("^4601", GEO), KOMMUNE := "Bergen"]
  data[grep("^5001", GEO), KOMMUNE := "Trondheim"]
  data[GEO < 9999, geolevel := "Kommune"]
  
  cat("GEOcodes included: ", stringr::str_c(unique(data$GEO), collapse = ", "), "\n")
  
  # Check if Kommune is present
  if(!"Kommune" %in% data$geolevel){
    cat("No rows corresponding to Kommune, comparison not possible")
    return(invisible(NULL))
  }
  
  # Sum compare value per strata of geolevel and grouping dims
  data <- data[, .("sum" = sum(get(compare), na.rm = T)), keyby = c("KOMMUNE", "geolevel", groupdim)]
  
  # Format output
  data <- data.table::dcast(data, ... ~ geolevel, value.var = "sum")
  
  # Estimate absolute and relative difference, format digits
  data[, `:=` (Absolute = Kommune - Bydel,
               Relative = round(Kommune/Bydel, 3))]
  format <- c("Kommune", "Bydel", "Absolute")
  data[, (format) := lapply(.SD, round, 0), .SDcols = format]
  
  if(nrow(data[Absolute != 0]) == 0) {
    cat("\nKOMMUNE and BYDEL are identical")
  } else if(nrow(data[Relative < 1]) == 0) {
    cat("\nKOMMUNE is always equal to or larger than BYDEL") 
  } else {
    cat("\nIn some rows, BYDEL is larger than KOMMUNE.\nSee rows with Absolute < 1")
  }
  
  # Convert KOMMUNE & groupdim (if present) to factor and set column order
  fct_col <- c("KOMMUNE", groupdim)
  data[, (fct_col) := lapply(.SD, as.factor), .SDcols = fct_col]
  data.table::setcolorder(data, c("KOMMUNE", groupdim, "Kommune", "Bydel", "Absolute", "Relative"))
  
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
                        run = CompareGEO){
  
  if(isFALSE(run)){
    cat("CompareGEO = FALSE, function inactive")
    return(invisible(NULL))
  }
  
  compare <- .find_compare(data, "TELLER")
  
  if(is.na(compare)){
    cat("\nNo sumTELLER(_uprikk) or TELLER present in data, no output generated\n")
    return(invisible(NULL))
  } else {
    cat("\nGeoLevels compared on", compare, "\n")
  }
  
  # Create subset, remove helseregion
  data <- data.table::copy(data)[GEO %in% c(3, 301)]
  data[, geolevel := "Oslo Kommune"]
  data[GEO == 3, geolevel := "Oslo Fylke"]
  
  # Check if both Oslo Fylke and Kommune is present
  if(!"Oslo Fylke" %in% data$geolevel){
    cat("No rows corresponding to Oslo Fylke, comparison not possible")
    return(invisible(NULL))
  }
  if(!"Oslo Kommune" %in% data$geolevel){
    cat("No rows corresponding to Oslo Kommune, comparison not possible")
    return(invisible(NULL))
  }
  
  # Sum compare value per strata of geolevel and grouping dims
  data <- data[, .("sum" = sum(get(compare), na.rm = T)), keyby = c("geolevel", groupdim)]
  
  # Format output
  data <- data.table::dcast(data, ... ~ geolevel, value.var = "sum")
  
  if("." %in% names(data)){
    data[, . := NULL]
  }
  
  # Estimate absolute and relative difference, format digits
  data[, `:=` (Absolute = `Oslo Fylke`- `Oslo Kommune`,
               Relative = round(`Oslo Fylke`/`Oslo Kommune`, 3))]
  format <- c("Oslo Fylke", "Oslo Kommune", "Absolute")
  data[, (format) := lapply(.SD, round, 0), .SDcols = format]
  
  if(nrow(data[Absolute != 0]) == 0) {
    cat("Oslo kommune is identical to Oslo fylke!") 
  } else {
    cat("Oslo fylke is not identical to Oslo fylke.\nSee rows where Absolute does not = 0")
  }
  
  # If any groupdims present, convert to factor and set column order
  if(!is.null(groupdim)){
  data[, (groupdim) := lapply(.SD, as.factor), .SDcols = groupdim]
  data.table::setcolorder(data, c(groupdim, "Oslo Fylke", "Oslo Kommune", "Absolute", "Relative"))
  }
  
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
  
  plotdata <- data.table::copy(data)
  
  # Identify dimension and value columns
  .IdentifyColumns(plotdata)
  
  # Identify all dimensions in the file, plotdims (- GEO and AAR) and plotvals (- RATE.n and SPVFLAGG)
  .TSplotdims <<- stringr::str_subset(.dims1, "GEO|AAR", negate = TRUE)
  .TSplotvals <<- stringr::str_subset(names(plotdata),
                                      stringr::str_c(c(.dims1, "RATE.n", "SPVFLAGG"), collapse = "|"),
                                      negate = TRUE)
  
  # Find plotheight for HTML report
  .PlotHeight <<- 2 + 2*ceiling(length(.TSplotvals)/2)
  
  # Extract only country level data,
  plotdata <- plotdata[GEO == 0]
  # Keep all dimensions, but only value columns included in plotval
  plotdata <- plotdata[, .SD, .SDcols = c(.dims1, .TSplotvals)]
  
  # Organize plotdata according to all dimensions
  data.table::setkeyv(plotdata, .dims1)
  
  # Create AARx for plotting on x-axis
  plotdata[, AARx := as.numeric(stringr::str_extract(AAR, "[:digit:]*(?=_)"))]
  
  # Identify dimensions to aggregate or keep total (if containing 0 or only 1 unique value), excluding dim to be plotted
  aggdims <- .TSplotdims
  containtotal <- character()
  for(i in aggdims){
    if(0 %in% plotdata[, unique(get(i))] | plotdata[, length(unique(get(i)))] == 1){
      # add to containtotal
      containtotal <- c(containtotal, i)
      # remove from aggdims
      aggdims <- stringr::str_subset(aggdims, i, negate = T) 
    }
  }
  
  # Find total age group, if existing
  ALDERtot_tf <- FALSE
  ALDERtot <- NULL
  if("ALDER" %in% names(plotdata)){
  ALDERl <- min(as.numeric(stringr::str_extract(plotdata$ALDER, "[:digit:]*(?=_)")))
  ALDERh <- max(as.numeric(stringr::str_extract(plotdata$ALDER, "(?<=_)[:digit:]*")))
  ALDERtot <- paste0(ALDERl, "_", ALDERh)
  ALDERtot_tf <- ALDERtot %in% unique(plotdata$ALDER)
  
    if(ALDERtot_tf){
      containtotal <- c(containtotal, "ALDER")
      aggdims <- stringr::str_subset(aggdims, "ALDER", negate = TRUE)
    } 
  }
  
  # Identify value columns to average or sum
  sumcols <- .TSplotvals[stringr::str_detect(.TSplotvals, c("TELLER"))]
  avgcols <- .TSplotvals[!.TSplotvals %in% sumcols]
  
  # Convert avgcols to numeric to avoid warning
  plotdata[, (avgcols) := lapply(.SD, as.numeric), .SDcols = avgcols]
  
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
  TS <<- purrr::map(.TSplotdims, ~.plot_ts(dim = .x,
                                           plotdata = plotdata,
                                           plotvals = .TSplotvals,
                                           aggdims = aggdims,
                                           containtotal = containtotal,
                                           sumcols = sumcols,
                                           avgcols = avgcols,
                                           dimexist = .dims1,
                                           ALDERtot = ALDERtot)
                    )
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
  aggdims <- stringr::str_subset(aggdims, dim, negate = TRUE)
  containtotal <- stringr::str_subset(containtotal, dim, negate = TRUE)
  
  # Copy plotdata
  d <- data.table::copy(plotdata)
  
  # Find n rows for plot legend
  nrow_legend <- ceiling(length(unique(d[[dim]]))/3)
  
  # Keep totals for columns with total present (containtotal). 
  # For ALDER, rows = aldertot is kept, for other dimensions rows == 0 is kept. 
  for(i in containtotal){
    if(i == "ALDER"){
      d <- d[get(i) == ALDERtot]
    } else if (0 %in% d[, unique(get(i))]) {
      d <- d[get(i) == 0]
    } else {
      d
    }
  }
  
  # Aggregate value columns for all dimensions without total (aggdims)
  for(i in aggdims){
    
    # Group by all existing standard dimensions
    groupcols <- stringr::str_subset(dimexist, i, negate = TRUE)
    d[, (avgcols) := lapply(.SD, mean, na.rm = T), .SDcols = avgcols, by = groupcols]
    d[, (sumcols) := lapply(.SD, sum, na.rm = T), .SDcols = sumcols, by = groupcols]
    if(!is.character(d[[i]])){
      d[, (i) := lapply(.SD, as.character), .SDcols = i]
    }
    d[, (i) := "Aggregated"]
    # Remove duplicates and return data
    d <- unique(d)
  }
  
  # Convert dim to factor and .TSplotvals to double for plotting
  d[, (dim) := lapply(.SD, as.factor), .SDcols = dim]
  d[, (plotvals) := lapply(.SD, as.numeric), .SDcols = plotvals]
  
  # convert to long format
  d <- data.table::melt(
    d,
    measure.vars = plotvals,
    variable.name = "targetnumber",
    value.name = "yval"
  )
  
  # Plot time series
  plot <- d %>%
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
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),
          panel.spacing = unit(0.5, "cm"))  +
    ggh4x::force_panelsizes(rows = unit(4, "cm"))
  
  if(nrow_legend > 4){
    plot +
      guides(color = "none")
  } else {
    plot +
      guides(color = guide_legend(title = NULL, 
                                  nrow = nrow_legend, 
                                  byrow = TRUE))
  }
}

PrintTimeseries <- function(dims = .TSplotdims,
                            plots = TS){
  
  for(i in 1:length(dims)){
    
    header <-  paste0("\n\n## Across ", dims[i], "\n")
    cat(header)
    
    print(plots[[i]])
    
    cat("\n")
  }
}

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
UnspecifiedBydel <- function(data = dfnew,
                             minteller_kommune = NULL,
                             maxrows = TRUE){
  
  kommunegeo <- c(301, 1103, 4601, 5001)
  bydelsgeo <- unique(data[GEO>9999]$GEO)
  
  # If no data on bydel, stop and return NULL
  if(length(bydelsgeo) < 1){
    cat("No geo-codes corresponding to bydel, not possible to estimate unspecified bydel.\n")
    return(invisible(NULL))
  } 
  
  d <- data.table::copy(data)
  
  # Identify dimension and value columns
  .IdentifyColumns(d)
  
  # Remove years with no data on bydel
  bydelaar <- d[GEO %in% bydelsgeo & SPVFLAGG == 0, unique(AAR)]
  d <- d[AAR %in% bydelaar]
  
  # If no rows left after filtering years, stop and return NULL.
  if(nrow(d) < 1){
      cat("No rows left in data after removing years without data on bydel.\n")
      return(invisible(NULL))
    } 
  
  # Pick relevant TELLER and NEVNER columns. 
  # sumX_uprikk > sumX > X
  tellerval <- .find_compare(data, "TELLER") 
  nevnerval <- .find_compare(data, "NEVNER")
  
  vals <- c(tellerval, nevnerval)
  
  if(length(vals) < 1){
    cat("No TELLER or NEVNER columns found in data, not possible to estimate unspecified bydel.\n")
    return(invisible(NULL))
  }
  
  outcols <- c(.dims1, vals, "SPVFLAGG")
        
  # Create subset, create geolevel and kommune variable
  d <- d[GEO %in% c(kommunegeo, bydelsgeo), ..outcols]
  d[, ':=' (KOMMUNE = character(),
            GEONIV = "Bydel")]
  d[grep("^301", GEO), KOMMUNE := "Oslo"]
  d[grep("^1103", GEO), KOMMUNE := "Stavanger"]
  d[grep("^4601", GEO), KOMMUNE := "Bergen"]
  d[grep("^5001", GEO), KOMMUNE := "Trondheim"]
  d[GEO < 9999, GEONIV := "Kommune"]
  
  # Identify complete strata within kommune and all dims except GEO
  # count number of missing tellerval and nevnerval in each strata
  d <- data.table::melt(d, measure.vars = c(vals), variable.name = "MALTALL")
  d[, sumprikk := sum(is.na(value)),
    by = c("KOMMUNE", 
           stringr::str_subset(c(.dims1, "MALTALL"), "GEO", negate = TRUE))]

  # Only keep complete strata
  d <- d[sumprikk == 0]
  if(nrow(d) < 1){
    cat("No complete strata, not possible to estimate unspecified bydel. Was bydelstart set to the correct year?\n")
    return(invisible(NULL))
  } 
  
  # sum value columns for kommune and bydel, and convert to wide format
  d <- d[, .(sum = sum(value, na.rm = T)), by = c("KOMMUNE", "GEONIV", "MALTALL",
                                               stringr::str_subset(.dims1, "GEO", negate = TRUE))]
  
  d <- data.table::dcast(d, ... ~ GEONIV, value.var = "sum")
  
  # If minteller provided, exclude rows where MALTALL == tellerval and bydel or kommune < minteller
  if(is.numeric(minteller_kommune)){
    d <- d[!(MALTALL == tellerval & Kommune < minteller_kommune)]
  }

  # Estimate unknown bydel
  d[, `UOPPGITT, %` := 100*(1 - Bydel/Kommune)]
  
  # Convert all dimensions to factor for search function, round numeric columns
  convert <- stringr::str_subset(names(d), str_c(c(.dims1, "KOMMUNE", "MALTALL"), collapse = "|"))
  d[, (convert) := lapply(.SD, as.factor), .SDcols = convert]
  d[Bydel == 0 & Kommune == 0, `UOPPGITT, %` := NA_real_]
  round <- which(sapply(d, is.numeric))
  d[, (round) := lapply(.SD, round, 2), .SDcols = round]
  
  # Order according to unspecified bydel
  d <- d[order(-`UOPPGITT, %`)]
  
  ### Consider saving to environment and make file dump, especially when files are too large for HTML-table
  
  # Find number of kommune and maltall
  n_kommune <- length(unique(d$KOMMUNE))
  n_maltall <- length(unique(d$MALTALL))
  
  # Print sumary information
  cat(paste0("Total number of strata with complete bydel (teller + nevner): ", nrow(d)))
  cat(paste0("\nOslo: ", nrow(d[KOMMUNE == "Oslo"])))
  cat(paste0("\nBergen: ", nrow(d[KOMMUNE == "Bergen"])))
  cat(paste0("\nStavanger: ", nrow(d[KOMMUNE == "Stavanger"])))
  cat(paste0("\nTrondheim: ", nrow(d[KOMMUNE == "Trondheim"])))
  
  # If nrow is > 8 000, show maximum 10 000 observations
  if(maxrows && nrow(d) > 8000){
    
    # Estimate observations per kommune*maltall to get total <= 8000
    n_obs <- floor(8000 / (n_kommune*n_maltall))
    
    cat(paste0("\nTop ", n_obs, " observations shown per MALTALL per KOMMUNE: "))
    
    d <- d[, .SD[1:n_obs], by = c("KOMMUNE", "MALTALL")]
  }
  
  # Set column order output
  data.table::setcolorder(d,
                          c("KOMMUNE",
                            stringr::str_subset(.dims1, "GEO", negate = TRUE),
                            "MALTALL",
                            "Kommune",
                            "Bydel"))
  
  # Make datatable output (max )
  DT::datatable(d, 
                filter = "top",
                rownames = F,
                options = list(
                  columnDefs = list(list(targets = c("Bydel", "Kommune"),
                                         searchable = FALSE)),
                  # Show length menu, table, pagination, and information
                  dom = 'ltpi')
                )
  
}

.find_compare <- function(data,
                          type){
  if(type == "TELLER"){
    val <- data.table::fcase("sumTELLER_uprikk" %in% names(data), "sumTELLER_uprikk",
                             "sumTELLER" %in% names(data), "sumTELLER",
                             "TELLER_uprikk" %in% names(data), "TELLER_uprikk",
                             "TELLER" %in% names(data), "TELLER",
                             default = NA_character_)
  }
  
  if(type == "NEVNER"){
    val <- data.table::fcase("sumNEVNER_uprikk" %in% names(data), "sumNEVNER_uprikk",
                             "sumNEVNER" %in% names(data), "sumNEVNER",
                             "NEVNER_uprikk" %in% names(data), "NEVNER_uprikk",
                             "NEVNER" %in% names(data), "NEVNER",
                             default = NA_character_)  
  }
  
  val
}

.CompareDim <- function(data1, 
                        data2, 
                        dim = NULL){
  
  # Identify unique levels of dim, 
  levels1 <- unique(data1[[dim]])
  levels2 <- unique(data2[[dim]])
  
  # Identify new or expired levels
  newlevels <- stringr::str_subset(levels1, stringr::str_c("^", levels2, "$", collapse = "|"), negate = TRUE)
  explevels <- stringr::str_subset(levels2, stringr::str_c("^", levels1, "$", collapse = "|"), negate = TRUE)
  
  # Replace with "none" if 0 new/expired levels
  if(length(newlevels) == 0){
    newlevels <- "none"
  } 
  
  if(length(explevels) == 0){
    explevels <- "none"
  }
  
  # Create output
  dplyr::tibble("Dimension" = dim,
                "N levels (new)" = length(levels1),
                "New levels" = stringr::str_c(newlevels, collapse = ", "),
                "Expired levels" = stringr::str_c(explevels, collapse = ", "))
}
