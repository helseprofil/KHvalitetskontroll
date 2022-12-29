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
  
  new <- names(data1)[!names(data1) %in% names(data2)]  
  
  exp <- names(data2)[!names(data2) %in% names(data1)]  
  
  msgnew <- case_when(length(new) == 0 ~ "No new columns.",
                      TRUE ~ paste0("New columns found: ", str_c(new, collapse = ", ")))
  
  msgexp <- case_when(length(exp) == 0 ~ "\nNo expired columns.",
                      TRUE ~ paste0("\nExpired columns found: ", str_c(exp, collapse = ", ")))
  
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
#' @param dims Character vector of dimensions to compare
#'
#' @return table with 4 columns: Dimension name, N levels, New levels, Expired levels, with one row per input dimension
#' @export
#'
#' @examples
CompareDims <- function(data1 = dfnew, 
                        data2 = dfold,
                        dims = c(STANDARDdims, EXTRAdims)){
  
  if(!exists(".ALL_DIMENSIONS")) {
    source("https://raw.githubusercontent.com/helseprofil/misc/main/alldimensions.R")
    .ALL_DIMENSIONS <- ALL_DIMENSIONS
    rm(ALL_DIMENSIONS)
  }
  
  dimnew <- names(data1)[names(data1) %in% .ALL_DIMENSIONS]
  dimold <- names(data2)[names(data2) %in% .ALL_DIMENSIONS]
  commondims <- dimnew[dimnew %in% dimold]
  newdims <- dimnew[!dimnew %in% dimold]
  expdims <- dimold[!dimold %in% dimnew]
  
  if(length(expdims) != 0 || length(newdims) != 0){
  cat(c("The following dimensions are not present in both files: ", print_dim(c(newdims, expdims)), "\n"))
  }
  
  .CompareDim <- function(data1, 
                         data2, 
                         dim = NULL){
    
    .levelsnew <- data1 %>% 
      pull(dim) %>% 
      unique()
    
    .levelsold <- data2 %>% 
      pull(dim) %>% 
      unique()
    
    .length <- length(.levelsnew)
    
    .newdims <- .levelsnew[!(.levelsnew %in% .levelsold)]
    
    .expdims <- .levelsold[!(.levelsold %in% .levelsnew)]
    
    all <- str_c(.levelsnew, collapse = ", ")
    newlevels <- ifelse(length(.newdims) > 0,
                        str_c(.newdims, collapse = ", "),
                        "none")
    explevels <- ifelse(length(.expdims) > 0,
                        str_c(.expdims, collapse = ", "),
                        "none")
    
    tibble("Dimension" = dim,
           "N levels" = .length,
           "New levels" = newlevels,
           "Expired levels" = explevels)
  }
  
  map_df(commondims, ~.CompareDim(data1, data2, dim = .x))
}


#' ComparePrikk
#' 
#' Calculate the number of censored observations in the new and old KUBE, and calculate the absolute and relative difference. Results can be further grouped by an additional dimension. 
#'
#' @param data1 new KUBE, defaults to dfnew set in INPUT
#' @param data2 old KUBE, defaults to dfold set in INPUT
#' @param groupdim dimension to group output by, in addition to SPVFLAGG and AAR
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
                rownames = F,
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
  
  # Identify common columns, and extract dimensions
  if(!exists(".ALL_DIMENSIONS")) {
    source("https://raw.githubusercontent.com/helseprofil/misc/main/alldimensions.R")
    .ALL_DIMENSIONS <- ALL_DIMENSIONS
  }
  commoncols <- names(data1)[names(data1) %in% names(data2)]
  dimexist <- commoncols[commoncols %in% .ALL_DIMENSIONS]
  groupdims <- dimexist[str_detect(dimexist, "AAR", negate = T)]
  
  # combine data
  data <- rbindlist(list(copy(data1)[, ..commoncols][, KUBE := "New"], 
                         copy(data2)[, ..commoncols][, KUBE := "Old"]))
  
  # Calculate n censored observations, 
  # Aggregate to N prikk per strata
  # Calculate proportions of time series with n prikk

  data <- data[, FLAGG := 0][SPVFLAGG != 0, FLAGG := 1]
  data <- data[, .(N_PRIKK = sum(FLAGG)), by = c(groupdims, "KUBE")]
  data <- data[, .(PRIKK = .N), by = .(KUBE, N_PRIKK)]
  data[, ANDEL := round(PRIKK/sum(PRIKK), 3), by = KUBE]

  
  # Create output table
  out <- dcast(data, 
               N_PRIKK~KUBE,
               value.var = c("PRIKK", "ANDEL"))
  outnames <- names(out)
  setcolorder(out, c("N_PRIKK",
                     outnames[str_detect(outnames, "New")],
                     outnames[str_detect(outnames, "Old")]))
  out[, "N_PRIKK" := as.factor(N_PRIKK)]
  
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
                       limit = PRIKKlimit,
                       standarddims = STANDARDdims,
                       extradims = EXTRAdims){
  
  filtered <- data1[data1[[val]] <= limit]
  
  cat(paste0("Controlled column: ", val))
  cat(paste0("\nLimit: ", limit))
  
  if(!anyNA(c(val, limit))){
  if(nrow(filtered) == 0) {
    cat("\nNo values < limit")
  } else {
    cat(paste0("\nN values <= limit: ", nrow(filtered)))
    output <- filtered %>% 
      select(any_of(standarddims), any_of(extradims), any_of(val), everything()) %>% 
      mutate(across(where(is.numeric), ~round(.x, 2)))
    DT::datatable(output, rownames = F)
  }
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
CompareFylkeLand <- function(data = dfnew, groupdim = GROUPdims, compare = COMPAREval){
  
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
  data[, (groupdim) := lapply(.SD, as.factor), .SDcols = c(groupdim)]
  setcolorder(data, c(groupdim, "Land", "Fylke", "Absolute", "Relative"))
  
  DT::datatable(data[order(-Relative)], 
                filter = "top",
                rownames = F,
                options = list(
                  columnDefs = list(list(targets = c("Land", "Fylke", "Absolute", "Relative"),
                                         searchable = FALSE)),
                  # Show length menu, table, pagination, and information
                  dom = 'ltpi', 
                  scrollX = TRUE)
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
CompareKommuneFylke <- function(data = dfnew, groupdim = GROUPdims, compare = COMPAREval){
  
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
  data[, (groupdim) := lapply(.SD, as.factor), .SDcols = c(groupdim)]
  setcolorder(data, c(groupdim, "Fylke", "Kommune", "Absolute", "Relative"))
  
  DT::datatable(data[order(-Relative)], 
                filter = "top",
                rownames = F,
                options = list(
                  columnDefs = list(list(targets = c("Fylke", "Kommune", "Absolute", "Relative"),
                                         searchable = FALSE)),
                  # Show length menu, table, pagination, and information
                  dom = 'ltpi', 
                  scrollX = TRUE)
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
CompareBydelKommune <- function(data1 = dfnew, groupdim = GROUPdims, compare = COMPAREval) {
  
  data <- data1 %>% 
    filter(GEO > 100) %>% 
    mutate(geolevel = case_when(GEO < 10000 ~ "Kommune",
                                TRUE ~ "Bydel")) %>%
    dplyr::filter(!(GEO %in% 3011:3019), # Deselect KOMMUNE in Viken, otherwise included in Oslo
                  str_detect(GEO, "^301|^1103|^4601|^5001")) %>% 
    mutate(KOMMUNE = case_when(str_detect(GEO, "^301") ~ "Oslo",
                               str_detect(GEO, "^1103") ~ "Stavanger",
                               str_detect(GEO, "^4601") ~ "Bergen",
                               str_detect(GEO, "^5001") ~ "Trondheim"))
  
  output <- data %>%
    group_by(across(c(KOMMUNE, geolevel, all_of(groupdim)))) %>% 
    summarise(sum = sum(.data[[compare]], na.rm = T), .groups = "drop") %>% 
    pivot_wider(names_from = geolevel, 
                values_from = sum) %>% 
    mutate(Absolute = Kommune-Bydel,
           Relative = Kommune/Bydel) %>%  
    arrange(desc(Relative)) %>% 
    mutate(across(c(Bydel, Kommune, Absolute), ~round(.x, 0)),
           across(Relative, ~case_when(Relative == Inf ~ NA_real_,
                                      TRUE ~ round(Relative, 3)))) %>% 
    select(KOMMUNE, all_of(groupdim), Kommune, Bydel, Absolute, Relative)
  
  cat("GEOcodes included: ", str_c(unique(data$GEO), collapse = ", "), "\n")
  
  if(nrow(output %>% 
          dplyr::filter(Relative < 1)) == 0) {
    cat("\nKOMMUNE is always larger than BYDEL") 
  } else {
    cat("\nIn some rows, BYDEL is larger than KOMMUNE.\nSee rows with Absolute < 1")
  }
  
  datatable(output, rownames = F)
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
CompareOslo <- function(data1 = dfnew, groupdim = GROUPdims, compare = COMPAREval){
  output <- data1 %>% 
    dplyr::filter(GEO %in% c("3", "301")) %>% 
    mutate(geolevel = case_when(GEO == 3 ~ "Oslo Fylke",
                                GEO == 301 ~ "Oslo Kommune")) %>% 
    group_by(across(c(geolevel, all_of(groupdim)))) %>% 
    summarise(sum = sum(.data[[compare]], na.rm = T), .groups = "drop") %>% 
    pivot_wider(names_from = geolevel, 
                values_from = sum) %>% 
    mutate(Absolute = `Oslo Fylke`-`Oslo Kommune`,
           Relative = `Oslo Fylke`/`Oslo Kommune`) %>% 
    arrange(desc(Relative)) %>% 
    mutate(across(c(`Oslo Fylke`, `Oslo Kommune`, Absolute), ~round(.x, 0)),
           across(Relative, ~case_when(Relative == Inf ~ NA_real_,
                                       TRUE ~ round(Relative, 3))))
           
  
  if(nrow(output %>% 
          dplyr::filter(Relative != 1)) == 0) {
    cat("Oslo kommune is identical to Oslo fylke!") 
  } else {
    cat("Oslo fylke is not identical to Oslo fylke.\nSee rows where Absolute does not = 0")
  }
  
  datatable(output, rownames = F)
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
PlotTimeseries <- function(data = dfnew,
                           plotdims = PLOTDIMS,
                           plotvals = PLOTVALS){
  
  if(!exists(".ALL_DIMENSIONS")) {
    source("https://raw.githubusercontent.com/helseprofil/misc/main/alldimensions.R")
    .ALL_DIMENSIONS <- ALL_DIMENSIONS
  }
  
  plotdata <- copy(data)
  
  # Identify all dimensions in the file
  dimexist <- names(plotdata)[names(plotdata) %in% .ALL_DIMENSIONS]
  
  # Extract only country level data,
  plotdata <- plotdata[GEO == 0]
  # Keep all dimensions, but only value columns included in plotval
  plotdata <- plotdata[, c(..dimexist, ..plotvals)]
  
  # If ALDER is included, only keep total (minALDERl_maxALDERh)
  if ("ALDER" %in% dimexist) {
    plotdata <- .AggregateAge(data = plotdata)
  }
  
  # Organize plotdata according to all dimensions
  setkeyv(plotdata, dimexist)
  
  # Identify extra dimensions
  # aggregate plotvals to totals, remove extra dimensions, remove duplicated rows
  dimextra <- dimexist[!dimexist %in% c("GEO", "AAR", "KJONN", "ALDER", "UTDANN", "INNVKAT", "LANDBAK")]
  
  # if (length(dimextra) > 0 && !dimextra %in% plotdims) {
  #   plotdata <- .AggregateExtradim(data = plotdata,
  #                                  dimexist = dimexist,
  #                                  dimextra = dimextra,
  #                                  plotvals = plotvals)
  # }
  
  # Create AARx for plotting on x-axis
  plotdata[, AARx := as.numeric(str_extract(AAR, "[:digit:]*(?=_)"))]
  
  
  totaldims <- dimexist[!dimexist %in% c("GEO", "AAR", "ALDER", dimextra)]
  
  # Loop through plotdims to generate the plots

  plots <- map(plotdims, ~.plot_ts(data = plotdata,
                                   dim = .x,
                                   plotvals = plotvals,
                                   totaldims = totaldims,
                                   dimextra = dimextra,
                                   dimexist = dimexist))

  # Print plots without plotting message
  walk(plots, print)
} 

.AggregateExtradim <- function(data, dimextra, dimexist, plotvals){
  # Group by all existing standard dimensions
  groupcols <- str_subset(dimexist, dimextra, negate = TRUE)
  # Identify value columns to average or sum
  sumcols <- plotvals[str_detect(plotvals, c("TELLER"))]
  avgcols <- plotvals[!plotvals %in% sumcols]
  # Aggregate plotvals, remove dimextra column, remove duplicated rows
  data[, (avgcols) := lapply(.SD, mean, na.rm = T), .SDcols = avgcols, by = groupcols]
  data[, (sumcols) := lapply(.SD, sum, na.rm = T), .SDcols = sumcols, by = groupcols]
  data[, (dimextra) := NULL]
  data <- unique(data)
  data
}

.AggregateAge <- function(data){
  ALDERl <- min(as.numeric(str_extract(data$ALDER, "[:digit:]*(?=_)")))
  ALDERh <- max(as.numeric(str_extract(data$ALDER, "(?<=_)[:digit:]*")))
  ALDERtot <- paste0(ALDERl, "_", ALDERh)
  data[ALDER == ALDERtot]
}

# create plotting function
.plot_ts <- function(data = plotdata, 
                     dim,
                     plotvals = plotvals,
                     totaldims = totaldims,
                     dimextra = dimextra,
                     dimexist = dimexist){
  
  data <- copy(data)
  
  nrow <- ceiling(length(unique(data[[dim]]))/3)
  
  if(dim %in% dimextra){
    dimextra <- dimextra[!dimextra %in% dim]
  }
             
  if(length(dimextra) > 0 && !dimextra %in% dim){
    data <- .AggregateExtradim(data = data,
                               dimextra = dimextra,
                               dimexist = dimexist,
                               plotvals = plotvals)
  }
  
  totaldims <- totaldims[!totaldims %in% dim]
  
  data %>%
    mutate(across(all_of(dim), as.factor)) %>% 
    filter(if_all(all_of(totaldims), ~ .x == 0)) %>%
    pivot_longer(cols = all_of(plotvals),
                 names_to = "targetnumber",
                 values_to = "yval") %>%
    ggplot(aes(
      x = AARx,
      y = yval,
      color = .data[[dim]],
      group = .data[[dim]]
    )) +
    geom_point() +
    geom_line() +
    facet_grid(rows = vars(targetnumber),
               scales = "free_y", 
               switch = "y") + 
    labs(x = "Year",
         y = NULL,
         title = paste0("Time series according to ", dim)) + 
    scale_x_continuous(breaks = seq(min(data$AARx), 
                                    max(data$AARx),
                                    by = 1),
                       expand = c(0,0)) + 
    guides(color = guide_legend(title = NULL, nrow = nrow)) + 
    theme(axis.text.x = element_text(angle = 30, vjust = 0.5),
          panel.spacing = unit(0.5, "cm"))  + 
    force_panelsizes(rows = unit(4.5, "cm"))
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
UnspecifiedBydel <- function(data = dfnew){
  
  kommunegeo <- c(301, 1103, 4601, 5001)
  bydelsgeo <- unique(data[GEO>9999]$GEO)
  
  # If no data on bydel, stop and return NULL
  if(length(bydelsgeo) < 1){
    cat("No geo-codes corresponding to bydel, not possible to estimate unspecified bydel.\n")
    return(invisible(NULL))
  } 
  
  d <- copy(data)
  
  # Remove years with no data on bydel
  
  bydelaar <- d[GEO %in% bydelsgeo & SPVFLAGG == 0, .N, by = AAR]$AAR
  d <- d[AAR %in% bydelaar]
  
  # If no rows left after filtering years, stop and return NULL.
  if(nrow(d) < 1){
      cat("No rows left in data after removing years < bydel start.\n")
      return(invisible(NULL))
    } 
  
  # Identify dimemsion and value columns
  dims <- names(d)[names(d) %in% .ALL_DIMENSIONS]
  vals <- names(d)[!names(d) %in% dims]
  vals <- str_subset(vals, "TELLER|NEVNER")
        
  # Create subset, create geolevel and kommune variable
  d <- d[GEO %in% c(kommunegeo, bydelsgeo), c(..dims, ..vals, "SPVFLAGG")][, ':=' (KOMMUNE = character(),
                                                                                      GEONIV = "Bydel")]
  d[grep("^301", GEO), KOMMUNE := "Oslo"]
  d[grep("^1103", GEO), KOMMUNE := "Stavanger"]
  d[grep("^4601", GEO), KOMMUNE := "Bergen"]
  d[grep("^5001", GEO), KOMMUNE := "Trondheim"]
  d[GEO < 9999, GEONIV := "Kommune"]
  
  # Identify complete strata within kommune and all dims except GEO
  d[, sumSPV := sum(SPVFLAGG), by = c("KOMMUNE", 
                                      dims[!dims %in% c("GEO")])]
  # Only keep complete strata
  d <- d[sumSPV == 0]
  if(nrow(d) < 1){
    cat("No complete strata, not possible to estimate unspecified bydel. Was bydelstart set to the correct year?\n")
    return(invisible(NULL))
  } 
  
  # sum `vals` columns for kommune and bydel
  d <- d[, lapply(.SD, sum, na.rm = T), .SDcols = vals, by = c("KOMMUNE", "GEONIV", 
                                                               str_subset(dims, "GEO", negate = TRUE))]
  
  # Format table
  d[, (vals) := lapply(.SD, as.numeric, na.rm = T), .SDcols = vals]
  d <- melt(d, measure.vars = c(vals), variable.name = "MALTALL")
  d <- dcast(d, ... ~ GEONIV, value.var = "value")
  
  # Estimate unknown bydel
  d[, `UOPPGITT, %` := round(100* (1 - Bydel/Kommune),1)]
  
  # Convert all dimensions to factor for search function
  convert <- c("KOMMUNE", names(d)[names(d) %in% dims], "MALTALL")
  d[, (convert) := lapply(.SD, as.factor), .SDcols = c(convert)]
  
  # Make datatable output
  DT::datatable(d[order(-`UOPPGITT, %`)], 
                filter = "top",
                rownames = F,
                options = list(
                  columnDefs = list(list(targets = c("Bydel", "Kommune", "UOPPGITT, %"),
                                         searchable = FALSE)),
                  # Show length menu, table, pagination, and information
                  dom = 'ltpi', 
                  scrollX = TRUE)
                )
  
}

