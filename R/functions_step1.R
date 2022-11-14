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
  
  new <- names(data1 %>% 
                 select(!any_of(as.character(names(data2))))) 
  
  exp <- names(data2 %>% 
                 select(!any_of(as.character(names(data1))))) 
  
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
  
  dimnew <- names(data1[, names(data1) %in% dims, with = F])
  dimold <- names(data2[, names(data2) %in% dims, with = F])
  commondims <- dimnew[dimnew %in% dimold]
  newdims <- dimnew[!dimnew %in% dimold]
  expdims <- dimold[!dimold %in% dimnew]
  
  if(length(expdims) != 0 || length(newdims) != 0){
  cat(c("The following dimensions are not present in both files: ", print_dim(c(newdims, expdims)), "\n"))
  }
  
  CompareDim <- function(data1, 
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
  
  map_df(commondims, ~CompareDim(data1, data2, dim = .x))
}


#' ComparePrikk
#' 
#' Calculate the number of censored observations in the new and old KUBE, and calculate the absolute and relative difference. Results can be further grouped by an additional dimension. 
#'
#' @param data1 new KUBE, defaults to dfnew set in INPUT
#' @param data2 old KUBE, defaults to dfold set in INPUT
#' @param groupdim dimension to group output by
#'
#' @return a table containing the number of flagged rows in the new and old KUBE, and the absolute and relative difference, grouped by type of SPVFLAGG and an additional dimension (optional)
#' @export
#'
#' @examples
#' ComparePrikk(groupdim = ALDER)
ComparePrikk <- function(data1 = dfnew, 
                         data2 = dfold, 
                         groupdim = EXTRAdims){
  
  output <- full_join(
    data1 %>% 
      group_by(across(c(SPVFLAGG, all_of(groupdim)))) %>% 
      summarise(N_New = n(), .groups = "drop"),
    data2 %>% 
      group_by(across(c(SPVFLAGG, all_of(groupdim)))) %>% 
      summarise(N_Old = n(), .groups = "drop"),
    by = c("SPVFLAGG", all_of(groupdim))) %>% 
    replace_na(list(N_New = 0,
                    N_Old = 0)) %>% 
    mutate(across(SPVFLAGG, as.character),
           SPVFLAGG = paste0("SPVFLAGG=", SPVFLAGG),
           Absolute = N_New - N_Old,
           Relative = round(N_New/N_Old, 3),
           Relative = case_when(Relative == Inf ~ NA_real_,
                                TRUE ~Relative))
  
  DT::datatable(output, rownames = F)
    
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
    rm(ALL_DIMENSIONS)
  }
  commoncols <- names(data1)[names(data1) %in% names(data2)]
  dimexist <- .ALL_DIMENSIONS[.ALL_DIMENSIONS %in% commoncols]
  groupdims <- dimexist[str_detect(dimexist, "AAR", negate = T)]
  
  # combine data
  data <- rbindlist(list(copy(data1)[, KUBE := "New"], 
                         copy(data2)[, KUBE := "Old"]))
  
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
  
  DT::datatable(out, rownames = F)
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

#' CompareLandFylke
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
CompareLandFylke <- function(data1 = dfnew, groupdim = GROUPdims, compare = COMPAREval){
  
  data <- data1 %>% 
    dplyr::filter(GEO < 100) %>% 
    dplyr::filter(!(GEO %in% 81:84)) %>% # Remove HELSEREGION
    mutate(geolevel = case_when(GEO == 0 ~ "Land",
                                GEO < 100 ~ "Fylke")) 
  
  output <- data %>% 
    group_by(across(c(geolevel, all_of(groupdim)))) %>% 
    summarise(sum = sum(.data[[compare]], na.rm = T), .groups = "drop") %>% 
    pivot_wider(names_from = geolevel, 
                values_from = sum) %>% 
    mutate(Absolute = Land-Fylke,
           Relative = Land/Fylke) %>% 
    arrange(desc(Relative)) %>% 
    mutate(across(c(Land, Fylke, Absolute), ~round(.x, 0)),
           across(Relative, ~case_when(Relative == Inf ~ NA_real_,
                                       TRUE ~ round(Relative, 3)))) %>% 
    select(all_of(groupdim), Land, Fylke, Absolute, Relative)
  
  cat("GEOcodes included: ", str_c(unique(data$GEO), collapse = ", "), "\n")
  
  if(nrow(output %>%
          dplyr::filter(Relative < 1)) == 0) {
    cat("\nLAND is always larger than FYLKE")
  } else {
    cat("\nIn some rows, FYLKE is larger than LAND.\n See rows with relative < 1")
  }
   
  datatable(output, rownames = F)
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
    cat("\nIn some rows, BYDEL is larger than KOMMUNE.\nSee rows with relative < 1")
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
    cat("Oslo fylke is not identical to Oslo fylke.\nSee rows where relative does not = 1")
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
    rm(ALL_DIMENSIONS)
  }
  
  plotdata <- copy(data)
  
  # Extract only country level data,
  plotdata <- plotdata[GEO == 0]
  # Keep all dimensions, but only value columns included in plotval
  plotdata <- plotdata[, names(plotdata) %in% c(.ALL_DIMENSIONS, plotvals), with = F]
  
  # Identify all dimensions in the file
  dimexist <- .ALL_DIMENSIONS[.ALL_DIMENSIONS %in% names(plotdata)]
  
  # If ALDER is included, only keep total (minALDERl_maxALDERh)
  if ("ALDER" %in% dimexist) {
    plotdata <- .AggregateAge(data = plotdata)
  }
  
  # Organize plotdata according to all dimensions
  setkeyv(plotdata, dimexist)
  
  # Identify extra dimensions
  # aggregate plotvals to totals, remove extra dimensions, remove duplicated rows
  dimextra <- dimexist[!dimexist %in% c("GEO", "AAR", "KJONN", "ALDER", "UTDANN", "INNVKAT", "LANDBAK")]
  
  if (length(dimextra) > 0 && !dimextra %in% plotdims) {
    plotdata <- .AggregateExtradim(data = plotdata,
                                   dimexist = dimexist,
                                   dimextra = dimextra,
                                   plotvals = plotvals)
  }
  
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
  groupcols <- dimexist[!dimexist %in% dimextra]
  # Identify value columns to average or sum
  sumcols <- plotvals[str_detect(plotvals, c("TELLER"))]
  avgcols <- plotvals[!plotvals %in% sumcols]
  # Aggregate plotvals, remove dimextra column, remove duplicated rows
  data[, (avgcols) := lapply(.SD, mean, na.rm = T), .SDcols = avgcols, by = groupcols]
  data[, (sumcols) := lapply(.SD, sum, na.rm = T), .SDcols = sumcols, by = groupcols]
  data[, (dimextra) := NULL]
  data <- unique(data)
}

.AggregateAge <- function(data){
  data[, ':=' (ALDERl = as.numeric(str_extract(ALDER, "[:digit:]*(?=_)")),
               ALDERh = as.numeric(str_extract(ALDER, "(?<=_)[:digit:]*")))]
  data <- data[ALDERl == min(ALDERl) & ALDERh == max(ALDERh)]
  data[, ':=' (ALDERl = NULL, ALDERh = NULL)]
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

