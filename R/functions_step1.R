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
  
  msgnew <- case_when(length(new) == 0 ~ "\nNo new columns.",
                      TRUE ~ paste0("\nNew columns found: ", str_c(new, collapse = ", ")))
  
  msgexp <- case_when(length(exp) == 0 ~ "\nNo expired columns.",
                      TRUE ~ paste0("\nExpired columns found: ", str_c(exp, collapse = ", ")))
  
  cat(msgnew)
  cat(msgexp)
  
}

#' CompareDim
#' 
#' Detects all existing levels of selected dimension in the new KUBE, and compares towards a previous KUBE. The total number of levels, all new or expired levels, and a list of all existing levels in the new KUBE are listed in the output. 
#'
#' @param data1 new KUBE, defaults to dfnew
#' @param data2 old KUBE, defaults do dfold
#' @param dim Dimension you want to check
#'
#' @return a list. If no new or expired columns are detected, these elements will return "none". 
#' @export
#'
#' @examples
#' CompareDim(dim = GEO)  
#' CompareDim(dim = AAR)
#' CompareDim(dim = KJONN)
#' CompareDim(dim = ALDER)
#' CompareDim(dim = YTELSE)
CompareDim <- function(data1 = dfnew, 
                       data2 = dfold, 
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

#' CompareDims
#' 
#' Wrapper around `CompareDim`, to print results for several dims simultaneously
#'
#' @param dims Character vector of dimensions to compare
#'
#' @return table with 4 columns: Dimension, N levels, New levels, Expired levels, with one row per input dimension
#' @export
#'
#' @examples
CompareDims <- function(dims = c(STANDARDdims, EXTRAdims)){
  map_df(dims, ~CompareDim(dim = .x))
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
  
  if(nrow(filtered) == 0) {
    cat("\nNo values < limit")
  } else {
    cat(paste0("\nN values > limit: ", nrow(filtered)))
    output <- filtered %>% 
      select(all_of(standarddims), all_of(extradims), all_of(val), everything()) 
    DT::datatable(output, rownames = F)
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
