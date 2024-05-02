# Functions that are either replaced or no longer used in the project

#' ReadFile
#' 
#' Superseeded by ReadFiles.
#'
#' @param file File name. Locates file by partial matching. 
#' @param modus Either "NH" or "KH", defaults to "KH"
#' @param folder Either a 4-digit number corresponding to profile year, "QC", or "DATERT". Defaults to QC
ReadFile <- function(file = NULL, 
                     modus = "KH", 
                     folder = "QC"){
  
  if(base::isFALSE(modus %in% c("KH", "NH"))) {
    stop("`modus` must be either 'KH' or 'NH'")
  }
  
  if(base::isFALSE(any(grepl("^\\d{4}$", folder), 
                 folder == "DATERT", 
                 folder == "QC"))) {
    stop("`folder` must be either 4 digits, 'QC', or 'DATERT'")
  }
  
  if(is.null(file)) {
    stop("file not selected")
  }
  
  # Select folder NH/KH
  MODUS <- dplyr::case_when(modus == "KH" ~ "KOMMUNEHELSA",
                            modus == "NH" ~ "NORGESHELSA")
  
  # Select subfolder QC, DATERT or NESSTAR
  FOLDER <- dplyr::case_when(folder == "DATERT" ~ paste0(folder, "/csv"),
                             folder == "QC" ~ folder,
                             TRUE ~ paste0(modus, folder, "NESSTAR"))
  
  path <- file.path(
    "O:/Prosjekt/FHP",
    "PRODUKSJON",
    "PRODUKTER",
    "KUBER",
    MODUS,
    FOLDER
  )
  
  filename <- list.files(path, pattern = file)
  
  if(length(filename) == 0){
    stop("File not found, check spelling")
  } else if(length(filename) > 1){
    message("More than 1 file found:")
    stop("Please specify file name to only select one file", 
         cat(filename, sep = "\n"))
  } else {
    filepath <- file.path(path, filename)
  }
  
  outdata <- data.table::fread(filepath)
  
  # Set attributes Filename and Filetype
  data.table::setattr(outdata, "Filename", basename(filepath))
  data.table::setattr(outdata, "Filetype", data.table::fcase(folder == "QC", "QC",
                                                             folder == "DATERT", "ALLVIS",
                                                             folder == paste0(modus, folder, "NESSTAR"), "NESSTAR"))
  cat(paste0("File loaded: ", MODUS, "/", FOLDER, "/", basename(filepath)))
  
  outdata
}

#' .SmallLargeKommune
#' 
#' Superseeded by .updatePopInfo/.getPopInfo
#'
#' Loads current BEFOLK_GK file, and separates out small and large kommune
.SmallLargeKommune <- function(){
  
  basepath <- file.path("O:/Prosjekt/FHP",
                        "PRODUKSJON", 
                        "PRODUKTER", 
                        "KUBER",
                        "KOMMUNEHELSA")
  
  thisyear <- file.path(basepath, paste0("KH", PROFILEYEAR, "NESSTAR"))
  popfile <- list.files(thisyear, pattern = "BEFOLK_GK", full.names = T)
  
  # If no file for current profileyear, use file from last year
  if(length(popfile) == 0){
    cat(paste0("Population file from ", PROFILEYEAR, " does not exist, file from ", PROFILEYEAR - 1, " is used to identify small and large KOMMUNE"))
    lastyear <- file.path(basepath, paste0("KH", PROFILEYEAR - 1, "NESSTAR"))
    popfile <- list.files(lastyear, pattern = "BEFOLK_GK", full.names = T)
  }
  
  # Select the "24aarg" file if present, because this is smaller
  if(length(popfile) > 1){
    popfile <-  grep("24aarg", file, value = T)
  }
  
  # Read file and filter out last year
  pop <- data.table::fread(popfile)
  .IdentifyColumns(pop)
  data.table::setkeyv(pop, .dims1)
  pop <- pop[KJONN == 0 & ALDER == "0_120" & AAR == max(AAR)]
  
  pop[, WEIGHTS := TELLER]
  .popweights <- pop[, .(GEO, WEIGHTS)]
  .allgeos <<- .popweights$GEO
  .allweights <<- .popweights$WEIGHTS
  
  # Export lists of large and small kommune
  .largekommune <<- pop[between(GEO, 99, 9999) & TELLER >= 10000, unique(GEO)]
  .smallkommune <<- pop[between(GEO, 99, 9999) & TELLER < 10000, unique(GEO)]
}

#' Compare new and old value
#' 
#' Prints one table per value column, highlighting the absolute and relative 
#' difference between the new and the old file. 
#' 
#' Produces an R object per value column for differing rows
#' 
#' Superseeded by comparekube
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
  
  dumppath <- file.path("O:/Prosjekt/FHP",
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