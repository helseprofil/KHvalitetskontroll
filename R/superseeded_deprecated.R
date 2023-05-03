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
  
  if(isFALSE(modus %in% c("KH", "NH"))) {
    stop("`modus` must be either 'KH' or 'NH'")
  }
  
  if(isFALSE(any(grepl("^\\d{4}$", folder), 
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
    "F:",
    "Forskningsprosjekter",
    "PDB 2455 - Helseprofiler og til_",
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