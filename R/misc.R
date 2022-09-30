# Functions to be used in several other files

#' ReadFile
#'
#' @param file File name. Locates file by partial matching. 
#' #' @param bank Either "NH" or "KH", defaults to "KH"
#' @param year Either a 4-digit number corresponding to production year, or "DATERT". Defaults to DATERT
#'
#' @return
#' @export
#'
#' @examples
ReadFile <- function(file = NULL, 
                     bank = "KH", 
                     folder = "DATERT"){
  
  if(!(bank %in% c("KH", "NH"))) {
    stop("`bank` must be either 'KH' or 'NH'")
  }
  
  if(!(str_detect(folder, "[:digit:]{4}") & str_length(folder) == 4| 
       folder == "DATERT")) {
    stop("`folder` must be either 4 digits or 'DATERT'")
  }
  
  if(is.null(file)) {
    stop("file not selected")
  }
  
  BANK <- case_when(bank == "KH" ~ "KOMMUNEHELSA",
                    bank == "NH" ~ "NORGESHELSA")
  
  FOLDER <- case_when(folder == "DATERT" ~ paste0(folder, "/csv"),
                      TRUE ~ paste0(bank, folder, "NESSTAR"))
  
  basepath <- file.path("F:", 
                        "Forskningsprosjekter", 
                        "PDB 2455 - Helseprofiler og til_",
                        "PRODUKSJON", 
                        "PRODUKTER", 
                        "KUBER", 
                        BANK, 
                        FOLDER)
  
  filename <- list.files(basepath, pattern = file)
  
  if(length(filename) == 0){
    stop("File not found, check spelling")
  } else if(length(filename) > 1){
    message("More than 1 file found:")
    stop("Please specify file name to only select one file", 
         cat(filename, sep = "\n"))
  } else {
    filepath <- file.path(basepath, filename)
  }
  
  message(paste0("Loads file: ", BANK, "/", FOLDER, "/", basename(filepath)))
  fread(filepath)
}