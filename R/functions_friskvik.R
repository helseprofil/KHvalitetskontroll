#' ReadFriskvik
#'
#' @param datotag Datatag used to identify friskvik file and corresponding kube
#' @param geolevel One of "B", "K", or "F"
#' @param profile One of "FHP" or "OVP"
#' @param friskvikyear Year to identify friskvik folder, defaults to 2023
#' @param modus "KH" or "NH"
#'
#' @return
#' @export
#'
#' @examples
ReadFriskvik <- function(datotag = NULL, 
                         geolevel = c("B", "K", "F"),
                         profile = c("FHP", "OVP"),
                         friskvikyear = 2023,
                         modus = "KH"){
  
  # Check arguments
  if(is.null(datotag)) {
    stop("file not selected")
  }
  
  if(!geolevel %in% c("B", "K", "F") | length(geolevel) != 1){
    stop("geolevel must be either 'B', 'K', or 'F'")
  }
  
  if(!profile %in% c("FHP",) | length(profile) != 1){
    stop("profile must be either 'FHP' or 'OVP'")
  }
  
  if(nchar(friskvikyear) != 4){
    stop("friskvikyear must be a 4 digit number")
  }
  
  if(!(modus %in% c("KH", "NH"))) {
    stop("`modus` must be either 'KH' or 'NH'")
  }
  
  # Construct file path parts
  basepath <- file.path("F:", 
                        "Forskningsprosjekter", 
                        "PDB 2455 - Helseprofiler og til_",
                        "PRODUKSJON", 
                        "PRODUKTER", 
                        "KUBER")
  
  GEOLEVEL <- if(geolevel == "B"){
    "BYDEL"
  } else if(geolevel == "F"){
    "FYLKE"
  } else if(geolevel == "K"){
    "KOMM"
  }
  
  PROFILE <- if(profile == "FHP"){
    "FRISKVIK"
  } else if(profile == "OVP"){
    "OVP"
  }
  
  MODUS <- if(modus == "KH"){
    "KOMMUNEHELSA" 
  } else if(modus == "NH"){
    "NORGESHELSA"
  }
  
  # Construct file path to NESSTAR kube
  nesstarpath <- file.path(basepath,
                           MODUS,
                           "DATERT/csv")
  
  nesstarfile <- list.files(nesstarpath, pattern = datotag)
  
  # construct file path to FRISKVIK file
  friskvikpath <- file.path(basepath,
                            paste(PROFILE, GEOLEVEL, sep = "_"),
                            friskvikyear,
                            "GODKJENT")
  

  
  
  
  if(length(filename) == 0){
    stop("File not found, check spelling")
  } else if(length(filename) > 1){
    message("More than 1 file found:")
    stop("Please specify file name to only select one file", 
         cat(filename, sep = "\n"))
  } else {
    filepath <- file.path(basepath, filename)
  }
  
  outdata <- fread(filepath)
  setattr(outdata, "Filename", basename(filepath))
  cat(paste0("File loaded: ", MODUS, "/", FOLDER, "/", basename(filepath)))
  
  outdata
}