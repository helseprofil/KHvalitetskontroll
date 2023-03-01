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
                         profile = c("FHP", "OVP"),
                         geolevel = c("B", "K", "F"),
                         profileyear = NULL,
                         modus = c("KH", "NH")){
  
  # Check arguments
  if(is.null(datotag)) {
    stop("file not selected")
  }
  
  if(!profile %in% c("FHP", "OVP") | length(profile) != 1){
    stop("profile must be either 'FHP' or 'OVP'")
  }
  
  if(!geolevel %in% c("B", "K", "F") | length(geolevel) != 1){
    stop("geolevel must be either 'B', 'K', or 'F'")
  }
  
  if(nchar(profileyear) != 4){
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
  
  # Construct file path to FRISKVIK file, most recent GODKJENT folder
  friskvikpath <- file.path(basepath,
                            paste(PROFILE, GEOLEVEL, sep = "_"),
                            profileyear,
                            "GODKJENT")
  godkjentdir <- max(list.dirs(friskvikpath, full.names = F, recursive = F))
  
  friskvikfile <- list.files(file.path(friskvikpath, godkjentdir),
                             pattern = datotag)
  
  if(length(friskvikfile) == 0){
    stop("FRISKVIK file not found, check arguments (datotag, profile, geolevel, profileyear)")
  } else if(length(friskvikfile) > 1){
    stop("> 1 FRISKVIK files with the same dato tag identified", 
         cat(friskvikfile, sep = "\n"))
  } else {
    friskvikpath <- file.path(friskvikpath, godkjentdir, friskvikfile)
  }
  
  FRISKVIK <- fread(friskvikpath)
  setattr(FRISKVIK, "Filename", basename(friskvikpath))
  cat(paste0("FRISKVIK loaded: ", PROFILE, "_", GEOLEVEL, "/", 
             profileyear, "/GODKJENT/", godkjentdir,"/", basename(friskvikpath)))
  
  # Construct file path to KUBE and load file
  kubepath <- file.path(basepath,
                        MODUS,
                        "DATERT/csv")
  
  kubefile <- list.files(kubepath, 
                         pattern = datotag)
  
  if(length(kubefile) == 0){
    stop("KUBE file not found, check arguments (datotag, modus)")
  } else if(length(kubefile) > 1){
    stop("> 1 KUBE files with the same dato tag identified", 
         cat(kubefile, sep = "\n"))
  } else {
    kubepath <- file.path(kubepath, kubefile)
  }
  
  KUBE <- fread(kubepath)
  setattr(KUBE, "Filename", basename(kubepath))
  cat(paste0("\nKUBE loaded: ", MODUS, "/DATERT/csv/", basename(kubepath)))
  
  # Identify dimension and value columns
  .IdentifyColumns(FRISKVIK, KUBE)
  
  # Filter KUBE to match FRISKVIK strata 
  ETAB <- FRISKVIK[, unique(ETAB)]
  if(!is.na(ETAB)){
  KUBE <- KUBE[eval(parse(text = ETAB))]
  }
  
  filtercols <- str_subset(.commondims, "AAR", negate = TRUE)
  for(i in filtercols){
    KUBE <- KUBE[get(i) %in% FRISKVIK[, unique(get(i))]]
  }
  
  # Save to global env
  KUBE <<- KUBE
  FRISKVIK <<- FRISKVIK

}
