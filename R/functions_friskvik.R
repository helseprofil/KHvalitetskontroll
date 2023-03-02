#' ReadFriskvik
#'
#' @param datotag full file name from FRISKVIK/GODKJENT
#' @param geolevel One of "B", "K", or "F"
#' @param profile One of "FHP" or "OVP"
#' @param profileyear 4-digit profileyear
#' @param modus "KH" or "NH", to locate kube file
#' @param friskvikpath can provide full path, defaults to NULL
#' @param kubepath can provide full path, defaults to NULL
#'
#' @return
#' @export
#'
#' @examples
ReadFriskvik <- function(filename = NULL, 
                         profile = NULL,
                         geolevel = NULL,
                         profileyear = NULL,
                         modus = NULL,
                         friskvikpath = NULL,
                         kubepath = NULL){
  
  # Check arguments
  if(is.null(filename)) {
    stop("file not selected")
  }
  
  # Create file paths
  if(is.null(friskvikpath)){
    friskvikpath <- .CreateFriskvikpath(profile = profile,
                                        geolevel = geolevel,
                                        profileyear = profileyear)
  }
  
  if(is.null(kubepath)){
    kubepath <- .CreateKubePath(modus = modus)
  }
  
  
  # Find and load FRISKVIK file
  friskvikfile <- list.files(friskvikpath,
                             pattern = filename)
  
  if(length(friskvikfile) < 1){
    stop("FRISKVIK file not found, check arguments (datotag, profile, geolevel, profileyear)")
  } else if(length(friskvikfile) > 1){
    stop("> 1 FRISKVIK files with the same name identified", 
         cat(friskvikfile, sep = "\n"))
  } else {
    # Generate file path FRISKVIK
    friskvik <- file.path(friskvikpath, friskvikfile)
  }
  
  FRISKVIK <- fread(friskvik)
  setattr(FRISKVIK, "Filename", basename(friskvik))
  cat(paste0("FRISKVIK loaded: ", 
             str_extract(friskvikpath, "(?<=KUBER/).*"), 
             "/", 
             basename(friskvik),
             "\n"))
  
  # Find and load KUBE file
  datotag <- str_extract(friskvikfile, "\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}")
  kubefile <- list.files(kubepath, 
                         pattern = datotag)
  
  if(length(kubefile) < 1){
    stop("corresponding KUBE file not found, check arguments (datotag, modus)")
  } else if(length(kubefile) > 1){
    stop("> 1 KUBE files with the same dato tag identified", 
         cat(kubefile, sep = "\n"))
  } else {
    # Generate file path KUBE
    kube <- file.path(kubepath, kubefile)
  }
  
  KUBE <- fread(kube)
  setattr(KUBE, "Filename", basename(kube))
  cat(paste0("KUBE loaded: ", 
             str_extract(kubepath, "(?<=KUBER/).*"), "/",
             basename(kube),
             "\n"))
  
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
  
  # Standard dimension filtering, hard coded for 2023 as dimensions not included in FRISKVIK
  friskvikname <- .GetKubename(FRISKVIK)
  
  if("INNVKAT" %in% .dims2){
    KUBE <- KUBE[INNVKAT == 0]
  }
  
  if("LANDBAK" %in% .dims2){
    if(friskvikname %in% c("Innvand_0_17", 
                           "INNVAND_barn")){
      KUBE <- KUBE[LANDBAK == 100]
    } else {
      KUBE <- KUBE[LANDBAK == 0]
    }
  }
  
  if("UTDANN" %in% .dims2){
    if(friskvikname %in% c("UTDANNING_NH",
                           "UTDANN_HOY")){
      KUBE <- KUBE[UTDANN == 23]
    } else {
      KUBE <- KUBE[UTDANN == 0]
    }
  }
  
  # Ensure same order
  setkeyv(KUBE, .commondims)
  setkeyv(FRISKVIK, .commondims)
 
  # Save to global env
  KUBE <<- KUBE
  FRISKVIK <<- FRISKVIK
}

CompareFriskvikYear <- function(data1 = FRISKVIK,
                                data2 = KUBE){
  
  kubeyears <- data.table(KUBE = data2[, sort(unique(AAR), decreasing = TRUE)])
  kubeyears[, join := KUBE]
  friskvikyears <- data.table(FRISKVIK = data1[, sort(unique(AAR), decreasing = TRUE)])
  friskvikyears[, join := FRISKVIK]
  
  out <- merge.data.table(friskvikyears, kubeyears, by = "join", all.y = T)[sort(join, decreasing = TRUE)]
  out[,join := NULL]
  
  out[]
}

FriskvikLastYear <- function(data1 = FRISKVIK,
                             data2 = KUBE){
  
  if(length(data1[, unique(AAR)]) > 1){
    out <- "> 1 year in FRISKVIK"
  } else if(data1[, unique(AAR)] == max(data2[, unique(AAR)])){
    out <- "Yes"
  } else {
    out <- "No"
  }
  
  out
}

CompareFriskvikPrikk <- function(data1 = FRISKVIK,
                                 data2 = KUBE){
  
  # Only include years included in FRISKVIK
  data2 <- data2[AAR %in% data1[, unique(AAR)]]
  
  # Compare values censored in FRISKVIK with values censored in KUBE
  if(all.equal(is.na(data1$MEIS), data2[, SPVFLAGG > 0])){
      "Yes"
    } else {
      "No"
    }
}

CompareFriskvikVal <- function(data1 = FRISKVIK,
                               data2 = KUBE){
  
  # Only include years included in FRISKVIK
  data2 <- data2[AAR %in% data1[, unique(AAR)]]
  
  # Find value columns in KUBE
  .IdentifyColumns(data1, data2)
  kubevals <- str_subset(.vals2, "RATE.n|SPVFLAGG", negate = TRUE)
  # Compare FRISKVIK$MEIS to all value columns to find match
  matches <- character()
  different <- character()
  
  # Map over value columns in KUBE, find the column(s) matching FRISKVIK$MEIS
  for(i in kubevals){
    if(isTRUE(all.equal(data1$MEIS, data2[, get(i)]))){
      matches <- c(matches, i)
    } else {
      different <- c(different, i)
    }
  }
  
  matches <- (str_c(matches, collapse = ", "))
  different <- (str_c(different, collapse = ", "))
  
  list(matches = matches, different = different)
  
}


#' Helper function to create path to most recent FRISKVIK/GODKJENT-folder
#'
#' @param profile 
#' @param geolevel 
#' @param profileyear 
#'
#' @return
#' @export
#'
#' @examples
.CreateFriskvikpath <- function(profile,
                                geolevel,
                                profileyear){
  
  if(!profile %in% c("FHP", "OVP") | length(profile) != 1){
    stop("profile must be either 'FHP' or 'OVP'")
  }
  
  if(!geolevel %in% c("B", "K", "F") | length(geolevel) != 1){
    stop("geolevel must be either 'B', 'K', or 'F'")
  }
  
  if(nchar(profileyear) != 4){
    stop("friskvikyear must be a 4 digit number")
  }
  
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
  
  friskvikpath <- file.path(basepath,
                            paste(PROFILE, GEOLEVEL, sep = "_"),
                            profileyear,
                            "GODKJENT")
  
  godkjentdir <- max(list.dirs(friskvikpath, full.names = F, recursive = F))
  
  friskvikpath <- file.path(friskvikpath, godkjentdir)
  
  friskvikpath
}

#' Helper function to create path to KUBE/DATERT/csv
#'
#' @param modus 
#'
#' @return
#' @export
#'
#' @examples
.CreateKubePath <- function(modus){
  
  if(!(modus %in% c("KH", "NH"))) {
    stop("`modus` must be either 'KH' or 'NH'")
  }
 
  basepath <- file.path("F:", 
                        "Forskningsprosjekter", 
                        "PDB 2455 - Helseprofiler og til_",
                        "PRODUKSJON", 
                        "PRODUKTER", 
                        "KUBER")
  
  MODUS <- if(modus == "KH"){
    "KOMMUNEHELSA/DATERT/csv/" 
  } else if(modus == "NH"){
    "NORGESHELSA/DATERT/csv/"
  }
  
  kubepath <- file.path(basepath,
                        MODUS)
  
  kubepath
}

#' CheckFriskvik
#'
#' @param profile "FHP" or "OVP"
#' @param geolevel "B", "K", or "F"
#' @param profileyear 4-digit year
#' @param modus "KH" or "NH"
#'
#' @return
#' @export
#'
#' @examples
CheckFriskvik <- function(profile = c("FHP", "OVP"),
                          geolevel = c("B", "K", "F"),
                          profileyear = NULL,
                          modus = c("KH", "NH")){
  
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
  
  # Generate friskvikpath and kubepath, and list of all datatags in the most recent FRISKVIK/GODKJENT-folder
  
  friskvikpath <- .CreateFriskvikpath(profile = profile, 
                                      geolevel = geolevel, 
                                      profileyear = profileyear)
  
  kubepath <- .CreateKubePath(modus = modus)
  
  # Extract all datotags in FRISKVIK/GODKJENT
  
  files <- list.files(friskvikpath, pattern = ".csv")
  datotags <- str_extract(files, "\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}")
  
  # Load files, generate 1-line output
  canread <- numeric()
  for(i in files){
    tryload <- try(ReadFriskvik(filename = i,
                                friskvikpath = friskvikpath,
                                kubepath = kubepath), 
                   silent = T)
    
    if("try-error" %in% class(tryload)){
      canread <- c(canread, 0)
    } else {
      canread <- c(canread, 1)
    }
    rm(tryload)
    gc()
  }
  