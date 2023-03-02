#' ReadFriskvik
#'
#' @param datotag Datatag used to identify friskvik file and corresponding kube, from DATERT
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
    cat("> 1 unique years in FRISKVIK")
    return(NA)
  } else if(data1[, unique(AAR)] == max(data2[, unique(AAR)])){
    return("Yes")
  } else {
    "No"
  }
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
  
  
  
  
}
