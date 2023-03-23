#' ReadFriskvik
#'
#' @param filename 
#' @param profile One of "FHP" or "OVP"
#' @param geolevel One of "B", "K", or "F"
#' @param profileyear 4-digit profileyear
#' @param friskvikpath can provide full path, defaults to NULL
#' @param kubefile Exact path to kube, starting with "KOMMUNEHELSA/" or "NORGESHELSA/"
#'
#' @return
#' @export
#'
#' @examples
ReadFriskvik <- function(filename = NULL, 
                         profile = NULL,
                         geolevel = NULL,
                         profileyear = NULL,
                         friskvikpath = NULL,
                         kubefile = NULL){
  
  # Check arguments
  if(is.null(filename)) {
    stop("file not selected")
  }
  
  if(is.null(friskvikpath)){
    if(is.null(profile)){
      stop("profile must be provided")
    }
    if(!profile %in% c("FHP", "OVP") | length(profile) != 1){
      stop("profile must be either 'FHP' or 'OVP'")
    }
    
    if(is.null(geolevel)){
      stop("geolevel must be provided")
    } 
    if(!geolevel %in% c("B", "K", "F") | length(geolevel) != 1){
      stop("geolevel must be either 'B', 'K', or 'F'")
    }
    
    if(is.null(profileyear)){
      stop("profileyear must be provided")
    } 
    if(nchar(profileyear) != 4){
      stop("friskvikyear must be a 4 digit number")
    }
  }
  
  # Create file paths
  if(is.null(friskvikpath)){
    friskvikpath <- .CreateFriskvikpath(profile = profile,
                                        geolevel = geolevel,
                                        profileyear = profileyear)
  }
  
  basepath <- file.path("F:", 
                        "Forskningsprosjekter", 
                        "PDB 2455 - Helseprofiler og til_",
                        "PRODUKSJON", 
                        "PRODUKTER", 
                        "KUBER")
  
  kubepath_kh <- file.path(basepath,
                           "KOMMUNEHELSA",
                           "DATERT",
                           "csv")
  
  kubepath_nh <- file.path(basepath,
                           "NORGESHELSA",
                           "DATERT",
                           "csv")
  
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
  # Search in kubepath_kh, and if not found search kubepath_nh
  if(is.null(kubefile)){
    datotag <- str_extract(friskvikfile, "\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}")
    
    kube <- list.files(kubepath_kh, 
                       pattern = datotag, 
                       full.names = T)
    
    if(length(kube) == 0){
      kube <- list.files(kubepath_nh, 
                         pattern = datotag,
                         full.names = T)
    }
    
  } else if(!is.null(kubefile)){
    kube <- file.path(basepath, kubefile)
  }
  
  if(length(kube) < 1){
    stop("corresponding KUBE file not found, check arguments")
  } else if(length(kube) > 1){
    stop("> 1 KUBE files with the same dato tag identified", 
         cat(kube, sep = "\n"))
  } x

    

  
  KUBE <- fread(kube)
  setattr(KUBE, "Filename", basename(kube))
  cat(paste0("KUBE loaded: ", 
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
  if(isTRUE(all.equal(is.na(data1$MEIS), data2[, SPVFLAGG > 0]))){
      "Yes"
    } else {
      geodiff <- data1[is.na(data1$MEIS) != data2[, SPVFLAGG > 0], GEO]
      paste("NO!! Diff for GEO:", str_c(geodiff, collapse = ", "))
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
  
  if(length(matches) == 0){
    matches <- "!!NO MATCH!!"
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
 
  GEOLEVEL <<- if(geolevel == "B"){
    "BYDEL"
  } else if(geolevel == "F"){
    "FYLKE"
  } else if(geolevel == "K"){
    "KOMM"
  }
  
  PROFILE <<- if(profile == "FHP"){
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

.UniqueLevel <- function(data = KUBE, 
                        dim = NULL){
  
  if(dim %in% names(data)){
    paste(data[, unique(get(dim))], collapse = ", ")
  } else {
    NA_character_
  }
}

#' CheckFriskvik
#'
#' @param profile "FHP" or "OVP"
#' @param geolevel "B", "K", or "F"
#' @param profileyear 4-digit year
#'
#' @return
#' @export
#'
#' @examples
CheckFriskvik <- function(profile = c("FHP", "OVP"),
                          geolevel = c("B", "K", "F"),
                          profileyear = NULL,
                          test = FALSE){
  
  if(!profile %in% c("FHP", "OVP") | length(profile) != 1){
    stop("profile must be either 'FHP' or 'OVP'")
  }
  
  if(!geolevel %in% c("B", "K", "F") | length(geolevel) != 1){
    stop("geolevel must be either 'B', 'K', or 'F'")
  }
  
  if(nchar(profileyear) != 4){
    stop("friskvikyear must be a 4 digit number")
  }
  
  # Generate friskvikpath and kubepath, and list of all datatags in the most recent FRISKVIK/GODKJENT-folder
  
  friskvikpath <- .CreateFriskvikpath(profile = profile, 
                                      geolevel = geolevel, 
                                      profileyear = profileyear)
  
  # Extract all datotags in FRISKVIK/GODKJENT
  friskvikfiles <- list.files(friskvikpath, pattern = ".csv")
  
  # Create savepath and report name
  savepath <- file.path("F:", 
                        "Forskningsprosjekter", 
                        "PDB 2455 - Helseprofiler og til_",
                        "PRODUKSJON", 
                        "VALIDERING", 
                        "FRISKVIK_vs_KUBE")
  
  # Add profileyear-folder if not existing
  if(test){
    savedir <- file.path(savepath, "testmappe")
  } else {
    savedir <- file.path(savepath, profileyear)
  }
  
  if(!dir.exists(savedir)){
    dir.create(savedir)
  }
  
  savename <- paste0(file.path(savedir,
                               paste(PROFILE, GEOLEVEL, format(Sys.time(), "%Y-%m-%d-%H-%M"), sep = "_")),
                     ".csv")
  
  # Loop trouch friskvikfiles, generate 1-line output per file
  output <- map_df(friskvikfiles[1:3], \(file)  {
    # Try to load files
    tryload <- try(ReadFriskvik(filename = file,
                                friskvikpath = friskvikpath), 
                   silent = T)
    
    # Define output columns
    Friskvik_name <- file
    Kube_name <- NA
    Last_year <- NA
    Identical_prikk <- NA
    Matching_kubecol <- NA
    Different_kubecol <- NA
    ETAB <- NA
    KUBE_KJONN <- NA
    KUBE_ALDER <- NA
    KUBE_UTDANN <- NA
    KUBE_INNVKAT <- NA
    KUBE_LANDBAK <- NA
      
    # If both files are read without error, replace output columns
    if(isFALSE("try-error" %in% class(tryload))){
      Kube_name <- attributes(KUBE)$Filename
      Last_year <- FriskvikLastYear()
      Identical_prikk <- CompareFriskvikPrikk()
      
      compvals <- CompareFriskvikVal()
      
      Matching_kubecol <- compvals$matches
      Different_kubecol <- compvals$different
      
      ETAB <- FRISKVIK[, unique(ETAB)]
      KUBE_KJONN <- .UniqueLevel(KUBE, "KJONN")
      KUBE_ALDER <- .UniqueLevel(KUBE, "ALDER")
      KUBE_UTDANN <- .UniqueLevel(KUBE, "UTDANN")
      KUBE_INNVKAT <- .UniqueLevel(KUBE, "INNVKAT")
      KUBE_LANDBAK <- .UniqueLevel(KUBE, "LANDBAK")
    }
    
    rm(tryload)
    
    data.table(Friskvik = Friskvik_name,
               Kube = Kube_name,
               Last_year = Last_year,
               Identical_prikk = Identical_prikk,
               Matching_kubecol = Matching_kubecol,
               Different_kubecol = Different_kubecol,
               ETAB = FRISKVIK_ETAB,
               KUBE_KJONN = KUBE_KJONN,
               KUBE_ALDER = KUBE_ALDER,
               KUBE_UTDANN = KUBE_UTDANN,
               KUBE_INNVKAT = KUBE_INNVKAT,
               KUBE_LANDBAK = KUBE_LANDBAK)
    }
  )
  
  cat("\nOutput generated")
  
  # Write result
  fwrite(output,
         file = savename,
         sep = ";")
  
  cat(paste("\nOutput written to", savename))
}
    