# Functions to be used in several other files

#' ReadFiles
#' 
#' Reads in two files to be used for quality control. The new file `dfnew` and the old file `dfold`. 
#' 
#' The data objects gains two attributes: `Filename`and `Filetype`. 
#' 
#' dfold, folderold, and modusold can be NULL, to only read in one file. 
#'
#' @param dfnew file name including full date tag
#' @param foldernew QC, DATERT or a 4-digit number indicating NESSTAR-folder
#' @param modusnew KH or NH
#' @param dfold file name including full date tag
#' @param folderold QC, DATERT or a 4-digit number indicating NESSTAR-folder
#' @param modusold KH or NH
#' @param recodeold TRUE/FALSE, should GEO codes in old file be recoded to current GEO
#' @param recodenew TRUE/FALSE, should GEO codes in new file be recoded to current GEO
#'
#' @return
#' @export
#'
#' @examples
ReadFiles <- function(dfnew = NULL,
                      foldernew = "QC",
                      modusnew = "KH",
                      dfold = NULL,
                      folderold = NULL,
                      modusold = NULL,
                      recodeold = FALSE,
                      recodenew = FALSE){
  
  # Check arguments new file
  if(!stringr::str_detect(dfnew, ".*_\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}")) {
    stop("dfnew must be provided in the format FILENAME_YYYY-MM-DD-hh-mm")
  }
  
  if(!modusnew %in% c("KH", "NH")) {
    stop("`modusnew` must be either 'KH' or 'NH'")
  }
  
  # Check arguments old file
  if(!any(stringr::str_detect(dfold, ".*_\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}"),
          is.null(dfold))){
    stop("dfold must be provided in the format FILENAME_YYYY-MM-DD-hh-mm, or NULL")
  }
  
  if(!is.null(dfold)){
    
    if(!any(modusold %in% c("KH", "NH"))) {
      stop("When dfold is specified, `modusold` must be either 'KH' or 'NH'")
    }
    
  }
  
  # Check if file(s) exists, if not stop before reading files
  pathnew <- .findpath(modusnew, foldernew)
  filenew <- list.files(pathnew, pattern = paste0("^",dfnew))
  filepathnew <- file.path(pathnew, filenew)
  
  if(!(length(filepathnew) == 1 && file.exists(filepathnew))){
    cat("\nTried to load:", ifelse(length(filepathnew) == 0, 
                                   "No file found, check name, did you forget 'QC_' or defined the wrong folder or modus?",
                                   filepathnew))
    stop("dfnew not found. Check arguments 'dfnew', 'foldernew', and 'modusnew")
  }
  
  if(!is.null(dfold)){
    
    pathold <- .findpath(modusold, folderold)
    fileold <- list.files(pathold, pattern = dfold)
    filepathold <- file.path(pathold, fileold)
    
    if(!(length(filepathold) == 1 && file.exists(filepathold))){
      cat("\nTried to load:", ifelse(length(filepathold) == 0, 
                                     "No file found, check name, did you forget 'QC_' or defined the wrong folder or modus?",
                                     filepathold))
      stop("dfold not found. Check arguments 'dfold', 'folderold', and 'modusold")
    }
  }
  
  # Read dfnew and store to global env
  outdatanew <- .readfile(filepathnew, foldernew)
  cat(paste0("New file (dfnew) loaded: ", str_extract(filepathnew, "(?<=PRODUKTER/).*")))
  if(attr(outdatanew, "colnameinfo")$diff == "yes"){
    .listcolrename(outdatanew, "dfnew")
  }
  cat("\ndfnew columns: ", names(outdatanew), "\n")
  
  if(isTRUE(recodenew)){
    outdatanew <- .doGeoRecode(outdatanew)
  }
  
  dfnew <<- outdatanew
  
  for(i in c("TELLER", "NEVNER", "sumTELLER", "sumNEVNER")){
    if(i %in% names(outdatanew)){
      cat("\nNB! New file contains ", i, ". Is this ok for ALLVIS?", sep = "")
    }
  }
  
  # If provided, read dfold and store to global env.Recode GEO if required
  if(!is.null(dfold)){
    outdataold <- .readfile(filepathold, folderold)
    cat(paste0("\n\nOld file (dfold) loaded: ", str_extract(filepathold, "(?<=PRODUKTER/).*")))
    if(attr(outdataold, "colnameinfo")$diff == "yes"){
      .listcolrename(outdataold, "dfold")
    }
    cat("\ndfold columns: ", names(outdataold), "\n")
      
    if(isTRUE(recodeold)){
      outdataold <- .doGeoRecode(outdataold)
    }
    
    dfold <<- outdataold
  }
}

#' .findpath
#' 
#' Helper function to create correct filepath in ReadFiles
#'
#' @param modus 
#' @param folder 
.findpath <- function(modus, folder){
  
  folder <- as.character(folder)
  
  basepath <- file.path("F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/PRODUKTER/KUBER")
  file.path(basepath, 
            data.table::fcase(modus == "KH", "KOMMUNEHELSA",
                              modus == "NH", "NORGESHELSA"),
            data.table::fcase(folder == "DATERT", paste0(folder, "/csv"),
                              folder == "QC", folder,
                              stringr::str_detect(folder, "^\\d{4}$"), paste0(modus, as.character(folder), "NESSTAR"),
                              default = folder))
}

#' .readfile
#'
#' Helper function to read file and set attributes
#' @param path 
#' @param folder 
.readfile <- function(path, folder){
  
  outdata <- data.table::fread(path)
  data.table::setattr(outdata, "Filename", basename(path))
  data.table::setattr(outdata, "Filetype", data.table::fcase(folder == "QC", "QC",
                                                             folder == "DATERT", "ALLVIS",
                                                             stringr::str_detect(folder, "\\d{4}"), "NESSTAR"))
  
  # Rename default columns. Set attribute colnameinfo to document colname changes
  .orgnames <- names(copy(outdata))
  
  .oldcols = c("antall", "Crude", "Adjusted", "sumteller", "sumnevner", "smr", "FLx")
  .newcols = c("TELLER", "RATE", "MEIS", "sumTELLER", "sumNEVNER", "SMR", "MEIS")
  
    # Due to encoding problems in R >= 4.2, only add 'utdanningsnivå' for R < 4.2
    if(as.numeric(R.version$minor) < 2){
      .oldcols = c(.oldcols, "utdanningsnivå")
      .newcols = c(.newcols, "UTDANN")
    }
  
  setnames(outdata, 
           old = .oldcols,
           new = .newcols,
           skip_absent = TRUE)
  .newnames <- names(outdata)
  
  .diff <- data.table::fcase(base::any(.orgnames != .newnames), "yes", default = "no")
  
  data.table::setattr(outdata, "colnameinfo", list(orgnames = .orgnames,
                                                   newnames = .newnames,
                                                   diff = .diff))
  
  outdata
}

#' .listcolrename
#' Helper function to list column names that are changed while reading file
#' @param data 
#' @param prefix 
.listcolrename <- function(data, prefix){
  orgcols <- attr(data, "colnameinfo")$orgnames
  newcols <- attr(data, "colnameinfo")$newnames
  namechange <- data.table::data.table(orgcols, newcols)[orgcols != newcols]
  outmsg <- namechange[, change := paste0(orgcols, " ==> ", newcols)][, change]
  cat("  ", prefix, " columns automatically renamed:", stringr::str_c(paste0("\n    * ", outmsg), collapse = ""), sep = "")
}

#' RenameColumns
#' 
#' wrapper around setnames, also printing a summary of columns manually renamed
#'
#' @param data 
#' @param old 
#' @param new 
RenameColumns <- function(data,
                          old = NULL,
                          new = NULL){
  
  if(base::isTRUE(base::any(is.null(old),
                             old == "",
                             is.null(new),
                             new == ""))){
    return(invisible(NULL))
  }
  
  if(length(old) != length(new)){
    stop(paste("The number of names provided to 'old' and 'new' must be equal.\n",
               length(old), "names provided to 'old', and",
               length(new), "names provided to 'new.'"))
  }
    
  orgcols <- names(copy(data))
  setnames(data, old = old, new = new, skip_absent = TRUE)
  newcols <- names(data)
  
  namechange <- data.table::data.table(orgcols, newcols)[orgcols != newcols]
  outmsg <- namechange[, change := paste0(orgcols, " ==> ", newcols)][, change]
  cat("\n", deparse(substitute(data)), " columns manually renamed:", stringr::str_c(paste0("\n* ", outmsg), collapse = ""), sep = "")
}

.allcombs <- function(data,
                      cols){
  d <- do.call(CJ, base::lapply(cols, function(x) unique(data[[x]])))
  d <- stats::setNames(d, cols)
  d
}

#' print_dim
#' 
#' Wrapper around `stringr::str_c()` to print grouping variables
print_dim <- function(...){
  
  if(is.null(...)){
    "(no grouping variables)"
    } else {
  stringr::str_c(..., collapse = ", ")
    }
}


#' Finds name of cube from filepath
#'
.GetKubename <- function(df){
  filename <- attributes(df)$Filename
  str_replace_all(filename, c("^QC_" = "", 
                              "_\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}\\.csv$" = ""))
}

#' CreateFolders
#' 
#' Create Folder structure according to profile year and new data file
#'
#' @param profileyear 
#' @param kubename 
#'
#' @return
#' @export
#'
#' @examples
.CreateFolders <- function(profileyear,
                           kubename){
  
  basepath <- file.path("F:", 
                        "Forskningsprosjekter", 
                        "PDB 2455 - Helseprofiler og til_",
                        "PRODUKSJON", 
                        "VALIDERING", 
                        "NESSTAR_KUBER")
  
  profileyeardir <- file.path(basepath, profileyear)
  kvalkontrolldir <- file.path(profileyeardir, "KVALITETSKONTROLL")
  kubedir <- file.path(kvalkontrolldir, kubename)
  filedumpdir <- file.path(kubedir, "FILDUMPER")
  plotdir <- file.path(kubedir, "PLOTT")
  bpdir <- file.path(plotdir, "BP")
  bpcdir <- file.path(plotdir, "BPc")
  tsdir <- file.path(plotdir, "TS")
  tscdir <- file.path(plotdir, "TSc")
  tldir <- file.path(plotdir, "TL")
  arkivdir <- file.path(kubedir, "ARKIV")
  
  folders <- c(profileyeardir, 
               kvalkontrolldir, 
               kubedir, 
               filedumpdir, 
               plotdir, 
               bpdir, 
               bpcdir, 
               tsdir, 
               tscdir,
               tldir, 
               arkivdir)
  
  for(i in folders){
    if(!dir.exists(i)){dir.create(i)}
  }
}

#' Save HTML-report
#'
#' @param profileyear 
#' @param inputfile "Kvalitetskontroll_del1" or "Kvalitetskontroll_del2"
#' @param shortname if true, drops kube name from output file name
#' @param savename 
SaveReport <- function(profileyear = PROFILEYEAR,
                       inputfile = NULL,
                       shortname = FALSE,
                       savename = NULL){
  
  if(is.null(inputfile)){
    stop("Inputfile missing, must be 'Kvalitetskontroll_del1.Rmd' or '_del2.Rmd'")
  }
  
  # Extract kubename
  kubename <- .GetKubename(dfnew)
  
  # Create folder structure, if not existing
  .CreateFolders(profileyear = profileyear,
                 kubename = kubename)
  
  # define path to save file
  filepath <- file.path("F:", 
                        "Forskningsprosjekter", 
                        "PDB 2455 - Helseprofiler og til_",
                        "PRODUKSJON", 
                        "VALIDERING", 
                        "NESSTAR_KUBER",
                        profileyear,
                        "KVALITETSKONTROLL",
                        kubename)
  

  # Create file name
  if(!is.null(savename)){
    filename <- savename
  } else {
    filename <- paste0(stringr::str_replace_all(attributes(dfnew)$Filename, c("^QC_" = "", 
                                                                              ".csv" = "")),
                       "_QC",
                       stringr::str_replace_all(inputfile, c("^Kvalitetskontroll" = "",
                                                             ".Rmd$" = "")))
  }
  
  # Remove kube name from file name if too long
  if(shortname){
    filename <- stringr::str_extract(filename, "\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}.*")
  }
  
  # Save report
  rmarkdown::render(input = inputfile,
                    output_file = filename,
                    output_dir = filepath)
}

#' IdentifyColumns
#' 
#' Helper function to identify dimensions and value columns.
#'
#' @param data1 One data frame must be provided
#' @param data2 Optional second data
#'
#' @return Hidden variable lists
#' @export
#'
#' @examples
.IdentifyColumns <- function(data1 = NULL,
                             data2 = NULL){
  
  if(is.null(data1)){
    stop("data1 not provided in .IdentifyColumns()")
  }
  
  if(!exists(".ALL_DIMENSIONS")) {
    source("https://raw.githubusercontent.com/helseprofil/misc/main/alldimensions.R")
    .ALL_DIMENSIONS <- ALL_DIMENSIONS
  }
  
  .dims1 <<- names(data1)[names(data1) %in% .ALL_DIMENSIONS]
  .vals1 <<- stringr::str_subset(names(data1), stringr::str_c("\\b",.dims1, "\\b", collapse = "|"), negate = T)
  
  # Create objects relevant for data2
  .dims2 <<- NULL
  .vals2 <<- NULL
  .commondims <<- NULL
  .newdims <<- NULL
  .expdims <<- NULL
  .commonvals <<- NULL
  .newvals <<- NULL
  .expvals <<- NULL
  .commoncols <<- NULL
  
  # If second data is provided, replace objects above
  if(!is.null(data2)){
    .dims2 <<- names(data2)[names(data2) %in% .ALL_DIMENSIONS]
    .vals2 <<- stringr::str_subset(names(data2), stringr::str_c(.dims2, collapse = "|"), negate = T)
    .commondims <<- .dims1[.dims1 %in% .dims2]
    .newdims <<- stringr::str_subset(.dims1, stringr::str_c("\\b", .dims2, "\\b", collapse = "|"), negate = T)
    .expdims <<- stringr::str_subset(.dims2, stringr::str_c("\\b", .dims1, "\\b", collapse = "|"), negate = T)
    .commonvals <<- .vals1[.vals1 %in% .vals2]
    .newvals <<- stringr::str_subset(.vals1, stringr::str_c("\\b", .vals2, "\\b", collapse = "|"), negate = T)
    .expvals <<- stringr::str_subset(.vals2, stringr::str_c("\\b", .vals1, "\\b", collapse = "|"), negate = T)
    .commoncols <<- c(.commondims, .commonvals)
  }
}

#' .doGeoRecode
#' 
#' Recode GEO according to georecode.csv
#'
#' @param data 
#' @param tab 
.doGeoRecode <- function(data){
  
  if(!exists(".georecode")){
    .georecode <- .readGeoRecode()
  }
  
  # Print message describing recodings
  outdataname <- data.table::fcase(deparse(substitute(data)) == "outdatanew", "dfnew",
                                   deparse(substitute(data)) == "outdataold", "dfold",
                                   default = deparse(substitute(data)))
  recodings <- (.georecode[old %in% data$GEO])

  if(nrow(recodings) > 0){
    cat(paste0("\n", nrow(recodings), " GEO-codes in ", outdataname, " were recoded to current GEO-codes\n"))
      for(i in (1:nrow(recodings))){
        cat(paste0(recodings[i, old], " -> ", recodings[i, current], "\n"))
      }
    assign(paste0("recodings_", outdataname), recodings, envir = .GlobalEnv)
  } else {
    cat(paste0("\n-No GEO-codes in ", outdataname, " were recoded.\n"))
  }
  d <- copy(data)
  out <- collapse::join(d, .georecode, on = c("GEO" = "old"), how = "left", verbose = 0)
  out[!is.na(current), GEO := current][]
  out[, current := NULL]
}

#' Helper function to connect to KHelsa ACCESS database
#'
.ConnectKHelsa <- function(){
  RODBC::odbcConnectAccess2007("F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/STYRING/KHELSA.mdb")
}

#' Helper function to connect to geo-koder ACCESS database
#'
.ConnectGeokoder <- function(){
  RODBC::odbcConnectAccess2007("F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/STYRING/raw-khelse/geo-koder.accdb")
}

#' .usebranch
#' 
#' Function to use specific branch for testing
.usebranch <- function(branch){
  rm(list = lsf.str(all.names = T))
  source(paste0("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/", branch, "/R/misc.R"))
  source(paste0("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/", branch, "/R/functions_step1.R"))
  source(paste0("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/", branch, "/R/functions_step2.R"))
  source(paste0("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/", branch, "/R/functions_plots.R"))
  source(paste0("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/", branch, "/R/functions_friskvik.R"))
  source(paste0("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/", branch, "/R/functions_interactive.R"))
  source(paste0("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/", branch, "/R/globals.R"))
}

#' .updatePopInfo
#' 
#' Generates popinfo.csv file containing weights and GEOniv including small/large kommune and Helseregion
#'
#' @param popfile complete file name of BEFOLK_GK file
#' @param year referring to NESSTAR-folder
.updatePopInfo <- function(popfile, year){
  
  # Read file
  basepath <- .findpath("KH", year)
  file <- list.files(basepath, pattern = popfile)
  pop <- data.table::fread(file.path(basepath, file))
  # Format file
  pop <- pop[KJONN == 0 & ALDER == "0_120" & AAR == max(AAR), .(GEO, TELLER)]
  data.table::setnames(pop, "TELLER", "WEIGHTS")
  pop[, GEOniv := data.table::fcase(GEO == 0, "L",
                                    GEO <= 99, "F",
                                    GEO < 10000 & WEIGHTS >= 10000, "K",
                                    GEO < 10000 & WEIGHTS < 10000, "k",
                                    GEO >= 10000, "B")]
  # Add helseregion GEO-codes with WEIGHTS = 0
  hreg <- data.table(GEO = 81:84,
                     WEIGHTS = 0,
                     GEOniv = "H")
  pop <- data.table::rbindlist(list(pop, hreg))
  # Save file
  data.table::fwrite(pop, "./data/popinfo.csv", sep = ";")
}

#' .readPopInfo
#' 
#' Read popinfo.csv from github
.readPopInfo <- function(){
  file <- paste0("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/data/popinfo.csv")
  d <- data.table::fread(file)
  d[, GEOniv := factor(GEOniv, levels = c("H", "L", "F", "K", "k", "B"))]
  d[]
}

#' .updateGeoRecode
#' 
#' Reads geo-koder database and generate a correspondance table to recode GEO to current year
#' Write file to data/georecode.csv
#'
#' @param year valid geo year
.updateGeoRecode <- function(year){
  
  .DB <- .ConnectGeokoder()
  
  tab <- data.table::rbindlist(list(setDT(sqlQuery(.DB, paste0("SELECT oldCode, currentCode FROM kommune", year))),
                                          setDT(sqlQuery(.DB, paste0("SELECT oldCode, currentCode FROM fylke", year)))))
  
  RODBC::odbcClose(.DB)
  
  names(tab) <- c("old", "current")
  tab[!is.na(old) & old != current]
  
  # Save file
  data.table::fwrite(tab, "./data/georecode.csv", sep = ";")
}

#' .readGeoRecode
#' Helper function to dead georecode.csv from github
#'
.readGeoRecode <- function(){
  file <- paste0("https://raw.githubusercontent.com/helseprofil/KHvalitetskontroll/main/data/georecode.csv")
  data.table::fread(file)
}
