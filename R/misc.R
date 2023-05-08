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
                      modusold = NULL){
  
  # Check arguments new file
  if(base::isFALSE(stringr::str_detect(dfnew, ".*_\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}"))) {
    stop("dfnew must be provided in the format FILENAME_YYYY-MM-DD-hh-mm")
  }
  
  if(base::isFALSE(any(stringr::str_detect(foldernew, "^\\d{4}$"), 
                 foldernew == "DATERT", 
                 foldernew == "QC"))) {
    stop("`foldernew` must be either 'QC', 'DATERT', or 4 digits")
  }
  
  if(base::isFALSE(modusnew %in% c("KH", "NH"))) {
    stop("`modusnew` must be either 'KH' or 'NH'")
  }
  
  # Check arguments old file
  if(base::isFALSE(any(stringr::str_detect(dfold, ".*_\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}"),
                 is.null(dfold)))){
    stop("dfold must be provided in the format FILENAME_YYYY-MM-DD-hh-mm, or NULL")
  }
  
  if(base::isFALSE(is.null(dfold))){
    
    if(base::isFALSE(any(stringr::str_detect(folderold, "^\\d{4}$"), 
                   folderold == "DATERT", 
                   folderold == "QC"))) {
      stop("When dfold is specified, `folderold` must be either 'QC', 'DATERT', or 4 digits")
    }
    
    if(base::isFALSE(any(modusold %in% c("KH", "NH")))) {
      stop("When dfold is specified, `modusold` must be either 'KH' or 'NH'")
    }
  }
  
  # Check if file(s) exists, if not stop before reading files
  pathnew <- .findpath(modusnew, foldernew)
  filenew <- list.files(pathnew, pattern = paste0("^",dfnew))
  filepathnew <- file.path(pathnew, filenew)
  
  if(base::isFALSE(file.exists(filepathnew) & length(filepathnew) == 1)){
    stop("dfnew not found. Check arguments 'dfnew', 'foldernew', and 'modusnew")
  }
  
  if(base::isFALSE(is.null(dfold))){
    
    pathold <- .findpath(modusold, folderold)
    fileold <- list.files(pathold, pattern = dfold)
    filepathold <- file.path(pathold, fileold)
    
    if(base::isFALSE(file.exists(filepathold) & length(filepathold) == 1)){
      stop("dfold not found. Check arguments 'dfold', 'folderold', and 'modusold")
    }
  }
  
  # Read dfnew and store to global env
  outdatanew <- .readfile(filepathnew, foldernew)
  cat(paste0("New file (dfnew) loaded: ", str_extract(filepathnew, "(?<=PRODUKTER/).*"), "\n"))
  if(attr(outdatanew, "colnameinfo")$diff == "yes"){
    .listcolrename(outdatanew, "dfnew")
  }
  dfnew <<- outdatanew
  
  # If provided, read dfold and store to global env
  if(base::isFALSE(is.null(dfold))){
    outdataold <- .readfile(filepathold, folderold)
    cat(paste0("Old file (dfold) loaded: ", str_extract(filepathold, "(?<=PRODUKTER/).*"), "\n"))
    if(attr(outdataold, "colnameinfo")$diff == "yes"){
      .listcolrename(outdataold, "dfold")
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
                              stringr::str_detect(folder, "^\\d{4}$"), paste0(modus, as.character(folder), "NESSTAR")))
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
  setnames(outdata, 
           old = c("antall", "Crude", "Adjusted", "sumteller", "sumnevner", "smr", "FLx", "utdanningsnivå"),
           new = c("TELLER", "RATE", "MEIS", "sumTELLER", "sumNEVNER", "SMR", "MEIS", "UTDANN"),
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
  cat("\n", prefix, " columns automatically renamed:", stringr::str_c(paste0("\n* ", outmsg), collapse = ""), sep = "")
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
  arkivdir <- file.path(kubedir, "ARKIV")
  
  if(!dir.exists(profileyeardir)){
    dir.create(profileyeardir)
  }
  
  if(!dir.exists(kvalkontrolldir)){
    dir.create(kvalkontrolldir)
  }
  
  if(!dir.exists(kubedir)){
    dir.create(kubedir)
  }
  
  if(!dir.exists(filedumpdir)){
    dir.create(filedumpdir)
  }
  
  if(!dir.exists(plotdir)){
    dir.create(plotdir)
  }
  
  if(!dir.exists(arkivdir)){
    dir.create(arkivdir)
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
  .vals1 <<- stringr::str_subset(names(data1), stringr::str_c(.dims1, collapse = "|"), negate = T)
  
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
    .newdims <<- stringr::str_subset(.dims1, stringr::str_c("^", .dims2, "$", collapse = "|"), negate = T)
    .expdims <<- stringr::str_subset(.dims2, stringr::str_c("^", .dims1, "$", collapse = "|"), negate = T)
    .commonvals <<- .vals1[.vals1 %in% .vals2]
    .newvals <<- stringr::str_subset(.vals1, stringr::str_c("^", .vals2, "$", collapse = "|"), negate = T)
    .expvals <<- stringr::str_subset(.vals2, stringr::str_c("^", .vals1, "$", collapse = "|"), negate = T)
    .commoncols <<- c(.commondims, .commonvals)
  }
  
}

#' .SmallLargeKommune
#'
#' Loads current BEFOLK_GK file, and separates out small and large kommune
.SmallLargeKommune <- function(){
  
  basepath <- file.path("F:", 
                        "Forskningsprosjekter", 
                        "PDB 2455 - Helseprofiler og til_",
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

#' Helper function to connect to KHelsa ACCESS database
#'
#' @return
#' @export
#'
#' @examples
.ConnectKHelsa <- function(){
  RODBC::odbcConnectAccess2007("F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/STYRING/KHELSA.mdb")
}
