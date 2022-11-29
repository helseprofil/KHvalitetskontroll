# Functions to be used in several other files

#' ReadFile
#'
#' @param file File name. Locates file by partial matching. 
#' #' @param bank Either "NH" or "KH", defaults to "KH"
#' @param year Either a 4-digit number corresponding to profile year, or "DATERT". Defaults to DATERT
#'
#' @return
#' @export
#'
#' @examples
ReadFile <- function(file = NULL, 
                     modus = "KH", 
                     folder = "DATERT"){
  
  if(!(modus %in% c("KH", "NH"))) {
    stop("`modus` must be either 'KH' or 'NH'")
  }
  
  if(!(str_detect(folder, "[:digit:]{4}") & str_length(folder) == 4| 
       folder == "DATERT")) {
    stop("`folder` must be either 4 digits or 'DATERT'")
  }
  
  if(is.null(file)) {
    stop("file not selected")
  }
  
  MODUS <- case_when(modus == "KH" ~ "KOMMUNEHELSA",
                     modus == "NH" ~ "NORGESHELSA")
  
  FOLDER <- case_when(folder == "DATERT" ~ paste0(folder, "/csv"),
                      TRUE ~ paste0(modus, folder, "NESSTAR"))
  
  basepath <- file.path("F:", 
                        "Forskningsprosjekter", 
                        "PDB 2455 - Helseprofiler og til_",
                        "PRODUKSJON", 
                        "PRODUKTER", 
                        "KUBER", 
                        MODUS, 
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
  
  outdata <- fread(filepath)
  setattr(outdata, "Filename", basename(filepath))
  cat(paste0("File loaded: ", MODUS, "/", FOLDER, "/", basename(filepath)))
  
  outdata
}

.IdentifyVariables <- function(data1 = NULL,
                               data2 = NULL){
  
  # Identify common columns, and extract dimensions
  if(!exists(".ALL_DIMENSIONS")) {
    source("https://raw.githubusercontent.com/helseprofil/misc/main/alldimensions.R")
    .ALL_DIMENSIONS <- ALL_DIMENSIONS
  }
  
  if(is.null(data1) && is.null(data2)){
    NULL
  }
  
}

#' Print dim
#' 
#' Wrapper around str_c for nice printing of variable names in .Rmd-reports
#' 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
print_dim <- function(...){
  
  if(is.null(...)){
    "(no grouping variables)"
    } else {
  stringr::str_c(..., collapse = ", ")
    }
}


#' Finds name of cube from filepath
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
.GetKubename <- function(df){
  filename <- attributes(df)$Filename
  str_extract(filename, "^.*(?=_[:digit:]{4})")
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
.CreateFolders <- function(profileyear = profileyear,
                           kubename = kubename){
  
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
}

#' Save HTML-report
#'
#' @param profileyear 
#' @param inputfile 
#' @param savename 
#'
#' @return
#' @export
#'
#' @examples
SaveReport <- function(profileyear = 2023,
                       inputfile = "Kvalitetskontroll_del1.Rmd",
                       savename = NULL){
  
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
    filename <- paste0(str_remove(attributes(dfnew)$Filename, ".csv"),
                       "_",
                       str_remove(inputfile, ".Rmd"))
  }
  
  # Save report
  rmarkdown::render(input = inputfile,
                    output_file = filename,
                    output_dir = filepath)
}
