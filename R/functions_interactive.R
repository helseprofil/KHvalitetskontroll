# Functions used in interactive quality control

#' Plot time serie for specific strata
#'
#' @param data name of data set
#' @param maltall which value to plot on y-axis, defaults to "MEIS"
#' @param geo select GEO
#' @param alder select age group
#' @param kjonn select gender
#' @param utdann select education
#' @param innvkat select immigrant status
#' @param landbak select country of origin
#' @param extradim any non-standard dimension
#' @param extraval selected level of non-standard dimension
#' #' @param tab should table be printed? defaults to TRUE
ShowTS <- function(data = NULL,
                   maltall = "MEIS",
                   geo = NULL,
                   alder = NULL,
                   kjonn = NULL,
                   utdann = NULL,
                   innvkat = NULL,
                   landbak = NULL,
                   extradim = NULL,
                   extraval = NULL,
                   tab = TRUE){
  
  if(!exists(".ALL_DIMENSIONS")) {
    source("https://raw.githubusercontent.com/helseprofil/misc/main/alldimensions.R")
    .ALL_DIMENSIONS <- ALL_DIMENSIONS
    rm(ALL_DIMENSIONS)
  }
  
  # Identify existing dimensions
  dims <- names(data)[names(data) %in% .ALL_DIMENSIONS]
  vals <- names(data)[!names(data) %in% dims]
  
  # Filter out correct strata
  data <- .MakeSubset(data = data,
                      dims = dims,
                      alder = alder,
                      kjonn = kjonn,
                      utdann = utdann,
                      innvkat = innvkat,
                      landbak = landbak,
                      extradim = extradim,
                      extraval = extraval)
  
  # Filter out correct GEO
  if ("GEO" %in% dims) {
   
    if(is.null(geo)){
      stop("\ngeo must be specified")
    }
    
   if(!is.numeric(geo)){
     stop("\ngeo must be numeric")
   }
   if(!geo %in% unique(data$GEO)){
     stop(paste0("\n", geo, " is not a valid level of GEO"))
   }
   
   data <- data[GEO == geo]
  }

  # Control that only one level is selected per dimension (except AAR) 
  for(i in str_subset(dims, "AAR", negate = T)){
    
    d <- unique(data[[i]])
    if(length(d) > 1){
      stop(paste("\n\nMore than one level selected for", i, "please specify"))
    }
  }
  
  # Create plot title
  caption <- paste0(if("GEO" %in% dims){paste0("GEO = ", geo)},
                    if("ALDER" %in% dims){paste0("\nALDER = ", alder)},
                    if("KJONN" %in% dims){paste0("\nKJONN = ", kjonn)},
                    if("UTDANN" %in% dims){paste0("\nUTDANN = ", utdann)},
                    if("INNVKAT" %in% dims){paste0("\nINNVKAT = ", innvkat)},
                    if("LANDBAK" %in% dims){paste0("\nLANDBAK = ", landbak)},
                    if(!is.null(extradim)){paste0("\n", extradim, " = ", extraval)})
  
  plot <- data[, AARx := as.numeric(str_extract(AAR, "(?<=_)[:digit:]*"))] %>% 
    ggplot(aes(x = AARx,
               !!!ensyms(y = maltall))) + 
    geom_point() +
    geom_line() + 
    labs(x = "AAR (highest in interval)",
         y = maltall,
         title = caption)
  
  if(tab){
  outdata <- data %>% arrange(AARx) %>% select(-AARx)
  print(outdata)
  }
  print(plot)
} 

#' Show data on all bydels
#' 
#' Prints a table with one row per bydel, and one column per year
#' 
#' Show MEIS if available, otherwise SMR or TELLER
#'
#' @param data 
#' @param maltall which number to show, defaults to SPVFLAGG but can also show other value columns
#' @param alder 
#' @param kjonn 
#' @param utdann 
#' @param innvkat 
#' @param landbak 
#' @param extraval 
#' @param extradim 
ShowBydel <- function(data = NULL,
                      maltall = "SPVFLAGG",
                      kommune = NULL,
                      alder = NULL,
                      kjonn = NULL,
                      utdann = NULL,
                      innvkat = NULL,
                      landbak = NULL,
                      extradim = NULL,
                      extraval = NULL){
  
  if(!exists(".ALL_DIMENSIONS")) {
    source("https://raw.githubusercontent.com/helseprofil/misc/main/alldimensions.R")
    .ALL_DIMENSIONS <- ALL_DIMENSIONS
    rm(ALL_DIMENSIONS)
  }
  
  # Identify existing dimensions
  dims <- names(data)[names(data) %in% .ALL_DIMENSIONS]
  vals <- names(data)[!names(data) %in% dims]
  
  # Filter out correct strata

  data <- .MakeSubset(data = data,
                      dims = dims,
                      alder = alder,
                      kjonn = kjonn,
                      utdann = utdann,
                      innvkat = innvkat,
                      landbak = landbak,
                      extradim = extradim,
                      extraval = extraval)
  
  if(!is.null(kommune) && !is.numeric(kommune) && !nchar(kommune) %in% c(3,4)){
    stop("kommune must be numeric and 3-4 digits, or NULL to show all")
  }
  
  # Make list of geo codes corresponding to bydel, and filter if kommune argument is specified
  geo <- unique(data[GEO>9999]$GEO)
  if(!is.null(kommune)){
  geo <- str_subset(geo, paste0("^", kommune))
  }
  
  # Filter out years with no data on bydel
  bydelaar <- data[GEO %in% geo & SPVFLAGG == 0, .N, by = AAR]$AAR
  data <- data[AAR %in% bydelaar][, KOMMUNE := character()]
  
  # Create KOMMUNE column
  data[grep("^301", GEO), KOMMUNE := "0301 Oslo"]
  data[grep("^1103", GEO), KOMMUNE := "1103 Stavanger"]
  data[grep("^4601", GEO), KOMMUNE := "4601 Bergen"]
  data[grep("^5001", GEO), KOMMUNE := "5001 Trondheim"]
  
  
  if(is.null(maltall)){
    maltall <- "SPVFLAGG"
  }
  
  data <- data[GEO %in% geo, c("KOMMUNE", ..dims, ..maltall)]
  setkeyv(data, c("KOMMUNE", dims))
  
  # Format table
  data <- dcast(data, ... ~ AAR, value.var = maltall)
  
  # Convert all dimensions to factor for search function
  filtercols <- c("KOMMUNE", str_subset(dims, "GEO|AAR", negate = T))
  data[, (filtercols) := lapply(.SD, as.factor), .SDcols = c(filtercols)]
  nofilter <- names(data)[!names(data) %in% filtercols]
  
  DT::datatable(data, 
                filter = "top",
                options = list(pageLength = 20,
                               lengthMenu = c(10, 20, 30),
                               scrollX = TRUE,
                               columnDefs = list(list(targets = nofilter,
                                                      searchable = FALSE))),
                rownames = F)
}


.MakeSubset <- function(data = data,
                        dims = dims,
                        alder = NULL,
                        kjonn = NULL,
                        utdann = NULL,
                        innvkat = NULL,
                        landbak = NULL,
                        extradim = NULL,
                        extraval = NULL){
  
  # Filter out correct strata
  
  if ("ALDER" %in% dims) {
    
    if(!is.null(alder) && !alder %in% unique(data$ALDER)){
      stop(paste0("\n", alder, " is not a valid level in ALDER"))
    }
    
    if(!is.null(alder)){
      data <- data[ALDER == alder]  
    }
  }
  
  if ("KJONN" %in% dims) {
    
    if(!is.null(kjonn) && !is.numeric(kjonn)){
      stop("\nkjonn must be numeric")
    }
    
    if(!is.null(kjonn) && !kjonn %in% unique(data$KJONN)){
      stop(paste0("\n", kjonn, " is not a valid level in KJONN"))
    }
    
    if(!is.null(kjonn)){
      data <- data[KJONN == kjonn]
    }
  }
  
  if ("UTDANN" %in% dims) {
    
    if(!is.null(utdann) && !is.numeric(utdann)){
      stop("\nutdann must be numeric")
    }
    
    if(!is.null(utdann) && !utdann %in% unique(data$UTDANN)){
      stop(paste0("\n", utdann, " is not a valid level in UTDANN"))
    }
    
    if(!is.null(utdann)){
      data <- data[UTDANN == utdann]
    }
  }
  
  if ("INNVKAT" %in% dims) {
    
    if(!is.null(innvkat) && !is.numeric(innvkat)){
      stop("\ninnvkat must be numeric")
    }
    
    if(!is.null(innvkat) && !innvkat %in% unique(data$INNVKAT)){
      stop(paste0("\n", innvkat, "is not a valid level in INNVKAT"))
    }
    
    if(!is.null(innvkat)){
      data <- data[INNVKAT == innvkat]
    }
  }
  
  if ("LANDBAK" %in% dims) {
    
    if(!is.null(landbak) && !is.numeric(landbak)){
      stop("\nlandbak must be numeric")
    }
    
    if(!is.null(landbak) && !landbak %in% unique(data$LANDBAK)){
      stop(paste0("\n", landbak, " is not a valid level in LANDBAK"))
    }
    
    if(!is.null(landbak)){
      data <- data[LANDBAK == landbak]
    }
    
  }
  
  # Special handling of exstra dimension
  
  if (!is.null(extradim)) {
    
    if(!extradim %in% names(data)){
      stop(paste0("\nNo column called ", extradim))
    }
    
    if(is.null(extraval)){
      stop("\nextradim is selected but not extraval, please specify")
    }
    
    if(!is.null(extraval) && !extraval %in% unique(data[[extradim]])){
      stop(paste0("\n'", extraval, "' is not a valid level in '", extradim, "'"))
    }
    
    data <- data[data[[extradim]] == extraval]
  }
  
  data
}
