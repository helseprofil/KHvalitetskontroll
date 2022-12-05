# Functions used in interactive quality control

#' Plot time serie for specific strata
#'
#' @param data defaults to dfnew
#' @param maltall which value to plot on y-axis
#' @param geo select GEO, if not selected it will default to 0
#' @param alder select age group, if not selected it will default to 0
#' @param kjonn select gender, if not selected it will default to 0
#' @param utdann select education, if not selected it will default to 0
#' @param innvkat select immigrant status, if not selected it will default to 0
#' @param landbak select country of origin, if not selected it will default to 0
#' @param extradim any non-standard dimension
#' @param extraval selected level of non-standard dimension
ShowTS <- function(data = dfnew,
                   maltall = "MEIS",
                   geo = NULL,
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
  
  # Filter out correct strata
  
  if ("GEO" %in% dims) {
    
    if(!is.null(geo) && !is.numeric(geo)){
      stop("\ngeo must be numeric or NULL")
    }
    if(!is.null(geo) && !geo %in% unique(data$GEO)){
      stop("\ngeo not found in file")
    }
    
    if(is.null(geo)){
      message("\nGEO not specified, GEO = 0 plotted by default")
      geo <- 0
    }
    
    data <- data[GEO == geo]
  }
  
  if ("ALDER" %in% dims) {
    
    if(!is.null(alder) && !alder %in% unique(data$ALDER)){
      stop(paste0("\n", alder, " is not a valid level in ALDER"))
    }
    
    if(is.null(alder)){
      # Find total age group (min_max)
      ALDERl <- min(as.numeric(str_extract(data$ALDER, "[:digit:]*(?=_)")))
      ALDERh <- max(as.numeric(str_extract(data$ALDER, "(?<=_)[:digit:]*")))
      alder <- paste0(ALDERl, "_", ALDERh)
      message(paste("\nALDER not specified, ALDER =", alder, "plotted by default"))
    }
    
    data <- data[ALDER == alder]
  }
  
  if ("KJONN" %in% dims) {
    
    if(!is.null(kjonn) && !is.numeric(kjonn)){
      stop("\nkjonn must be numeric")
    }
    
    if(!is.null(kjonn) && !kjonn %in% unique(data$KJONN)){
      stop(paste0("\n", kjonn, " is not a valid level in KJONN"))
    }
    
    if(is.null(kjonn)){
      message("\nKJONN not specified, KJONN = 0 plotted by default")
      kjonn <- 0
    }
    data <- data[KJONN == kjonn]
  }
  
  if ("UTDANN" %in% dims) {
    
    if(!is.null(utdann) && !is.numeric(utdann)){
      stop("\nutdann must be numeric")
    }
    
    if(!is.null(utdann) && !utdann %in% unique(data$UTDANN)){
      stop(paste0("\n", utdann, " is not a valid level in UTDANN"))
    }
    
    if(is.null(utdann)){
      message("\nUTDANN not specified, UTDANN = 0 plotted by default")
      utdann <- 0
    }
    data <- data[UTDANN == utdann]
  }
  
  if ("INNVKAT" %in% dims ) {
    
    if(!is.null(innvkat) && !is.numeric(innvkat)){
      stop("\ninnvkat must be numeric")
    }
    
    if(!is.null(innvkat) && !innvkat %in% unique(data$INNVKAT)){
      stop(paste0("\n", innvkat, "is not a valid level in INNVKAT"))
    }
    
    if(is.null(innvkat)){
      message("\nINNVKAT not specified, INNVKAT = 0 plotted by default")
      innvkat <- 0
    }
    data <- data[INNVKAT == innvkat]
  }
  
  if ("LANDBAK" %in% dims) {
    
    if(!is.null(landbak) && !is.numeric(landbak)){
      stop("\nlandbak must be numeric")
    }
    
    if(!is.null(landbak) && !landbak %in% unique(data$LANDBAK)){
      stop(paste0("\n", landbak, " is not a valid level in LANDBAK"))
    }
    
    if(is.null(landbak)){
      message("\nLANDBAK not specified, LANDBAK = 0 plotted by default")
      landbak <- 0
    }
    data <- data[LANDBAK == landbak]
  }
  
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
  
  # Control that only one level is selected per dimension (except AAR) 
  for(i in str_subset(dims, "AAR", negate = T)){
    
    d <- unique(data[[i]])
    if(length(d) > 1){
      stop(paste("\n\nMore than one level selected for", i, "please specify"))
    }
  }
  
  caption <- paste0(if("GEO" %in% dims){paste0("GEO = ", geo)},
                    if("ALDER" %in% dims){paste0("\nALDER = ", alder)},
                    if("KJONN" %in% dims){paste0("\nKJONN = ", kjonn)},
                    if("UTDANN" %in% dims){paste0("\nUTDANN = ", utdann)},
                    if("INNVKAT" %in% dims){paste0("\nINNVKAT = ", innvkat)},
                    if("LANDBAK" %in% dims){paste0("\nLANDBAK = ", landbak)},
                    if(!is.null(extradim)){paste0("\n", extradim, " = ", extraval)})
  
  data[, AARx := as.numeric(str_extract(AAR, "[:digit:]*(?=_)"))] %>% 
    ggplot(aes_string(x = "AARx",
                      y = maltall)) + 
    geom_point() +
    geom_line() + 
    labs(x = "AAR",
         y = maltall,
         title = caption)
} 