#' BoxPlot
#' 
#' Creates boxplots to visualize outliers.
#' Plots are stored in folders PLOTT/BP and PLOTT/BPc
#' 
#' @param data Dataset flagged for outliers
#' @param onlynew Should only new outliers be indicated on the plot? Default = TRUE
#' @param change Should plots be based on year-to-year changes. Default = FALSE
#' @param profileyear default = PROFILEYEAR
BoxPlot <- function(data = dfnew_flag,
                    onlynew = TRUE,
                    change = FALSE,
                    profileyear = PROFILEYEAR,
                    data2 = dfold_flag){
  
  # Extract kubename and create path and base filename
  kubename <- .GetKubename(data)
  .CreateFolders(profileyear,kubename)
  datetag <- .GetKubedatetag(data)
  savefolder <- ifelse(change, "BPc", "BP")
  savebase <- file.path("F:", 
                        "Forskningsprosjekter", 
                        "PDB 2455 - Helseprofiler og til_",
                        "PRODUKSJON", 
                        "VALIDERING", 
                        "NESSTAR_KUBER",
                        profileyear,
                        "KVALITETSKONTROLL",
                        kubename,
                        "PLOTT",
                        savefolder)
  filenamebase <- paste(kubename,
                        datetag,
                        savefolder,
                        sep = "_")
  
  # Remove rows with missing GEOniv (probably 99-codes) and remove unused GEOniv levels. 
  data <- data[!is.na(GEOniv)][, GEOniv := base::droplevels(GEOniv)]
  
  # Identify dimensions, value column, and outliercolumns
  .IdentifyColumns(data)
  .val <- attributes(data)$outliercol
  .outlier <- "OUTLIER"
  .newoutlier <- "NEW_OUTLIER"
  .quantiles <- c("wq25", "wq50", "wq75")
  .ollimits <- c("LOW", "HIGH")
  
  if(change){
    .val <- paste0("change_", .val)
    .outlier <- paste0("change_", .outlier)
    .newoutlier <- paste0("change_", .newoutlier)
    .quantiles <- paste0("change_", .quantiles)
    .ollimits <- paste0("change_", .ollimits)
  }
  
  # Cannot filter only new outliers if not present
  if(!.newoutlier %in% names(data) & isTRUE(onlynew)){
    onlynew <- FALSE
    cat("Column NEW_OUTLIER not present, all present outliers are included")
  }
  
  
  # Extract data to generate boxplots, including N observations per strata, and maximum and minimum non-outlier for boxplot whiskers
  bycols <- c("GEOniv", stringr::str_subset(.dims1, "\\bGEO\\b|\\bAAR\\b", negate = T))
  g <- collapse::GRP(data, c(bycols, .quantiles, .ollimits))
  
  baseplotdata <- collapse::join(g[["groups"]], 
                                 data[, .(N_obs = collapse::fsum(!is.na(get(.val))),
                                          MINABOVELOW = collapse::fmin(get(.val)[get(.val) >= get(.ollimits[1])]),
                                          MAXBELOWHIGH = collapse::fmax(get(.val)[get(.val) <= get(.ollimits[2])])),
                                      by = bycols],
                                 overid = 0, verbose = 0)
  
  # Extract data containing outliers and add label. If onlynew = TRUE, only extract new outliers.
  if(onlynew){
    outlierdata <- data[get(.newoutlier) == 1]
  } else{
    outlierdata <- data[get(.outlier) == 1]
  }
  outlierdata[, label := paste0(GEO, "'", stringr::str_sub(AAR, -2L, -1L))]
  outlierdata <- outlierdata[, (.SD), .SDcols = c(bycols, .val, "label")]
  
  # Find total number of strata. Split plots into separate files of max 25 plots per file
  facets <- stringr::str_subset(bycols, "GEOniv", negate = TRUE)
  filedims <- character()
  filedims <- c(filedims, .findBoxplotSubset(d = baseplotdata, b = facets))
  if(length(filedims > 0)){
    facets <- stringr::str_subset(facets, 
                                  stringr::str_c("^", filedims, "$", collapse = "|"),
                                  negate = TRUE)
  }
  
  # Create list of filters for subsetting. 
  if(length(filedims > 0)){
    subsets <- GRP(baseplotdata, filedims)[["groups"]]
    cols <- names(subsets)
    for(i in cols){
      subsets[, (i) := paste0(i, "=='", get(i), "'")]
    }
    filter <- subsets[, filter := do.call(paste, c(.SD, sep = " & ")), .SDcols = cols][, (filter)]
  } else {
    # If fildims = 0, filter = TRUE to select all rows with DT[TRUE] 
    filter <- "TRUE"
  }
  
  # Generate global plot elements
  plotby <- c("GEOniv", facets)
  plotdims <- .allcombs(baseplotdata, plotby)
  n_strata <- nrow(plotdims[, .N, by = facets])
  n_rows <- base::ceiling(n_strata/5)
  title <- paste0("File: ", attributes(baseplotdata)$Filename, ", Date: ", Sys.Date())
  caption <- paste0("Plots grouped by: ", paste0(facets, collapse = ", "))
  ylab <- ifelse(change, paste0(stringr::str_remove(.val, "change_"), ", (% change)"), .val)
  filenameold <- ifelse(!is.null(data2),
                        attributes(data2)$Filename,
                        "not specified")
  comp <- ifelse(onlynew, filenameold, NA_character)
  plotvar <- paste0("Variable plotted: ", ylab)
  plotvar <- if(onlynew){
    paste0(plotvar, ", only new outliers indicated. Old file: ", comp)
    }
  
  # Generate subsets, filenames, and make/save plot.
  for(i in filter){
    
    # subset baseplotdata and outlierdata
    bp <- baseplotdata[eval(parse(text = i))][N_obs > 2]
    ol <- outlierdata[eval(parse(text = i))]
    
    # Dynamically generate filename, savepath, and varying plot elements
    if(i == "TRUE"){
      name <- "_alle.png"
    } else {
      name <- character()
      for(i in filedims){
        name <- paste0(name, "_", unique(bp[[i]]))
      }
      name <- paste0(name, ".png")
    }
    filename <- paste0(filenamebase, name)
    savepath <- file.path(savebase, filename)
    
    subtitle <- paste0(plotvar, "\n")
    for(i in filedims){
      subtitle <- paste0(subtitle, i, ": ", unique(bp[[i]]), "\n")
    }
    
    # Generate and save plots. 
    p <- ggplot(data = plotdims,
                aes(x = GEOniv)) +
      facet_wrap(facets,
                 labeller = labeller(.multi_line = F),
                 scales = "free_x",
                 ncol = 5) +
      scale_x_discrete(limits = rev(levels(ol$GEOniv)), 
                       drop = T) +
      labs(y = ylab,
           x = NULL,
           title = title,
           subtitle = subtitle,
           caption = caption) +
      coord_flip() +
      geom_text(data = ol,
                aes(y = get(.val),
                    label = label),
                angle = 90,
                size = 8/.pt) +
      geom_boxplot(
        data = bp,
        aes(ymin = MINABOVELOW,
            lower = get(.quantiles[1]),
            middle = get(.quantiles[2]),
            upper = get(.quantiles[3]),
            ymax = MAXBELOWHIGH),
        stat = "identity") + 
      ggh4x::force_panelsizes(cols = unit(8, "cm"),
                              rows = unit(6, "cm")) +
      theme(text = element_text(color = "black"),
            plot.title = element_text(face = "bold", size = 20),
            plot.subtitle = element_text(size = 16),
            plot.caption = element_text(size = 16),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 12))
    
    ggsave(filename = savepath,
           plot = p, 
           device = "png", 
           dpi = 300,
           width = 45,
           height = n_rows*6 + 10,
           units = "cm")
  }
}


#' TimeSeries
#' 
#' Creates time-series plots to visualize outliers.The purpose of these plots are to 
#' evaluate whether an outlier is also unreasonable within its own time series. 
#' The plots also contain information regarding the underlying numbers (TELLER or sumTELLER), as 
#' small absolute changes may have a big impact on RATE in small geographical units.
#' 
#' Plots are stored in folders PLOTT/BP and PLOTT/BPc
#'
#' @param data Dataset flagged for outliers
#' @param profileyear default = PROFILEYEAR
#'
#' @return
#' @export
#'
#' @examples
TimeSeries <- function(data = dfnew_flag,
                       profileyear = PROFILEYEAR){
  
  kubename <- .GetKubename(data)
  .CreateFolders(profileyear,kubename)
  datetag <- .GetKubedatetag(data)
  savebase <- file.path("F:", 
                        "Forskningsprosjekter", 
                        "PDB 2455 - Helseprofiler og til_",
                        "PRODUKSJON", 
                        "VALIDERING", 
                        "NESSTAR_KUBER",
                        profileyear,
                        "KVALITETSKONTROLL",
                        kubename,
                        "PLOTT",
                        "TS")
  filenamebase <- paste(kubename,
                        datetag,
                        "TS",
                        sep = "_")
  
  # Identify target columns, outlier column, and TELLER column
  .IdentifyColumns(data)
  .val <- attributes(data)$outliercol
  .outlier <- "OUTLIER"
  .teller <- data.table::fcase("TELLER_uprikk" %in% .vals1, "TELLER_uprikk",
                               "TELLER" %in% .vals1, "TELLER",
                               "sumTELLER" %in% .valse1, "sumTELLER",
                               default = NA_character_)
  
  # Find strata containing > 0 outlier, only keep strata with outliers
  bycols <- stringr::str_subset(.dims1, "\\bAAR\\b", negate = T)
  data[, n_outlier := sum(get(.outlier), na.rm = T), by = bycols]
  data <- data[n_outlier > 0]
  outlierdata <- data[get(.outlier) == 1]
  
  # Generate filter to split into files
  
  
  
  # Loop through filter. generate and save plot

    ggplot(pd, aes(x = AAR,
                 y = get(.val))) + 
    facet_wrap(facetcol,
               labeller = labeller(.multi_line = F),
               scales = "free_y",
               ncol = 5) + 
    geom_point() +
    geom_line(group = 1) + 
    geom_point(data = pdo, aes(color = "red", fill = "black", size = 3)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8))
    
  
  
}

#' .findBoxplotSubset
#'
#' @param d plotdata
#' @param b bycols
.findBoxplotSubset <- function(d,
                               b){
  
  orgstrata <- nrow(d[, .N, by = b])
  optnfiles <- base::ceiling(orgstrata/25)
  
  if(optnfiles == 1){
    return(NULL)
  }
  # Create a reference table containing dim and n levels (this may replace CompareDims(), and called here)
  ref <- data.table(dim = character(), n = numeric())
  for(i in b){
    l <- length(unique(d[[i]]))
    ref = data.table::rbindlist(list(ref, 
                                     data.table(dim = i,
                                                n = l)))
  }
  
  # Generate a table with all combinations of 1, 2, or 3 dimensions
  combs <- data.table(dim1 = character(), 
                      dim2 = character(), 
                      dim3 = character())
  for(i in 1:3){
    if(length(b) >= i){
      x <- data.table(base::t(utils::combn(b, i)))
      colnames(x) <- paste0("dim", 1:i)
      combs <- data.table::rbindlist(list(combs, x), fill = TRUE)
    }
  }
  
  # Add n levels for dim1-3
  combs[ref, n1 := i.n, on = c("dim1"= "dim")]
  combs[ref, n2 := i.n, on = c("dim2"= "dim")]
  combs[ref, n3 := i.n, on = c("dim3"= "dim")]
  
  # Replace NA with 1 and calculate product
  data.table::setnafill(combs, fill = 1, cols = c("n1", "n2", "n3"))
  combs[, prod := n1*n2*n3]
  optimal <- combs[prod > optnfiles][which.min(prod)]
  v <- c(optimal$dim1, optimal$dim2, optimal$dim3)
  v <- v[!is.na(v)]
  v
}

