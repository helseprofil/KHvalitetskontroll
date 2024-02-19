
QCPlots <- function(data = dfnew_flag,
                    data2 = dfold_flag,
                    onlynew = TRUE,
                    profileyear = PROFILEYEAR,
                    overwrite = FALSE,
                    BP = TRUE,
                    BPc = TRUE,
                    TS = TRUE,
                    TSc = TRUE,
                    TL = TRUE){
  
  # Boxplot abs
  if(BP){
    cat("\nPlotting boxplots for absolute values")
    BoxPlot(data = data,
            onlynew = onlynew,
            change = FALSE,
            profileyear = profileyear,
            data2 = data2,
            overwrite = overwrite)
  }
  
  # Boxplot change
  if(BPc){
    cat("\nPlotting boxplots for year-to-year change")
    BoxPlot(data = data,
            onlynew = onlynew,
            change = TRUE,
            profileyear = profileyear,
            data2 = data2,
            overwrite = overwrite)
  }
  
  # Timeseries abs
  if(TS){
    cat("\nPlotting time-series for absolute values")
    TimeSeries(data = data,
               onlynew = onlynew,
               change = FALSE,
               profileyear = profileyear,
               data2 = data2,
               overwrite = overwrite)
  }
  
  # Timeseries change
  if(TSc){
    cat("\nPlotting time-series for year-to-year change")
    TimeSeries(data = data,
               onlynew = onlynew,
               change = TRUE,
               profileyear = profileyear,
               data2 = data2, 
               overwrite = overwrite)
  }
  
  # Timeline
  if(TL){
    cat("\nPlotting timeline bydel")
    TimelineBydel(data = data,
                  profileyear = profileyear,
                  overwrite = overwrite)
  }
}


#' BoxPlot
#' 
#' Creates boxplots to visualize outliers.
#' Plots are stored in folders PLOTT/BP and PLOTT/BPc
#' 
#' @param data Dataset flagged for outliers
#' @param onlynew Should only new outliers be indicated on the plot? Default = TRUE
#' @param change Should plots be based on year-to-year changes. Default = FALSE
#' @param profileyear default = PROFILEYEAR
#' @param data2 old file, only used 
#' @param overwrite 
BoxPlot <- function(data = dfnew_flag,
                    onlynew = TRUE,
                    change = FALSE,
                    profileyear = PROFILEYEAR,
                    data2 = NULL,
                    overwrite = FALSE){
  
  if(is.null(attr(data, "outliercol"))){
    cat("Outliercol not detected, does dfnew_flag contain outlier flags?")
    return(invisible(NULL))
  }
  
  # Extract kubename and create path and base filename
  savefolder <- ifelse(change, "BPc", "BP")
  savebase <- .getPlotSaveBase(profileyear = profileyear, kubename = .GetKubename(data), savefolder = savefolder)
  filenamebase <- .getPlotFilenameBase(kubename = .GetKubename(data), datetag = .GetKubedatetag(data), savefolder = savefolder)
  
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
  
  # If change is requested but not present, return here
  if(change & !.val %in% .vals1){
    cat("\nChange variable not found in data, year-to-year plot not generated")
    return(invisible(NULL))
  }
  
  # Cannot filter only new outliers if not present
  if(onlynew & !.newoutlier %in% names(data)){
    onlynew <- FALSE
    cat(paste0("Column ", .newoutlier, " not present, all outliers are included"))
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
  baseplotdata[, c(.ollimits) := NULL]
  
  # Extract data containing outliers and add label. If onlynew = TRUE, only extract new outliers.
  if(onlynew){
    outlierdata <- data[get(.newoutlier) == 1]
  } else{
    outlierdata <- data[get(.outlier) == 1]
  }
  outlierdata[, label := paste0(GEO, "'", stringr::str_sub(AAR, -2L, -1L))]
  outlierdata <- outlierdata[, (.SD), .SDcols = c(bycols, .val, "label")]
  
  # Generate filter to save as multiple files with max 25 panels per page
  facets <- stringr::str_subset(bycols, "GEOniv", negate = TRUE)
  filedims <- character()
  filedims <- c(filedims, .findPlotSubset(d = baseplotdata, b = facets, s = 25))
  if(length(filedims > 0)){
    facets <- stringr::str_subset(facets, 
                                  stringr::str_c("^", filedims, "$", collapse = "|"),
                                  negate = TRUE)
  }
  filter <- .findPlotFilter(baseplotdata, filedims)
  
  # Generate global plot elements
  plotby <- c("GEOniv", facets)
  plotdims <- .allcombs(baseplotdata, plotby)
  n_strata <- nrow(plotdims[, .N, by = facets])
  n_rows <- base::ceiling(n_strata/5)
  title <- paste0("File: ", attributes(baseplotdata)$Filename, ", Date: ", Sys.Date())
  caption <- paste0("Plots grouped by: ", paste0(facets, collapse = ", "))
  ylab <- ifelse(change, paste0(stringr::str_remove(.val, "change_"), ", (% change)"), .val)
  plotvar <- paste0("Variable plotted: ", ylab)
  
  if(onlynew){
    if(!exists(deparse(substitute(data2)), envir = .GlobalEnv) || is.null(data2)){
      filenameold <- "not specified"
    } else {
      filenameold <- attributes(data2)$Filename
    }
    plotvar <- paste0(plotvar, ", only new outliers indicated. Old file: ", filenameold)
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
    
    subtitle <- plotvar
    for(i in filedims){
      subtitle <- paste0(subtitle, "\n", i, ": ", unique(bp[[i]]))
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
    
    .saveBoxPlot(file = savepath, 
                 plot = p, 
                 rows = n_rows,
                 overwrite = overwrite)
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
#' @param data Dataset flagged for outliers, defaults to dfnew_flag
#' @param change Should plots be based on year-to-year changes. Default = FALSE
#' @param profileyear default = PROFILEYEAR
#'
#' @return
#' @export
#'
#' @examples
TimeSeries <- function(data = dfnew_flag,
                       onlynew = TRUE,
                       change = FALSE,
                       profileyear = PROFILEYEAR,
                       data2 = NULL,
                       overwrite = FALSE){
  
  if(data[, length(unique(AAR))] < 2){
    cat("Whoops! Only one unique AAR in file, time series not possible. No plots generated.")
    return(invisible(NULL))
  }
  
  if(is.null(attr(data, "outliercol"))){
    cat("Outliercol not detected, does dfnew_flag contain outlier flags?")
    return(invisible(NULL))
  }
  
  savefolder <- ifelse(change, "TSc", "TS")
  savebase <- .getPlotSaveBase(profileyear = profileyear, kubename = .GetKubename(data), savefolder = savefolder)
  filenamebase <- .getPlotFilenameBase(kubename = .GetKubename(data), datetag = .GetKubedatetag(data), savefolder = savefolder)
    
  # Identify target columns, outlier column, and TELLER column. Create savepath
  .IdentifyColumns(data)
  .val <- attributes(data)$outliercol
  .outlier <- "OUTLIER"
  .newoutlier <- "NEW_OUTLIER"
  .teller <- data.table::fcase("TELLER_uprikk" %in% .vals1, "TELLER_uprikk",
                               "TELLER" %in% .vals1, "TELLER",
                               "sumTELLER" %in% .vals1, "sumTELLER",
                               default = NA_character_)
  
  if(change){
    .val <- paste0("change_", .val)
    .outlier <- paste0("change_", .outlier)
    .newoutlier <- paste0("change_", .newoutlier)
    filenamebase <- paste0(filenamebase, "_y2y_(", format(Sys.time(), "%H%M"), ")")
  } else {
    filenamebase <- paste0(filenamebase, "_(", format(Sys.time(), "%H%M"), ")")
  }
  
  # If change is requested but not present, return here
  if(change & !.val %in% .vals1){
    cat("\nChange variable not found in data, year-to-year plot not generated")
    return(invisible(NULL))
  }
  
  # Cannot filter only new outliers if not present
  if(!.newoutlier %in% names(data) & isTRUE(onlynew)){
    onlynew <- FALSE
    cat(paste0("Column ", .newoutlier, " not present, all outliers are included"))
  }
  
  # Remove rows with missing data on plot value
  data <- data[!is.na(get(.val))]
  bycols <- stringr::str_subset(.dims1, "\\bAAR\\b", negate = T)
  
  # Find strata containing > 0 outlier, only keep strata with outliers
  data[, n_outlier := sum(get(.outlier), na.rm = T), by = bycols]
  data <- data[n_outlier > 0]
  
  # Return here if no strata with outliers exist
  if(nrow(data) == 0){
    cat("\nNo strata containing outliers in data, plots not generated")
    return(invisible(NULL))
  }
  
  # If data on new/prev outlier, reduce data to only strata with new outliers.
  if(onlynew){
    data[, n_new_outlier := sum(get(.newoutlier), na.rm = T), by = bycols]
    data <- data[n_new_outlier > 0]
  }
  
  # Return here if no strata with new outliers exist
  if(nrow(data) == 0){
    cat("\nNo strata containing new outliers in data, plots not generated")
    return(invisible(NULL))
  }
  
  outlierdata <- data[get(.outlier) == 1]
  
  # For lines, only keep strata with >= 2 non-missing rows.
  data[, n_obs := sum(!is.na(get(.val))), by = bycols]
  linedata <- data[n_obs > 1]
  
  # Add middle-point for labels
  data[, y_middle := 0.5*(max(get(.val), na.rm = T) + min(get(.val), na.rm = T)), by = bycols]
  
  # Generate filter to save as multiple files with max 25 panels per page
  facets <- stringr::str_subset(bycols, "GEO", negate = TRUE)
  filedims <- character()
  filedims <- c(filedims, .findPlotSubset(d = data, b = facets, s = 25))
  if(length(filedims > 0)){
    facets <- stringr::str_subset(facets, 
                                  stringr::str_c("^", filedims, "$", collapse = "|"),
                                  negate = TRUE)
  }
  filter <- .findPlotFilter(data, filedims)
  
 # Generate global plot elements
  plotby <- c("GEO", facets)
  ylab <- ifelse(change, paste0(stringr::str_remove(.val, "change_"), ", (% change)"), .val)
  plotvar <- paste0("Variable plotted: ", ylab)
  caption <- paste0("Tellervariabel: ", .teller, "\nPlots grouped by: ", paste0(plotby, collapse = ", "))
  
  if(onlynew){
    if(!exists(deparse(substitute(data2)), envir = .GlobalEnv) || is.null(data2)){
      filenameold <- "not specified"
    } else {
      filenameold <- attributes(data2)$Filename
    }
    plotvar <- paste0(plotvar, ", only new outliers indicated. Old file: ", filenameold)
  }
  
  # Generate subsets, filenames, and make/save plot.
  cat(paste0("Plots printed to PLOTT/", savefolder))
  for(i in filter){
    
    # Generate subsets
    d <- data[eval(parse(text = i))]
    ld <- linedata[eval(parse(text = i))]
    od <- outlierdata[eval(parse(text = i))]
    
    n_pages <- ceiling(nrow(d[, .N, by = plotby])/25)
    
    if(nrow(d) > 0){
      # Dynamically generate filename, savepath, and varying plot elements
      if(i == "TRUE"){
        name <- "_alle.pdf"
      } else {
        name <- character()
        for(i in filedims){
          name <- paste0(name, "_", unique(d[[i]]))
        }
        name <- paste0(name, ".pdf")
      }
      filename <- paste0(filenamebase, name)
      savepath <- file.path(savebase, filename)
      
      subtitle <- paste0(plotvar, "\n")
      for(i in filedims){
        subtitle <- paste0(subtitle, i, ": ", unique(d[[i]]), "\n")
      }
      
      # Make plot
      p <- ggplot(data = d, aes(x = AAR, y = get(.val)))
      
      if(.newoutlier %in% names(od)){
        od[, label := factor(fcase(get(.newoutlier) == 0, "Previous outlier",
                                   get(.newoutlier) == 1, "New outlier"),
                             levels = c("Previous outlier", "New outlier"))]
        p <- p + 
          geom_point(data = od, aes(color = label), size = 5) + 
          scale_color_manual(values = c("blue", "red")) + 
          guides(color = guide_legend(title = NULL)) +
          geom_point()
      } else {
        p <- p + 
          geom_point(data = od, color = "red", size = 5) + 
          geom_point()
      }
      
      if(nrow(ld) > 0){
        p <- p + 
          geom_line(data = ld, aes(y = get(.val), group = 1))
      }
      
      p <- p +
        ggtext::geom_richtext(aes(label = round(get(.teller),0), y = y_middle),
                              hjust = 0.5, angle = 90, alpha = 0.8, size = 8/.pt) +
        ggh4x::force_panelsizes(cols = unit(8, "cm"),
                                rows = unit(5, "cm")) + 
        labs(y = ylab,
             caption = caption,
             subtitle = subtitle) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 
      
      # Save plot
      if(file.exists(savepath) & !overwrite){
        cat("\n", basename(savepath), "already exists")
      } else {
        pdf(savepath, width = 18, height = 12)
        for(i in 1:n_pages){
          print(p +
                  ggforce::facet_wrap_paginate(bycols,
                                               labeller = labeller(.multi_line = F),
                                               scales = "free_y",
                                               ncol = 5,
                                               nrow = 5,
                                               page = i))
        }
        dev.off()
        cat(paste0("\n...", filename))
      }
    }
  }
}

#' TimelineBydel
#' 
#' Plots timelines for each bydel, with the weighted mean timeline for the bydel and kommune superimposed. 
#' Used to check the validity of data on bydel level. The mean timeline for bydel should approximately match 
#' the timeline for kommune. 
#'
#' @param data 
#' @param profileyear 
#' @param overwrite 
#'
#' @return
#' @export
#'
#' @examples
TimelineBydel <- function(data = dfnew_flag,
                          profileyear = PROFILEYEAR,
                          overwrite = FALSE){

  savebase <- .getPlotSaveBase(profileyear = profileyear, kubename = .GetKubename(data), savefolder = "TL")
  filenamebase <- .getPlotFilenameBase(kubename = .GetKubename(data), datetag = .GetKubedatetag(data), "TL")  
  
  .IdentifyColumns(data)
  .val <- data.table::fcase("MEIS" %in% .vals1, "MEIS",
                            "RATE" %in% .vals1, "RATE",
                            "SMR" %in% .vals1, "SMR",
                            "MALTALL" %in% .vals1, "MALTALL",
                            default = NA)
  data <- data[!is.na(get(.val))]
  data[, KOMMUNE := data.table::fcase(grepl("^301", GEO), "Oslo",
                                   grepl("^1103", GEO), "Stavanger",
                                   grepl("^4601", GEO), "Bergen",
                                   grepl("^5001", GEO), "Trondheim",
                                   default = "nonrelevant")]
  data <- data[KOMMUNE != "nonrelevant"]
  
  bycols <- c("KOMMUNE", stringr::str_subset(.dims1, "\\bGEO\\b", negate = T))
  
  # Generate filter to save as multiple files with max 4 (x 4) panels per page
  facets <- stringr::str_subset(bycols, "\\bKOMMUNE\\b|\\bAAR\\b", negate = TRUE)
  filedims <- character()
  filedims <- c(filedims, .findPlotSubset(d = data, b = facets, s = 5))
  if(length(filedims > 0)){
    facets <- stringr::str_subset(facets, 
                                  stringr::str_c("^", filedims, "$", collapse = "|"),
                                  negate = TRUE)
  }
  filter <- .findPlotFilter(data, filedims)
  # Add rows for faceting in plot
  data[, rows := interaction(mget(facets))]
  
  # Split data into bydel/kommune
  d <- data[GEO > 9999]
  d[, n_geo := .N, by = c("GEO", filedims, "rows")]
  kd <- data[GEO %in% c(301, 1103, 4601, 5001) & AAR %in% unique(d$AAR)]
  
  # estimate weighted mean .val for kommune and bydel
  kg <- collapse::GRP(kd, c(bycols, "rows"))
  kw <- kd$WEIGHTS
  kd <- collapse::fmutate(kg[["groups"]],
                          y = fmean(kd[[.val]], w = kw, g = kg))
  kd[, type := "Kommune"]
  
  # estimate weighted mean .val for bydel
  bg <- collapse::GRP(d, c(bycols, "rows"))
  bw <- d$WEIGHTS
  bd <- collapse::fmutate(bg[["groups"]],
                          y = fmean(d[[.val]], w = bw, g = bg))
  bd[, type := "Vektet bydel"]
  
  # Combine trend-data, and remove trends with <= 1 observation
  trends <- data.table::rbindlist(list(kd, bd))
  trends[, N := .N, by = c("KOMMUNE", filedims, "rows", "type")]
  trends <- trends[N > 1]
  
  # Generate global plot elements
  caption <- paste0("Plots grouped by: ", stringr::str_c(facets, collapse = ", "))
  ylab <- .val
  title <- paste0("File: ", attributes(data)$Filename, ", Date: ", Sys.Date())
  plotvar <- paste0("Variable plotted: ", ylab)
  plotdims <- .allcombs(d, c("KOMMUNE", "rows"))
  n_rows <- nrow(plotdims[, .N, by = rows])
  
  # Generate subsets, filenames, and make/save plot.
  for(i in filter){
    
    pd <- d[eval(parse(text = i))]
    td <- trends[eval(parse(text = i))]
    
    if(nrow(pd) > 0){
    # Dynamically generate filename, savepath, and varying plot elements
    if(i == "TRUE"){
      name <- "_alle.png"
    } else {
      name <- character()
      for(i in filedims){
        name <- paste0(name, "_", unique(pd[[i]]))
      }
      name <- paste0(name, ".png")
    }
    filename <- paste0(filenamebase, name)
    savepath <- file.path(savebase, filename)
    
    subtitle <- plotvar
    for(i in filedims){
      subtitle <- paste0(subtitle, "\n", i, ": ", unique(pd[[i]]))
    }

    # Generate plot
    p <- ggplot(plotdims) + 
      facet_grid(cols = vars(KOMMUNE),
                 rows = vars(rows),
                 switch = "y",
                 scales = "free") + 
      labs(title = title,
           subtitle = subtitle,
           caption = caption,
           y = ylab) + 
      geom_line(data = td,
                aes(x = AAR, y = y, color = type, group = type), 
                linewidth = 1.5) + 
      scale_color_manual(values = c("red", "blue")) + 
      geom_line(data = pd,
                aes(x = AAR, y = get(.val), group = GEO), linetype = 2) + 
      geom_point(data = pd,
                 aes(x = AAR, y = get(.val)),
                 size = 3, shape = 1) +
      guides(color = guide_legend(title = NULL)) + 
      theme(legend.position = "top",
            axis.text.x = element_text(angle = 90, vjust = 0.5))
      
    # Save plot
    .saveTimeLine(file = savepath, 
                  plot = p, 
                  rows = n_rows, 
                  overwrite = overwrite)
    }
  }
}

# Helper functions

#' Find subset of plots 
#' 
#' The algorithm select the combination of dimensions yielding the maximum number of panels per page, but less than s
#'
#' @param d dataset
#' @param b all bycols
#' @param s maximum number of panels per page
.findPlotSubset <- function(d,
                            b,
                            s){
  
  orgstrata <- nrow(d[, .N, by = b])
  
  if(orgstrata <= s){
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
  
  # Generate a table with all combinations of dimensions
  combs <- data.table()
  for(i in seq_along(b)){
      x <- data.table(base::t(utils::combn(b, i)))
      colnames(x) <- paste0("dim", 1:i)
      combs <- data.table::rbindlist(list(combs, x), fill = TRUE)
    }
  
  # Add columsn showing n levels for each dimension
  for(j in seq_along(b)){
      new <- paste0("n", j)
      old <- paste0("dim", j)
      combs[ref, (new) := i.n, on = setNames("dim", old)]
      }
  
  # Replace NA with 1 and calculate n files per combination
  data.table::setnafill(combs, fill = 1, cols = grep("^n", names(combs)))
  combs[, files := Reduce("*", .SD), .SDcols = patterns("^n")]
  
  # Calculate panels per page
  incdims <- combs[, do.call(paste, c(.SD, sep = "|")), .SDcols = patterns("^dim")]
  nondims <- list()
  for(i in seq_along(incdims)){
    nondims[[i]] <- str_subset(b, incdims[i], negate = T)
  }
  nondims <- lapply(nondims, function(x) ref[dim %in% x, n])
  nondims <- as.integer(lapply(nondims, function(x) if(length(x) > 0) Reduce("*", x) else 1))
  combs[, panels := nondims]
  
  # Select optimal combination
  optimal <- combs[panels <= s][panels == max(panels)]
  if(nrow(optimal > 1)){
    optimal <- optimal[1]
  }
  v <- unlist(optimal[, .SD, .SDcols = patterns("^dim")], use.names = F)
  v <- v[!is.na(v)]
  v
}

#' .findPlotFilter
#' 
#' Helper function to filter subset for plotting to different files
#'
.findPlotFilter <- function(data,
                            dims){
  if(length(dims) == 0){
    filter <- "TRUE"
  } else {
    subsets <- GRP(data, dims)[["groups"]]
    cols <- names(subsets)
    for(i in cols){
      subsets[, (i) := paste0(i, "=='", get(i), "'")]
    }
    filter <- subsets[, filter := do.call(paste, c(.SD, sep = " & ")), .SDcols = cols][, (filter)]
  }
  
  filter
}

# .SaveBoxplot
.saveBoxPlot <- function(file,
                         plot,
                         rows = n_rows,
                         overwrite = FALSE){
  
  if(file.exists(file) & !overwrite){
    cat("\n", basename(file), "already exists")
  } else {
    ggsave(filename = file,
           plot = plot, 
           device = "png", 
           dpi = 300,
           width = 45,
           height = rows*6 + 10,
           units = "cm")
    cat("\nSave file: ", basename(file))
  } 
}

.saveTimeLine <- function(file,
                          plot,
                          rows = n_rows,
                          overwrite = FALSE){
  if(file.exists(file) & !overwrite){
    cat("\n", basename(file), "already exists")
  } else {
    ggsave(filename = file,
           plot = plot, 
           device = "png", 
           dpi = 300,
           width = 37,
           height = rows*6 + 10,
           units = "cm")
    cat("\nSave file: ", basename(file))
  } 
}

.getPlotSaveBase <- function(profileyear,
                             kubename,
                             savefolder){
  
  .CreateFolders(profileyear,kubename)
  
  file.path("F:",
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
}

.getPlotFilenameBase <- function(kubename,
                                 datetag,
                                 savefolder){
  paste(kubename, datetag, savefolder, sep = "_")
}
