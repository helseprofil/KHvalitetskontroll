# Load list of all dimensions, and make hidden object
source("https://raw.githubusercontent.com/helseprofil/misc/main/alldimensions.R") 
.ALL_DIMENSIONS <- ALL_DIMENSIONS
rm(ALL_DIMENSIONS)

# Set ggplot theme and color palette
ggplot2::theme_set(theme_bw())
ggplot2::theme_update(legend.position = "top", 
                      panel.grid.minor = element_blank(),
                      text = element_text(color = "black"),
                      plot.margin = margin(t = 1, b = 1, r = 1, unit = "cm"))


# Set global options
PROFILEYEAR <- 2024  # For saving in correct folder
DUMPS <- c("dfnew_flag", "dfold_flag", "compareKUBE") # Default is to create all file dumps
.currentgeo <- 2024 # used for recoding when reading files
.georecode <- .readGeoRecode()
.popinfo <- .readPopInfo()
