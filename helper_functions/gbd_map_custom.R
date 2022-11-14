################################################################################
## Author:
## Date Created: 13 July 2011
## Customized for MenAfriVac coverage paper by
## Description: mapping function that takes the following arguments:
##
##  REQUIRED INPUT:
##    data        ->  A data frame or table with two variables, 'location_id' and 'mapvar'.
##    limits      ->  A vector that defines the bins for the map. This should be
##                    the minimum value, each of the break points in ascending order
##                    and then the maximum value. By default, the bins are currently
##                    assigned such that a value equal to a breakpoint is included in
##                    the bin where that breakpoint is the lower bound.
##
##  OPTIONAL INPUT:
##    just_menbelt->  Logical. Creates map for meningitis belt only. Defaults to F.
##    legend      ->  Logical. Prints map with or without legend. Defaults to map with legend.
##    labels      ->  Vector of the labels to use in the legend for each group.
##                    If this is not specified, these are assigned automatically
##                    based on the provided values in 'limits'.
##    col         ->  Either a string specifying the name of a color palette from
##                    ColorBrewer () or a vector of colors.
##    col.reverse ->  Logical. Should the order of the colors be reversed (this
##                    may be useful when specifying a palette from ColorBrewer)
##    na.color    ->  The color to be used for countries not in the dataset
##                    (defaults to white)
##    title       ->  A string specifying the title (defaults to "")
##    title.cex   ->  Multiplier for the font size of the title
##    fname       ->  The filename for saving the map. Currently supports '.tif'
##                    '.eps', and '.pdf'. If nothing is specifed, the map is
##                    printed to screen so long as there is not a pdf device
##                    already open (if there is a pdf device already open, the
##                    map is saved to this device; this is to allow for making
##                    pdfs with multiple figures using the onefile=T option in
##                    the pdf function)
##    legend.title -> The title for the legend
##    legend.columns -> The number of columns in the lengend. Defaults to 1 for
##                    7 or less groups and 2 otherwise.
##    legend.cex   -> Multiplier for the size of the legend
##    legend.shift -> Shift the legend (vector of size 2: horizontal shift and
##                    vertical shift)
################################################################################


gbd_map <- function(data, 
		    just_menbelt = FALSE, # Crop to only the meningitis belt
                    limits, # No inset or sub_nat argument
                    color_legend = TRUE, 
                    intro_legend = FALSE, # custom legend arguments for men belt maps
                    labels=NULL,
                    col="RdYlBu", col.reverse=FALSE, na.color = "white",
                    title="", fname=NULL,
                    legend.title=NULL, legend.columns = NULL, legend.cex=1, legend.shift=c(0,0)) {
  
  ## load libraries
  library(RColorBrewer)
  library(maptools)
  library(raster)
  require(sp)
  
  ## test for consistency & give warnings 
  if (length(limits)-1 != length(labels) & !is.null(labels)) stop("length(limits) must equal length(labels) + 1")
  if (length(limits)-1 != length(col) & length(col)>1) stop("number of colors does not match number of groups")
  if (!"location_id" %in% names(data) | !"mapvar" %in% names(data)) stop("data is missing location_id or mapvar variables")

  data <- as.data.frame(data)
  data$loc_id <- data$location_id
  data$location_id <- NULL
  
  ## load shapefile and prep data and insest data
  load(paste("/FILEPATH/2019GBD_PREPPED_DATA_NOSUBMAP.RData",sep=""))
  # crop to menbelt only
  if(just_menbelt) {
    nosubmap <- crop(nosubmap, extent(-20, 50, -15, 30))
  }
  map <- copy(nosubmap)
  main_data <- merge(data, map@data[,c("loc_id", "ID")], by="loc_id", sort= TRUE, all.y=T)
  kmlPolygon(border=.01)
  
  ## assign colors and patterns
  main_data$color <- NA
  n <- length(limits) - 1
  
  if (length(col)==1) {
    if (brewer.pal.info[col,1] >= n) mapcolors <- brewer.pal(n, col)
    else mapcolors <- colorRampPalette(brewer.pal(brewer.pal.info[col,1], col))(n)
  } else {
    mapcolors <- col
  }
  if (col.reverse) mapcolors <- mapcolors[n:1]
  
  for (g in 1:n) {
    ii <- (main_data$mapvar >= limits[g] & main_data$mapvar <= limits[g+1])
    main_data$color[ii] <- mapcolors[g]
  }
  
  main_data$color[is.na(main_data$color)] <- na.color
  main_data$pattern[is.na(main_data$pattern)] <- 1
  main_data$density <- ifelse(main_data$pattern==1, 200, 0)

  ####disputed locations
  load(paste(j,"/FILEPATH/2019GBD_PREPPED_DISPUTED_DATA.RData",sep=""))
  if(just_menbelt) {
    disputed <- crop(disputed, extent(-20, 50, -15, 30))
  }
  disputed_data <- merge(data, disputed@data[,c("ADM0_NAME", "ID", "DISP_AREA")], all.y=T)
  kmlPolygon(border=.01)
  # crop to menbelt only


  ## generate labels if necessary
  if (is.null(labels)) {
    for (g in 1:n) {
      labels <- c(labels, paste(limits[g], " to ", limits[g+1]))
    }
  }
  
  # graphics.off()
  # par("mar")
  # par(mar=c(1,1,1,1))

  ###plot only mainmap
  # These settings are necessary to stop plots from getting cut off in cowplot or ggarrange
  par(lwd=0.1, mai=c(.001,.001,.001,.001),
      xpd = NA, # switch off clipping, necessary to always see axis labels
      bg = "transparent", # switch off background to avoid obscuring adjacent plots
      oma = c(2, 2, 0, 0) # move plot to the right and up
  )
  plot(map, col=main_data$color[order(main_data$ID)], cex=0.5)
  plot(map, density=main_data$density[order(main_data$ID)], add=T)
  plot(disputed,border="white", lty = 1, cex=0.5, lwd=0.3, add= T)
  plot(disputed,border="grey29", lty = 2, cex=0.5, lwd=0.2, add=T)

  ## plot legend
  if (is.null(legend.columns)) legend.columns <- 1
  if (is.null(legend.shift)) legend.shift <- c(0,0)
  if (color_legend==TRUE) {
      legend(0+legend.shift[1], 0+legend.shift[2], labels, fill=mapcolors, border=mapcolors,
             cex=0.75*legend.cex, pt.cex = 2, bg="white", bty = "n", title=legend.title, ncol = legend.columns)
  }
  if (intro_legend==TRUE) {
      legend(0+legend.shift[1], 0+legend.shift[2], c('Not introduced', 'Not included in modeling'), 
                           fill = NA, 
                           border = "black", 
                           density = c(0, 200),
                           cex=0.75*legend.cex, pt.cex = 2,
                           ncol=2, bty = "n")
  }
  
  title(main=title, cex=0.95)
  
  # save the plot
  my_plot <- recordPlot()
 
  return(my_plot)
}