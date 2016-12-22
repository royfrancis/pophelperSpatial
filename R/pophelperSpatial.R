# Begin ------------------------------------------------------------------------

# pophelperSpatial v1.0.0
# Functions
# 22-Dec-2016

#check packages
pkgs <- c("akima","fields","grid","gridExtra","ggplot2","gtable","PBSmapping","spatstat","tidyr")
if(any(!pkgs %in% installed.packages()))
{
  warning(paste0("Package(s) '",paste0(pkgs[which(!pkgs %in% installed.packages())],collapse=", "),"' is not installed."))
}
rm(pkgs)

# getColours -------------------------------------------------------------------

#' @title Internal: Generate colours based on number of K
#' @description Internal: Generate colours based on number of K.
#' @param k A numeric indicating the number of colours required
#' @return Returns a character vector of k colours in hexadecimal format
#' @details Colours 1 to 12 are custom unique colours. Colours beyond 15 are generated from colour ramp \code{rich.colors()} from package \code{gplots}.
# @export
#'
getColors <- getColours <- function(k)
{
  if(length(k) > 1) stop("getColours: Input has than one value. Argument k must be a single numeric or integer.")
  if(!is.integer(k) && !is.numeric(k) ) stop("getColours: Input is not an integer. Argument k must be a single numeric or integer.")
  k <- as.integer(k)
  col1 <- c("#2121D9", "#9999FF", "#DF0101", "#04B404", "#FFFB23", "#FF9326", "#A945FF", "#0089B2", "#B26314", "#610B5E", "#FE2E9A", "#BFF217")
  # col1 <- c("#1D72F5","#DF0101","#77CE61",
  #           "#FF9326","#A945FF","#0089B2",
  #           "#FDF060","#FFA6B2","#BFF217",
  #           "#60D5FD","#CC1577","#F2B950",
  #           "#7FB21D","#EC496F","#326397",
  #           "#B26314","#027368","#A4A4A4",
  #           "#610B5E")
  if(k <= 12) return(col1[1:k])
  if(k > 12)
  {
    cr <- colorRampPalette(colors=c("#000040FF", "#00004FFF", "#000060FF", "#000074FF", "#000088FF", "#00009DFF", "#0000B2FF",
                                    "#0000C6FF", "#000CD8FF", "#0022E7FF", "#0037F3FF", "#004BFBFF", "#005EFFFF", "#0070FEFF",
                                    "#0081F8FF", "#0091EEFF", "#00A0E0FF", "#00ADCFFF", "#00BABCFF", "#00C6A7FF", "#01D092FF",
                                    "#02DA7EFF", "#03E26AFF", "#07E958FF", "#0EF047FF", "#1BF539FF", "#31F92CFF", "#54FC22FF",
                                    "#80FE1AFF", "#ABFF13FF", "#CEFF0EFF", "#E4FE0AFF", "#F1FB07FF", "#F8F805FF", "#FCF403FF",
                                    "#FDEE02FF", "#FEE801FF", "#FFE001FF", "#FFD801FF", "#FFCE00FF", "#FFC300FF", "#FFB800FF",
                                    "#FFAB00FF", "#FF9D00FF", "#FF8E00FF", "#FF7E00FF", "#FF6D00FF", "#FF5B00FF", "#FF4700FF",
                                    "#FF3300FF"),space="rgb")
    return(cr(k))
  }
}

# getRowsAndCols ---------------------------------------------------------

#' @title Internal: Determine rows and columns for arbitrary number of plots
#' @description Internal: Determine rows and columns for figures from arbitrary number of plots
#' @param numplots A numeric indicating the number of plots available for plot.
#' @param nrow A numeric indicating number of rows in the plot.
#' @param ncol A numeric indicating number of columns in the plot.
#' @return A 2 value vector wih first value denoting row and second value as column.
# @export
#'
getRowsAndCols <- function(numplots=NA,nrow=NA,ncol=NA)
{
  if(is.na(numplots)) stop("getRowsAndCols: No input for number of plots.")

  #if nrow and ncol are both NA
  if(is.na(nrow) && is.na(ncol))
  {
    if(numplots==1) return(c(1,1))
    if(numplots==2) return(c(1,2))
    if(numplots==3) return(c(1,3))
    if(numplots==4) return(c(2,2))
    if(numplots==5) return(c(2,3))
    if(numplots==6) return(c(2,3))
    if(numplots==7) return(c(2,4))
    if(numplots==8) return(c(2,4))
    if(numplots==9) return(c(3,3))
    if(numplots==10) return(c(2,5))
    if(numplots==11) return(c(3,4))
    if(numplots==12) return(c(3,4))
    if(numplots==13) return(c(4,4))
    if(numplots==14) return(c(4,4))
    if(numplots==15) return(c(5,3))
    if(numplots==16) return(c(4,4))
    if(numplots==17) return(c(5,4))
    if(numplots==18) return(c(6,3))
    if(numplots==19) return(c(4,5))
    if(numplots==20) return(c(4,5))
    if(numplots>20) stop("getRowsAndCols: Number of clusters > 20. Specify number of rows and columns for figures manually using the option nrow and ncol arguments.")
  }

  #if nrow is NA and ncol is not NA
  if(is.na(nrow) && !is.na(ncol))
  {
    return(c(ceiling(numplots/ncol),ncol))
  }

  #if nrow is not NA and ncol is NA
  if(!is.na(nrow) && is.na(ncol))
  {
    return(c(nrow,ceiling(numplots/nrow)))
  }

  #if nrow and ncol are both not NA
  if(!is.na(nrow) && !is.na(ncol))
  {
    return(c(nrow,ncol))
  }
}

# llToUtm ----------------------------------------------------------------------

#' @title Internal: Find UTM zone from a latitude and longitude
#' @description Internal: Find UTM zone from a given latitude and longitude
#' @param lat The latitude in decimals. Southern hemisphere must be negative.
#' @param long The longitude in decimals
#' @details For a given latitude and longitude, the UTM zone must be determined
#' prior to UTM coordinate conversion.
#' @return A list of two values are returned namely UTMZone and Hemisphere.
# @export
#'
llToUtmzone <- function(lat,long)
{
  lat <- as.numeric(lat)
  if(is.na(lat)) stop("llToUtmzone: Non-numeric input to latitude.")
  long <- as.numeric(long)
  if(is.na(long)) stop("llToUtmzone: Non-numeric input to longitude.")

  #basic UTM Zone calculation
  utm=floor((long + 180)/6) + 1
  if( lat >= 56.0 && lat < 64.0 && long >= 3.0 && long < 12.0 ) utm=32

  #Special zones for Svalbard
  if( lat >= 72.0 && lat < 84.0 )
  {
    if( long >= 0.0  && long <  9.0 ) utm=31
    if( long >= 9.0  && long < 21.0 ) utm=33
    if( long >= 21.0 && long < 33.0 ) utm=35
    if( long >= 33.0 && long < 42.0 ) utm=37
  }

  #Hemisphere calculation
  if(lat>0) hem<-"N"
  if(lat<0) hem<-"S"

  return(list(UTMZone=utm,Hemisphere=hem))
}

# plotQInterpolate ----------------------------------------------------------

#' @title Spatially interpolate a qlist using xy coordinates
#' @description Spatially interpolate a qlist using xy coordinates
#' @param qlist A qlist (list of dataframes) with one element. An output from \code{\link{readQ}} with one element.
#' If a list with more than one element is provided, only the first run is used along with a warning.
#' @param coords A dataframe. The number of rows must be equal to the number of
#' individuals in qlist. The dataframe must have 2 columns in the
#' order: x (latitude) and then y (longitude). Coordinates must be in standard
#' longitude latitude (LL) decimals. eg: 21.0232, 43.0232
#' @param method A character indicating the method employed for interpolation. Options are "bilinear",
#' "bicubic", "krig" (Kriging), "idw" (Inverse distance weighting) or "nn" (nearest
#' neighbour). See Details for more information.
#' @param coordsratio A numeric indicating ratio between x and y axes. If set to 1,
#' then 1 unit on x axis is the same as 1 unit on y axis.
#' @param duplicate A character indicating how to deal with duplicate spatial locations. This is only
#' applicable to 'bilinear' and 'bicubic' methods. Options are "error" (error message
#' if duplicate points), "strip" (remove all duplicate points), "mean" (mean of
#' duplicate points), "median" (median of duplicate points).
#' @param idwpower A numeric indicating the power of inverse distance weighting if method is set to "idw". Default set to 2.
#' @param clusters A numeric or numeric vector indicating the clusters to plot. If NA, all clusters are plotted. For ex.
#' If set to 2, cluster 2 is plotted. If set to 2:4, clusters 2, 3 and 4 are plotted.
#' If set to c(1,4,5), clusters 1, 4 and 5 are plotted.
#' @param gridsize A numeric indicating the size of the image grid on which interpolation is to be carried
#' out. Set to 60 by default. Higher values produces less pixelated grids, but more
#' computationally intensive.
#' @param imgoutput A character. To plot each cluster as a seperate figure, set to "sep". To plot
#' multiple clusters in a single figure, set to "join". If number of clusters is less
#' than 2, then automatically set to "sep" with a warning.
#' @param colours A vector of 2 or more colours. R colour names or hexadecimal values. Set to
#' 9 value 'Blues' palette from RColorBrewer by default.
#' @param nrow A numeric indicating the number of rows of plots in a joined plot. Determined automatically
#' if number of plots <20 and \code{nrow=NA}.
#' @param ncol A numeric indicating the number of columns of plots in a joined plot. Determined automatically
#' if number of plots <20 and \code{ncol=NA}.
#' @param exportplot A logical. If set to FALSE, no image is exported.
#' @param imgtype A character indicating the export format for figures. Options are "png", "jpeg" or "pdf".
#' If pdf, height and width must be in inches and res argument is ignored (set to 300).
#' @param height A numeric indicating the height of export figure in cm unless units are changed.
#' @param width A numeric indicating the width of export figure in cm unless units are changed.
#' @param units A character indicating units of measurement for figure dimensions. Set to 'cm' by default.
#' @param res A numeric indicating the pixel resolution of the export image. Set to 200 by default.
#' @param showaxis A logical. If TRUE, then axis text, axis ticks and plot border are plotted.
#' @param addpoints A logical. If TRUE, then sample coordinates are overplotted on interpolated grid.
#' @param pointcol Colour of sample points. An R colour or hexadecimal value.
#' @param pointtype The shape/pch of sample points. A numeric or a character. Eg: 18,19,21,"x" etc.
#' @param pointsize The numeric indicating size of sample points. A number usually 0.4,0.8,1,3 etc.
#' @param pointalpha A numeric between 0 and 1 indicating transparency of points. Default set to 1.
#' @param legend A logical. If TRUE, the legend for the colours is plotted.
#' @param legendpos A character of 2-value numeric vector indicating the position of the legend. If "right","left","top" or "bottom", then,
#' legend is plotted outside the plot area. To plot inside plot area use a 2 vale vector.
#' If a vector like c(1,1), first value denotes x-axis from 0 to 1 and second value
#' denotes y-axis from 0 to 1. For ex. to plot in bottom left corner, use c(0,0).
#' @param legendjust The x and y axis justification of the legend. A 2 value vector or one of "right","left","top" or "bottom".
#' @param legendkeysize A numeric indicating the size of the legend key in cm. Usually values like 0.5,0.7,1.2 etc.
#' @param legendtextsize A numeric indicating the size of the text in the legend.
#' @param plotcolour A character R colour or hexadecimal denoting colour for plot elements. Defaults to "grey30".
#' @param dataout A logical. If set to TRUE, a list of one or more \code{ggplot} gtable elements are returned.
#' This output can be modified using \code{ggplot} themes() for more figure control if required.
#' @return If \code{dataout=TRUE}, a list of one or more \code{ggplot} gtable output is returned for more theme
#' control if required.
#' @details The "bilinear", "bicubic", "idw" and "nn" are essentially direct interpolation
#' between spatial points. The "krig" option is predictive rather than direct interpolation.
#' The kriging function is same function provided by the TESS authors in their R script.
#' The function uses great circle distances \code{rdist.earth()} from \code{fields} package to
#' determine theta. Therefore coordinates must be in LL and not UTM.\cr
#' \cr
#' For more details of methods, see R package \code{akima} function \code{interp} for "bilinear"
#' and "bicubic" methods, see R package \code{fields} function \code{Krig} for "krig" method, see
#' R package \code{spatstat} function \code{idw} for "idw" and function \code{nnmark} for "nn" method.
#' The model for "krig" is automatically determined and may produce warning messages if
#' the GCV algorithm does not converge optimally. This shouldn't be an issue for exploratory
#' analyses. All methods require full coordinate data. No missing data allowed in coords.\cr
#' @examples
#' \dontrun{
#' # tess files
#' q <- readQ(list.files(path=system.file("files/tess", package="pophelperSpatial"), full.names=T))[1]
#' xy <- read.delim(system.file("files/coords75.txt", package="pophelperSpatial"), header=F)
#' plotQInterpolate(qlist=q, coords=xy)
#'
#' # adjust dimensions and legend size as required
#' plotQInterpolate(qlist=q, coords=xy, height=15, width=22, legendkeysize=0.4)
#' # finer grid # very slow
#' plotQInterpolate(qlist=q, coords=xy, height=15, width=22, legendkeysize=0.4, gridsize=100)
#' }
#' @import akima
#' @import fields
#' @import gridExtra
#' @import spatstat
#' @export
#'
plotQInterpolate <- function(qlist=NULL, coords=NULL, method="krig", coordsratio=NA,
                               duplicate="mean", idwpower=2, clusters=NA, gridsize=60,
                               imgoutput="join", colours=NA, nrow=NA, ncol=NA,plotcolour="grey30",exportplot=TRUE,
                               imgtype="png", height=NA, width=NA, units="cm", res=200,
                               showaxis=FALSE, addpoints=TRUE, pointcol="grey10", pointtype="+",pointsize=4, pointalpha=1,
                               legend=TRUE, legendpos=c(0.99,0.99), legendjust=c(1,1),legendkeysize=NA, legendtextsize=NA,
                               dataout=FALSE)
{
  #basic checks
  if(is.null(qlist) | length(qlist)==0) stop("plotQInterpolate: No content in qlist.")
  if((class(qlist) != "list")) stop("plotQInterpolate: Argument 'qlist' must be a list datatype.")
  if(!all(sapply(qlist, is.list))) stop("plotQInterpolate: List element is not a list datatype.")
  if(is.null(coords) | length(coords)==0) stop("plotQInterpolate: No content in coords.")
  method <- tolower(method)
  if(method != "bilinear" && method != "bicubic" && method != "krig" && method != "idw" && method != "nn") stop("plotQInterpolate: Argument 'method' set incorrectly. Set as 'bilinear', bicubic', 'krig', 'idw' or 'nn'.")
  imgoutput <- tolower(imgoutput)
  if(imgoutput != "sep" && imgoutput != "join") stop("plotQInterpolate: Argument 'imgoutput' set incorrectly. Set as 'sep' to plot each cluster seperately. Set as 'join' to plot multiple clusters in one figure.")
  imgtype <- tolower(imgtype)
  if(imgtype!="png" && imgtype != "pdf" && imgtype != "jpeg") stop("plotQInterpolate: Argument 'imgtype' set incorrectly. Set as 'png', 'jpeg' or 'pdf'.")
  duplicate <- tolower(duplicate)
  if(duplicate != "error" && duplicate != "strip" && duplicate != "mean" && duplicate != "median") stop("plotQInterpolate: Argument 'duplicate' not set correctly. Set as 'error','strip','mean' or 'median'.")
  if(!is.logical(exportplot)) stop("plotQInterpolate: Argument 'exportplot' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(showaxis)) stop("plotQInterpolate: Argument 'showaxis' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(addpoints)) stop("plotQInterpolate: Argument 'addpoints' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(legend)) stop("plotQInterpolate: Argument 'legend' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(dataout)) stop("plotQInterpolate: Argument 'dataout' set incorrectly. Set as TRUE or FALSE.")

  #declare colours
  if(all(is.na(colours))) colours <- rev(c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6","#4292C6", "#2171B5", "#08519C", "#08306B"))

  #READ DATA FILES AND CHECK
  if(length(qlist)>1) warning("plotQInterpolate: Argument 'qlist' has more than one element. Using the first element only.")
  #sample name
  fname <- names(qlist)[1]
  if(is.null(fname)) fname <- paste0("sample",1)

  #check files
  df1 <- qlist[[1]]
  if(!is.data.frame(df1)) stop(paste0("plotQInterpolate: List item ", fname," is not a data.frame object."))
  if(!any(sapply(df1, is.numeric))) stop(paste0("plotQInterpolate: List item ", fname," has non-numeric columns."))

  #READ COORDS AND CHECK
  if(!is.data.frame(coords)) stop("plotQInterpolate: Argument 'coords' is not a data.frame datatype.")
  #coords check
  if(!any(sapply(coords, is.numeric))) stop("plotQInterpolate: Non numeric content in coords.")
  if(any(is.na(coords))) stop("plotQInterpolate: Missing data detected in coords. Methods cannot handle missing coordinate data.")
  colnames(coords) <- c("X","Y")

  #data coords length check
  if(nrow(df1) != nrow(coords)) stop("plotQInterpolate: Number of individuals not equal to number of coordinates.")

  #determine clusters to plot
  if(all(is.na(clusters))) flen <- 1:length(colnames(df1))
  if(length(clusters)==1) {if(is.numeric(clusters)) flen <- clusters:clusters}
  if(length(clusters) > 1) flen <- clusters
  if(!is.numeric(clusters) && !is.na(clusters)) stop("plotQInterpolate: Argument clusters in non-numeric.")

  #determine if atleast 2 plots are available for joined option
  if(length(flen) < 2 && imgoutput=="join")
  {
    imgoutput <- "sep"
    message("Less than 2 plots available for joined. Argument imgoutput changed to 'sep'.")
  }

  #get dimensions for sep figures
  height1 <- height
  width1 <- width
  if(is.na(height)) height1 <- 8
  if(is.na(width)) width1 <- 8
  if(imgtype=="pdf" && any(!is.na(height) | !is.na(width))) warning("plotQInterpolate: Height and width will be taken as inches if argument imgtype is set to pdf.")
  if(imgtype=="pdf" && all(is.na(height) && is.na(width)))
  {
    height1 <- round(height1*0.394,2)
    width1 <- round(width1*0.394,2)
    units <- "in"
  }

  plist <- vector("list", length(flen))
  datalist <- vector("list", length(flen))
  #start loop
  i <- 1
  while(i <= length(flen))
  {
    j <- flen[i]
    plottitle <- colnames(df1)[j]

    #bicubic and bilinear methods
    if(method=="bilinear" | method=="bicubic")
    {
      X <- coords$X
      Y <- coords$Y
      interpX <- seq(min(X, na.rm=T), max(X, na.rm=T), le=gridsize)
      interpY <- seq(min(Y, na.rm=T), max(Y, na.rm=T), le=gridsize)
      if(method=="bilinear") tempimg <- akima::interp(X, Y, df1[ ,j], xo=interpX, yo=interpY, duplicate=duplicate,linear=TRUE)
      if(method=="bicubic") tempimg <- akima::interp(X, Y, df1[ ,j], xo=interpX, yo=interpY, duplicate=duplicate,linear=FALSE)
      rm(X,Y,interpX,interpY)
    }

    #kriging method
    if(method=="krig")
    {
      sc <- mean(fields::rdist.earth(data.frame(Y=coords$Y, X=coords$X, stringsAsFactors=FALSE)), miles=FALSE)
      fit <- fields::Krig(x=coords, Y=df1[,j], theta=sc, m=1, Distance="rdist.earth", na.rm=TRUE)
      #fit <- fields::Krig(x=coords,Y=df1[,j], m=1,na.rm=TRUE)
      tempimg <- fields::predictSurface(fit,nx=gridsize,ny=gridsize)
      #surface(tempimg)
      rm(sc,fit)
    }

    #inverse distance weighting
    if(method=="idw")
    {
      pp1 <- spatstat::as.ppp(coords, c(min(coords$X,na.rm=T),max(coords$X,na.rm=T),min(coords$Y,na.rm=T),max(coords$Y,na.rm=T)))
      pp1$marks <- as.vector(df1[,j])
      pp1$markformat <- "vector"
      pp2 <- spatstat::idw(pp1,power=idwpower,at="pixels",dimyx=c(gridsize,gridsize))
      tempimg <- list(x=pp2$xcol,y=pp2$yrow,z=pp2$v)
      rm(pp1,pp2)
    }

    #nearest neighbour
    if(method=="nn")
    {
      pp1 <- spatstat::as.ppp(coords, c(min(coords$X,na.rm=T),max(coords$X,na.rm=T),min(coords$Y,na.rm=T),max(coords$Y,na.rm=T)))
      pp1$marks <- as.vector(df1[,j])
      pp1$markformat <- "vector"
      nn1 <- spatstat::nnmark(X=pp1,k=1,at="pixels",dimyx=c(gridsize,gridsize))
      tempimg <- list(x=nn1$xcol,y=nn1$yrow,z=nn1$v)
      rm(nn1)
    }

    if(all(is.na(tempimg$z))) stop("plotQInterpolate: NA in output. Method does not work.")

    #expand grid over gridsize
    tempimg1 <- expand.grid(x=tempimg$x, y=tempimg$y)
    tempimg1$z <- as.vector(tempimg$z)
    attr(tempimg1,"out.attrs") <- NULL
    rm(tempimg)

    #store to a list
    datalist[[i]] <- tempimg1
    #name list item
    names(datalist)[i] <- plottitle
    tempimg1$plot <- rep(plottitle,length(tempimg1$z))

    #plot
    p <- ggplot2::ggplot(tempimg1)+
      geom_tile(aes(x=x,y=y,fill=z))+
      scale_x_continuous(expand=c(0, 0))+
      scale_y_continuous(expand=c(0, 0))+
      scale_fill_gradientn(colours=rev(colours),na.value="#FFFFFF00")+
      theme_bw()+labs(x=NULL, y=NULL, title=plottitle)+
      theme(legend.title=element_blank(),plot.title=element_text(colour=plotcolour,hjust=0),
            axis.text=element_text(colour=plotcolour),axis.ticks=element_line(colour=plotcolour),
            panel.border=element_rect(colour=plotcolour),
            legend.justification=legendjust, legend.position=legendpos,
            legend.text=element_text(colour=plotcolour))

    #edit plot conditionally
    if(!is.na(coordsratio)) p <- p + coord_fixed(ratio=coordsratio)
    if(!showaxis) p <- p + theme(axis.text=element_blank(),axis.ticks=element_blank(), panel.border=element_blank())
    if(addpoints) p <- p + geom_point(data=coords,aes(x=X,y=Y),size=pointsize,shape=pointtype,fill=pointcol,colour=pointcol,alpha=pointalpha)
    if(!legend) p <- p + theme(legend.position="none")
    if(!is.na(legendkeysize)) p <- p + theme(legend.key.size=grid::unit(legendkeysize, "cm"))
    if(!is.na(legendtextsize)) p <- p + theme(legend.text=element_text(colour=plotcolour,size=legendtextsize))

    plist[[i]] <- p
    if(exportplot && imgoutput=="sep")
    {
      if(imgtype=="png") png(paste0(fname,"-Interpolation-",method,"-",colnames(df1)[i],".png"),height=height1,width=width1,units=units,res=res,type="cairo")
      if(imgtype=="jpeg") jpeg(paste0(fname,"-Interpolation-",method,"-",colnames(df1)[i],".jpg"),height=height1,width=width1,units=units,res=res, quality=100)
      if(imgtype=="pdf") pdf(paste0(fname,"-Interpolation-",method,"-",colnames(df1)[i],".pdf"),height=height1,width=width1)
      print(p)
      dev.off()
      if(imgtype=="png") cat(paste0(fname,"-Interpolation-",method,"-",colnames(df1)[i],".png exported.\n"))
      if(imgtype=="jpeg") cat(paste0(fname,"-Interpolation-",method,"-",colnames(df1)[i],".jpg exported.\n"))
      if(imgtype=="pdf") cat(paste0(fname,"-Interpolation-",method,"-",colnames(df1)[i],".pdf exported.\n"))
    }
    i=i+1
  }

  if(exportplot && imgoutput=="join")
  {
    #determine rows and cols
    rc <- pophelperSpatial:::getRowsAndCols(length(flen),nrow,ncol)
    nrow1 <- rc[1]
    ncol1 <- rc[2]

    #determine height and width
    height2 <- height
    width2 <- width
    if(is.na(height)) height2 <- height1*nrow1
    if(is.na(width)) width2 <- width1*ncol1

    alist <- c(plist, nrow1, ncol1)
    names(alist) <- c(as.character(flen), "nrow", "ncol")

    if(imgtype=="png") png(paste0(fname, "-Interpolation-",method,"-", length(flen), "Clusters.png"), height=height2, width=width2, res=res, units=units, type="cairo")
    if(imgtype=="jpeg") jpeg(paste0(fname, "-Interpolation-",method,"-", length(flen), "Clusters.jpg"), height=height2, width=width2, res=res, units=units, quality=100)
    if(imgtype=="pdf") pdf(paste0(fname, "-Interpolation-",method,"-", length(flen), "Clusters.pdf"), height=height2, width=width2)

    do.call(gridExtra::grid.arrange, alist)
    dev.off()

    if(imgtype=="png") cat(paste0(fname, "-Interpolation-",method,"-", length(flen), "Clusters.png exported.\n"))
    if(imgtype=="jpeg") cat(paste0(fname, "-Interpolation-",method,"-", length(flen), "Clusters.jpg exported.\n"))
    if(imgtype=="pdf") cat(paste0(fname, "-Interpolation-",method,"-", length(flen), "Clusters.pdf exported.\n"))

  }
  if(dataout) return(plist)
}

# ellipseCI --------------------------------------------------------------------

#' @title Internal: ellipseCI
#' @description Internal: Calculate ellipse for bivariate quantile
#' @param x A numeric vector of x coordinates
#' @param y A numeric vector of y coordinates
#' @param conf A numeric indicating confidence interval
#' @param np A numeric indicaitng the number of points
#' @details  Obtained from Claude J (2008) Morphometrics with R, Springer
#' @return A dataframe with x and y coordinates of the ellipse. Number of
#' rows is equal to argument np.
# @export
#'
ellipseCI <- function(x,y,conf=0.95,np=100)
{
  if(!is.numeric(x)) stop("ellipseCI: Argument 'x' is not numeric.")
  if(!is.numeric(y)) stop("ellipseCI: Argument 'y' is not numeric.")
  if(!is.numeric(conf)) stop("ellipseCI: Argument 'conf' is not numeric.")
  if(!is.numeric(np)) stop("ellipseCI: Argument 'np' is not numeric.")

  centroid <- apply(cbind(x,y),2,mean)
  ang <- seq(0,2*pi,length=np)
  z <- cbind(cos(ang),sin(ang))
  radiuscoef <- qnorm((1-conf)/2, lower.tail=F)
  vcvxy <- var(cbind(x,y))
  r <- cor(x,y)
  M1 <- matrix(c(1,1,-1,1),2,2)
  M2 <- matrix(c(var(x), var(y)),2,2)
  M3 <- matrix(c(1+r, 1-r),2,2, byrow=T)
  ellpar <- M1*sqrt(M2*M3/2)
  t1 <- t(centroid + radiuscoef * ellpar %*% t(z))
  t1 <- as.data.frame(t1,stringsAsFactors=FALSE)
  colnames(t1) <- c("x","y")
  return(t1)
}

# plotQSpatial --------------------------------------------------------------

#' @title Plot runs spatially and colour individuals by max assignment cluster.
#' @description Plot runs spatially and colour individuals by max assignment cluster.
#' @param qlist A qlist (list of dataframes) with one element. An output from \code{\link{readQ}} with one element.
#' @param coords A dataframe. The number of rows must be equal to the number of
#' samples in qlist. The dataframe must have 2 columns in the
#' order: x (latitude) and then y (longitude). Coordinates must be in standard
#' longitude latitude (LL) decimals.
#' @param clustercol A vector of colours for the clusters. R colour names or hexadecimal
#' values. If NA, colours are automatically generated. K 1 to 12 are custom unique
#' colours while K>12 are coloured by function \code{rich.color()}.
#' @param setutm A logical. If TRUE, then LL coordinates are converted to UTM coordinates. The midpoint
#' of the longitude within the dataset is used to determine the UTM zone.
#' @param imgtype A character indicating the export format for figures. Options are "png", "jpeg" or "pdf".
#' If pdf, height and width must be in inches and res argument is ignored (set to 300).
#' @param height A numeric indicating the height of export figure. Default in cm unless units are changed. If \code{imgtype}
#' is pdf, then height must be in inches.
#' @param width A numeric indicating the the width of export figure. Default in cm unless units are changed. If \code{imgtype}
#' is pdf, then height must be in inches.
#' @param units A character indicating the units of measurement for figure dimensions. "cm", "mm" or "in".
#' @param res A numeric indicating the pixel resolution of the export image. Set to 200 by default.
#' @param showaxis A logical indicating if the axis text, axis ticks and plot border are plotted.
#' @param coordsratio A numeric indicating ratio between x and y axes. If set to 1,
#' then 1 unit on x axis is the same as 1 unit on y axis.
#' @param plotcolour A character R colour or hexadecimal denoting colour for plot elements. Defaults to "grey30".
#' @param pointcol The colour character for sample points. An R colour or hexadecimal value.
#' @param pointtype The shape/pch of sample points. A numeric or a character. By default,
#' "+" is used for all points. If NA, then each cluster is plotted using a different shape.
#' If a numeric or character of length one is used, then it is used for all points.
#' If a vector is used, then it must be equal to number clusters.
#' @param pointsize A numeric indicating the size of sample points. A number usually 0.4,0.8,1,3 etc.
#' @param pointalpha A numeric between 0 and 1 indicating the transparency of points.
#' @param chull A logical indicating if the convex hull is computed for each cluster. The outer
#' points of each cluster are connected by lines. If less than 3 points are available
#' in a cluster, then convex hull is not computed and a warning is shown.
#' @param chullalpha A numeric between 0 and 1 indicating the transparency of the convex hull.
#' @param chullsize A numeric indicating the thickness of the convex hull border.
#' @param chulltype A numeric indicating the line type of the convex hull border. Option pch in standard R.
#' @param ellipse A logical indicating if an ellipse around the clusters. Set to F to supress ellipse.
#' @param ellconf A numeric indicating the confidence interval of the ellipse. Defaults to 0.95.
#' @param ellsize A numeric indicating the thickness of the ellipse line.
#' @param elltype A numeric indicating the linetype for the ellipse. Option lty in standard R.
#' @param ellpoints A numeric indicating the number of points on the ellipse.
#' @param legend A logical indicating if the legend for the colours is plotted.
#' @param legendlab A vector of labels for the legend denoting clusters. Defaults to cluster numbers.
#' @param legendpos A character or 2-value numeric vector indicating the position of the legend. If "right","left","top" or "bottom", then,
#' legend is plotted outside the plot area. To plot inside plot area use a 2 vale vector.
#' If a vector like c(1,1), first value denotes x-axis from 0 to 1 and second value
#' denotes y-axis from 0 to 1. For ex. to plot in bottom left corner, use c(0,0).
#' @param legendjust The x and y axis justification of the legend. A 2-value vector.
#' @param legendkeysize A numeric indicating the size of the legend in cm. Usually values like 0.5,0.7,1.2 etc.
#' @param legendtextsize A numeric indicating the size of the text in the legend.
#' @param plottitle A character for the title of the plot. NULL by default.
#' @param exportplot If set to FALSE, no image is exported.
#' @param exportfilename A character name for the export figure. Automatically computed if NA.
#' @param dataout A logical. If set to TRUE, a list of one or more \code{ggplot} gtable elements are returned.
#' This output can be modified using \code{ggplot} themes() for more figure control if required.
#' @return If \code{dataout=T}, a list of one or more \code{ggplot} gtable output is returned for more theme
#' control if required.
#' @details The coordinates must always be provided as standard longitude-latitude (LL) decimal
#' format.
#' @examples
#' \dontrun{
#' #structure file
#' q <- readQ(system.file("files/Structure239_4",package="pophelperSpatial"))
#' xy <- read.delim(system.file("files/coords239.txt",package="pophelperSpatial"),header=F)
#'
#' # basic usage
#' plotQSpatial(qlist=q,coords=xy)
#' # set UTM coordinates. Better geographic distance representation over a scale such as countries.
#' plotQSpatial(q,xy,height=12,setutm=T)
#' # without ellipses
#' plotQSpatial(q,xy,height=12,ellipse=F)
#' # show axis
#' plotQSpatial(q,xy,height=12,showaxis=T)
#' # change plot element colours
#' plotQSpatial(q,xy,height=12,showaxis=T,plotcolour="blue")
#' }
#' @import PBSmapping
#' @export
#'
plotQSpatial <- function(qlist=NULL,coords=NULL,clustercol=NA,setutm=FALSE,
                            imgtype="png",height=NA,width=NA,units="cm",res=200,
                            showaxis=FALSE,coordsratio=NA,plotcolour="grey30",
                            pointcol="grey10",pointtype="+",pointsize=4,pointalpha=0.9,chull=FALSE,
                            chullalpha=0.02,chullsize=0.4,chulltype=1,
                            ellipse=TRUE,ellconf=0.95,ellsize=0.4,elltype=1,ellpoints=100,
                            legend=TRUE,legendlab=NA,legendpos=c(0.99,0.99),legendjust=c(1,1),legendkeysize=NA,legendtextsize=NA,
                            plottitle=NULL,exportplot=TRUE,exportfilename=NA,dataout=FALSE)

{
  #basic checks
  if(is.null(qlist) | length(qlist)==0) stop("plotQSpatial: No content in qlist.")
  if((class(qlist) != "list")) stop("plotQInterpolate: Argument 'qlist' must be a list datatype.")
  if(!all(sapply(qlist,is.data.frame))) stop("plotQInterpolate: List element is not a list datatype.")

  if(is.null(coords) | length(coords)==0) stop("plotQSpatial: No content in coords.")
  if(!is.numeric(res)) stop("plotQSpatial: Argument 'res' set incorrectly. Use a numeric.")
  if(!is.character(pointcol)) stop("plotQSpatial: Argument 'pointcol' set incorrectly. Use a character.")
  if(!is.numeric(pointsize)) stop("plotQSpatial: Argument 'pointsize' set incorrectly. Use a numeric.")
  if(!is.numeric(pointalpha)) stop("plotQSpatial: Argument 'pointalpha' set incorrectly. Use a numeric.")
  if(!is.numeric(chullalpha)) stop("plotQSpatial: Argument 'chullalpha' set incorrectly. Use a numeric.")
  if(!is.numeric(chullsize)) stop("plotQSpatial: Argument 'chullsize' set incorrectly. Use a numeric.")
  if(!is.numeric(chulltype)) stop("plotQSpatial: Argument 'chulltype' set incorrectly. Use a numeric.")
  if(!is.numeric(ellconf)) stop("plotQSpatial: Argument 'ellconf' set incorrectly. Use a numeric.")
  if(!is.numeric(ellsize)) stop("plotQSpatial: Argument 'ellsize' set incorrectly. Use a numeric.")
  if(!is.numeric(ellpoints)) stop("plotQSpatial: Argument 'ellpoints' set incorrectly. Use a numeric.")
  if(!is.logical(exportplot)) stop("plotQSpatial: Argument 'exportplot' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(showaxis)) stop("plotQSpatial: Argument 'showaxis' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(chull)) stop("plotQSpatial: Argument 'chull' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(ellipse)) stop("plotQSpatial: Argument 'ellipse' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(legend)) stop("plotQSpatial: Argument 'legend' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(setutm)) stop("plotQSpatial: Argument 'setutm' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(dataout)) stop("plotQSpatial: Argument 'dataout' set incorrectly. Set as TRUE or FALSE.")
  imgtype <- tolower(imgtype)
  if(imgtype!="png" && imgtype != "pdf" && imgtype != "jpeg") stop("plotQSpatial: Argument 'imgtype' set incorrectly. Set as 'png', 'jpeg' or 'pdf'.")

  #READ DATA FILES AND CHECK
  #sample name
  fname <- names(qlist)[1]
  if(is.null(fname)) fname <- paste0("sample",1)

  #check files
  df1 <- qlist[[1]]
  if(!is.data.frame(df1)) stop(paste0("plotQSpatial: List item ",fname," is not a data.frame object."))
  if(!any(sapply(df1,is.numeric))) stop(paste0("plotQSpatial: List item ",fname," has non-numeric columns."))

  #READ COORDS AND CHECK
  if(!is.data.frame(coords)) stop("plotQInterpolate: Argument 'coords' is not a data.frame datatype.")
  #coords check
  if(!any(sapply(coords,is.numeric))) stop("plotQSpatial: Non numeric content in coords.")
  if(any(is.na(coords))) stop("plotQSpatial: Missing data detected in coords. Cannot handle missing coordinate data.")
  colnames(coords)<-c("X","Y")

  #data coords length check
  if(nrow(df1) != nrow(coords)) stop("plotQSpatial: Number of rows of qlist not equal to number of rows of coords.")

  #copy to new variable
  if(setutm)
  {
    utmval <- pophelperSpatial:::llToUtmzone(mean(coords$X,na.rm=T),mean(coords$Y,na.rm=T))
    attr(coords,"projection") <- "LL"
    attr(coords,"zone") <- utmval$UTMZone
    df2 <- PBSmapping::convUL(coords,km=T)
  }else{
    df2 <- coords
  }

  #find max value cluster
  fun_maxprob <- function(x) match(max(x),x)
  df2$Clusters <- factor(as.numeric(apply(df1,1,fun_maxprob)))
  clev <- levels(factor(as.character(df2$Clusters)))
  len <- length(clev)

  #Convex hull calculation
  if(chull)
  {
    slist <- vector("list",length=len)
    i <- 1
    while(i <= len)
    {
      j <- as.numeric(clev[i])
      s1 <- as.data.frame(subset(df2,df2$Clusters==j,drop=T),stringsAsFactors=FALSE)
      #compute chull only if >2 coordinates are present
      if(nrow(s1) > 2) {slist[[i]] <- s1[chull(s1$X,s1$Y),]}else{warning(paste0("plotQSpatial: Less than 3 coordinates in cluster ",j,". Convex hull not computed."))}
      i=i+1
    }
    s2 <- do.call("rbind",slist)
    s2$X <- as.numeric(s2$X)
    s2$Y <- as.numeric(s2$Y)
    s2$Clusters <- factor(as.numeric(as.character(s2$Clusters)))
    #levels of chull clusters
    llev <- levels(s2$Clusters)
  }

  #ellipse calculation
  if(ellipse)
  {
    slist <- vector("list",length=len)
    i <- 1
    while(i <= len)
    {
      j <- as.numeric(clev[i])
      s1 <- as.data.frame(subset(df2,df2$Clusters==j,drop=T),stringsAsFactors=FALSE)
      #compute ellipse only if >2 coordinates are present
      if(nrow(s1) > 2)
      {
        el1 <- pophelperSpatial:::ellipseCI(x=s1$X, y=s1$Y, conf=ellconf, np=ellpoints)
        el1$group <- rep(i,ellpoints)
        slist[[i]] <- el1
      }else{warning(paste0("plotQSpatial: Less than 3 coordinates in cluster ",j,". Ellipse not computed."))}
      i=i+1
    }
    s3 <- do.call("rbind",slist)
    s3$x <- as.numeric(s3$x)
    s3$y <- as.numeric(s3$y)
    s3$group <- factor(as.numeric(as.character(s3$group)))
    #levels of chull clusters
    elev <- levels(s3$group)
  }

  #legend details
  #if(is.na(legendheader)) legendheader <- "Clusters"
  if(all(is.na(legendlab))) legendlab <- clev
  if(length(legendlab) != length(clev)) stop(paste0("plotQSpatial: Number of provided legend labels (",length(legendlab),") is not equal to number of clusters (",length(levels(df2$Clusters)),")."))
  llp <- legendlab
  #chull legend
  if(chull) llc <- llp[match(llev,clev)]
  #ellipse legend
  if(ellipse) lle <- llp[match(elev,clev)]

  #get colours
  clustercol1 <- clustercol
  if(all(is.na(clustercol))) clustercol1 <- pophelperSpatial:::getColours(len)
  if(length(clustercol1) < length(levels(factor(as.character(df2$Clusters))))) stop("plotQSpatial: Number of colours less than number of clusters.")
  #chull colours
  if(chull) clustercol2 <- clustercol1[match(llev,clev)]
  if(ellipse) clustercol3 <- clustercol1[match(elev,clev)]

  #get dimensions for sep figures
  height1 <- height
  width1 <- width
  if(length(height) > 1) stop("plotQSpatial: Height must be a single numeric and not a vector.")
  if(length(width) > 1) stop("plotQSpatial: Width must be a single numeric and not a vector.")
  if(is.na(height)) height1 <- 8
  if(is.na(width)) width1 <- 8

  if(imgtype=="pdf" && any(!is.na(height) | !is.na(width))) warning("plotQSpatial: Height and width will be taken as inches if argument imgtype is set to pdf.")
  if(imgtype=="pdf" && all(is.na(height) && is.na(width)))
  {
    height1 <- round(height1*0.394,2)
    width1 <- round(width1*0.394,2)
    units <- "in"
  }

  #plotting
  p <- ggplot2::ggplot()
  #pointtype adjustment
  if(all(is.na(pointtype))) p <- p + geom_point(data=df2,aes(x=X,y=Y,colour=Clusters,shape=Clusters),size=pointsize,fill=pointcol,alpha=pointalpha)
  if(!all(is.na(pointtype)) && length(pointtype)==1) p <- p + geom_point(data=df2,aes(x=X,y=Y,colour=Clusters),size=pointsize,shape=pointtype,fill=pointcol,alpha=pointalpha)
  if(!all(is.na(pointtype)) && length(pointtype) > 1) p <- p + geom_point(data=df2,aes(x=X,y=Y,colour=Clusters,shape=Clusters),size=pointsize,fill=pointcol,alpha=pointalpha) + scale_shape_manual(values=pointtype)

  p <-  p + scale_colour_manual(values=clustercol1,labels=llp)+
    theme_bw()+labs(x=NULL, y=NULL, title=plottitle)+
    theme(plot.title=element_text(colour=plotcolour,hjust=0),axis.text=element_text(colour=plotcolour),
          axis.ticks=element_line(colour=plotcolour),legend.justification=legendjust,legend.position=legendpos,
          legend.text=element_text(colour=plotcolour),legend.title=element_blank(),
          panel.border=element_rect(colour=plotcolour),plot.background=element_blank())

  #show convex hulls if true
  if(chull) p <- p + geom_polygon(data=s2,aes(x=X,y=Y,group=Clusters,colour=Clusters,fill=Clusters),alpha=chullalpha,linetype=chulltype,size=chullsize)+
    scale_fill_manual(values=clustercol2,labels=llc)
  if(ellipse) p <- p + geom_path(data=s3, aes(x=x, y=y,colour=group),size=ellsize,linetype=elltype)
  #hide axis if false
  if(!showaxis) p <- p + theme(axis.text=element_blank(),axis.ticks=element_blank(),panel.border=element_blank())
  #hide legend if true
  if(!legend) p <- p + theme(legend.position="none")
  #adjust legend size if not NA
  if(!is.na(legendkeysize)) p <- p + theme(legend.key.size=grid::unit(legendkeysize, "cm"))
  #adjust legend text size if not NA
  if(!is.na(legendtextsize)) p <- p + theme(legend.text=element_text(colour=plotcolour, size=legendtextsize))
  #coordsratio
  if(!is.na(coordsratio)) p <- p + coord_fixed(ratio=coordsratio)

  if(exportplot)
  {
    fname1 <- paste0(fname,"-Spatial")
    if(!is.na(exportfilename)) fname1 <- exportfilename

    #cat(paste("Height: ",height1,"\n"))
    #cat(paste("Width: ",width1,"\n"))
    #cat(paste("Res: ",res,"\n"))
    #cat(paste("Units: ",units,"\n"))

    if(imgtype=="png") png(paste0(fname1,".png"), height=height1, width=width1, res=res, units=units,type="cairo")
    if(imgtype=="jpeg") jpeg(paste0(fname1,".jpg"), height=height1, width=width1, res=res, units=units, quality=100)
    if(imgtype=="pdf") pdf(paste0(fname1,".pdf"), height=height1, width=width1)
    print(p)
    dev.off()
    if(imgtype=="png") cat(paste0(fname1,".png exported.\n"))
    if(imgtype=="jpeg") cat(paste0(fname1,".jpg exported.\n"))
    if(imgtype=="pdf") cat(paste0(fname1,".pdf exported.\n"))
  }
  if(dataout) return(p)
}

# On Load ----------------------------------------------------------------------

#ON LOAD
.onLoad <- function(...) {
    packageStartupMessage("pophelperSpatial v1.0.0 ready.")
}

# End --------------------------------------------------------------------------
