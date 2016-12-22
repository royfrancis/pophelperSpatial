## ----echo=FALSE,message=FALSE,results='hide',warning=FALSE,eval=TRUE-----
options(rpubs.upload.method = "internal")
knitr::opts_chunk$set(collapse = TRUE)
#source("D:/Data/Dropbox/Rwork/pophelperRpackage/pophelper/R/pophelper.R")
pkgname <- "pophelper"
library(pophelperSpatial)

## ----echo=TRUE,eval=FALSE,results='hide'---------------------------------
#  install.packages(c("akima","Cairo","devtools","fields","ggplot2","gridExtra","gtable","PBSmapping","spatstat","tidyr"),dependencies=T)

## ----echo=TRUE,eval=FALSE,results='hide'---------------------------------
#  # Install devtools package
#  install.packages('devtools',dependencies=T)
#  library(devtools)

## ----echo=TRUE,eval=FALSE,results='hide'---------------------------------
#  # Install the current version of pophelper
#  install_github('royfrancis/pophelper')
#  # Install the current version of pophelperSpatial
#  install_github('royfrancis/pophelperSpatial')

## ----echo=TRUE,eval=FALSE,results='hide'---------------------------------
#  # load library
#  library(pophelper)
#  library(pophelperSpatial)
#  
#  # check version
#  packageDescription("ggplot2", fields="Version")
#  packageDescription("pophelper", fields="Version")
#  packageDescription("pophelperSpatial", fields="Version")

## ----echo=TRUE,eval=FALSE,results='hide'---------------------------------
#  setwd("path")
#  # setwd(choose.dir())

## ----echo=TRUE,eval=FALSE,results='hide'---------------------------------
#  ?plotQInterpolate
#  ?plotQSpatial
#  # If using RStudio, press tab inside function to see arguments.
#  # plotQSpatial(<press tab>)

## ---- eval=FALSE,echo=TRUE-----------------------------------------------
#  # vector of paths
#  slist <- list.files(path=system.file("files/structure",package="pophelperSpatial"),full.names=T)
#  # convert one file to a qlist
#  sf <- readQ(files=slist[1])
#  str(sf)

## ---- eval=FALSE,echo=TRUE-----------------------------------------------
#  # create two data frames
#  df1 <- data.frame(Cluster1=c(0.2,0.4,0.6,0.2),Cluster2=c(0.8,0.6,0.4,0.8))
#  df2 <- data.frame(Cluster1=c(0.3,0.1,0.5,0.6),Cluster2=c(0.7,0.9,0.5,0.4))
#  
#  # one-element qlist
#  q1 <- list("sample1"=df1)
#  str(q1)
#  
#  # two-element qlist
#  q2 <- list("sample1"=df1,"sample2"=df2)
#  str(q2)

## ---- eval=FALSE,echo=TRUE-----------------------------------------------
#  # coords path
#  coordfile <- system.file("files/coords239.txt",package="pophelperSpatial")
#  # read coordfile
#  xy <- read.delim(coordfile,header=F)
#  str(xy)

## ---- eval=FALSE,echo=TRUE-----------------------------------------------
#  plotQInterpolate()     # Spatially interpolate clusters from a qlist
#  plotQSpatial()         # Cluster by max assignment and plot points spatially

## ----echo=TRUE, eval=FALSE-----------------------------------------------
#  # using STRUCTURE files
#  q <- readQ(system.file("files/Structure239_4",package="pophelperSpatial"))
#  xy <- read.delim(system.file("files/coords239.txt",package="pophelperSpatial"),header=F)
#  plotQInterpolate(qlist=q,coords=xy)
#  # adjust dimensions and legend size as required
#  plotQInterpolate(qlist=q,coords=xy,height=15,width=22,legendkeysize=0.4)
#  # finer grid # very slow
#  plotQInterpolate(qlist=q,coords=xy,height=15,width=22,legendkeysize=0.4,
#                      gridsize=100)
#  
#  # using TESS runs
#  # specify path for datafile and coordsfile
#  q <- readQ(list.files(path=system.file("files/tess",package="pophelperSpatial"),full.names=T))[1]
#  xy <- read.delim(system.file("files/coords75.txt",package="pophelperSpatial"),header=F)
#  
#  plotQInterpolate(qlist=q,coords=xy)
#  # adjusting legend size and legend text
#  plotQInterpolate(qlist=q,coords=xy,legendkeysize=0.4,legendtextsize=6)
#  # removing legend
#  plotQInterpolate(qlist=q,coords=xy,legend=FALSE)
#  # set aspect ratio to 1
#  plotQInterpolate(qlist=q,coords=xy,coordsratio=1)
#  # show axes
#  plotQInterpolate(qlist=q,coords=xy,showaxis=T,legendpos=c(0.99,0.99))
#  # change plot element colours
#  plotQInterpolate(qlist=q,coords=xy,showaxis=T,legendpos=c(0.99,0.99),plotcolour="blue")
#  
#  # try ADMIXTURE files
#  q <- readQ(system.file("files/admixture/admixture_01",package="pophelperSpatial"))
#  xy <- read.delim(system.file("files/coords1592.txt",package="pophelperSpatial"),header=F)
#  plotQInterpolate(qlist=q,coords=xy,method = "bilinear",height=10,width=26)
#  
#  # try FASTSTRUCTURE files
#  q <- readQ(system.file("files/faststructure/fast-structure_02.meanQ",package="pophelper"))
#  xy <- read.delim(system.file("files/coords22.txt",package="pophelper"),header=F)
#  plotQInterpolate(qlist=q,coords=xy,method = "bilinear",height=10,width=26)

## ----echo=TRUE, eval=FALSE-----------------------------------------------
#  q <- readQ(system.file("files/Structure239_4",package="pophelperSpatial"))
#  xy <- read.delim(system.file("files/coords239.txt",package="pophelperSpatial"),header=F)
#  
#  # plot cluster 2 only
#  plotQInterpolate(qlist=q,coords=xy,clusters=2,height=10,width=12)
#  # plot clusters 1 and 3
#  plotQInterpolate(qlist=q,coords=xy,clusters=c(1,3),height=10,width=16)

## ----echo=TRUE, eval=FALSE-----------------------------------------------
#  # turn off legend
#  plotQInterpolate(qlist=q,coords=xy,clusters=2,legend=F)
#  # legend inside plot top right
#  plotQInterpolate(qlist=q,coords=xy,clusters=2,legendpos=c(1,1))
#  # legend outside plot right top
#  plotQInterpolate(qlist=q,coords=xy,clusters=2,legendpos="right",legendjust="top")
#  # legend outside plot top right
#  plotQInterpolate(qlist=q,coords=xy,clusters=2,legendpos="top",legendjust="right")

## ----echo=TRUE, eval=FALSE-----------------------------------------------
#  # separate files
#  plotQInterpolate(qlist=q,coords=xy,height=10,width=12,imgoutput="sep")

## ----echo=TRUE, eval=FALSE-----------------------------------------------
#  # joined default rows and columns
#  plotQInterpolate(qlist=q,coords=xy,height=10,width=22)
#  # 1 row and 5 columns
#  plotQInterpolate(qlist=q,coords=xy,height=8,width=32,ncol=5,nrow=1)

## ----echo=TRUE, eval=FALSE-----------------------------------------------
#  # default gridsize
#  plotQInterpolate(qlist=q,coords=xy,clusters=2,height=10,width=12,gridsize=60)
#  # higher resolution grid size
#  plotQInterpolate(qlist=q,coords=xy,clusters=2,height=10,width=12,gridsize=120)

## ----echo=TRUE, eval=FALSE-----------------------------------------------
#  q <- readQ(list.files(path=system.file("files/tess",package="pophelperSpatial"),full.names=T))[1]
#  xy <- read.delim(system.file("files/coords75.txt",package="pophelperSpatial"),header=F)
#  
#  p1 <- plotQInterpolate(q,xy,legendkeysize=0.3,legendtextsize=5,
#                            clusters=2,dataout=T,method="bilinear",exportplot=F)
#  p2 <- plotQInterpolate(q,xy,legendkeysize=0.3,legendtextsize=5,
#                            clusters=2,dataout=T,method="bicubic",exportplot=F)
#  p3 <- plotQInterpolate(q,xy,legendkeysize=0.3,legendtextsize=5,
#                            clusters=2,dataout=T,method="idw",exportplot=F)
#  p4 <- plotQInterpolate(q,xy,legendkeysize=0.3,legendtextsize=5,
#                            clusters=2,dataout=T,method="nn",exportplot=F)
#  p5 <- plotQInterpolate(q,xy,legendkeysize=0.3,legendtextsize=5,
#                            clusters=2,dataout=T,method="krig",exportplot=F)
#  
#  library(gridExtra)
#  png("MethodsComparison.png",height=16,width=22,res=200,units="cm",type="cairo")
#  grid.arrange(p1[[1]],p2[[1]],p3[[1]],p4[[1]],p5[[1]],nrow=2,ncol=3)
#  dev.off()

## ----echo=TRUE, eval=FALSE-----------------------------------------------
#  #change colours
#  plotQInterpolate(qlist=q,coords=xy,clusters=2,height=10,width=12,colours=c("coral","steelblue"))
#  plotQInterpolate(qlist=q,coords=xy,clusters=2,height=10,width=12,colours=c("#A6CEE3", "#3F8EAA","#79C360"))
#  
#  # view Colorbrewer colours
#  library(RColorBrewer)
#  display.brewer.all()
#  
#  # sample plots with custom colours
#  p1 <- plotQInterpolate(q,xy,clusters=1:2,colours=brewer.pal(8,"RdYlBu"),
#                            legend=F,exportplot=F,dataout=T)
#  p2 <- plotQInterpolate(q,xy,clusters=2,colours=brewer.pal(8,"Spectral"),
#                            legend=F,exportplot=F,dataout=T)
#  p3 <- plotQInterpolate(q,xy,clusters=2,colours=rev(brewer.pal(8,"BuPu")),
#                            legend=F,exportplot=F,dataout=T)
#  
#  png("PlotColours.png",height=8,width=24,res=200,units="cm",type="cairo")
#  grid.arrange(p1[[1]],p1[[2]],p2[[1]],p3[[1]],ncol=4)
#  dev.off()

## ----echo=TRUE, eval=FALSE-----------------------------------------------
#  library(RColorBrewer)
#  q <- readQ(system.file("files/Structure239_4",package="pophelperSpatial"))
#  xy <- read.delim(system.file("files/coords239.txt",package="pophelperSpatial"),header=F)
#  
#  # basic usage
#  plotQSpatial(qlist=q,coords=xy)
#  # set UTM coordinates. Better geographic distance representation over a scale
#  # such as countries.
#  plotQSpatial(q,xy,height=12,setutm=T)
#  # without ellipses
#  plotQSpatial(q,xy,height=12,ellipse=F)
#  # show axis
#  plotQSpatial(q,xy,height=12,showaxis=T)
#  # change plot element colours
#  plotQSpatial(q,xy,height=12,showaxis=T,plotcolour="blue")
#  
#  # create a 2x2 montage with varying parameters
#  # don't export plot, export data, add title
#  p1 <- plotQSpatial(q,xy,exportplot=F,dataout=T, plottitle="A")
#  # without ellipse, with square points and transparency added
#  p2 <- plotQSpatial(q,xy,exportplot=F,dataout=T, plottitle="B",
#                        ellipse=F,pointtype=15,pointalpha=0.4)
#  # without ellipse, with convex hulls, coordinates in UTM, points by cluster,
#  # custom colours,show axis
#  p3 <- plotQSpatial(q,xy,exportplot=F,dataout=T,
#                        plottitle="C",ellipse=F,chull=T, setutm=T,pointtype=NA,
#                        pointsize=2,clustercol=brewer.pal(5,"Dark2"),showaxis=T)
#  # no ellipse, with convex hull, decreased convexhull transparency, convexhull linetype,
#  # change cluster labels, custom colours, show axis
#  p4 <- plotQSpatial(q,xy,exportplot=F,dataout=T, plottitle="D",
#                        ellipse=F,chull=T,chullalpha=0.2,chulltype=3,
#                        legendlab=c("GrpA","GrpB","GrpC","GrpD","GrpE"),
#                        clustercol=brewer.pal(5,"Set1"),showaxis=T)
#  
#  png("plotQSpatial.png",height=20,width=20,res=250,units="cm",type="cairo")
#  gridExtra::grid.arrange(p1,p2,p3,p4,nrow=2,ncol=2)
#  dev.off()

## ----echo=TRUE, eval=FALSE-----------------------------------------------
#  # try some FASTSTRUCTURE files
#  q <- system.file("files/faststructure/fast-structure_02.meanQ",package="pophelper")
#  xy <- system.file("files/coords22.txt",package="pophelper")
#  plotQSpatial(q,xy)

