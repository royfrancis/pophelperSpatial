# Test Script
# 22-Dec-2016

library(testthat)
library(pophelper)
library(pophelperSpatial)

#devtools::test()

# Start ------------------------------------------------------------------------

#Preparation
deleteoutput = TRUE
#create a new folder and set as wd
currwd <- getwd()
dir.create(paste(currwd,"/pophelperspatial-demo",sep=""))
setwd(paste(currwd,"/pophelperspatial-demo",sep=""))

#read sample STRUCTURE files from R package
sfiles <- list.files(path=system.file("files/structure",package="pophelperSpatial"),full.names=TRUE)
sfiles1 <- list.files(path=system.file("files/structure-ci",package="pophelperSpatial"),full.names=TRUE)
#read sample TESS files from R package
tfiles <- list.files(path=system.file("files/tess",package="pophelperSpatial"),full.names=TRUE)
#read sample ADMIXTURE files from R package
afiles <- list.files(path=system.file("files/admixture",package="pophelperSpatial"),full.names=TRUE)
#read sample fastSTRUCTURE files from R package
ffiles <- list.files(path=system.file("files/faststructure",package="pophelperSpatial"),full.names=TRUE)
#read sample MATRIX files from R package
mcfiles <- list.files(path=system.file("files/matrix/comma",package="pophelperSpatial"),full.names=TRUE)
mtfiles <- list.files(path=system.file("files/matrix/tab",package="pophelperSpatial"),full.names=TRUE)
msfiles <- list.files(path=system.file("files/matrix/space",package="pophelperSpatial"),full.names=TRUE)

# plotQInterpolate Structure ------------------------------------------------

context("plotQInterpolate Structure")
qlist <- readQ(system.file("files/Structure239_4",package="pophelperSpatial"))
xy <- read.delim(system.file("files/coords239.txt",package="pophelperSpatial"),header=FALSE)

plotQInterpolate(qlist,xy)
test_that("check output krig",{
  expect_equal(any(grepl("Interpolation-krig",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-krig",list.files())])

plotQInterpolate(qlist,xy,method = "bilinear")
test_that("check output bilinear",{
  expect_equal(any(grepl("Interpolation-bilinear",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-bilinear",list.files())])

plotQInterpolate(qlist,xy,method = "bicubic")
test_that("check output bicubic",{
  expect_equal(any(grepl("Interpolation-bicubic",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-bicubic",list.files())])

plotQInterpolate(qlist,xy,method = "idw")
test_that("check output idw",{
  expect_equal(any(grepl("Interpolation-idw",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-idw",list.files())])

plotQInterpolate(qlist,xy,method = "nn")
test_that("check output nn",{
  expect_equal(any(grepl("Interpolation-nn",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-nn",list.files())])

plotQInterpolate(qlist,xy,clusters=2)
plotQInterpolate(qlist,xy,clusters=2:3)
plotQInterpolate(qlist,xy,coordsratio=2)
plotQInterpolate(qlist,xy,method="idw",idwpower=4)
plotQInterpolate(qlist,xy,ncol=2)
plotQInterpolate(qlist,xy,exportplot=F)
plotQInterpolate(qlist,xy,showaxis=T,addpoints=F)
plotQInterpolate(qlist,xy,legend=F)
plotQInterpolate(qlist,xy,legendpos="right")
plotQInterpolate(qlist,xy,legendkeysize=0.5)

# plotQInterpolate Tess -----------------------------------------------------

context("plotQInterpolate Tess")
xy <- read.delim(system.file("/files/coords75.txt",package="pophelperSpatial"),header=F)
tlist <- readQ(tfiles)

plotQInterpolate(tlist[2],xy)
test_that("check output krig",{
  expect_equal(any(grepl("Interpolation-krig",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-krig",list.files())])

plotQInterpolate(tlist[2],xy,imgtype="jpeg")
test_that("check output krig jpeg",{
  expect_equal(any(grepl("Clusters.jpg",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Clusters.jpg",list.files())])

plotQInterpolate(tlist[2],xy,imgtype="pdf")
test_that("check output krig pdf",{
  expect_equal(any(grepl("Clusters.pdf",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Clusters.pdf",list.files())])

plotQInterpolate(tlist[2],xy,method = "bilinear")
test_that("check output bilinear",{
  expect_equal(any(grepl("Interpolation-bilinear",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-bilinear",list.files())])

plotQInterpolate(tlist[2],xy,method = "bicubic")
test_that("check output bicubic",{
  expect_equal(any(grepl("Interpolation-bicubic",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-bicubic",list.files())])

plotQInterpolate(tlist[2],xy,method = "idw")
test_that("check output idw",{
  expect_equal(any(grepl("Interpolation-idw",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-idw",list.files())])

plotQInterpolate(tlist[2],xy,method = "nn")
test_that("check output nn",{
  expect_equal(any(grepl("Interpolation-nn",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-nn",list.files())])

# plotQInterpolate Admixture ------------------------------------------------

context("plotQInterpolate Admixture")
xy <- read.delim(system.file("/files/coords1592.txt",package="pophelperSpatial"),header=F)
alist <- readQ(afiles)

plotQInterpolate(alist[2],xy)
test_that("check output krig",{
  expect_equal(any(grepl("Interpolation-krig",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-krig",list.files())])

plotQInterpolate(alist[2],xy,imgtype="jpeg")
test_that("check output krig jpeg",{
  expect_equal(any(grepl("Clusters.jpg",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Clusters.jpg",list.files())])

plotQInterpolate(alist[2],xy,imgtype="pdf")
test_that("check output krig pdf",{
  expect_equal(any(grepl("Clusters.pdf",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Clusters.pdf",list.files())])

plotQInterpolate(alist[2],xy,method = "bilinear")
test_that("check output bilinear",{
  expect_equal(any(grepl("Interpolation-bilinear",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-bilinear",list.files())])

plotQInterpolate(alist[2],xy,method = "bicubic")
test_that("check output bicubic",{
  expect_equal(any(grepl("Interpolation-bicubic",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-bicubic",list.files())])

plotQInterpolate(alist[2],xy,method = "idw")
test_that("check output idw",{
  expect_equal(any(grepl("Interpolation-idw",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-idw",list.files())])

plotQInterpolate(alist[2],xy,method = "nn")
test_that("check output nn",{
  expect_equal(any(grepl("Interpolation-nn",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-nn",list.files())])

# plotQInterpolate Matrix ---------------------------------------------------

context("plotQInterpolate fastStructure")
xy <- read.delim(system.file("/files/coords22.txt",package="pophelperSpatial"),header=F)
flist <- readQ(ffiles)

plotQInterpolate(flist[2],xy)
test_that("check output krig",{
  expect_equal(any(grepl("Interpolation-krig",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-krig",list.files())])

plotQInterpolate(flist[2],xy,imgtype="jpeg")
test_that("check output krig jpeg",{
  expect_equal(any(grepl("Clusters.jpg",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Clusters.jpg",list.files())])

plotQInterpolate(flist[2],xy,imgtype="pdf")
test_that("check output krig pdf",{
  expect_equal(any(grepl("Clusters.pdf",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Clusters.pdf",list.files())])

plotQInterpolate(flist[2],xy,method = "bilinear")
test_that("check output bilinear",{
  expect_equal(any(grepl("Interpolation-bilinear",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-bilinear",list.files())])

# plotQInterpolate(flist[2],xy,method = "bicubic")
# test_that("check output bicubic",{
#   expect_equal(any(grepl("Interpolation-bicubic",list.files())),TRUE)
# })
# if(deleteoutput) file.remove(list.files()[grep("Interpolation-bicubic",list.files())])

plotQInterpolate(flist[2],xy,method = "idw")
test_that("check output idw",{
  expect_equal(any(grepl("Interpolation-idw",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-idw",list.files())])

plotQInterpolate(flist[2],xy,method = "nn")
test_that("check output nn",{
  expect_equal(any(grepl("Interpolation-nn",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-nn",list.files())])

# plotQSpatial --------------------------------------------------------------

context("plotQSpatial")
xy <- read.delim(system.file("files/coords239.txt",package="pophelperSpatial"),header=FALSE)
qlist <- readQ(system.file("files/Structure239_4",package="pophelperSpatial"))

plotQSpatial(qlist,xy)
test_that("check output",{
  expect_equal(any(grepl("Spatial.png",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Spatial.png",list.files())])

xy <- read.delim(system.file("/files/coords75.txt",package="pophelperSpatial"),header=F)
tlist <- readQ(tfiles)

plotQSpatial(tlist[2],xy)
test_that("check output",{
  expect_equal(any(grepl("Spatial.png",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Spatial.png",list.files())])

alist <- readQ(afiles)
xy <- read.delim(system.file("/files/coords1592.txt",package="pophelperSpatial"),header=F)

plotQSpatial(alist[2],xy)
test_that("check output",{
  expect_equal(any(grepl("Spatial.png",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Spatial.png",list.files())])

flist <- readQ(ffiles)
xy <- read.delim(system.file("/files/coords22.txt",package="pophelperSpatial"),header=F)
plotQSpatial(flist[2],xy)

test_that("check output",{
  expect_equal(any(grepl("Spatial.png",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Spatial.png",list.files())])

# End --------------------------------------------------------------------------

setwd(currwd)
if(deleteoutput) unlink("pophelperspatial-demo",recursive = T,force = T)

