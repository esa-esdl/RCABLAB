rm(list=ls())
library(RNetCDF)
source("/Users/fgans/scratch/CABLAB/DataCubeReader/R/ParseConfigFile.R")
source("/Users/fgans/scratch/CABLAB/DataCubeReader/R/Cube.R")

cubepath   <- "/Users/fgans/scratch/my-cablab-cube/"
cube  <- open.cube(cubepath)

variable<-"Precip"
time<-as.POSIXlt(c("2001-01-01","2001-01-01"))
latitude<-c(-90,90)
longitude<-c(-180,180)

x<-cube.read(cube,variable,time,latitude = latitude,longitude = longitude)
x$Precip[x$Precip<0]<-NA
