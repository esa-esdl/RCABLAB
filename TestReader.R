rm(list=ls())
library(RCABLAB)

cubepath   <- "/Users/fgans/scratch/my-cablab-cube/"
cube  <- open.cube(cubepath)

variable<-"t2m"
time<-as.POSIXlt(c("2001-01-01","2001-02-01"))
latitude<-c(-90,90)
longitude<-c(-180,180)

#Read a single map
x<-cube.read(cube,"t2m",time)
image(x$t2m[,720:1])

#Read the whole time series for a region
x<-cube.read(cube,variable = c("fpar","Rg"),latitude = c(0,10),longitude = c(10,20))
plot(x$time,scale(x$Rg[3,3,]),"l")
lines(x$time,scale(x$fpar[3,3,]),col=2)
