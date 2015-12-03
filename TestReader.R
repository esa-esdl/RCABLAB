rm(list=ls())
library(DataCubeReader)

cubepath   <- "/Users/fgans/scratch/my-cablab-cube/"
cube  <- open.cube(cubepath)

variable<-"Precip"
time<-as.POSIXlt(c("2001-01-01","2001-01-01"))
latitude<-c(-90,90)
longitude<-c(-180,180)

#Read a single map
x<-cube.read(cube,"Precip",time)
x$Precip[x$Precip<0]<-NA
image(x$Precip[,720:1])

#Read the whole time series for a region
t<-getTimeRanges(cube)
x<-cube.read(cube,variable = c("Precip","BurntArea"),latitude = c(0,10),longitude = c(10,20))
x$Precip[x$Precip<0]<-NA
plot(t,x$Precip[3,3,],"l")
lines(t,x$BurntArea[3,3,]/1000,col=2)
