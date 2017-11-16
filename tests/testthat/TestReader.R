rm(list=ls())
library(RCABLAB)

cubepath   <- "/Net/Groups/BGI/scratch/DataCube/v1.0.0/low-res"
cube  <- open.cube(cubepath)

variable<-"t2m"
time<-as.POSIXlt(c("2001-01-01","2001-01-01"))
latitude<-c(-90,90)
longitude<-c(-180,180)

#Read a single map
cubedata<-read.cube(cube,"fpar",time)


image(
  cubedata$fpar[,,1],
  x = cubedata$longitude,
  y = rev(cubedata$latitude)
)
cubedata$fpar %>% dim
#Read the whole time series for a region
x<-cube.read(cube,variable = c("fpar","Rg"),latitude = c(0,10),longitude = c(10,20))
plot(x$time,scale(x$Rg[3,3,]),"l")
lines(x$time,scale(x$fpar[3,3,]),col=2)



test_that("output dimensions of the cube", {
  for(v in cube$data.dir.entries) {
    d <- read.cube(
      cube,
      variable = v,
      latitude = c(40, 45),
      longitude = c(40, 45),
      time = cube$config$start_time
    )
    expect_equal(length(dim(d[[v]])), 3)
  }
})
