# RCABLAB

R interface for reading data from the [Earth System Datacube](http://earthsystemdatacube.net/).

## Installing:
```R
devtools::install_github("CAB-LAB/RCABLAB")
```


## Usage:
```R
cube <- open.cube("/path/to/cube")
dat <- read.cube(
  cube, variable = cube$data.dir.entries,
  time = c(cube$config$start_time, 
           cube$config$end_time - as.difftime(1, units = "days")),
  longitude = c(-180, 180), latitude = c(-90, 90)
)
```
