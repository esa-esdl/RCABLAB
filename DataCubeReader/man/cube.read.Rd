\name{cube.read}
\alias{cube.read}
\title{Read data from an open Earth System Data Cube}
\description{Reads data from a datacube as specified by the user for a given variable, time, longitude and latitude range}
\usage{cube.read(cube, variable = cube$data.dir.entries, time = c(cube$config$start_time, 
    cube$config$end_time - as.difftime(1, units = "days")), longitude = c(-180, 
    180), latitude = c(-90, 90))}
\arguments{
  \item{cube}{Datacube handle as returned from cube.open}
  \item{variable}{A list of variables to read, defaults to all available variables}
  \item{time}{Start and end time of the range to be read given as POSIXlt or POSIXct. Default is the whole datacube time range}
  \item{longitude}{Longitude range , default is c(-180,180)}
  \item{latitude}{Latitude range, default is c(-90,90)}
}

\value{A named list where each list entry contains the data for a single variable.}

\author{Fabian Gans, Miguel D. Mahecha, MPI BGC Jena, Germany, fgans@bgc-jena.mpg.de mmahecha@bgc-jena.mpg.de
Fabian Gans}





