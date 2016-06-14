\name{getTimeRanges}
\alias{getTimeRanges}
\title{Returns the datacube time steps}
\description{For a given datacube and time range, returns all the time steps that would be returned by cube.read. This might be useful for plotting, time series analysis etc.}
\usage{getTimeRanges(cube, time = c(cube$config$start_time, cube$config$end_time - 
    as.difftime(1, units = "days")))}
\arguments{
  \item{cube}{The Datacube}
  \item{time}{Start and end time of the range to be read given as POSIXlt or POSIXct. Default is the whole datacube time range}
}

\value{a list of time steps}

\author{Fabian Gans}





