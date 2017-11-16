open.cube<-function(
  ##title<< Open an Earth System Data Cube
  ##description<< Open the Datacube by scanning datacube's base directory and returns an opaque DataCube object handle
  cubepath ##<< Location the the DataCube
) {
  ##author<<
  ##Fabian Gans, Miguel D. Mahecha, MPI BGC Jena, Germany, fgans@bgc-jena.mpg.de mmahecha@bgc-jena.mpg.de
  cube.config<-.parseConfigFile(cubepath)
  
  data.dir<-file.path(cubepath,"data")
  data.dir.entries       <-list.files(data.dir)
  data.dir.entries       <-sort(data.dir.entries)
  var.name.to.index  <-1:length(data.dir.entries)
  names(var.name.to.index) <- data.dir.entries
  names(var.name.to.index)<-data.dir.entries
  firstYearOffset=(as.double(strftime(cube.config$start_time,format="%j"))-1)/cube.config$temporal_res
  return(list(base.dir=cubepath,config=cube.config,data.dir.entries=data.dir.entries,var.name.to.index=var.name.to.index,firstYearOffset=firstYearOffset))
  ##value<< 
  ##A list a list containing some cube info, to be used for reading cube data.
}

read.cube<-function(
  ##title<< Read data from an open Earth System Data Cube
  ##description<< Reads data from a datacube as specified by the user for a given variable, time, longitude and latitude range
  cube, ##<< Datacube handle as returned from cube.open
  variable  = cube$data.dir.entries, ##<< A list of variables to read, defaults to all available variables
  time      = c(cube$config$start_time,cube$config$end_time-as.difftime(1,units="days")), ##<< Start and end time of the range to be read given as POSIXlt or POSIXct. Default is the whole datacube time range 
  longitude = c(-180,180), ##<< Longitude range , default is c(-180,180)
  latitude  = c(-90,90) ##<< Latitude range, default is c(-90,90)
) {
  ##author<<
  ##Fabian Gans, Miguel D. Mahecha, MPI BGC Jena, Germany, fgans@bgc-jena.mpg.de mmahecha@bgc-jena.mpg.de
  
  if (length(longitude)== 1) longitude <- c(longitude, longitude)
  if (length(latitude) == 1) latitude  <- c(latitude, latitude)
  if (length(time)     == 1) time      <- c(time, time)
  
  config  <- cube$config

  with(config, {
    grid_y1 <- round((90.0 - latitude[2]) / spatial_res) - grid_y0 + 1
    grid_y2 <- round((90.0 - latitude[1]) / spatial_res) - grid_y0
    grid_x1 <- round((180.0 + longitude[1]) / spatial_res) - grid_x0 + 1
    grid_x2 <- round((180.0 + longitude[2]) / spatial_res) - grid_x0
  })
  
  if (grid_y1-1 == grid_y2) grid_y2 <- grid_y2 + 1
  if (grid_x1-1 == grid_x2) grid_x2 <- grid_x2 + 1
  
  r <- .getTimesToRead(time[1],time[2],config)
  v <- r$v
  y1 <- v[1]; i1 <- v[2];
  y2 <- v[3]; i2 <- v[4];
  ntime <- v[5]; NpY <- v[6]

  outlist <- list()
  outlist$time      <- r$tout
  outlist$longitude <- ((grid_x1:grid_x2)+config$grid_x0)*config$spatial_res-180-config$spatial_res/2
  outlist$latitude  <- 90-((grid_y1:grid_y2)+config$grid_y0)*config$spatial_res+config$spatial_res/2
  outlist$data      <- list()

  for (ivar in 1:length(variable)) {
    if (y1 == y2) {
      filenamecur<-file.path(cube$base.dir,"data",variable[ivar],paste0(y1,"_",variable[ivar],".nc"))
      if (file.exists(filenamecur)) {
        nc<-ncdf4::nc_open(filenamecur)
        outlist[[variable[ivar]]] <- ncdf4::ncvar_get(nc,variable[ivar],start=c(grid_x1,grid_y1,i1),count=c(grid_x2-grid_x1+1,grid_y2-grid_y1+1,i2-i1+1))
        ncdf4::nc_close(nc)
      } else {
        outlist[[variable[ivar]]] <- array(NA,dim=c(grid_x2-grid_x1+1,grid_y2-grid_y1+1,ntime))
      }
    } else {
      #Allocate Space
      outar=array(0,dim = c(grid_x2-grid_x1+1,grid_y2-grid_y1+1,ntime))
      #Read from first year
      filenamecur<-file.path(cube$base.dir,"data",variable[ivar],paste0(y1,"_",variable[ivar],".nc"))
      if (file.exists(filenamecur)) {
        nc<-ncdf4::nc_open(filenamecur)
        outar[,,1:(NpY-i1+1)]=ncdf4::ncvar_get(nc,variable[ivar],start=c(grid_x1,grid_y1,i1),count=c(grid_x2-grid_x1+1,grid_y2-grid_y1+1,NpY-i1+1))
        ncdf4::nc_close(nc)
      } else {
        outar[,,1:(NpY-i1+1)]=NA
      }
      #Read full "sandwich" years
      ifirst=NpY-i1+2
      if (y1+1<y2) {
        for (y in (y1+1):(y2-1)) {
          filenamecur <- file.path(cube$base.dir,"data",variable[ivar],paste0(y,"_",variable[ivar],".nc"))
          if (file.exists(filenamecur)) {
            nc<-ncdf4::nc_open(filenamecur)
            outar[,,ifirst:(ifirst+NpY-1)]=ncdf4::ncvar_get(nc,variable[ivar],start=c(grid_x1,grid_y1,1),count=c(grid_x2-grid_x1+1,grid_y2-grid_y1+1,NpY))
            ncdf4::nc_close(nc)
          } else {
            outar[,,ifirst:(ifirst+NpY-1)]=NA
          }
            ifirst<-ifirst+NpY
        }
      }
      #Read from last Year
      filenamecur<-file.path(cube$base.dir,"data",variable[ivar],paste0(y2,"_",variable[ivar],".nc"))
      if (file.exists(filenamecur)) {
        nc<-ncdf4::nc_open(filenamecur)
        outar[,,(ntime-i2+1):ntime]=ncdf4::ncvar_get(nc,variable[ivar],start=c(grid_x1,grid_y1,1),count=c(grid_x2-grid_x1+1,grid_y2-grid_y1+1,i2))
        ncdf4::nc_close(nc)
      } else {
        outar[,,(ntime-i2+1):ntime]=NA
      }
      outlist$data[[variable[ivar]]]<-outar
    }
  }
  outlist
  ##value<< 
  ##A named list where each list entry contains the data for a single variable.
}

.getTimesToRead<- function(time1, time2, config) {

  NpY        <- ceiling(365/config$temporal_res)
  y1         <- as.numeric(strftime(time1, "%Y"))
  y2         <- as.numeric(strftime(time2, "%Y"))
  d1         <- as.numeric(strftime(time1, "%j"))
  index1     <- round(d1/config$temporal_res)+1
  d2         <- as.numeric(strftime(time2, "%j"))
  index2     <- min(round(d2/config$temporal_res)+1, NpY)
  ntimesteps <- -index1 + index2 + (y2 - y1) * NpY + 1

  tout <- if (y1 == y2) {
      ISOdate(y1, 1, 1, 0) + as.difftime(seq(d1 - 1, d2, 8), units = "days")
  } else if (y2 == (y1 + 1)) {
    c(
      ISOdate(y1, 1, 1, 0) + as.difftime(seq(d1 - 1, 365, 8), units = "days"), 
      ISOdate(y2, 1, 1, 0) + as.difftime(seq(0,       d2, 8), units = "days")
    )
  } else {
    c(
      ISOdate(y1, 1, 1, 0) + as.difftime(seq(d1 - 1, 365, 8), units = "days"), 
      ISOdate(rep((y1 + 1):(y2 - 1), each = NpY), 1, 1, 0) +
        as.difftime(rep(seq(0, 365, 8), (y2-y1-1)), units = "days"), 
      ISOdate(y2, 1, 1, 0)+as.difftime(seq(0, d2, 8), units = "days")
    )
  }

  list(v = c(y1, index1, y2, index2, ntimesteps, NpY), tout = tout)
}
