open.cube<-function(cubepath) {
  cube.config<-parseConfigFile(cubepath)
  
  data.dir<-file.path(cubepath,"data")
  data.dir.entries       <-list.files(data.dir)
  data.dir.entries       <-sort(data.dir.entries)
  var.name.to.index  <-1:length(data.dir.entries)
  names(var.name.to.index) <- data.dir.entries
  names(var.name.to.index)<-data.dir.entries
  firstYearOffset=(as.double(strftime(cube.config$start_time,format="%j"))-1)/cube.config$temporal_res
  return(list(base.dir=cubepath,config=cube.config,data.dir.entries=data.dir.entries,var.name.to.index=var.name.to.index,firstYearOffset=firstYearOffset))
}

getTimesToRead<- function(time1,time2,config) {
  NpY    = ceiling(365/config$temporal_res)
  y1     = as.numeric(strftime(time1,"%Y"))
  y2     = as.numeric(strftime(time2,"%Y"))
  d1     = as.numeric(strftime(time1,"%j"))
  index1 = round(d1/config$temporal_res)+1
  d2     = as.numeric(strftime(time2,"%j"))
  index2 = min(round(d2/config$temporal_res)+1,NpY)
  ntimesteps = -index1 + index2 + (y2-y1)*NpY + 1
  return(c(y1,index1,y2,index2,ntimesteps,NpY))
}

cube.read<-function(cube,
                    variable  = cube$data.dir.entries,
                    time      = c(cube$config$start_time,cube$config$end_time),
                    longitude = c(-180,180),
                    latitude  = c(-90,90)) {
  config  <- cube$config
  grid_y1 = round((90.0 - latitude[2]) / config$spatial_res) - config$grid_y0 + 1
  grid_y2 = round((90.0 - latitude[1]) / config$spatial_res) - config$grid_y0
  grid_x1 = round((180.0 + longitude[1]) / config$spatial_res) - config$grid_x0 + 1
  grid_x2 = round((180.0 + longitude[2]) / config$spatial_res) - config$grid_x0
  
  
  v<-getTimesToRead(time[1],time[2],config)
  y1<-v[1];i1<-v[2];y2<-v[3];i2<-v[4];ntime<-v[5];NpY<-v[6]
  outlist=list()
  for (ivar in 1:length(variable)) {
    nc<-open.nc(file.path(cube$base.dir,"data",variable[ivar],paste0(y1,"_",variable[ivar],".nc")))
    if (y1==y2) {
      outlist[[variable[ivar]]] <- var.get.nc(nc,variable[ivar],start=c(grid_x1,grid_y1,i1),count=c(grid_x2-grid_x1+1,grid_y2-grid_y1+1,i2-i1+1))
    } else {
      #Allocate Space
      outar=array(0,dim = c(grid_x2-grid_x1+1,grid_y2-grid_y1+1,ntime))
      #Read from first year
      outar[,,1:(NpY-i1+1)]=var.get.nc(nc,variable[ivar],start=c(grid_x1,grid_y1,i1),count=c(grid_x2-grid_x1+1,grid_y2-grid_y1+1,NpY-i1+1))
      close.nc(nc)
      #Read full "sandwich" years
      ifirst=NpY-i1+2
      if (y1+1<y2) {
        for (y in (y1+1):(y2-1)) {
          nc<-open.nc(file.path(cube$base.dir,"data",variable[ivar],paste0(y,"_",variable[ivar],".nc")))
          outar[,,ifirst:(ifirst+NpY-1)]=var.get.nc(nc,variable[ivar],start=c(grid_x1,grid_y1,1),count=c(grid_x2-grid_x1+1,grid_y2-grid_y1+1,NpY))
          ifirst<-ifirst+NpY
          close.nc(nc)
        }
      }
      #Read from last Year
      nc<-open.nc(file.path(cube$base.dir,"data",variable[ivar],paste0(y2,"_",variable[ivar],".nc")))
      outar[,,(ntime-i2+1):ntime]=var.get.nc(nc,variable[ivar],start=c(grid_x1,grid_y1,1),count=c(grid_x2-grid_x1+1,grid_y2-grid_y1+1,i2))
      close.nc(nc)
      outlist[[variable[ivar]]]<-outar
    }
  }
  outlist
}