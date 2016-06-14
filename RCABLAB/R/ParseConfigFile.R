parseConfigFile<-function(
  ##title<< Parse the config file of the DataCube
  ##description<< Reads the cube.config file from the datacube's base directory and returns a list with its content
  cubepath ##<< Path to the datacube
  ) {
  ##author<<
  ##Fabian Gans, Miguel D. Mahecha, MPI BGC Jena, Germany, fgans@bgc-jena.mpg.de mmahecha@bgc-jena.mpg.de
  configfile <- paste(cubepath,"cube.config",sep="/")
  cube.config<- list()
  x=read.table(configfile,sep="=",stringsAsFactors = F,strip.white=T)
  for (i in 1:nrow(x)) {
    if (grepl("^datetime",x[i,2])) {
      cube.config[[x[i,1]]]=as.POSIXlt(strptime(x[i,2],"datetime.datetime(%Y, %m, %d, %H, %M)"))
    } else if (grepl("^\\D",x[i,2])) {
      cube.config[[x[i,1]]]=x[i,2]
    } else {
      cube.config[[x[i,1]]]=as.double(x[i,2])
    }
  }
  cube.config
  ##value<< 
  ##A list with the cube config entries
}
