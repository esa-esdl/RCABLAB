parseConfigFile<-function(cubepath) {
  configfile <- paste(cubepath,"cube.config",sep="/")
  cube.config<- list()
  x=read.table(configfile,sep="=",stringsAsFactors = F,strip.white=T)
  for (i in 1:nrow(x)) {
    if (!is.na(as.double(x[i,2]))) {
      cube.config[[x[i,1]]]=as.double(x[i,2])
    } else if (grepl("^datetime",x[i,2])) {
      cube.config[[x[i,1]]]=strptime(x[i,2],"datetime.datetime(%Y, %m, %d, %H, %M)")
    }
    else {
      cube.config[[x[i,1]]]=x[i,2]
    }
  }
}