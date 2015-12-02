open.cube<-function(cubepath) {
  cube.config<-parseConfigFile(cubepath)
  
  data.dir<-file.path(cubepath,"data")
  data.dir.entries       <-list.files(data.dir)
  data.dir.entries       <-sort(data.dir.entries)
  var.name.to.index  <-1:length(data.dir.entries)
  names(var_name_to_var_index)<-data.dir.entries
  firstYearOffset=(as.double(strftime(cube.config$start_time,format="%j"))-1)/cube.config$temporal_res
  CubeData(cube,data_dir_entries,var_name_to_var_index,firstYearOffset)
  list(base.dir=cubepath,config=cube.config,data.dir.entries=data.dir.entries,var.name.to.index=var.name.to.index,firstYearOffset=firstYearOffset)
}