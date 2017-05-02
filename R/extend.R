structify_extend <- function(filename,count){

  #extend spatial
  if(!is.null(count$i) || !is.null(count$j)) structify_extend_spatial(filename,count)

  #extend date
  if(!is.null(count$k)) structify_extend_date(filename,count)

  #extend dataset
  structify_extend_dataset(filename,count)
  # update dim
  spatial <- structify_get(filename=filename,vname="spatial")
  date <- structify_get(filename=filename,vname="date")
  dim <- spatialDate_dim(spatial=spatial,date=date)
  structify_put(filename=filename,vname="dimension",obj=unlist(dim,use.names = FALSE))
  invisible()
}

structify_extend_spatial <- function(filename,count){
  spatial <- structify_get(filename=filename,vname="spatial")
  dimInfo <- spatialManip::buildGridTopo(spatial)
  MINMAX <- spatialManip::minmax(spatial)
  extend <- c(0,0)
  if(!is.null(count$i) && count$i>0 ) extend[1] <- count$i*dimInfo$cellsize[1]
  if(!is.null(count$j) && count$j>0) extend[2] <- count$j*dimInfo$cellsize[2]
  x <- spatialManip::xGridBuild(xmin=MINMAX[1,1],xmax=MINMAX[1,2]+extend[1],ymin=MINMAX[2,1],ymax=MINMAX[2,2]+extend[2],cellsize=min(dimInfo$cellsize[2]))
  new_spatial <- spatialManip::construct(type="grid",x,proj4S=spatialManip::proj4string(spatial))
  structify_put(filename=filename,vname="spatial",obj=new_spatial)
  invisible()
}

structify_extend_date <- function(filename,count){
  date <- structify_get(filename=filename,vname="date")
  if(count$k>0) {
    toPeriod_date <- timeManip::addition_nonsec(date=timeManip::YYYYmmddHHMMSS_chr(date$toPeriod()),timeResolution=date$timeResolution(),v=count$k)
    new_date <- timeManip::timeManip(fromPeriod=date$fromPeriod(),toPeriod=timeManip::YYYYmmddHHMMSS(toPeriod_date),timeResolution=date$timeResolution(),v=date$v(),precision=date$precision())
    structify_put(filename=filename,vname="date",obj=new_date)
  }
  invisible()
}

structify_extend_dataset <- function(filename,count){
  structify_extend_core(filename,group="dataset/values", count=count)
  invisible()
}

structify_extend_core <- function(filename,group,count=NULL){
   file <- h5::h5file(filename, 'a')
   dim <- dim(file[group])
   h5::extendDataSet(file[group], c( (dim[1]+count$i),(dim[2]+count$j), (dim[3]+count$k) ))
   h5::h5close(file)
   invisible()
}
