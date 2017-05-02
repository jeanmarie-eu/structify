#' structify_put
#'
#' structify_put
#' @param filename filename
#' @param vname variable name "date", "spatial", "dataset",...
#' @param obj object to be put
#' @param ... other options according to the variable name
#' @param NEW boolean if new object
#' @keywords structify
#' @export
#' @examples
#' \dontrun{
#' structify_get()
#' }
structify_put <- function(filename,vname,obj,...,NEW=FALSE){
   switch(vname,
     "date"        = structify_put_date(filename=filename,obj=obj,NEW=NEW),
     "spatial"     = structify_put_spatial(filename=filename,obj=obj,NEW=NEW),
     "variable"    = structify_put_variable(filename=filename,obj=obj,NEW=NEW),
     "description" = structify_put_description(filename=filename,obj=obj,NEW=NEW),
     "dataset"     = structify_put_dataset(filename=filename,obj=obj,...,NEW=NEW),
     "dimension"   = structify_put_dimension(filename=filename,obj=obj,NEW=NEW),
     "source"      = structify_put_source(filename=filename,obj=obj,NEW=NEW),
     stop("Variable name not recognized: ", vname))
}


structify_put_date <- function(filename,obj,NEW=FALSE){
  structify_put_core(filename=filename,group="date/fromPeriod",obj=obj$fromPeriod(),NEW=NEW)
  structify_put_core(filename=filename,group="date/toPeriod",obj=obj$toPeriod(),NEW=NEW)
  structify_put_core(filename=filename,group="date/timeResolution",obj=obj$timeResolution(),NEW=NEW)
  structify_put_core(filename=filename,group="date/v",obj=obj$v(),NEW=NEW)
  structify_put_core(filename=filename,group="date/nbStep",obj=obj$nbStep(),NEW=NEW)
  structify_put_core(filename=filename,group="date/Timeresinsec",obj=obj$Timeresinsec(),NEW=NEW)
  structify_put_core(filename=filename,group="date/precision",obj=obj$precision(),NEW=NEW)
  structify_put_core(filename=filename,group="date/tzone",obj=obj$tzone(),NEW=NEW)
  invisible()
}

structify_put_spatial <- function(filename,obj,NEW=FALSE){
  structify_put_core(filename=filename,group="spatial/grid/cellcentre.offset",obj=obj@grid@cellcentre.offset,NEW=NEW)
  structify_put_core(filename=filename,group="spatial/grid/cellsize",obj=obj@grid@cellsize,NEW=NEW)
  structify_put_core(filename=filename,group="spatial/grid/cells.dim",obj=obj@grid@cells.dim,NEW=NEW)
  structify_put_core(filename=filename,group="spatial/grid/bbox",obj=obj@bbox,NEW=NEW)
  structify_put_core(filename=filename,group="spatial/grid/proj4string",obj=obj@proj4string@projargs,NEW=NEW)
  invisible()
}

structify_put_variable <- function(filename,obj,NEW=FALSE){
  structify_put_core(filename=filename,group="variable/longname",obj=as.character(obj),NEW=NEW)
  invisible()
}

structify_put_description <- function(filename,obj,NEW=FALSE){
  structify_put_core(filename=filename,group="description/info",obj=as.character(obj),NEW=NEW)
  invisible()
}

structify_put_dimension <- function(filename,obj,NEW=FALSE){
  structify_put_core(filename=filename,group="dimension/values",obj=as.numeric(obj),NEW=NEW)
  invisible()
}

structify_put_source <- function(filename,obj,NEW=FALSE){
  structify_put_core(filename=filename,group="source/filename",obj=obj$filename,NEW=NEW)
  structify_put_core(filename=filename,group="source/format",obj=obj$format,NEW=NEW)
  invisible()
}

structify_put_dataset <- function(filename,obj,offset=NULL,count=NULL,varid=NULL,target=NULL,NEW=FALSE){
  structify_put_core(filename=filename,group="dataset/values",obj=obj,offset=offset,count=count,varid=varid,target=target,NEW=NEW)
  invisible()
}

structify_put_core <- function(filename,group,obj,offset=NULL,count=NULL,varid=NULL,target=NULL,NEW=FALSE){
   file <- h5::h5file(filename, 'a')
   if(NEW){
     file[group] <- obj
   } else if (!NEW){
     objT <- file[group]
     structify_put_core2(objT=objT,obj=obj,offset=offset,count=count,varid=varid,target=target,NEW=NEW)
     h5::h5close(objT)
   } else stop("NEW or not NEW")
   h5::h5close(file)
   invisible()
}

structify_put_core2 <- function(objT,obj,offset=NULL,count=NULL,varid=NULL,target=NULL,NEW=FALSE){
  if (is.null(offset) && (is.null(count) && is.null(varid) && is.null(target))) {
    objT[] <- obj
  } else if (!is.null(offset) && (!is.null(count)) ) {
    lapply(count,function(x) stopifnot(!identical(x,0)))
    if (is.null(target)) target <- offset
    fillin_i <- paste(sort(names(offset)),collapse=",")
    switch(fillin_i,
      "i"     = objT[seq.int(target$i,length.out=count$i),target$j,target$k] <- obj_extract(obj=obj,offset=offset,count=count,varid=varid),
      "j"     = objT[target$i,seq.int(target$j,length.out=count$j),target$k] <- obj_extract(obj=obj,offset=offset,count=count,varid=varid),
      "k"     = objT[target$i,target$j,seq.int(target$k,length.out=count$k)] <- obj_extract(obj=obj,offset=offset,count=count,varid=varid),
      "i,j"   = objT[seq.int(target$i,length.out=count$i),seq.int(target$j,length.out=count$j),target$k] <- obj_extract(obj=obj,offset=offset,count=count,varid=varid),
      "i,k"   = objT[seq.int(target$i,length.out=count$i),target$j,seq.int(target$k,length.out=count$k)] <- obj_extract(obj=obj,offset=offset,count=count,varid=varid),
      "j,k"   = objT[target$i,seq.int(target$j,length.out=count$j),seq.int(target$k,length.out=count$k)] <- obj_extract(obj=obj,offset=offset,count=count,varid=varid),
      "i,j,k" = objT[seq.int(target$i,length.out=count$i),seq.int(target$j,length.out=count$j),seq.int(target$k,length.out=count$k)] <- obj_extract(obj=obj,offset=offset,count=count,varid=varid),
      stop("dimensions not recognized, ",fillin_i))
  }
  invisible()
}
