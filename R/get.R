#' structify_get
#'
#' structify_get
#' @param filename filename
#' @param vname variable name "date", "spatial", "dataset",...
#' @param ... other options according to the variable name
#' @keywords structify
#' @export
#' @examples
#' \dontrun{
#' structify_get()
#' }
structify_get <- function(filename,vname,...){
   switch(vname,
     "date"        = structify_get_date(filename,...),
     "spatial"     = structify_get_spatial(filename,...),
     "variable"    = structify_get_variable(filename),
     "description" = structify_get_description(filename),
     "dataset"     = structify_get_dataset(filename,...),
     "dimension"   = structify_get_dimension(filename,...),
     "source"      = structify_get_source(filename,...),
     stop("Variable name not recognized: ", vname))
}

 structify_get_variable <- function(filename){
   res <- structify_get_simple(filename=filename,group="variable/longname")
   return(res)
 }

 structify_get_description <- function(filename){
   res <- structify_get_simple(filename=filename,group="description/info")
   return(res)
 }

 structify_get_dimension <- function(filename){
   res <- structify_get_simple(filename=filename,group="dimension/values")
   return(res)
 }

 structify_get_source <- function(filename){
   res <- list(filename = structify_get_simple(filename=filename,group="source/filename"),
               format   = structify_get_simple(filename=filename,group="source/format"))
   return(res)
 }

structify_get_date <- function(filename,offset=NULL,count=NULL){

  res <- list(fromPeriod = structify_get_simple(filename=filename,group="date/fromPeriod"),
              toPeriod = structify_get_simple(filename=filename,group="date/toPeriod"),
              timeResolution = structify_get_simple(filename=filename,group="date/timeResolution"),
              v = structify_get_simple(filename=filename,group="date/v"),
              precision = structify_get_simple(filename=filename,group="date/precision"),
              tzone = structify_get_simple(filename=filename,group="date/tzone"))

  date <- timeManip::timeManip(fromPeriod     = res$fromPeriod,
                               toPeriod       = res$toPeriod,
                               timeResolution = res$timeResolution,
                               v              = res$v,
                               precision      = res$precision,
                               tzone          = res$tzone)
  if(!is.null(offset) && !is.null(count)){
    date2 <- timeManip::timeManip(fromPeriod    = timeManip::YYYYmmddHHMMSS(date$seqPeriod()[offset]),
                                 toPeriod       = timeManip::YYYYmmddHHMMSS(date$seqPeriod()[count]) ,
                                 timeResolution = date$timeResolution(),
                                 v              = date$v,
                                 precision      = date$precision(),
                                 tzone          = date$tzone())
    return(date2)
  } else return(date)


}

structify_get_spatial <- function(filename,offset=NULL,count=NULL){

  res <- list(cellcentre.offset = structify_get_simple(filename=filename,group="spatial/grid/cellcentre.offset"),
              cellsize = structify_get_simple(filename=filename,group="spatial/grid/cellsize"),
              cells.dim = structify_get_simple(filename=filename,group="spatial/grid/cells.dim"),
              projargs = structify_get_simple(filename=filename,group="spatial/grid/proj4string"))

  spatial <- spatialManip::construct(type="grid",
                                     x=list(cellcentre.offset=as.numeric(res$cellcentre.offset),
                                            cellsize=as.numeric(res$cellsize),
                                            cells.dim=as.numeric(res$cells.dim),CELLCENTER=FALSE),
                                     proj4S=spatialManip::crs(as.character(res$projargs)))

  if ( (!is.null(offset)) && (!is.null(count))) {
    spatial <- spatialManip::extract(spatial,indice=list(offset=offset,count=count))
  }

  return(spatial)
}


# rem:
# data : i:row, j:col <=> i:Y, j=X
structify_get_dataset<-function(filename,d=NULL,s=NULL,offset=NULL,count=NULL){

  if ( (is.null(offset)) && (is.null(count)) && (is.null(s)) && (is.null(d)) ){
    res <- structify_get_simple(filename=filename,group="dataset/values")
  } else if ( (is.null(offset)) && (is.null(count)) ) {

    spatial <- structify_get_spatial(filename)
    date <- structify_get_date(filename)
    if (is.null(s)) s <- structify_get_spatial(filename)
    if (is.null(d)) d <- structify_get_date(filename)

    indice <- indice(spatial,date,s,d)

    dim_num <- structify_get_dimension(filename)
    dim <- list(i=dim_num[1],j=dim_num[2],k=dim_num[3])

    res <- structify_getV(filename = filename, offset = list(i=indice$indice_main$indiceX$offset,j=indice$indice_main$indiceY$offset,k=indice$indice_main$indiceT$offset),
                                               count  = list(i=indice$indice_main$indiceX$count,j=indice$indice_main$indiceY$count,k=indice$indice_main$indiceT$count))

    # crop. Not the best method. To be improved
    if( !is.null(s) && !identical(indice$indice_sub$cropX,numeric(0)) && !identical(indice$indice_sub$cropY,numeric(0)) ){
      filter <- matrix(1,nrow=dim(res)[1],ncol=dim(res)[2])
      filter[cbind(indice$indice_sub$cropX,indice$indice_sub$cropY)] <- NA
      if (length(dim(res))>2) {
        res<-res*replicate(dim(res)[3],filter)
      } else if (length(dim(res))==2) {
        res<-res*filter
      }
    }
  } else if ( (!is.null(offset)) && (!is.null(count)) ) {
    res <- structify_getV(filename = filename,offset = offset,count  = count)
  } else stop("missing arguments")
  return(res)
}

# get Values
structify_getV<-function(filename,offset,count){
  h5_offset <-unlist(offset,use.names = FALSE)
  h5_count <- unlist(count,use.names = FALSE)
  file <- h5::h5file(filename, 'r')
  res <- h5::readDataSet(file["dataset/values"],h5::selectDataSpace(file["dataset/values"], offset=h5_offset, count=h5_count))
  h5::h5close(file)
  return(res)
}

structify_get_simple <- function(filename,group,i=NULL){
   file <- h5::h5file(filename, 'r')
   objT <- file[group]
   if (is.null(i)){
     obj <- objT[]
   } else obj <- objT[as.integer(i)]
   h5::h5close(objT)
   h5::h5close(file)
   return(obj)
 }
