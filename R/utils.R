spatialDate_dim <- function(spatial=NULL,date=NULL){
  if ( (!is.null(spatial)) && (!is.null(date))) {
    dim <- list( i = spatial@grid@cells.dim[1],
                 j = spatial@grid@cells.dim[2],
                 k = date$nbStep())
  } else if ( (!is.null(spatial)) && (is.null(date)) ) {
    dim <- list( i = spatial@grid@cells.dim[1],
                 j = spatial@grid@cells.dim[2],
                 k = 1)
  } else if ( (is.null(spatial)) && (!is.null(date)) ) {
    dim <- list( i = 1,
                 j = 1,
                 k = date$nbStep())
  } else stop("Both spatial and date are NULL")

  return(dim)
}

obj_extract <- function(obj,offset=NULL,count=NULL,varid=NULL){
  type <- class(obj)[1]
  res <- switch(type,
    "limonade"   = obj$extract(varid=varid,offset=offset,count=count)$vals,
    "limonadeST" = obj$values(of=offset,co=count),
    "vector"     = obj_arrayLike(obj=obj,offset=offset,count=count),
    "matrix"     = obj_arrayLike(obj=obj,offset=offset,count=count),
    "array"      = obj_arrayLike(obj=obj,offset=offset,count=count),
    stop("Object type not recognized: ", type))
  return(res)
}

obj_arrayLike <- function(obj,offset=NULL,count=NULL){
  if (is.null(offset) && is.null(count)){
    return(obj)
  } else {
    fillin_i <- paste(sort(names(offset)),collapse=",")
    res <- switch(fillin_i,
      "i"     = obj[(offset$i:(offset$i+count$i-1))],
      "j"     = obj[(offset$j:(offset$j+count$j-1))],
      "k"     = obj[(offset$k:(offset$k+count$k-1))],
      "i,j"   = obj[(offset$i:(offset$i+count$i-1)),offset$j:(offset$j+count$j-1)],
      "i,k"   = obj[(offset$i:(offset$i+count$i-1)),offset$k:(offset$k+count$k-1)],
      "j,k"   = obj[(offset$j:(offset$j+count$j-1)),offset$k:(offset$k+count$k-1)],
      "i,j,k" = obj[(offset$i:(offset$i+count$i-1)),offset$j:(offset$j+count$j-1),offset$k:(offset$k+count$k-1)],
      stop("dimensions not recognized, ",fillin_i))
     return(res)
  }
}


#' @export
print.structify <- function(x,...){
  x$summary()
}
