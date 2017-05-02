#' indice
#'
#' indice
#' @param spatial S4 spatial object
#' @param date object date from timeManip
#' @param s sub S4 spatial object
#' @param d sub object date from timeManip
#' @keywords structify
#' @export
#' @examples
#' \dontrun{
#' indice()
#' }
indice <- function(spatial=NULL,date=NULL,s=NULL,d=NULL) {

  indice_spatial <- NULL
  indice_date <- NULL

  if(!is.null(s)) {
    indice_spatial <- spatialManip::indice_subspatial(spatial,s)
  }
  if (!is.null(d)){
    indice_date <- timeManip::indice_subdate(date,d)
  }

  return(list(indice_sub  = c(indice_spatial$indice_sub,indice_date$indice_sub),
              indice_main = c(indice_spatial$indice_main,indice_date$indice_main)))
}
