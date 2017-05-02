#' structify_summary
#'
#' structify_summary
#' @param filename filename
#' @keywords structify
#' @export
#' @examples
#' \dontrun{
#' structify_summary()
#' }
structify_summary <- function(filename){
  return(str(get_summary(filename)))
}


get_summary <- function(filename){
  res <- list(description    = structify_get_simple(filename=filename,group="description/info"),
              dimension      = structify_get_simple(filename=filename,group="dimension/values"),
              fromPeriod     = structify_get_simple(filename=filename,group="date/fromPeriod"),
              toPeriod       = structify_get_simple(filename=filename,group="date/toPeriod"),
              timeResolution = structify_get_simple(filename=filename,group="date/timeResolution"),
              v              = structify_get_simple(filename=filename,group="date/v"),
              precision      = structify_get_simple(filename=filename,group="date/precision"),
              nbStep         = structify_get_simple(filename=filename,group="date/nbStep"),
              tzone          = structify_get_simple(filename=filename,group="date/tzone"),
              cellsize       = structify_get_simple(filename=filename,group="spatial/grid/cellsize"),
              cells.dim      = structify_get_simple(filename=filename,group="spatial/grid/cells.dim"),
              bbox           = structify_get_simple(filename=filename,group="spatial/grid/bbox"),
              projargs       = structify_get_simple(filename=filename,group="spatial/grid/proj4string")
              )
  return(res)
}
