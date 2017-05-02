#' structify
#'
#' structify
#' @param filename filename
#' @keywords structify
#' @export
#' @examples
#' \dontrun{
#' structify()
#' }
structify <- function(filename){

   structify_object(filename)
}

structify_object <- function(filename){

  object <- local({

    new <- function(spatial=NULL,date=NULL,vinit=NULL,description=NULL,variable=NULL,source=NULL){
       structify_new(filename,spatial=spatial,date=date,vinit=vinit,description=description,variable=variable,source=source)
    }

    put <- function(vname,obj,...){
      structify_put(filename,vname=vname,obj=obj,...)
      invisible()
    }

    get <- function(vname,...){
      structify_get(filename,vname=vname,...)
    }

    extend <- function(count){
      structify_extend(filename,count)
    }

    summary <- function(){
      structify_summary(filename)
    }

    environment()
    })
    lockEnvironment(object, TRUE)
    structure(object, class=c("structify", class(object)))

}
