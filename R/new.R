
structify_new <- function(filename,spatial=NULL,date=NULL,vinit=0,description=NULL,variable=NULL,source=NULL){
  if (file.exists(filename)) stop(filename," already exists!")

  dim <- spatialDate_dim(spatial=spatial,date=date)
  structify_put(filename=filename,vname="dimension",obj=unlist(dim,use.names = FALSE),NEW=TRUE)

  if (!is.null(vinit)) structify_put(filename=filename,vname="dataset",obj=array(as.numeric(vinit), dim = unlist(dim,use.names = FALSE)),offset=NULL,count=NULL,target=NULL,NEW=TRUE)
  if (!is.null(date)) structify_put(filename=filename,vname="date",obj=date,NEW=TRUE)
  if (!is.null(spatial)) structify_put(filename=filename,vname="spatial",obj=spatial,NEW=TRUE)

  if (is.null(description)) description <- "empty"
  if (is.null(variable))    variable <- "empty"
  if (is.null(source))      source <- list(filename="empty",format="empty")

  structify_put(filename=filename,"description",description,NEW=TRUE)
  structify_put(filename=filename,"variable",variable,NEW=TRUE)
  structify_put(filename=filename,"source",source,NEW=TRUE)

  invisible()
}
