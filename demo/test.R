library(structify)

# spatial grid
x <- spatialManip::xGridBuild(xmin=0,xmax=1000,ymin=0,ymax=1000,cellsize=1,CELLCENTER=TRUE)
spatial <- spatialManip::construct(type="grid",x,proj4S=spatialManip::crs("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs"))

# date
date <- timeManip::timeManip(fromPeriod="2017010106",toPeriod="2017011006",timeResolution="daily")

filename <- "~\\x.tmp"
structObj <- structify::structify(filename)

# new structify object
structObj$new(spatial,date,-999.99)

# put information
structObj$put("variable","donuts")
structObj$put("description","a lot of donuts")
structObj$put("dataset",array(rnorm(1000*1000),dim=c(1000,1000,10)),offset=list(i=1,j=1,k=1),count=list(i=1000,j=1000,k=10),varid=NULL,target=list(i=1,j=1,k=1))

# print
structObj

# get information
tmp  <- structObj$get("spatial")
tmp2 <- structObj$get("date")
tmp3 <- structObj$get("dataset")
tmp4 <- structObj$get("dimension")
tmp5 <- structObj$get("dataset",offset=list(i=1,j=1,k=1),count=list(i=1000,j=1000,k=1))
tmp6 <- structObj$get("spatial",offset=list(i=1,j=1),count=list(i=1000,j=1000))
tmp7 <- structObj$get("date",offset=1,count=1) # error because one day so not a vector


# extend
structObj$extend(count=list(i=1,j=8,k=3))
