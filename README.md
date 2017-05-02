# structify

[![Travis-CI Build Status](https://travis-ci.org/jeanmarielepioufle/timeManip.svg?branch=master)](https://travis-ci.org/jeanmarielepioufle/structify)

Structify makes easy the manipulation of large space-time datasets.

The package focuses on providing simple and efficient ways to manipulate large space-time datasets.

i- it reduces the problem of memory limit by using data container.

ii- Spatial and temporal information and values are inter-connected for an easier exploitation.

iii- The data container can be exploited by any language using the hdf5 protocol.

## Installation

```R
# install.packages("devtools")
devtools::install_github("jeanmarielepioufle/structify")
```

## Usage

```R
library(structify)

# spatial grid
x <- spatialManip::xGridBuild(xmin=0,xmax=1000,ymin=0,ymax=1000,cellsize=1,CELLCENTER=TRUE)
spatial <- spatialManip::construct(type="grid",x,proj4S=spatialManip::crs("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs"))

# date
date <- timeManip::timeManip(fromPeriod="2017010106",toPeriod="2017011006",timeResolution="daily")

# new structify object
structObj <- structify::structify(filename="~\\x.tmp")
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
```

I am working on making a vignette.

## Questions and remarks
Don't hesitate to contact me for more details and suggestions.
