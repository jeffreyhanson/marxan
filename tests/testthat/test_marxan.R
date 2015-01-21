library(rgdal)
library(raster)
library(maptools)
library(plyr)
library(data.table)

# test main marxan function
test_that("main marxan function doesn't work", {
	# make some data
	template<-disaggregate(raster(matrix(1:9, ncol=3), xmn=0, xmx=1, ymn=0, ymx=1, crs=CRS('+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ')),fact=5)
	polys<-rasterToPolygons(template, n=4, dissolve=TRUE)	
	species<-setValues(template, round(runif(ncell(template))))
	# marxan function
	ms1<-marxan(polys, species, NUMITNS=8L, NUMTEMP=4L)
	ms2<-marxan(polys, species, species=data.frame(id=1L, spf=5, target=30, name='species1'), NUMITNS=8L, NUMTEMP=4L)
	ms3<-marxan(polys, species, species=data.frame(id=1L, spf=90, target=12, name='species1'), PROP=0.9, NUMITNS=8L, NUMTEMP=4L)
})
