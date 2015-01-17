##### Load Dependencies
library(maptools)
library(raster)

##### Load data

##### Data preparation
# rasterize
test_that("rasterize functions produce similar output", {
	data(planningunits, species)
	purast1<-rasterize(planningunits, species[[1]])
	purast2<-rasterize.gdal(planningunits, species[[1]])
	expect_equal(purast1, purast2)
})

# zonal sum
test_that("RasterLayer zonal sum functions produce similar output", {
	data(planningunits, species)
	purast<-rasterize(planningunits, species[[1]])
	zs1<-zonalSum(purast, species[[1]], ncores=2)
	zs2<-zonalSum(purast, species[[1]])
	expect_equal(round(getValues(zs1),5), round(getValues(zs2),5))
})

test_that("RasterStack zonal sum functions produce similar output", {
	data(planningunits, species)
	purast<-rasterize(planningunits, species[[1]])
	zs1<-zonalSum(purast, species, ncores=2)
	zs2<-zonalSum(purast, species)
	expect_equal(zs1, zs2)
})

# convert SpatialPolygons to PolyData
test_that("SpatialPolygons2PolyData functions produce similar output", {
data(planningunits, species)
	data(planningunits, species)
	pdf1<-marxan:::rcpp_Polygons2PolySet(planningunits@polygons)
	pdf2<-maptools::SpatialPolygons2PolySet(planningunits)
	expect_equal(zs1, zs2)
})

# generate boundary length data.frame
test_that("boundary length data function works", {
	data(planningunits, species)
	bldf2<-calcBoundaryData(maptools::SpatialPolygons2PolySet(planningunits))
	bldf1<-calcBoundaryData(planningunits@polygons)
	expect_equal(bldf1, bldf2)
})

# calculate puvspecies data
test_that("pu v species calculations functions work for RasterLayer", {
	data(planningunits, species)
	pvs1<-zonal(planninguits, species[[1]], "sum")
	pvs2<-calcPuVsSpeciesData(planninguits, species[[1]])
	pvs3<-calcPuVsSpeciesData(planninguits, species[[1]], ncores=2)
	expect_equal(pvs1,pvs2,pvs3)
})

test_that("pu v species calculations functions work for RasterStack", {
	data(planningunits, species)
	pvs1<-zonal(planninguits, species, "sum")
	pvs2<-calcPuVsSpeciesData(planninguits, species)
	pvs3<-calcPuVsSpeciesData(planninguits, species, ncores=2)
	expect_equal(pvs1,pvs2,pvs3)
})

