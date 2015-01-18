# library(maptools)
# library(raster)

# # rasterize
# test_that("native rasterize function and GDAL interface produce different output", {
	# template<-disaggregate(raster(matrix(1:9, ncol=3), xmn=0, xmx=1, ymn=0, ymx=1, crs=CRS('+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ')),fact=5)
	# polys<-rasterToPolygons(template, n=4, dissolve=TRUE)
	# purast1<-rasterize(polys, template)
	# purast2<-rasterize.gdal(polys, template, field='layer')
	# expect_equal(getValues(purast1), getValues(purast2))
# })

# # zonal sum
# test_that("Zonal sum functions produce different output", {
	# purast<-disaggregate(raster(matrix(1:9, ncol=3)),fact=5)
	# species<-purast*abs(rnorm(ncell(purast)))
	# zs1<-zonal(species, purast)
	# zs2<-zonalSum(purast, species[[1]], ncores=2)
	# zs3<-zonalSum(purast, species[[1]])
	# expect_equal(zs1[,2], round(zs2[[3]],5), round(zs3[[3]],5))
# })

# # convert SpatialPolygons to PolySet
# test_that("raster and marxan polygons to PolySet functions produce different output", {
	# template<-disaggregate(raster(matrix(1:9, ncol=3), xmn=0, xmx=1, ymn=0, ymx=1, crs=CRS('+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ')),fact=5)
	# polys<-rasterToPolygons(template, n=4, dissolve=TRUE)
	# pdf1<-marxan:::rcpp_Polygons2PolySet(polys@polygons)
	# pdf2<-maptools::SpatialPolygons2PolySet(polys)
	# expect_identical(pdf1[[1]], pdf2[[1]])
	# expect_identical(pdf1[[2]], pdf2[[2]])
	# expect_identical(pdf1[[3]], pdf2[[3]])
	# expect_identical(pdf1[[4]], pdf2[[4]])
	# expect_identical(pdf1[[5]], pdf2[[5]])
# })

# # generate boundary length data.frame
# test_that("boundary length data doesn't work", {
	# polys<-rasterToPolygons(
		# raster(
			# matrix(1:9, ncol=3), 
			# xmn=0, xmx=1, ymn=0, ymx=1, 
			# crs=CRS('+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ')
		# ),
		# n=4
	# )
	# bldf1<-calcBoundaryData(maptools::SpatialPolygons2PolySet(polys))
	# bldf2<-calcBoundaryData(polys)
	# expect_equal(bldf1, bldf2)
# })

# # calculate puvspecies data
# test_that("pu v species calculations functions don't work", {
	# template<-disaggregate(raster(matrix(1:9, ncol=3), xmn=0, xmx=1, ymn=0, ymx=1, crs=CRS('+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ')),fact=5)
	# polys<-rasterToPolygons(template, n=4, dissolve=TRUE)
	# species<-setValues(template, round(runif(ncell(template))))
	# pvs1<-zonal(species, template, "sum")
	# pvs2<-calcPuVsSpeciesData(polys, species)
	# pvs3<-calcPuVsSpeciesData(polys, species, ncores=2)
	# pvs4<-calcPuVsSpeciesData(polys, species, gdal=TRUE)
	# expect_equal(pvs1[,2],pvs2[[3]],pvs3[[3]],pvs4[[3]])
# })

