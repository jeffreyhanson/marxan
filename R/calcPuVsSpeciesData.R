#' @include RcppExports.R marxan-internal.R misc.R zonalSum.R
NULL

#' Calculate planning units vs. species data
#'
#' This function calculates the sum of species values in each planning unit.
#' Be aware that using polygons with overlaps will result in inaccuracies.
#' By default all polygons will be treated as having separate ids.
#' 
#' @param x "SpatialPolygons" or "SpatialPolygonsDataFrame" object.
#' @param y "RasterLayer", "RasterStack", or "RasterBrick" object.
#' @param ids "integer" vector of ids. Defaults to indices of layers in the \code{y}. 
#' @param ncores "integer" Number of cores to use for processing. Defaults to 1.
#' @param gdal "logical" Should raster processing be performed using GDAL libraries? Defaults to \code{FALSE}.
#' @param field "character" "integer" index or "character" name of column with planning unit ids. Valid only for "SpatialPolygonsDataFrame" objects. Default behaviour is to treat each polygon as a different planning unit.
#' @param ... Not used.
#' @return data.frame with sum of raster values in each polygon.
#' @seealso \code{\link{is.gdalInstalled}}, \code{\link{zonalSum}}, \url{http://www.gdal.org/}, \url{http://trac.osgeo.org/gdal/wiki/DownloadingGdalBinaries}.
#' @export
#' @examples
#' data(species, planningunits)
#' puvspr1.dat<-calcPuVsSpeciesData(planningunits, species[[1]])
#' puvspr2.dat<-calcPuVsSpeciesData(planningunits, species)
calcPuVsSpeciesData<-function(x, ...) UseMethod("calcPuVsSpeciesData")

#' @export
#' @describeIn calcPuVsSpeciesData
calcPuVsSpeciesData.SpatialPolygons<-function(x,y,ids=seq_len(nlayers(y)), ncores=1, gdal=FALSE, ...) {
	# check for invalid inputs
	stopifnot(inherits(y, "Raster"))
	stopifnot(nlayers(y)!=length(ids))
	return(
		calcPuVsSpeciesData.SpatialPolygonsDataFrame(
			x=SpatialPolygonsDataFrame(x@polygons, data=data.frame(id=seq_len(nrow(x@data)), row.names=laply(x@polygons, slot, name="ID"))),
			y=y,
			ids=ids,
			ncores=ncores,
			gdal=gdal,
			field="id"
		)
	)
}

#' @export
#' @describeIn calcPuVsSpeciesData
calcPuVsSpeciesData.SpatialPolygonsDataFrame<-function(x,y,ids=seq_len(nlayers(y)), ncores=1, gdal=FALSE, field=NULL, ...) {
	# check for invalid inputs
	stopifnot(inherits(y, "Raster"))
	stopifnot(nlayers(y)==length(ids))
	# prepare attribute table
	if (is.null(field)) {
		x@data<-data.frame(id=seq_len(nrow(x@data)), row.names=row.names(x@data))
	} else {
		x@data<-data.frame(id=x@data[[field]], row.names=row.names(x@data))
	}
	# generate raster layer with polygons
	if (gdal & is.gdalInstalled()) {
		x<-rasterize.gdal(x, y[[1]], "id")
	} else {
		x<-rasterize(x, y[[1]], method="ngb")
	}
	# main processing
	return(zonalSum(x, y, ids, ncores))
}
