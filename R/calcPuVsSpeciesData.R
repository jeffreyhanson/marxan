#' @include RcppExports.R marxan-internal.R misc.R zonalSum.R
NULL

#' Calculate Planning Units vs. Species Data
#'
#' This function calculates the sum of species values in each planning unit.
#' Be aware that using polygons with overlaps will result in inaccuracies.
#' By default all polygons will be treated as having seperate ids.
#' 
#' @param x SpatialPolygons or SpatialPolygonsDataFrame object
#' @param y RasterLayer, RasterStack, or RasterBrick
#' @param speciesNames character vector of species names
#' @param ncores Number of cores to use for processing
#' @param gdal Logical. Should raster processing be performed using python gdal libraries?
#' @param field character if not NULL and \code{x} is a SpatialPolygonsDataFrame object, the index or name of the column with planning unit ids
#'
#' @return data.frame with sum of raster values in each polygon.
#' @seealso \code{\link{is.gdalInstalled}}, \code{\link{zonalSum}}
#' @export
#' @examples
#' data(species, planningunits)
#' calcPuVsSpeciesData(planningunits, species[[1]])
#' calcPuVsSpeciesData(planningunits, species)
calcPuVsSpeciesData <- function(x, y, speciesNames=names(y), ncores=1, gdal=FALSE, field=NULL) {
	# check for invalid inputs
	stopifnot(inherits(x, "SpatialPolygons"))
	if (!is.null(field) & !inherits(x, "SpatialPolygonsDataFrame"))
		stop("argument to field must be null if argument to x is not SpatialPolygonsDataFrame")
	stopifnot(inherits(y, c("RasterLayer", "RasterStack", "RasterBrick")))
	if(inherits(y, "RasterLayer") & length(speciesNames)>1)
		warning("y is a RasterLayer and multiple speciesNames provided, so only first name will be used.")
	if (inherits(y, c("RasterLayer", "RasterStack"))) {
		if (length(speciesNames)!=nlayers(y)) {
			warning("The number of layers in y and the length of speciesNames is different, so speciesNames will be set as names(y).")
			speciesNames=names(y)
		}
	}
	# prepare attribute table
	if (is.null(field)) {
		if (inherits(x, "SpatialPolygonsDataFrame")) {
			x@data<-data.frame(id=seq_len(nrow(x@data)), row.names=row.names(x@data))
		} else {
			x@data<-SpatialPolygonsDataFrame(x@polygons, data=data.frame(id=seq_len(nrow(x@data)), row.names=laply(x@polygons, slot, name="ID")))
		}
	} else {
		x<-SpatialPolygonsDataFrame(x, data=data.frame(id=x@data[[field]], row.names=row.names(x@data)))
	}
	# generate raster layer with polygons
	temprast=y
	if (inherits(y, c("RasterBrick", "RasterStack")))
		temprast=y[[1]]
	if (gdal & is.gdalInstalled()) {
		x<-rasterize.gdal(x, temprast, "id")
	} else {
		x<-rasterize(x, temprast, method="ngb")
	}
	# main processing
	return(zonalSum(x, y, speciesNames, ncores))
}
