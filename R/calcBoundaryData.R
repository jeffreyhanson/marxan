#' @include RcppExports.R marxan-internal.R misc.R
NULL

#' Calculate boundary data for planning units
#'
#' This function calculates boundary length data for "PolySet", "SpatialPolygons", and "SpatialPolygonsDataFrame" objects.
#' Be aware that this function is designed to be as fast as possible, as a result it depends on C++ code and if this 
#' function is used improperly, then it will crash R. Furthermore, multipart polygons with touching edges will likely result in inaccuracies.
#' If argument set to SpatialPolygons or SpatialPolygonsDataFrame, this will be converted to PolySet before processing.
#'
#' @param x "PolySet", "SpatialPolygons" or "SpatialPolyognsDataFrame" object.
#' @param tolerance "numeric" to specify precision of calculations (ie. how far apart do vertices have to be to be considered different).
#' @param lengthFactor "numeric" to scale boundary lengths.
#' @param edgeFactor "numeric" to scale boundary lengths for edges that do not have any neighbors, such as those that occur along the margins.
#' @param ... not used.
#' @return "data.frame" with "integer" 'id1', "integer" 'id2', and "numeric" 'amount' columns.
#' @seealso this function is based on the algorithm on by QMARXAN \url{http://aproposinfosystems.com/products/qmarxan/} for calculating boundary length.
#' @export
#' @examples 
#' data(taspu)
#' bound.dat <- calcBoundaryData(taspu)
#' summary(bound.dat)
calcBoundaryData<-function(x,...) UseMethod("calcBoundaryData")

#' @rdname calcBoundaryData
#' @inheritParams calcBoundaryData
#' @export
calcBoundaryData.PolySet<-function(x, tolerance=0.001, lengthFactor=1.0, edgeFactor=1.0, ...) {
	ret<-rcpp_calcBoundaryDF(x, tolerance=tolerance, lengthFactor=lengthFactor, edgeFactor=edgeFactor)
	if (length(ret$warnings)!=0) {
		warning("Invalid geometries detected, see \"warnings\" attribute for more information.")
		attr(ret$bldf, "warnings")<-ret$warnings
	}
	return(ret$bldf)
}


#' @rdname calcBoundaryData
#' @inheritParams calcBoundaryData
#' @export
calcBoundaryData.SpatialPolygons<-function(x, tolerance=0.001, lengthFactor=1.0, edgeFactor=1.0, ...) {
	return(calcBoundaryData(rcpp_Polygons2PolySet(x@polygons), tolerance=tolerance, lengthFactor=lengthFactor, edgeFactor=edgeFactor))
}


