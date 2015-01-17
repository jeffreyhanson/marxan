#' @include RcppExports.R marxan-internal.R misc.R
NULL

#' Calculate Boundary Data
#'
#' This function calculates boundary length data for PolySet, SpatialPolygons, and SpatialPolygonsDataFrame objects.
#' Be aware that this function is designed to be as fast as possible, as a result it depends on C++ code and if this 
#' function is used improperly, then it will crash R. Furthermore, multipart polygons with touching edges will likely result in inaccuracies.
#' If argument set to SpatialPolygons or SpatialPolygonsDataFrame, this will be converted to PolySet before processing.
#'
#' @param x PolySet, SpatialPolygons or SpatialPolyognsDataFrame.
#' @param tolerance numeric to specify precision of vertices.
#' @param lengthFactor numeric to scale boundary lengths.
#' @param edgeFactor numeric to scale boundary lengths that do not have any neighbors.
#' @return data.frame with columns 'id1', 'id2', and 'amount'.
#' @seealso this function is based on the QMARXAN algorithm \code{\url{http://aproposinfosystems.com/products/qmarxan/}} for calculating boundary length.
#' @export
#' @examples 
#' data(planningunits)
#' bound.dat <- calcBoundaryData(planningunits)
#' summary(bound.dat)
setGeneric("calcBoundaryData", function(x, ...) standardGeneric("calcBoundaryData"))

#' describeIn calcBoundaryData
#' @export
setMethod(
	"calcBoundaryData",
	signature(x="PolySet"),
	function(x, tolerance=0.001, lengthFactor=1.0, edgeFactor=1.0) {
		ret<-.rcpp_calcBoundaryDF(x, tolerance=tolerance, lengthFactor=lengthFactor, edgeFactor=edgeFactor)
		if (!is.null(ret$warnings)) {
			warning("Invalid geometries detected, see element \"warnings\" for more information.")
			return(ret)
		}
		return(ret)
	}
)

#' describeIn calcBoundaryData
#' @export
setMethod(
	"calcBoundaryData",
	signature(x="SpatialPolygons"),
	function(x, tolerance=0.001, lengthFactor=1.0, edgeFactor=1.0) {
		ret<-rcpp_calcBoundaryDF(rcpp_Polygons2PolySet(x@polygons), tolerance=tolerance, lengthFactor=lengthFactor, edgeFactor=edgeFactor)
		if (!is.null(ret$warnings)) {
			warning("Invalid geometries detected, see element \"warnings\" for more information.")
			return(ret)
		}
		return(ret)
	}
)



