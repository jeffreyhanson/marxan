#' @include RcppExports.R marxan-internal.R misc.R
NULL

#' Convert "SpatialPolygons" to "PolySet" data
#'
#' This function converts spatial objects "SpatialPolygons" and "SpatialPolygonsDataFrame" to "PolySet".
#' 
#' @param x "SpatialPolygons" or "SpatialPolygonsDataFrame" object.
#' @param n_preallocate "integer" How much memory should be preallocated for processing? Ideally, this number should equal the number of vertices in the "SpatialPolygons" object. If dtaa processing is taking too long consider increasing this value.
#' @usage SpatialPolygons2PolySet(x, n=10000L)
#' @return "PolySet" object.
#' @note Be aware that this function is designed to be as fast as possible, but as a result it depends on C++ code and if used inappropriately this function will crash R.
#' @seealso For a slower, more stable equivalent see \code{\link[maptools]{SpatialPolygons2PolySet}}.
#' @export
#' @examples 
#' data(planningunits)
#' x <- SpatialPolygons2PolySet(planningunits)
setGeneric("SpatialPolygons2PolySet", function(x, ...) standardGeneric("SpatialPolygons2PolySet"))

#' describeIn SpatialPolygons2PolySet
#' @export
setMethod(
	"SpatialPolygons2PolySet",
	signature(x="SpatialPolygonsDataFrame"),
	function(x, n_preallocate = 10000L) {
		return(rcpp_Polygons2PolySet(x@polygons))
	}
)

#' describeIn SpatialPolygons2PolySet
#' @export
setMethod(
	"SpatialPolygons2PolySet",
	signature(x="SpatialPolygons"),
	function(x, n_preallocate = 10000L) {
		return(rcpp_Polygons2PolySet(x@polygons))
	}
)



