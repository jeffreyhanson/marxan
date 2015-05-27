#' @include marxan-internal.R
NULL

#' Zonal sum
#'
#' This function calculates the zonal sum of "SpatialPolygons" in a "Raster" object.
#' For most cases it is expected to run faster than the zonal functon in the raster package.
#' This function is useful for extracting the sum of raster values inside each planning unit.
#' 
#' @param x "SpatialPolygons" or "SpatialPolygonsDataFrame" object.
#' @param y "RasterLayer", "RasterStack", or "RasterBrick" object.
#' @param ids "integer" vector of species ids in \code{y}. Defaults to indices of \code{y}.
#' @param ncores "integer" Number of cores to use for processing data. Defaults to 1.
#' @return "data.frame" with "integer" 'species', "integer" 'pu', and "numeric" 'amount' columns.
#' @note This function is designed to be used to generate inputs for Marxan. The column names are appropriate for the 'puvspr.dat' file. Data is sorted by values in 'pu' column.
#' @seealso \code{\link[raster]{zonal}}.
#' @export
#' @examples
#' data(species, planningunits)
#' rast<-rasterize(planningunits, species[[1]], 'sum')
#' zonalSum(planningunitsraster, species[[1]])
#' zonalSum(planningunitsraster, species)
setGeneric("zonalSum", function(x, y, ...) standardGeneric("zonalSum"))

#' @rdname zonalSum
#' @inheritParams zonalSum
#' @export
setMethod(
	"zonalSum",
	signature(x="RasterLayer", y="Raster"),
	function(x, y, ids=names(y), ncores=1) {
		if (canProcessInMemory(x,2)) {
			x<-rbind.fill(llply(seq_len(nlayers(y)), function(l) {
					return(zonalSum.RasterLayerInMemory(x, y[[l]], ids[l]))
			}))
		} else {
			bs<-blockSize(x)	
			if (ncores>1) {
				clust<-makeCluster(ncores, type="SOCK")
				clusterEvalQ(clust, {library(raster);library(Rcpp)})
				clusterExport(clust, c("bs", "x", "rcpp_groupsum"))
				registerDoSNOW(clust)
			}
			x<-rbind.fill(llply(seq_len(nlayers(y)), function(l) {
				return(zonalSum.RasterLayerNotInMemory(bs, x, y[[l]], ids[l], registered=ncores>1))
			}))
			if (ncores>1)
				clust<-stopCluster(clust)
		}
		# sort data and return
		return(x[order(x$pu),])
	}
)

