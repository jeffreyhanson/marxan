#' @include marxan-internal.R
NULL

#' Zonal Sum
#'
#' This function calculates the zonal sum of SpatialPolygons in a RasterLayer, RasterStack, or RasterBrick.
#' For most cases it is expected to run faster than the zonal functon in the raster package.
#' This function is useful for extracting the sum of raster values inside each planning unit.
#' 
#' @param x SpatialPolygons or SpatialPolygons object
#' @param y RasterLayer, RasterStack, or RasterBrick
#' @param speciesName character vector of species names
#' @param ncores Number of cores to use for processing data.
#'
#' @return data.frame with sum of raster values in each polygon.
#' @seealso \code{\link[raster]{zonal}}
#' @export
#' @examples
#' data(species, planningunits)
#' purast<-rasterize(planningunits, species[[1]])
#' zonalSum(purast, species[[1]])
#' zonalSum(purast, species)
setGeneric("zonalSum", function(x, y, ...) standardGeneric("zonalSum"))

#' @describeIn zonalSum
#' @export
setMethod(
	"zonalSum",
	signature(x="RasterLayer", y="RasterStackOrBrick"),
	function(x, y, speciesNames=names(y), ncores=1) {
		if (canProcessInMemory(x,2)) {
			return(rbind.fill(llply(seq_len(nlayers(y)), function(l) {
					return(.zonalSum.RasterLayerInMemory(x, y[[l]], speciesNames[l]))
			})))
		} else {
			bs<-blockSize(x)	
			if (ncores>1) {
					clust<-makeCluster(ncores, type="SOCK")
					clusterEvalQ(clust, {library(raster);library(Rcpp)})
					clusterExport(clust, c("bs", "x", "rcpp_groupsum"))
					registerDoSNOW(clust)
			}
			# main processing
			return(rbind.fill(llply(seq_len(nlayers(y)), function(l) {
				return(.zonalSum.RasterLayerNotInMemory(bs, x, y[[l]], speciesNames[l], registered=ncores>1))
			})))
		}
	}
)

#' @describeIn zonalSum
#' @export
setMethod(
	"zonalSum",
	signature(x="RasterLayer", y="RasterLayer"),
	function(x, y, speciesNames=names(y), ncores=1) {
		if (canProcessInMemory(x,2)) {
			return(.zonalSum.RasterLayerInMemory(x, y, speciesNames))
		} else {
			bs<-blockSize(x)
			if(ncores>1) {
				clust<-makeCluster(ncores, type="SOCK")
				clusterEvalQ(clust, {library(raster);library(Rcpp)})
				registerDoSNOW(clust)
			}
			return(.zonalSum.RasterLayerNotInMemory(bs, y, speciesNames, ncores, ncores>1))
		}
	}
)
