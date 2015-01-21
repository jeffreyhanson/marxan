#' @include RcppExports.R marxan-internal.R misc.R generics.R MarxanOpts.R MarxanData.R MarxanUnsolved.R MarxanResults.R MarxanSolved.R
NULL

#' General Marxan Function
#'
#' This is a general function to create Marxan objects from scratch and run the Marxan program to generate solutions.
#'
#' @param x generic argument.
#' @param ... arguments are passed to MarxanData and MarxanOpts functions.
#' @param path 'character' file path to Marxan input parameters file.
#' @param polygons 'SpatialPolyogns' object representing planning units.
#' @param rasters 'Raster' object with species distribution data.
#' @param pu "data.frame" planning unit data; with "integer" 'id', "numeric" 'cost', "integer" 'status' columns.
#' @param species "data.frame" with species data; with "integer" 'id', "numeric" 'target', "numeric" 'spf', and "character" 'name' columns.
#' @param puvspecies "data.frame" with data on species density in each planning unit, with "integer" 'species', "integer" 'pu', and "numeric" 'target' columns. This "data.frame" is sorted in order of 'pu' column.
#' @param boundary "data.frame" with data on the shared boundary length of planning; with "integer" 'id1', "integer" 'id2', and "numeric" 'amount' columns.
#' @param solve "logical" should the problem be solved using Marxan?
#' @export
#' @note See the package vignette for help.
#' @return "MarxanSolved"  or "MarxanUnsolved"
#' @seealso \code{\link{MarxanOpts}}, \code{\link{MarxanData}}, \code{\link{MarxanResults}}, \code{\link{MarxanUnsolved}} 
marxan<-function(x, ...) UseMethod('marxan')

#' @rdname marxan
#' @inheritParams marxan
#' @export
marxan.character<-function(path, solve=TRUE) {
	if(file.exists(file.path(basename(path), 'output_sum.csv'))) {
		return(read.MarxanSolved(path))
	} else {
		x<-read.MarxanUnsolved(path)
		if (solve)
			x<-solve.MarxanUnsolved(x)
		return(x)
	}
}

#' @rdname marxan
#' @inheritParams marxan
#' @export
marxan.SpatialPolygons<-function(polygons, rasters, ..., solve=TRUE) {
	x<-MarxanUnsolved(
		MarxanOpts(..., ignore.extra=TRUE),
		format.MarxanData(polygons=polygons, rasters=rasters, ...)
	)
	if (solve)
		x<-solve.MarxanUnsolved(x)
	return(x)
}
	
#' @rdname marxan
#' @inheritParams marxan
#' @export
marxan.data.frame<-function(pu,species,puvspecies,boundary, ..., solve=TRUE) {
	x<-MarxanUnsolved(
		MarxanOpts(..., ignore.extra=TRUE),
		MarxanData(pu=pu, species=species, puvspecies=puvspecies, boundary=boundary, ...)
	)
	if (solve)
		x<-solve.MarxanUnsolved(x)
	return(x)
}


	
