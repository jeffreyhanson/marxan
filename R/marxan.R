#' @include RcppExports.R marxan-internal.R misc.R generics.R MarxanOpts.R MarxanData.R MarxanUnsolved.R MarxanResults.R MarxanSolved.R
NULL

#' General Marxan Function
#'
#' This is a general function to create Marxan objects from scratch and run the Marxan program to generate solutions.
#'
#' @param ... arguments are passed to MarxanData and MarxanOpts functions
#' @param solve "logical" should the problem be solved using Marxan?
#' @export
#' @note See the package vignette for help.
#' @return "MarxanSolved"  or "MarxanUnsolved"
#' @seealso \code{\link{MarxanOpts}}, \code{\link{MarxanData}}, \code{\link{MarxanResults}}, \code{\link{MarxanUnsolved}} 
marxan<-function(x, ...) UseMethod('marxan')

#' @describeIn marxan
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

#' @describeIn marxan
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
	
#' @describeIn marxan
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


	
