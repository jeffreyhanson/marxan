#' MarxanSolved: An S4 class to represent Marxan inputs and outputs
#'
#' This class is used to store Marxan input and output data in addition to input parameters.
#'
#' @slot data "MarxanData" object used to store input data.
#' @slot opts "MarxanOpts" object used to store input parameters.
#' @slot results "MarxanResults" object used to store results.
#' @seealso \code{\link{MarxanOpts-class}}, \code{\link{MarxanData-class}}, \code{\link{MarxanResults-class}}
setClass("MarxanSolved",
	representation(
		data="MarxanData",
		opts="MarxanOpts",
		results="MarxanResults"
	)
)

#' Create new MarxanSolved object
#'
#' This function creates a MarxanSolved object using a "MarxanUnsolved" object and a "MarxanResults" object.
#'
#' @param unsolved "MarxanUnsolved" object.
#' @param results "MarxanResults" object.
#' @return "MarxanUnsolved" object.
#' @seealso \code{\link{MarxanUnsolved-class}}, \code{\link{MarxanResults-class}}
MarxanSolved<-function(MarxanUnsolved, MarxanResults) {
	return(new("MarxanUnsolved", opts=unsolved@opts, data=unsolved@data reuslts=results))
}

#' @describein solve
solve.MarxanUnsolved=function(x, wd=tempdir(), seeds=sample.int(n=10000L, size=x@opts@NCORES), clean=TRUE, force_reset=FALSE) {
	if (!force_reset)
		stop("This object already has Marxan solutions. Use force_reset=TRUE to force recalculation of solutions.")
	return(solve(MarxanUnsolved(opts=x@opts,data=x@data), wd, seeds, clean)
}

#' @describein selection
selection.MarxanSolved<-function(x, y="best") {
	return(selection.MarxanResults(x@results, y))
}

#' @describein score
score.MarxanSolved<-function(x, y="best") {
	return(score.MarxanResults(x@results, y))
}

#' @describein summary
summary.MarxanSolved<-function(x) {
	return(summary.MarxanResults(x@results))
}

#' @describein print
print.MarxanSolved=function(x) {
	cat("MarxanSolved object.\n")
	print.MarxanOpts(x@opts, FALSE)
	print.MarxanData(x@data, FALSE)
	print.MarxanResults(x@results, FALSE)
}

#' @describein log
log.MarxanSolved=function(x) {
	log.MarxanResults(x@results)
}

#' @describein amountHeld
amountHeld.MarxanSolved<-function(x, y="best") {
	return(amountHeld.MarxanResults(x@results, y))
}

#' @describein occHeld
occHeld.MarxanSolved<-function(x, y="best") {
	return(occHeld.MarxanResults(x@results, y))

}

#' @describein pca
pca.MarxanSolved=function(x, var='selections', ... force_reset=FALSE) {
	return(dist.MarxanResults(x@results, var, ..., force_reset=force_reset))
}

#' @describein dist
dist.MarxanSolved=function(x, var='selections', method="bray", force_reset=FALSE) {
	return(dist.MarxanResults(x@results, var, method, force_reset=force_reset))
}

#' @describein mds
mds.MarxanSolved=function(x, var='selections', method="bray", ..., force_reset=FALSE) {
	return(mds.MarxanResults(x@results, var, method, ..., force_reset=force_reset))
}

#' @describein hclust
hclust.MarxanSolved=function(x, type='mds', var='selections', ..., force_reset=FALSE) {
	return(mds.MarxanResults(x@results, type='mds', var='selections', ..., force_reset=force_reset))
}

#' @describein ordiplot
ordiplot.MarxanSolved=function(x, type='mds', var='selections', nbest=1, ..., force_reset=FALSE) {
	return(ordiplot.MarxanResults(x@results, type, var, nbest, ..., force_reset=force_reset))
}

#' @describein dendrogram
dendrogram.MarxanSolved=function(x, type='mds', var='selections', nbest=1, ..., force_reset=FALSE) {
	return(dendrogram.MarxanResults(x@results, type, var, nbest, ..., force_reset=force_reset))
}

#' @describein dotchart
dotchart.MarxanSolved(x, var="score", nbest=1) {
	dotchart.MarxanResults(x@results, var, nbest)
}

#' @describein basemap
basemap.MarxanSolved<-function(x, basemap="none", alpha=1, grayscale=FALSE, xzoom=c(1,1), yzoom=c(1,1), force_reset=FALSE) {
	return(basemap.MarxanData(x@data, basemap, alpha, grayscale, xzoom, yzoom, force_reset))
}

#' @describein update
update.MarxanSolved<-function(x, formula, evaluate=TRUE, force_reset=TRUE) {
	return(update.MarxanUnsolved(MarxanUnsolved(x@opts, x@data), formula, evaluate, force_reset))
}




