#' @include RcppExports.R marxan-internal.R misc.R generics.R MarxanOpts.R MarxanOptsGUROBI.R MarxanData.R MarxanUnsolved.R
NULL

#' MarxanUnsolvedGUROBI: An S4 class to represent Marxan input parameter and data for Gurobi
#'
#' This class is used to store Marxan input parameters and data to generate solutions using the Gurobi program.
#'
#' @slot opts "MarxanOptsGUROBI" object used to store input parameters.
#' @slot data "MarxanData" object used to store input data.
#' @export
#' @seealso  \code{\link{MarxanOptsGUROBI-class}}, \code{\link{MarxanData-class}}.
setClass("MarxanUnsolvedGUROBI",
	contains="MarxanUnsolved"
	representation(
		opts="MarxanOptsGUROBI"
	)
)

#' Create a new "MarxanUnsolvedGUROBI" object
#'
#' This function creates a "MarxanUnsolvedGUROBI" object using "MarxanOptsGUROBI" and "MarxanData" objects.
#'
#' @param opts "MarxanOptsGUROBI" object.
#' @param data "MarxanData" object.
#' @return "MarxanUnsolvedGUROBI" object.
#' @export
#' @seealso \code{\link{MarxanUnsolvedGUROBI-class}}, \code{\link{MarxanOptsGUROBI-class}}, \code{\link{MarxanData-class}}.
MarxanUnsolvedGUROBI<-function(opts, data) {
	return(new("MarxanUnsolvedGUROBI", opts=opts, data=data))
}

#' @rdname solve
#' @inheritParams solve
#' @export
solve.MarxanUnsolvedGUROBI=function(x, problem="ILP", verbose=1, wd=tempdir(), seed=sample.int(n=10000L, size=x@opts@NCORES), clean=TRUE, force_reset=FALSE) {




}

