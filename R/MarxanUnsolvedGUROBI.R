#' @include RcppExports.R marxan-internal.R misc.R generics.R MarxanOpts.R MarxanOptsGurobi.R MarxanData.R MarxanUnsolved.R
NULL

#' MarxanUnsolvedGurobi: An S4 class to represent Marxan input parameter and data for Gurobi
#'
#' This class is used to store Marxan input parameters and data to generate solutions using the Gurobi program.
#'
#' @slot opts "MarxanOptsGurobi" object used to store input parameters.
#' @slot data "MarxanData" object used to store input data.
#' @export
#' @seealso  \code{\link{MarxanOptsGurobi-class}}, \code{\link{MarxanData-class}}.
setClass("MarxanUnsolvedGurobi",
	contains="MarxanUnsolved"
	representation(
		opts="MarxanOptsGurobi"
	)
)

#' Create a new "MarxanUnsolvedGurobi" object
#'
#' This function creates a "MarxanUnsolvedGurobi" object using "MarxanOptsGurobi" and "MarxanData" objects.
#'
#' @param opts "MarxanOptsGurobi" object.
#' @param data "MarxanData" object.
#' @return "MarxanUnsolvedGurobi" object.
#' @export
#' @seealso \code{\link{MarxanUnsolvedGurobi-class}}, \code{\link{MarxanOptsGurobi-class}}, \code{\link{MarxanData-class}}.
MarxanUnsolvedGurobi<-function(opts, data) {
	return(new("MarxanUnsolvedGurobi", opts=opts, data=data))
}

#' @rdname solve
#' @inheritParams solve
#' @export
solve.MarxanUnsolvedGurobi=function(x, problem="ILP", verbose=1, wd=tempdir(), seed=sample.int(n=10000L, size=x@opts@NCORES), clean=TRUE, force_reset=FALSE) {




}
