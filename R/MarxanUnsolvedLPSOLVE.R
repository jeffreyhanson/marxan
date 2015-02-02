#' @include RcppExports.R marxan-internal.R misc.R generics.R MarxanOpts.R MarxanOptsGUROBI.R MarxanData.R MarxanUnsolved.R
NULL

#' MarxanUnsolvedLPSOLVE: An S4 class to represent Marxan input parameter and data for lpsolve
#'
#' This class is used to store Marxan input parameters and data to generate solutions using the lpsolve program.
#'
#' @slot opts "MarxanOptsLPSOLVE" object used to store input parameters.
#' @slot data "MarxanData" object used to store input data.
#' @export
#' @seealso  \code{\link{MarxanOptsLPSOLVE-class}}, \code{\link{MarxanData-class}}.
setClass("MarxanUnsolvedLPSOLVE",
	contains="MarxanUnsolved"
	representation(
		opts="MarxanOptsLPSOLVE"
	)
)

#' Create a new "MarxanUnsolvedLPSOLVE" object
#'
#' This function creates a "MarxanUnsolvedLPSOLVE" object using "MarxanOptsLPSOLVE" and "MarxanData" objects.
#'
#' @param opts "MarxanOptsLPSOLVE" object.
#' @param data "MarxanData" object.
#' @return "MarxanUnsolvedLPSOLVE" object.
#' @export
#' @seealso \code{\link{MarxanUnsolvedLPSOLVE-class}}, \code{\link{MarxanOptsLPSOLVE-class}}, \code{\link{MarxanData-class}}.
MarxanUnsolvedLPSOLVE<-function(opts, data) {
	return(new("MarxanUnsolvedLPSOLVE", opts=opts, data=data))
}

#' @rdname solve
#' @inheritParams solve
#' @export
solve.MarxanUnsolvedLPSOLVE=function(x, problem="ILP", verbose=1, wd=tempdir(), seed=sample.int(n=10000L, size=x@opts@NCORES), clean=TRUE) {
	## init
	# check argument validity
	match.arg(problem, c('ILP','LP'))
	match.arg(verbose, 1:3)
	## prelim
	# sink model to file
	writeLines(lpsolvemodel(x, problem), file=file.path(wd, 'marxax.lp'))
	mod<-read.lp(file.path(wd, 'marxax.lp'), type='lp')
	lp.control(mod, , reset=FALSE)
	
	## main
	
	## post















}


