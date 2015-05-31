#' @include RcppExports.R marxan-internal.R misc.R
NULL

#' MarxanOpts: An S4 super-class to represent input parameters
#'
#' This class is used to store Marxan input parameters used in all solvers.
#'
#' @slot BLM "numeric" Boundary length modifier. Defaults to 100.
#' @slot COSTTHRESH "numeric" Cost threshold. Defaults to 0.
#' @slot MISSLEVEL "numeric" Amount of target below which it is counted as 'missing'. Defaults to 1.
#' @slot NCORES "integer" Number of cores to use for processing. Defaults to 1L.
#' @export
MarxanOpts<-setClasss("Opts",
	contains="Opts",
	representation(
		BLM="numeric",
		COSTTHRESH="numeric",
		MISSLEVEL="numeric",
		NCORES="integer"
	),
	prototype=list(
		BLM=100,
		COSTTHRESH=0,
		MISSLEVEL=1,
		NCORES=1L
	),
	validity=function(object) {
		# check for NA or non-finite values
		for (i in c('BLM','PROP','COSTTHRESH','MISSLEVEL','NCORES'))
			if (!is.finite(slot(object, i)))
				stop('argument to ',i,'is NA or non-finite')
	}
)

#' @export
#' @rdname update
update.Opts<-function(x, formula) {
	ops<-llply(as.list(attr(terms(formula),"variables"))[-1L], eval)
	findInvalidMarxanOperations(ops)
	ops<-ops[which(laply(ops, inherits, "Opts"))]
	for (i in seq_along(ops)) {
		for (j in seq_along(ops[[i]]$value)) {
			slot(x, ops[[i]]$slot[[j]])<-ops[[i]]$value[[j]]
		}
	}
	validObject(x, test=FALSE)
	return(x)
}

#' Update input parameters to solve a Marxan problem
#'
#' This function is used in the formula argument of the update function to change input parameters of an "Opts" object.
#'
#' @param ... arguments to update "Opts" object.
#' @return "OperationUpdate" object.
#' @export
#' @seealso \code{\link{Opts-class}}, \code{\link{MarxanUnsolved-class}}, \code{\link{MarxanSolved-class}} \code{\link{update}}, \code{\link{spp}}, \code{\link{pu}}
#' @examples
#' opt(BLM=90)
#' opt(PROP=0.7, NUMITNS=100)
opt<-function(...) {
	args<-llply(
		as.list(substitute(list(...)))[c(-1L)],
		eval,
		envir=parent.frame()
	)
	return(
		structure(
			list(names(args),args),
			.Names = c("slot", "value"),
			class = c("OperationUpdate", "OperationOpts")
		)
	)
}

#' @export
as.list.Opts<-function(x, ...) {
	ret<-llply(slotNames(x), slot, x)
	names(ret)<-slotNames(x)
	return(x)
}

