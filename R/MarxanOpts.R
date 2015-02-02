#' @include RcppExports.R marxan-internal.R misc.R MarxanOpts.R
NULL

#' MarxanOpts: An S4 super-class to represent input parameters
#'
#' This class is used to store Marxan input parameters used in all solvers.
#'
#' @slot BLM "numeric" Boundary length modifier. Defaults to 100.
#' @slot PROP "numeric" Proportion of planning units in initial reserve system. Defaults to 0.
#' @slot COSTTHRESH "numeric" Cost threshold. Defaults to 0.
#' @slot MISSLEVEL "numeric" Amount of target below which it is counted as 'missing'. Defaults to 1.
#' @slot CLUMPTYPE "integer" Clumping penalty type. Defaults to 0L.
#' @slot NCORES "integer" Number of cores to use for processing. Defaults to 1L.
#' @export
MarxanOpts<-setClasss("MarxanOpts",
	representation(
		BLM="numeric",
		PROP="numeric",
		COSTTHRESH="numeric",
		MISSLEVEL="numeric",
		CLUMPTYPE="integer"	,
		NCORES="integer"
	),
	prototype=list(
		BLM=100,
		PROP=0,
		COSTTHRESH=0,
		CLUMPTYPE=0L,
		MISSLEVEL=1,	
		NCORES=1L
	),
	validity=function(object) {
		# check for NA or non-finite values
		for (i in c('BLM','PROP','COSTTHRESH','MISSLEVEL','CLUMPTYPE','NCORES','VERBOSITY'))
			if (!is.finite(slot(object, i)))
				stop('argument to ',i,'is NA or non-finite')
		# check for valid parameters
		if (object@PROP > 1 || object@PROP < 0)
			stop('argument to PROP is not a numeric between 0 and 1')	
	}
)

#' @export
#' @rdname update
update.MarxanOpts<-function(x, formula) {
	ops<-llply(as.list(attr(terms(formula),"variables"))[-1L], eval)
	findInvalidMarxanOperations(ops)
	ops<-ops[which(laply(ops, inherits, "MarxanOptsOperation"))]
	for (i in seq_along(ops)) {
		for (j in seq_along(ops[[i]]$value)) {
			slot(x, ops[[i]]$slot[[j]])<-ops[[i]]$value[[j]]
		}
	}
	validObject(x, test=FALSE)
	return(x)
}

#' Update Marxan input parameters
#'
#' This function is used in the formula argument of the update function to change input parameters of a "MarxanOpts" object.
#'
#' @param ... arguments to update "MarxanOpts" object.
#' @return "MarxanOptsOperation" object.
#' @export
#' @seealso \code{\link{MarxanOpts-class}}, \code{\link{MarxanUnsolved-class}}, \code{\link{MarxanSolved-class}} \code{\link{update}}, \code{\link{spp}}, \code{\link{pu}}
#' @examples
#' opt(BLM=90)
#' opt(PROP=0.7, NUMITNS=100)
opt<-function(...) {
	args<-as.list(substitute(list(...)))[c(-1L)]
	llply(names(args), match.arg, names(getSlots("MarxanOpts")))
	args<-llply(args, eval, envir=parent.frame())
	return(
		structure(
			list(names(args),args),
			.Names = c("slot", "value"),
			class = c("MarxanUpdateOperation", "MarxanOptsOperation")
		)
	)
}

#' @export
as.list.MarxanOpts<-function(x, ...) {
	ret<-llply(slotNames(x), slot, x)
	names(ret)<-slotNames(x)
	return(x)
}

