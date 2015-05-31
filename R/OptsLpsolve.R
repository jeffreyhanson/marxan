#' @include RcppExports.R marxan-internal.R misc.R Opts.R
NULL

#' OptsLpsolve: An S4 class to store input parameters for lpsolve
#'
#' This class is used to store input parameters for lpsolve.
#'
#' @slot anti.degen "character" determining anti-degeneracy handling. Defaults to \code{c('infeasible','stalling','fixedvars')}.
#' @slot basis.crash "character" specifying basis crash mode. Defaults to 'none'.
#' @slot bb.depthlimit "integer" specifying maximum branch-and-bound-depth. Defaults to \code{-50}.
#' @slot bb.floorfirst "character" which branch to take first in the branch-and-bound algorithm. Defaults to 'auto'.
#' @slot bb.rule "character" specifying branch-and-bound rule. Defaults to \code{c('pseudononint', 'greedy', 'dynamic', 'rcostfixing')}.
#' @slot break.at.first "logical" If \code{FALSE} the algorithm will continue until an optimal solution is found, otherwise the algorithm stops at the first solution. Defaults to \code{FALSE}.
#' @slot break.at.value "numeric" value indicating that the algorithm should stop at if a solution is found with a better objective function. Defaults to \code{Inf}, to continue until an optimal solution is found.
#' @slot epslevel "character" specifying tolerance thresholds. Defaults to 'tight'.
#' @slot epsb "numeric" tolerance used to determine whether a right-hand-side value should be considered zero. Defaults to \code{1e-10}.
#' @slot epsd "numeric" tolerance used to determine whether a computed reduced cost should be considered zero. Defaults to \code{1e-9}.
#' @slot epsel "numeric" tolerance used for rounding values to zero. Defaults to \code{1e-12}. 
#' @slot epsint "numeric" tolerance used to determine whether a floating point number if an integer. Defaults to \code{1e-7}.
#' @slot epsperturb "numeric" pertubration scalar for degenerate problems. Defaults to \code{1e-5}.
#' @slot epspivot "numeric" tolerance used to determine wheter a pivot element is zero. Defaults to \code{2e-7}.
#' @slot improve "character" iterative improvement level. Defaults to \code{c('dualfeas', 'thetagap')}.
#' @slot infinite "numeric" practical value for infinity. Defaults to \code{1e30}.
#' @slot maxpivot "integer" maximum number of pivots between re-inversion of the matrix. Defaults to \code{250L}.
#' @slot min.gap "numeric" absolute and relative MIP gaps used in the branch-and-bound algorithm. Defaults to \code{1e-11}.
#' @slot negrange "numeric" value below which variables are split into negative and positive parts. Defaults to \code{-1e6}.
#' @slot obj.in.bas "logical" if \code{TRUE} the objective function is stored in the top of row of the matrix, else it is stored seperately. Defaults to \code{TRUE}.
#' @slot pivoting "character" specifying the pivot rule and mode. Defaults to \code{c('devex','adaptive')}.
#' @slot presolve "character" specifying presolve steps to be carried out before solving. Defaults to \code{c('none')}.
#' @slot scalelimit "numeric" relative scaling convergence criterion for the active scaling mode. Defaults to \code{5}.
#' @slot scaling "character" name of scaling algorithm used and zero or more augmentations. Defaults to \code{c('geometric','equilibrate','integers')}.
#' @slot simplextype "character" name of simplex type to use in each phase of the algorithm. Defaults to \code{c('dual','primal')}.
#' @slot timeout "integer" maximum number of seconds allotted for processing. If algorithm fails to solve within this time it will stop. If \code{0}, then algorithm run until solved. Defaults to \code{3000L} (10 minutes).
#' @slot seealso \code{\link[lpSolveAPI]{lp.control.options}}
#' @export
setClass("OptsLpsolve",
	contains="Opts"
	representation(
		anti.degen="character",
		basis.crash="character",
		bb.depthlimit="integer",
		bb.floorfirst="character",
		bb.rule ="character",
		break.at.first="logical",
		break.at.value="numeric",
		epslevel="character",
		epsb="numeric",
		epsd="numeric",
		epsel="numeric",
		epsint="numeric",
		epsperturb="numeric",
		epspivot="numeric",
		improve="character",
		infinite="numeric",
		maxpivot="integer",
		min.gap="numeric",
		negrange="numeric",
		obj.in.bas="logical",
		pivoting="character",
		presolve="character",
		scalelimit="numeric",
		scaling="character",
		simplextype="character",
		timeout="integer"
	),
	prototype=list(
		anti.degen=c('infeasible','stalling','fixedvars'),
		basis.crash='none',
		bb.depthlimit=-50,
		bb.floorfirst='auto',
		bb.rule=c('pseudononint', 'greedy', 'dynamic', 'rcostfixing'),
		break.at.first=FALSE,
		break.at.value=Inf,
		epslevel='tight',
		epsb1=1e-10,
		epsd=1e-9,
		epsel=1e-12,
		epsint=1e-7,
		epsperturb=1e-5,
		epspivot=2e-7,
		improve=c('dualfeas', 'thetagap'),
		infinite=1e30,
		maxpivot=250L,
		min.gap=1e-11,
		negrange=-1e6,
		obj.in.bas=TRUE,
		pivoting=c('devex','adaptive'),
		presolve='none',
		scalelimit=5,
		scaling=c('geometric','equilibriate','integers'),
		simplextype=c('daul','primal'),
		timeout=3000L
	),
	validity=function(object) {
		# check for NA or non-finite values
		for (i in slotNames(object))
			if (!is.finite(slot(object, i)) & i!="break.at.value")
				stop('argument to ',i,'is NA or non-finite')
		return(TRUE)
	}
)

#' Create "OptsLpsolve" object
#'
#' This function creates a new "OptsLpsolve" object.
#'
#' @param ... arguments to set slots in a "OptsLpsolve" object.
#' @param ignore.extra "logical" Should extra arguments be ignored? Defaults to \code{FALSE}.
#' @details
#' The slots of class "OptsLpsolve" are shown below for reference.
#' \tabular{cccl}{
#' \strong{Name} \tab \strong{Class} \tab \strong{Default} \tab \strong{Description}\cr
#' anti.degen \tab "character" \tab \code{c('infeasible','stalling','fixedvars')} \tab names of anti-degeneracy handling operations \cr
#' basis.crash \tab "character" \tab \code{'none'} \tab basis crash mode \cr
#' bb.depthlimit \tab "integer" \tab \code{-50} \tab maximum branch-and-bound-depth \cr
#' bb.floorfirst \tab "character" \tab \code{'auto'} \tab name of method for which branch to take first in the branch-and-bound algorithm \cr
#' bb.rule \tab "character" \tab \code{c('pseudononint', 'greedy', 'dynamic', 'rcostfixing')} \tab specifying branch-and-bound rule \cr
#' break.at.first \tab "logical" \tab \code{FALSE} \tab if \code{FALSE} the algorithm will continue until an optimal solution is found, otherwise the algorithm stops at the first solution \cr
#' break.at.value \tab "numeric" \tab \code{Inf} \tab value that the algorithm should stop at if a solution is found with a better objective function \cr
#' epslevel \tab "character" \tab \code{'tight'} \tab tolerance thresholds \cr
#' epsb \tab "numeric" \tab \code{1e-10} \tab tolerance used to determine whether a right-hand-side value should be considered zero \cr
#' epsd \tab "numeric" \tab \code{1e-9} \tab tolerance used to determine whether a computed reduced cost should be considered zero \cr
#' epsel \tab "numeric" \tab \code{1e-12} \tab tolerance used for rounding values to zero \cr
#' epsint \tab "numeric" \tab \code{1e-7} \tab tolerance used to determine whether a floating point number if an integer \cr
#' epsperturb \tab "numeric" \tab \code{1e-5} \tab pertubration scalar for degenerate problems \cr
#' epspivot \tab "numeric" \tab \code{2e-7} \tab tolerance used to determine wheter a pivot element is zero \cr
#' improve \tab "character" \tab \code{c('dualfeas', 'thetagap')} \tab iterative improvement level \cr
#' infinite \tab "numeric" \tab \code{1e30} \tab practical value for infinity \cr
#' maxpivot \tab "integer" \tab \code{250L} \tab maximum number of pivots between re-inversion of the matrix \cr
#' min.gap \tab "numeric" \tab \code{1e-11} \tab absolute and relative MIP gaps used in the branch-and-bound algorithm \cr
#' negrange \tab "numeric" \tab \code{-1e6} \tab value below which variables are split into negative and positive parts \cr
#' obj.in.bas \tab "logical" \tab \code{TRUE} \tab if \code{TRUE} the objective function is stored in the top of row of the matrix, else it is stored separately \cr
#' pivoting \tab "character" \tab \code{c('devex','adaptive')} \tab specifying the pivot rule and mode \cr
#' presolve \tab "character" \tab \code{c('none')} \tab specifying presolve steps to be carried out before solving \cr
#' scalelimit \tab "numeric" \tab \code{5} \tab relative scaling convergence criterion for the active scaling mode \cr
#' scaling \tab "character" \tab \code{c('geometric','equilibrate','integers')} \tab name of scaling algorithm used and zero or more augmentations \cr
#' simplextype \tab "character" \tab \code{c('dual','primal')} \tab name of simplex type to use in each phase of the algorithm \cr
#' timeout \tab "integer" \tab \code{3000L} \tab maximum number of seconds allotted for processing; if algorithm fails to solve within this time it will stop \cr
#' }
#' The slots for the 'Opts' super-class are also shown below for reference.
#' \tabular{cccl}{
#' \strong{Name} \tab \strong{Class} \tab \strong{Default} \tab \strong{Description}\cr
#' BLM \tab "numeric" \tab 100 \tab boundary length modifier \cr
#' PROP \tab "numeric" \tab 0 \tab proportion of planning units in initial reserve system \cr
#' COSTTHRESH \tab "numeric" \tab 0 \tab cost threshold \cr
#' MISSLEVEL \tab "numeric" \tab 1 \tab amount of target below which it is counted as 'missing' \cr
#' NCORES \tab "integer" \tab 1L \tab number of cores to use for processing \cr
#' VERBOSITY \tab "integer" \tab 1L \tab amount of output displayed on the program screen \cr
#' }
#' @return "OptsLpsolve" object
#' @seealso code{\link{OptsLpsolve-class}}, \code{\link{Opts-class}}.
#' @export
#' @examples
#' x<-OptsLpsolve(NCORES=4, scalelimit=3)
OptsLpsolve<-function(..., ignore.extra=FALSE) {
	return(
		constructOpts(
			'OptsLpsolve',
			as.list(substitute(list(...)))[c(-1L)],
			ignore.extra
		)
	)
}

#' @export
print.OptsLpsolve<-function(x, header=TRUE) {
	if (header)
		cat("OptsLpsolve object.\n")
}

#' @export
setMethod(
	'show',
	'OptsLpsolve',
	function(object)
		print.OptsLpsolve(object)
)


