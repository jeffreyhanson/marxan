#' @include RcppExports.R marxan-internal.R misc.R
NULL

#' MarxanOpts: An S4 class to represent Marxan input parameters
#'
#' This class is used to store Marxan input parameters.
#'
#' @slot BLM "numeric" Boundary length modifier. Defaults to 100.
#' @slot PROP "numeric" Proportion of planning units in initial reserve system. Defaults to 0.
#' @slot NUMREPS "integer" Number of replicate runs. Defaults to 100L.
#' @slot NUMITNS "integer" Number of iterations for annealing. Defaults to 1000000L.
#' @slot STARTTEMP "numeric" Initial temperature for annealing. Default to -1.
#' @slot COOLFAC "numeric" Cooling factor for annealing. Defaults to 0.
#' @slot NUMTEMP "integer" Number of temperature decreases for annealing. Defaults to 10000L.
#' @slot COSTTHRESH "numeric" Cost threshold. Defaults to 0.
#' @slot THRESHPEN1 "numeric" Size of cost threshold penalty. Defaults to 0.
#' @slot THRESHPEN2 "numeric" Shape of cost threshold penalty. Defaults to 0.
#' @slot MISSLEVEL "numeric" Amount of target below which it is counted as 'missing'. Defaults to 1.
#' @slot ITIMPTYPE "integer" Iterative improvement type. Defaults to 1L.
#' @slot HEURTYPE "integer" Heuristic type. Defaults to 1L.
#' @slot CLUMPTYPE "integer" Clumping penalty type. Defaults to 0L.
#' @slot VERBOSITY "integer" Amount of output displayed on the program screen. Defaults to 1L.
#' @slot NCORES "integer" Number of cores to use for processing. Defaults to 1L.

#' @note This class was not called "MarxanPars" due to the inevitable conflicts with \code{\link[base]{par}}.
#' @export
setClass("MarxanOpts",
	representation(
		BLM="numeric",
		PROP="numeric",
		NUMREPS="integer",

		NUMITNS="integer",
		STARTTEMP="numeric",
		COOLFAC="numeric",
		NUMTEMP="integer",
		
		COSTTHRESH="numeric",
		THRESHPEN1="numeric",
		THRESHPEN2="numeric",

		MISSLEVEL="numeric",
		ITIMPTYPE="integer",
		HEURTYPE="integer",
		CLUMPTYPE="integer",
		VERBOSITY="integer",
		
		NCORES="integer"

	),
	prototype=list(
		BLM=100,
		PROP=0,
		NUMREPS=100L,
	
		NUMITNS=1000000L,
		STARTTEMP=-1,
		COOLFAC=0,
		NUMTEMP=10000L,
			
		COSTTHRESH=0,
		THRESHPEN1=0,
		THRESHPEN2=0,

		MISSLEVEL=1,
		ITIMPTYPE=1L,
		HEURTYPE=1L,
		CLUMPTYPE=0L,
		VERBOSITY=1L,
		NCORES=1L
	),
	validity=function(object) {
		expect_true(object@PROP <= 1 & object@PROP >= 0, info="opts@PROP must be a numeric between 0 and 1")
		expect_true(object@ITIMPTYPE <= 3L & object@ITIMPTYPE >= 0L, info="opts@ITIMPTYPE must be an integer between 0 and 3")
		expect_true(object@HEURTYPE <= 7L & object@HEURTYPE >= 0L, info="opts@ITIMPTYPE must be an integer between 0 and 7")
		expect_true(object@VERBOSITY <= 3L & object@VERBOSITY >= 0L, info="opts@VERBOSITY must be an integer between 0 and 3")
	}
)

#' Create "MarxanOpts" object
#'
#' This function creates a new MarxanOpts object.
#'
#' @param BLM "numeric" Boundary length modifier. Defaults to 1000000.
#' @param PROP "numeric" Proportion of planning units in initial reserve system. Defaults to -1.
#' @param NUMREPS "integer" Number of replicate runs. Defaults to 100.
#' @param NUMITNS "integer" Number of iterations for annealing. Defaults to 1000000.
#' @param STARTTEMP "numeric" Initial temperature for annealing. Default to -1.
#' @param COOLFAC "numeric" Cooling factor for annealing. Defaults to 0.
#' @param NUMTEMP "integer" Number of temperature decreases for annealing. Defaults to 100000.
#' @param COSTTHRESH "numeric" Cost threshold. Defaults to 0.
#' @param THRESHPEN1 "numeric" Size of cost threshold penalty. Defaults to 0.14.
#' @param THRESHPEN2 "numeric" Shape of cost threshold penalty. Defaults to 0.
#' @param MISSLEVEL "numeric" Amount of target below which it is counted as 'missing'. Defaults to 1.
#' @param ITIMPTYPE "integer" Iterative improvement type. Defaults to 1.
#' @param HEURTYPE "integer" Heuristic type. Defaults to 0.
#' @param CLUMPTYPE "integer" Clumping penalty type. Defaults to 0.
#' @param VERBOSITY "integer" Amount of output displayed on the program screen. Defaults to 3.
#' @param NCORES "integer" Number of cores to use for processing. Defaults to 1.
#' @param ignore.extra "logical" Should extra arguments be ignored? Defaults to \code{FALSE}.
#' @return "MarxanOpts" object
#' @seealso \code{\link{MarxanOpts-class}},  \code{\link{read.MarxanOpts}}, \code{\link{write.MarxanOpts}}.
#' @export
#' @examples
#' x<-MarxanOpts(NCORES=4, NUMREPS=2, NUMITNS=5)
MarxanOpts<-function(..., ignore.extra=FALSE) {
	x<-new('MarxanOpts')
	args<-as.list(substitute(list(...)))[c(-1L)]
	extra<-which(!names(args) %in% names(getSlots("MarxanOpts")))
	if (length(extra)>0) {		
		if (ignore.extra) {
			args<-args[-extra]
		} else {
			stop("These are not valid or changeable Marxan parameters: ",paste(names(extra), collapse=","))
		}
	}
	for (i in seq_along(args)) {
		slot(x, names(args)[[i]])=args[[i]]
	}
	return(x)
}

#' Read Marxan input parameters from disk
#'
#' This function reads Marxan parameter settings from an input file.
#'
#' @param path "character" directory path for location to save input parameters file.
#' @seealso \code{\link{MarxanOpts-class}},  \code{\link{MarxanOpts}}, \code{\link{write.MarxanOpts}}.
#' @export
#' @examples
#' x<-MarxanOpts()
#' write.MarxanData(x, file.path(tempdir(), 'input.dat'))
#' y<-read.MarxanData(file.path(tempdir(), 'input.dat'))
#' stopifnot(identical(x,y))
read.MarxanOpts<-function(path) {
	x<-new('MarxanOpts')
	marxanOptsFile=readLines(path)
	sl<-getSlots("MarxanOpts")
	for (i in seq_along(sl)) {
		pos<-grep(names(sl)[i],marxanOptsFile)
		if (length(pos)!=0)
			slot(x, names(sl)[i])<-as(strsplit(marxanOptsFile[pos[1]]," ", fixed=TRUE)[[1]][[2]], sl[[i]])
	}
	return(x)
}

#' Write Marxan Input Parameters to Disk
#'
#' This function writes Marxan parameter settings to a file.
#'
#' @param dir "character" directory path for location to save input parameters file.
#' @param outputdir "character" directory path for location where Marxan input data files are saved.
#' @param seed "integer" seed for random number generation in Marxan.
#' @seealso \code{\link{MarxanOpts-class}},  \code{\link{MarxanOpts}}, \code{\link{read.MarxanOpts}}.
#' @export
#' @examples
#' x<-MarxanOpts()
#' write.MarxanData(x, file.path(tempdir(), 'input.dat'))
#' y<-read.MarxanData(file.path(tempdir(), 'input.dat'))
#' stopifnot(identical(x,y))
write.MarxanOpts<-function(x,inputdir,outputdir=inputdir,seed=sample.int(n=10000L,size=1L)) {
	cat(
'Input file for Annealing program.

This file generated by the marxan R package.

General Parameters
VERSION 0.1
BLM ',x@BLM,'
PROP ',x@PROP,'
RANDSEED ',seed,'
BESTSCORE -1
NUMREPS ',ceiling(x@NUMREPS/x@NCORES),'

Annealing Parameters
NUMITNS ',x@NUMITNS,'
STARTTEMP ',x@STARTTEMP,'
COOLFAC ',x@COOLFAC,'
NUMTEMP ',x@NUMTEMP,'

Cost Threshold
COSTTHRESH ',x@COSTTHRESH,'
THRESHPEN1 ',x@THRESHPEN1,'
THRESHPEN2 ',x@THRESHPEN2,'

Input File
INPUTDIR ',inputdir,'
SPECNAME spec.dat
PUNAME pu.dat
PUVSPRNAME puvspr.dat
BOUNDNAME bound.dat
MATRIXSPORDERNAME puvspr_sporder.dat

Save Files
SCENNAME output
SAVERUN 3
SAVEBEST 3
SAVESUMMARY 3
SAVESCEN 2
SAVETARGMET 3
SAVESUMSOLN 3
SAVESOLUTIONSMATRIX 3
SAVELOG 1
SAVESNAPSTEPS 0
SAVESNAPCHANGES 0
SAVESNAPFREQUENCY 0
OUTPUTDIR ',outputdir,'

Program control.
RUNMODE 1
MISSLEVEL ',x@MISSLEVEL,'
ITIMPTYPE ',x@ITIMPTYPE,'
HEURTYPE ',x@HEURTYPE,'
CLUMPTYPE ',x@CLUMPTYPE,'
VERBOSITY ',x@VERBOSITY,'

',file=file.path(outputdir, 'input.dat'), sep=""
	)
}

#' @export
print.MarxanOpts<-function(x, header=TRUE) {
	if (header)
		cat("MarxanOpts object.\n")
}

#' @export
setMethod(
	'show',
	'MarxanOpts',
	function(object)
		print.MarxanOpts(object)
)


#' @export
#' @rdname update
update.MarxanOpts<-function(x, formula) {
	ops<-llply(as.list(attr(terms(formula),"variables"))[-1L], eval)
	findInvalidMarxanOperations(ops)
	ops<-ops[which(laply(ops, inherits, "MarxanOptsOperation"))]
	for (i in seq_along(ops))
		slot(x, ops[[i]]$slot)<-ops[[i]]$value[[1]]
	return(x)
}

#' Update Marxan input parameters
#'
#' This function is used in the formula argument of the update function to change input parameters of a "MarxanOpts" object.
#'
#' @param name "character" name of parameter to change.
#' @param value "numeric" new value.
#' @return "MarxanOptsOperation" object.
#' @export
#' @seealso \code{\link{MarxanOpts-class}}, \code{\link{MarxanUnsolved-class}}, \code{\link{MarxanSolved-class}} \code{\link{update}}, \code{\link{spp}}, \code{\link{pu}}
#' @examples
#' opt(BLM=90)
#' opt(PROP=0.7, NUMITNS=100)
opt<-function(...) {
	args<-unlist(as.list(substitute(list(...)))[c(-1L)])
	llply(names(args), match.arg, names(getSlots("MarxanOpts")))
	return(
		structure(
			list(names(args),args),
			.Names = c("slot", "value"),
			class = c("MarxanUpdateOperation", "MarxanOptsOperation")
		)
	)
}

