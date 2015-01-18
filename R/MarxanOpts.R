#' @include RcppExports.R marxan-internal.R misc.R
NULL

#' MarxanOpts: An S4 class to represent Marxan input parameters
#'
#' This class is used to store Marxan input parameters.
#'
#' General parameters
#' @slot BLM "numeric" boundary length modifier (default=1000000)
#' @slot PROP "numeric" proportion of planning units in initial reserve system (default=-1)
#' @slot NUMREPS "integer" number of replicate runs (default=100)
#' Annealing parameters
#' @slot NUMITNS "integer" mumber of iterations for annealing (default=1000000)
#' @slot STARTTEMP "numeric" initial temperature for annealing (default=-1)
#' @slot COOLFAC "numeric" cooling factor for annealing (default=0)
#' @slot NUMTEMP "integer" number of temperature decreases for annealing (default=100000)
#' Cost threshold 
#' @slot COSTTHRESH "numeric" cost threshold (default=0)
#' @slot THRESHPEN1 "numeric" size of cost threshold penalty (default=0.14)
#' @slot THRESHPEN2 "numeric" shape of cost threshold penalty (default=0)
#' Program control
#' @slot MISSLEVEL "numeric" amount of target below which it is counted as 'missing' (default=1)
#' @slot ITIMPTYPE "integer" iterative improvement type (default=1)
#' @slot HEURTYPE "integer" heuristic type (default=0)
#' @slot CLUMPTYPE "integer" clumping penalty type (default=0)
#' @slot VERBOSITY "integer" amount of output displayed on the program screen (default=3)
#' @slot NCORES "integer "number of cores to use for processing (default=1)
#' @export
setClass("MarxanOpts",
	representation(
		BLM="numeric",
		PROP="numeric",
		NUMREPS="numeric",

		NUMITNS="numeric",
		STARTTEMP="numeric",
		COOLFAC="numeric",
		NUMTEMP="numeric",
		
		COSTTHRESH="numeric",
		THRESHPEN1="numeric",
		THRESHPEN2="numeric",

		MISSLEVEL="numeric",
		ITIMPTYPE="numeric",
		HEURTYPE="numeric",
		CLUMPTYPE="numeric",
		VERBOSITY="numeric",
		
		NCORES="numeric"

	),
	prototype=list(
		BLM=100,
		PROP=0.5,
		NUMREPS=100,
	
		NUMITNS=10,
		STARTTEMP=-1,
		COOLFAC=0,
		NUMTEMP=10,
			
		COSTTHRESH=0,
		THRESHPEN1=0.14,
		THRESHPEN2=0,

		MISSLEVEL=1,
		ITIMPTYPE=1,
		HEURTYPE=0,
		CLUMPTYPE=0,
		VERBOSITY=3,
		NCORES=1
	)
)

#' Create MarxanOpts object
#'
#' This function creates a new MarxanOpts object.
#'
#' General parameters
#' @param BLM "numeric" boundary length modifier (default=1000000)
#' @param PROP "numeric" proportion of planning units in initial reserve system (default=-1)
#' @param NUMREPS "integer" number of replicate runs (default=100)
#' Annealing parameters
#' @param NUMITNS "integer" mumber of iterations for annealing (default=1000000)
#' @param STARTTEMP "numeric" initial temperature for annealing (default=-1)
#' @param COOLFAC "numeric" cooling factor for annealing (default=0)
#' @param NUMTEMP "integer" number of temperature decreases for annealing (default=100000)
#' Cost threshold 
#' @param COSTTHRESH "numeric" cost threshold (default=0)
#' @param THRESHPEN1 "numeric" size of cost threshold penalty (default=0.14)
#' @param THRESHPEN2 "numeric" shape of cost threshold penalty (default=0)
#' Program control
#' @param MISSLEVEL "numeric" amount of target below which it is counted as 'missing' (default=1)
#' @param ITIMPTYPE "integer" iterative improvement type (default=1)
#' @param HEURTYPE "integer" heuristic type (default=0)
#' @param CLUMPTYPE "integer" clumping penalty type (default=0)
#' @param VERBOSITY "integer" amount of output displayed on the program screen (default=3)
#' @param NCORES "integer "number of cores to use for processing (default=1)
#' @return "MarxanOpts" object
#' @seealso \code{\link{MarxanOpts-class}},  \code{\link{read.MarxanOpts}}, \code{\link{write.MarxanOpts}}
#' @export
#' @examples
#' x<-MarxanOpts(NCORES=4, NUMREPS=2, NUMITNS=5)
MarxanOpts<-function(...) {
	x<-new('MarxanOpts')
	args<-as.list(substitute(list(...)))[c(-1L)]
	for (i in seq_along(args))
		slot(x, names(args))=args[[i]]
	return(x)
}

#' Read Marxan Input Parameters from Disk
#'
#' This function reads Marxan parameter settings from an input file.
#'
#' @param path "character" directory path for location to save input parameters file
#' @seealso \code{\link{MarxanOpts-class}},  \code{\link{MarxanOpts}}, \code{\link{write.MarxanOpts}}
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
#' @param dir "character" directory path for location to save input parameters file
#' @param outputdir "character" directory path for location where Marxan input data files are saved
#' @param seed "integer" seed for random number generation in Marxan
#' @seealso \code{\link{MarxanOpts-class}},  \code{\link{MarxanOpts}}, \code{\link{read.MarxanOpts}}
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

#' @describeIn print
#' @export
print.MarxanOpts<-function(x, header=TRUE) {
	if (header)
		cat("MarxanOpts object.\n")
}

#' @export
# setMethod(
	# 'show',
	# 'MarxanOpts',
	# function(x, ...)
		# print.MarxanOpts(x, ...)
# )


#' @describeIn update
#' @export
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
#' This function is used in the formula argument of the update function to change input parameters of a "MarxanOpts", "MarxanUnsolved", or "MarxanSolved" object.
#'
#' @param name "character" name of parameter to change.
#' @param value "numeric" new value.
#' @return "MarxanOptsOperation" object.
#' @export
#' @seealso \code{\link{MarxanOpts-class}}, \code{\link{MarxanUnsolved-class}}, \code{\link{MarxanSolved-class}} \code{\link{update}}, \code{\link{spp}}, \code{\link{pu}}
opt<-function(...) {
	args<-unlist(as.list(substitute(list(...)))[c(-1L)])
	match.arg(names(args), names(getSlots("MarxanOpts")))
	return(
		structure(
			list(names(args),args),
			.Names = c("slot", "value"),
			class = c("MarxanUpdateOperation", "MarxanOptsOperation")
		)
	)
}

