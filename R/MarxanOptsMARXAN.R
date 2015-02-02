#' @include RcppExports.R marxan-internal.R misc.R MarxanOpts.R
NULL

#' MarxanOptsMARXAN: An S4 class to represent Marxan input parameters for MARXAN
#'
#' This class is used to store input parameters to solve the Marxan problem with MARXAN.
#'
#' @slot NUMREPS "integer" Number of replicate runs. Defaults to 100L.
#' @slot NUMITNS "integer" Number of iterations for annealing. Defaults to 1000000L.
#' @slot STARTTEMP "numeric" Initial temperature for annealing. Default to -1.
#' @slot COOLFAC "numeric" Cooling factor for annealing. Defaults to 0.
#' @slot NUMTEMP "integer" Number of temperature decreases for annealing. Defaults to 100000L.
#' @slot THRESHPEN1 "numeric" Size of cost threshold penalty. Defaults to 0.
#' @slot THRESHPEN2 "numeric" Shape of cost threshold penalty. Defaults to 0.
#' @slot ITIMPTYPE "integer" Iterative improvement type. Defaults to 1L.
#' @slot HEURTYPE "integer" Heuristic type. Defaults to 1L.
#' @export
setClass("MarxanOptsMARXAN",
	contains="MarxanOpts",
	representation(
		NUMREPS="integer",
		NUMITNS="integer",
		STARTTEMP="numeric",
		COOLFAC="numeric",
		NUMTEMP="integer",
		THRESHPEN1="numeric",
		THRESHPEN2="numeric",
		ITIMPTYPE="integer",
		HEURTYPE="integer"
	),
	prototype=list(
		NUMREPS=100L,
		NUMITNS=1000000L,
		STARTTEMP=-1,
		COOLFAC=0,
		NUMTEMP=100000L,
		THRESHPEN1=0,
		THRESHPEN2=0,
		ITIMPTYPE=1L,
		HEURTYPE=1L
	),
	validity=function(object) {
		# check for NA or non-finite values
		for (i in c('NUMREPS','NUMITNS','STARTTEMP','COOLFAC','NUMTEMP','THRESHPEN1','THRESHPEN2','ITIMPTYPE','HEURTYPE'))
			if (!is.finite(slot(object, i)))
				stop('argument to ',i,'is NA or non-finite')	
		if (object@ITIMPTYPE > 3L || object@PROP < 0L)
			stop('argument to ITIMPTYPE is not an integer between 0L and 3L')
		if (object@HEURTYPE > 7L || object@HEURTYPE < 0L)
			stop('argument to HEURTYPE is not an integer between 0L and 7L')
		if (object@VERBOSITY > 3L || object@VERBOSITY < 0L)
			stop('argument to VERBOSITY is not an integer between 0L and 3L')
		return(TRUE)
	}
)

#' Create "MarxanOptsMARXAN" object
#'
#' This function creates a new "MarxanOptsMARXAN" object.
#'
#' @param ... arguments to set slots in a "MarxanOptsMARXAN" object.
#' @param ignore.extra "logical" Should extra arguments be ignored? Defaults to \code{FALSE}.
#' @details
#' The slots of class "MarxanOptsMARXAN" are shown below for reference.
#' \tabular{cccl}{
#' \strong{Name} \tab \strong{Class} \tab \strong{Default} \tab \strong{Description}\cr
#' NUMREPS \tab "integer" \tab 100L \tab number of replicate runs\cr
#' NUMITNS \tab "integer" \tab 1000000L \tab number of iterations for annealing\cr
#' STARTTEMP \tab "numeric" \tab -1 \tab initial temperature for annealing\cr
#' COOLFAC \tab "numeric" \tab 0 \tab cooling factor for annealing\cr
#' NUMTEMP \tab "integer" \tab 100000L \tab number of temperature decreases for annealing\cr
#' THRESHPEN1 \tab "numeric" \tab 0 \tab size of cost threshold penalty\cr
#' THRESHPEN2 \tab "numeric" \tab 0 \tab shape of cost threshold penalty\cr
#' ITIMPTYPE \tab "integer" \tab 1L \tab iterative improvement type\cr
#' HEURTYPE \tab "integer" \tab 0L \tab heuristic type\cr
#' }
#' The slots for the 'MarxanOpts' super-class are also shown below for reference.
#' \tabular{cccl}{
#' \strong{Name} \tab \strong{Class} \tab \strong{Default} \tab \strong{Description}\cr
#' BLM \tab "numeric" \tab 100 \tab boundary length modifier \cr
#' PROP \tab "numeric" \tab 0 \tab proportion of planning units in initial reserve system \cr
#' COSTTHRESH \tab "numeric" \tab 0 \tab cost threshold \cr
#' MISSLEVEL \tab "numeric" \tab 1 \tab amount of target below which it is counted as 'missing' \cr
#' CLUMPTYPE \tab "integer" \tab 0L \tab clumping penalty type \cr
#' NCORES \tab "integer" \tab 1L \tab number of cores to use for processing \cr
#' VERBOSITY \tab "integer" \tab 1L \tab amount of output displayed on the program screen \cr
#' }
#' @return "MarxanOptsMARXAN" object
#' @seealso \code{\link{MarxanOptsMARXAN-class}},  \code{\link{read.MarxanOptsMARXAN}}, \code{\link{write.MarxanOptsMARXAN}}.
#' @export
#' @examples
#' x<-MarxanOptsMARXAN(NCORES=4, NUMREPS=2, NUMITNS=5)
MarxanOptsMARXAN<-function(..., ignore.extra=FALSE) {
	return(
		MarxanOptsConstructor(
			'MarxanOptsMARXAN',
			as.list(substitute(list(...)))[c(-1L)],
			ignore.extra
		)
	)
}

#' Read MARXAN input parameters from disk
#'
#' This function reads MARXAN parameter settings from an input file.
#'
#' @param path "character" directory path for location to save input parameters file.
#' @seealso \code{\link{MarxanOptsMARXAN-class}},  \code{\link{MarxanOptsMARXAN}}, \code{\link{write.MarxanOptsMARXAN}}.
#' @export
#' @examples
#' x<-MarxanOptsMARXAN()
#' write.MarxanOptsMARXAN(x, file.path(tempdir(), 'input.dat'))
#' y<-read.MarxanOptsMARXAN(file.path(tempdir(), 'input.dat'))
#' stopifnot(identical(x,y))
read.MarxanOptsMARXAN<-function(path) {
	x<-new('MarxanOptsMARXAN')
	marxanOptsFile=readLines(path)
	sl<-getSlots("MarxanOptsMARXAN")
	for (i in seq_along(sl)) {
		pos<-grep(names(sl)[i],marxanOptsFile)
		if (length(pos)!=0)
			slot(x, names(sl)[i])<-as(strsplit(marxanOptsFile[pos[1]]," ", fixed=TRUE)[[1]][[2]], sl[[i]])
	}
	validObject(x)
	return(x)
}

#' Write MARXAN Input Parameters to Disk
#'
#' This function writes MARXAN parameter settings to a file.
#'
#' @param x "MarxanOptsMARXAN" object.
#' @param inputdir "character" directory path for of input data files.
#' @param outputdir "character" directory path for location where Marxan input file and result data files are saved.
#' @param seed "integer" seed for random number generation in Marxan.
#' @seealso \code{\link{MarxanOptsMARXAN-class}},  \code{\link{MarxanOptsMARXAN}}, \code{\link{read.MarxanOptsMARXAN}}.
#' @export
#' @examples
#' x<-MarxanOptsMARXAN()
#' write.MarxanOptsMARXAN(x, file.path(tempdir(), 'input.dat'))
#' y<-read.MarxanOptsMARXAN(file.path(tempdir(), 'input.dat'))
#' stopifnot(identical(x,y))
write.MarxanOptsMARXAN<-function(x,inputdir,outputdir=inputdir,seed=sample.int(n=10000L,size=1L)) {
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
print.MarxanOptsMARXAN<-function(x, header=TRUE) {
	if (header)
		cat("MarxanOptsMARXAN object.\n")
}

#' @export
setMethod(
	'show',
	'MarxanOptsMARXAN',
	function(object)
		print.MarxanOptsMARXAN(object)
)


