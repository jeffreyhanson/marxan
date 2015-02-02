#' @include RcppExports.R marxan-internal.R misc.R generics.R MarxanOpts.R MarxanData.R MarxanUnsolved.R
NULL

#' MarxanUnsolved: An S4 class to represent Marxan input parameter and data for MARXAN
#'
#' This class is used to store Marxan input parameters and data to generate solutions using the MARXAN program.
#'
#' @slot opts "MarxanOptsMARXAN" object used to store input parameters.
#' @slot data "MarxanData" object used to store input data.
#' @export
#' @seealso  \code{\link{MarxanUnsolvedMARXAN}}, \code{\link{MarxanOptsMARXAN}}, \code{\link{MarxanOptsMARXAN-class}}, \code{\link{MarxanData-class}}.
setClass("MarxanUnsolvedMARXAN",
	contains="MarxanUnsolved"
	representation(
		opts="MarxanOptsMARXAN"
	)
)

#' Create a new "MarxanUnsolvedMARXAN" object
#'
#' This function creates a "MarxanUnsolvedMARXAN" object using "MarxanOptsMARXAN" and "MarxanData" objects.
#'
#' @param opts "MarxanOptsMARXAN" object.
#' @param data "MarxanData" object.
#' @return "MarxanUnsolved" object.
#' @export
#' @seealso \code{\link{MarxanUnsolvedMARXAN-class}}, \code{\link{MarxanOptsMARXAN-class}}, \code{\link{MarxanData-class}}.
MarxanUnsolvedMARXAN<-function(opts, data) {
	return(new("MarxanUnsolvedMARXAN", opts=opts, data=data))
}

#' @rdname solve
#' @inheritParams solve
#' @export
solve.MarxanUnsolvedMARXAN=function(x, wd=tempdir(), seeds=sample.int(n=10000L, size=x@opts@NCORES), clean=TRUE, verbose=TRUE) {
	# check that Marxan is installed properly
	findMarxanExecutablePath()
	stopifnot(is.marxanInstalled())
	# check inputs are valid
	stopifnot(file.exists(wd))
	stopifnot(length(seeds)==x@opts@NCORES)
	# set up marxan dir structure
	wd<-file.path(wd, paste0('M',paste(sample(letters, 10),collapse="")))
	coredirs<-file.path(wd,seq_len(x@opts@NCORES))
	dir.create(file.path(wd, 'input'),recursive=TRUE, showWarnings=TRUE)
	laply(coredirs, dir.create, recursive=TRUE, showWarnings=TRUE)
	# sink to disk marxan files
	write.MarxanData(x@data,dir=file.path(wd, 'input'))
	# create input files
	for (i in seq_along(coredirs))
		write.MarxanOpts(x@opts, file.path(wd, 'input'), coredirs[i], seed=seeds[i])
	# copy marxan to core dir
	file.copy(options()$marxanExecutablePath, file.path(coredirs, basename(options()$marxanExecutablePath)))
	# run chmod on marxan executable if windows
	if (.Platform$OS.type=="unix")
		laply(paste0('chmod +x "', file.path(coredirs, basename(options()$marxanExecutablePath)), '"'), system)
	# set up parrallelisation
	if (x@opts@NCORES>1) {
		clust<-makeCluster(x@opts@NCORES, type="SOCK")
		registerDoSNOW(clust)
	}
	# run marxan
	oldwd<-getwd()
	suppressWarnings(status<-alply(
		data.frame(
			file.path(coredirs, basename(options()$marxanExecutablePath)),
			replace(rep(FALSE, x@opts@NCORES), which(verbose & seq_along(coredirs)==1), ''),
			stringsAsFactors=FALSE
		), 1, .parallel=x@opts@NCORES>1, 
		function(x) {
			setwd(dirname(x[[1]]))
			return(system2(x[[1]], c('input.dat', '-s'), stdout=x[[2]]))
		}
	))
	setwd(oldwd)
	# end parallelisation
	if (x@opts@NCORES>1) {
		clust<-stopCluster(clust)
	}
	# check to see how it went
	if (any(unlist(status, use.names=FALSE, recursive=FALSE)!=0))
		stop("Marxan failed to execute.")
	# if succesful; import and merge results
	x=new("MarxanSolvedSANN", data=x@data, opts=x@opts, results=merge.MarxanResults(llply(coredirs, read.MarxanResults)))
	# clean dir
	if (clean)
		unlink(wd, recursive=TRUE, force=FALSE)
	return(x)
}

#' Read Marxan input data and parameters
#'
#' This function saves "MarxanUnsolved" objects to disk.
#'
#' @param path "character" path for input file to load.
#' @param skipchecks "logical" should data integrity checks be skipped?
#' @export
#' @seealso \code{\link{MarxanUnsolvedMARXAN}}, \code{\link{MarxanUnsolvedMARXAN-class}}, \code{\link{read.MarxanOptsMARXAN}}, \code{\link{read.MarxanData}}.
read.MarxanUnsolvedMARXAN<-function(path, skipchecks=FALSE) {
	return(
		MarxanUnsolved(
			read.MarxanOptsMARXAN(path),
			read.MarxanData(path, skipchecks=skipchecks)
		)
	)
}

#' Write Marxan input data and parameters
#'
#' This function saves "MarxanUnsolvedMARXAN" objects to disk.
#'
#' @param x "MarxanUnsolvedMARXAN" object to save.
#' @param dir "character" directory path for location to save data.
#' @export
#' @seealso \code{\link{MarxanUnsolvedMARXAN}}, \code{\link{MarxanUnsolvedMARXAN-class}}, \code{\link{write.MarxanOptsMARXAN}}, \code{\link{write.MarxanData}}.
write.MarxanUnsolvedMARXAN<-function(x, dir=getwd()) {
	write.MarxanData(x@data, dir)
	write.MarxanOpts(x@opts, dir)
}


