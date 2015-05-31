#' @include RcppExports.R marxan-internal.R misc.R generics.R MarxanOpts.R MarxanOptsMarxan.R MarxanData.R MarxanUnsolved.R
NULL

#' MarxanUnsolvedMarxan: An S4 class to represent Marxan input parameter and data for MARXAN
#'
#' This class is used to store Marxan input parameters and data to generate solutions using the MARXAN program.
#'
#' @slot opts "MarxanOptsMarxan" object used to store input parameters.
#' @slot data "MarxanData" object used to store input data.
#' @export
#' @seealso  \code{\link{MarxanOptsMarxan-class}}, \code{\link{MarxanData-class}}.
setClass("MarxanUnsolvedMarxan",
	contains="MarxanUnsolved"
	representation(
		opts="MarxanOptsMarxan"		
	)
)

#' Create a new "MarxanUnsolvedMarxan" object
#'
#' This function creates a "MarxanUnsolvedMarxan" object using "MarxanOptsMarxan" and "MarxanData" objects.
#'
#' @param opts "MarxanOptsMarxan" object.
#' @param data "MarxanData" object.
#' @return "MarxanUnsolvedMarxan" object.
#' @export
#' @seealso \code{\link{MarxanUnsolvedMarxan-class}}, \code{\link{MarxanOptsMarxan-class}}, \code{\link{MarxanData-class}}.
MarxanUnsolvedMarxan<-function(opts, data) {
	return(new("MarxanUnsolvedMarxan", opts=opts, data=data))
}

#' @rdname solve
#' @inheritParams solve
#' @export
solve.MarxanUnsolvedMarxan=function(x, method='SANN', start=ifelse(method=='SANN','LP','1'), verbose=1, wd=tempdir(), seeds=sample.int(n=10000L, size=x@opts@NCORES), clean=(wd==tempdir())) {
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
		write.MarxanOpts(x@opts, file.path(wd, 'input'), coredirs[i], seed=seeds[i], verbose=verbose, start=start)
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
	
	
	
	x=new("MarxanSolvedMarxanSANN", data=x@data, opts=x@opts, results=merge.MarxanResults(llply(coredirs, read.MarxanResults)))
	# clean dir
	if (clean)
		unlink(wd, recursive=TRUE, force=FALSE)
	return(x)
}

#' Read Marxan input data and parameters
#'
#' This function reads MARXAN data from disk.
#'
#' @param path "character" path for input file to load.
#' @param skipchecks "logical" should data integrity checks be skipped?
#' @return "MarxanUnsolvedMarxan" object.
#' @export
#' @seealso \code{\link{MarxanUnsolvedMarxan}}, \code{\link{MarxanUnsolvedMarxan-class}}, \code{\link{read.MarxanOptsMarxan}}, \code{\link{read.MarxanData}}.
read.MarxanUnsolvedMarxan<-function(path, skipchecks=FALSE) {
	return(
		MarxanUnsolved(
			read.MarxanOptsMarxan(path),
			read.MarxanData(path, skipchecks=skipchecks)
		)
	)
}

#' Write Marxan input data and parameters
#'
#' This function saves "MarxanUnsolvedMarxan" objects to disk.
#'
#' @param x "MarxanUnsolvedMarxan" object to save.
#' @param dir "character" directory path for location to save data.
#' @export
#' @seealso \code{\link{MarxanUnsolvedMarxan}}, \code{\link{MarxanUnsolvedMarxan-class}}, \code{\link{write.MarxanOptsMARXAN}}, \code{\link{write.MarxanData}}.
write.MarxanUnsolvedMarxan<-function(x, dir=getwd()) {
	write.MarxanData(x@data, dir)
	write.MarxanOpts(x@opts, dir)
}


