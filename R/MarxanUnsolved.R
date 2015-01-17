#' MarxanUnsolved: An S4 class to represent Marxan inputs
#'
#' This class is used to store Marxan input data and input parameters.
#'
#' @slot data "MarxanData" object used to store input data.
#' @slot opts "MarxanOpts" object used to store input parameters.
#' @seealso  \code{\link{MarxanOpts-class}}, \code{\link{MarxanData-class}}
setClass("MarxanUnsolved",
	representation(
		data="MarxanData",
		opts="MarxanOpts",
	)
)

#' Create new MarxanUnsolved object
#'
#' This function creates a MarxanUnsolved object using MarxanOpts and MarxanData objects.
#'
#' @param opts "MarxanOpts" object.
#' @param data "MarxanData" object.
#' @return "MarxanUnsolved" object.
#' @seealso \code{\link{MarxanOpts-class}}, \code{\link{MarxanData-class}},
MarxanUnsolved<-function(opts, data) {
	return(new("MarxanUnsolved", opts=opts, data=data))
}

#' @describein solve solve "MarxanUnsolved" object.
solve.MarxanUnsolved=function(x, wd=tempdir(), seeds=sample.int(n=10000L, size=x@opts@NCORES), clean=TRUE) {
	# check that Marxan is installed properly
	findMarxanExecutable()
	stopifnot(is.marxanInstalled())
	# check inputs are valid
	stopifnot(file.exists(wd))
	stopifnot(length(seeds)==x@opts@NCORES)
	# set up marxan dir structure
	wd<-file.path(wd, paste0('M',paste(sample(letters, 10),collapse="")))
	coredirs<-file.path(wd,seq_len(x@opts@NCORES))
	dir.create(file.path(wd, 'input'),recursive=TRUE, showWarnings=TRUE)
	laply(coredirs, dir.create, recursive=TRUE, showWarnings=TRUE))
	# sink to disk marxan files
	write.MarxanData(x@data)
	# create input files
	Map(write.MarxanOpts, x@opts, dir=coredirs, seed=seeds)
	# copy marxan to core dir
	file.copy(options()$marxanExecutablePath, coredirs, options()$marxanExecutablePath))
	# set up parallelisation
	if (x@opts@NCORES>1)
		registerDoSnow(makeCluster(x@opts@NCORES, type="SOCK"))
	llply(x=options()$marxanExecutablePath, y=coredirs, .parallel=x@opts@NCORES>1, 
		function(x,y) {
			system(paste0('"',x,'" "',y,'"'))
		}
	)
	# import and merge results
	x=new("MarxanSolved", data=x@data, opts=x@opts, results=merge.MarxanResults(laply(coredirs, read.MarxanResults)))
	# clean dir
	if (clean)
		unlink(wd, recurisve=TRUE, force=FALSE)
	return(x)
}

#' @describein print
print.MarxanUnsolved=function(x) {
	cat("MarxanUnsolved object.\n")
	print.MarxanOpts(x@opts, FALSE)
	print.MarxanData(x@data, FALSE)
}

#' @describein basemap
basemap.MarxanUnsolved<-function(x, basemap="none", alpha=1, grayscale=FALSE, xzoom=c(1,1), yzoom=c(1,1), force_reset=FALSE) {
	return(basemap.MarxanData(x@data, basemap, alpha, grayscale, xzoom, yzoom, force_reset))
}

#' @describein update
update.MarxanUnsolved<-function(x, formula, evaluate=TRUE, force_reset) {
	m<-MarxanUnsolved(
		opts=update.MarxanOpts(x@opts, formula, force_reset),
		data=update.MarxanData(x@data, formula, force_reset)
	)
	if (evaluate)
		m<-solve(m)
	return(m)
}
