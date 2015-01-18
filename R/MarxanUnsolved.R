#' @include RcppExports.R marxan-internal.R misc.R generics.R MarxanOpts.R MarxanData.R
NULL

#' MarxanUnsolved: An S4 class to represent Marxan inputs
#'
#' This class is used to store Marxan input data and input parameters.
#'
#' @slot data "MarxanData" object used to store input data.
#' @slot opts "MarxanOpts" object used to store input parameters.
#' @export
#' @seealso  \code{\link{MarxanOpts-class}}, \code{\link{MarxanData-class}}
setClass("MarxanUnsolved",
	representation(
		data="MarxanData",
		opts="MarxanOpts"
	)
)

#' Create a new MarxanUnsolved object
#'
#' This function creates a MarxanUnsolved object using MarxanOpts and MarxanData objects.
#'
#' @param opts "MarxanOpts" object.
#' @param data "MarxanData" object.
#' @return "MarxanUnsolved" object.
#' @export
#' @seealso \code{\link{MarxanOpts-class}}, \code{\link{MarxanData-class}}
MarxanUnsolved<-function(opts, data) {
	return(new("MarxanUnsolved", opts=opts, data=data))
}

#' @describeIn solve
#' @export
solve.MarxanUnsolved=function(x, wd=tempdir(), seeds=sample.int(n=10000L, size=x@opts@NCORES), clean=TRUE, verbose=FALSE) {
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
	# set up parrallelisation
	if (x@opts@NCORES>1)
		registerDoSNOW(makeCluster(x@opts@NCORES, type="SOCK"))
	# run marxan
	if (verbose) {
		status<-alply(
			cbind.data.frame(
				file.path(coredirs, basename(options()$marxanExecutablePath)),
				file.path(coredirs, 'input.dat'),
				(verbose & seq_along(coredirs)==1)
			), 1, .parallel=x@opts@NCORES>1, 
			function(x) {
				return(system(paste0('"',x[[1]],'" "',x[[2]],'" -s'), show.output.on.console=x[[3]]))
			}
		)
	} else {
		suppressWarnings(status<-alply(
			cbind.data.frame(
				file.path(coredirs, basename(options()$marxanExecutablePath)),
				file.path(coredirs, 'input.dat'),
				(verbose & seq_along(coredirs)==1)
			), 1, .parallel=x@opts@NCORES>1, 
			function(x) {
				return(system(paste0('"',x[[1]],'" "',x[[2]],'" -s'), show.output.on.console=x[[3]]))
			}
		))
	}
	# check to see how it went
	if (any(unlist(status, use.names=FALSE, recursive=FALSE)!=0))
		stop("Marxan failed to execute.")
	# if succesful; import and merge results
	x=new("MarxanSolved", data=x@data, opts=x@opts, results=merge.MarxanResults(llply(coredirs, read.MarxanResults)))
	# clean dir
	if (clean)
		unlink(wd, recursive=TRUE, force=FALSE)
	return(x)
}

#' @describeIn print
#' @export
print.MarxanUnsolved=function(x) {
	cat("MarxanUnsolved object.\n")
	print.MarxanOpts(x@opts, FALSE)
	print.MarxanData(x@data, FALSE)
}

#' @export
# setMethod(
	# 'show',
	# 'MarxanUnsolved',
	# function(x, ...)
		# print.MarxanUnsolved(x, ...)
# )

#' @export
#' @describeIn names
names.MarxanUnsolved<-function(x) {
	return(names(x@data))
}


#' @describeIn basemap
#' @export
basemap.MarxanUnsolved<-function(x, basemap="none", alpha=1, grayscale=FALSE, xzoom=c(1,1), yzoom=c(1,1), force_reset=FALSE) {
	return(basemap.MarxanData(x@data, basemap, alpha, grayscale, xzoom, yzoom, force_reset))
}

#' @describeIn update
#' @export
update.MarxanUnsolved<-function(x, formula, solve=TRUE, force_reset=TRUE) {
	m<-MarxanUnsolved(
		opts=update.MarxanOpts(x@opts, formula),
		data=update.MarxanData(x@data, formula, force_reset)
	)
	if (solve)
		m<-solve.MarxanUnsolved(m)
	return(m)
}

#' @describeIn plot
#' @export
setMethod(
	"plot", 
	signature(x="MarxanUnsolved", y="character"),
	function(x, y, basemap="none", colramp="BuGn", alpha=1, grayscale=FALSE, xzoom=c(1,1), yzoom=c(1,1), force_reset=FALSE) {
		plot(x@data, y, basemap, colramp, alpha, grayscale, xzoom, yzoom, force_reset=force_reset)
	}
)
