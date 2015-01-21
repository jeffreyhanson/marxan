#' @include RcppExports.R marxan-internal.R misc.R generics.R MarxanOpts.R MarxanData.R
NULL

#' MarxanUnsolved: An S4 class to represent Marxan inputs
#'
#' This class is used to store Marxan input data and input parameters.
#'
#' @slot opts "MarxanOpts" object used to store input parameters.
#' @slot data "MarxanData" object used to store input data.
#' @export
#' @seealso  \code{\link{MarxanOpts-class}}, \code{\link{MarxanData-class}}.
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
#' @seealso \code{\link{MarxanOpts-class}}, \code{\link{MarxanData-class}}.
MarxanUnsolved<-function(opts, data) {
	return(new("MarxanUnsolved", opts=opts, data=data))
}

#' @describeIn solve
#' @export
solve.MarxanUnsolved=function(x, wd=tempdir(), seeds=sample.int(n=10000L, size=x@opts@NCORES), clean=TRUE, verbose=TRUE) {
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
	oldwd<-getwd()
	suppressWarnings(status<-alply(
		data.frame(
			file.path(coredirs, basename(options()$marxanExecutablePath)),
			file.path(coredirs, 'input.dat'),
			(verbose & seq_along(coredirs)==1),
			stringsAsFactors=FALSE
		), 1, .parallel=x@opts@NCORES>1, 
		function(x) {
			setwd(dirname(x[[2]]))
			return(system(paste0('"',x[[1]],'" "',x[[2]],'" -s'), show.output.on.console=x[[3]]))
		}
	))
	setwd(oldwd)
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

#' @export
print.MarxanUnsolved=function(x) {
	cat("MarxanUnsolved object.\n")
	print.MarxanOpts(x@opts, FALSE)
	print.MarxanData(x@data, FALSE)
}

#' @export
setMethod(
	'show',
	'MarxanUnsolved',
	function(object)
		print.MarxanUnsolved(object)
)


#' @describeIn basemap
#' @export
basemap.MarxanUnsolved<-function(x, basemap="none", grayscale=FALSE, force_reset=FALSE) {
	return(basemap.MarxanData(x@data, basemap, grayscale, force_reset))
}

#' @describeIn spplot
#' @export
spplot.MarxanUnsolved<-function(x, y, var='amount', basemap="none", colramp="YlOrRd", alpha=ifelse(basemap=="none", 1, 0.7), grayscale=FALSE, force_reset=FALSE) {
	return(spplot.MarxanData(x@data, y, var, basemap, colramp, alpha, grayscale, force_reset))
}


#' @export
#' @rdname update
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
	function(x, y, basemap="none", colramp="BuGn", alpha=1, grayscale=FALSE, force_reset=FALSE) {
		plot(x@data, y, basemap, colramp, alpha, grayscale, force_reset=force_reset)
	}
)


#' Read Marxan input data and parameters
#'
#' This function saves "MarxanUnsolved" objects to disk.
#'
#' @param path "character" path for input file to load.
#' @param skipchecks "logical" Should data integrity checks be skipped?
#' @export
#' @seealso \code{\link{MarxanUnsolved}}, \code{\link{MarxanUnsolved-class}}, \code{\link{read.MarxanOpts}}, \code{\link{read.MarxanData}}.
read.MarxanUnsolved<-function(path, skipchecks=FALSE) {
	return(
		MarxanUnsolved(
			read.MarxanOpts(path),
			read.MarxanData(path, skipchecks=skipchecks)
		)
	)
}


#' Write Marxan input data and parameters
#'
#' This function saves "MarxanUnsolved" objects to disk.
#'
#' @param x "MarxanUnsolved" object to save.
#' @param dir "character" directory path for location to save data.
#' @export
#' @seealso \code{\link{MarxanUnsolved}}, \code{\link{MarxanUnsolved-class}}, \code{\link{write.MarxanOpts}}, \code{\link{write.MarxanData}}.
write.MarxanUnsolved<-function(x, dir=getwd()) {
	write.MarxanData(x@data, dir)
	write.MarxanOpts(x@opts, dir)
}


#' @export
#' @rdname names
names.MarxanUnsolved<-function(x) {
	return(names.MarxanData(x@data))
}

#' @export
#' @rdname names
`names<-.MarxanUnsolved`<-function(x,value) {
	stopifnot(length(value)==nrow(x@data@species) & is.character(value) & !any(is.na(value)))
	x@data@species$name<-value
	return(x)
}


#' @export
#' @describeIn spfs
spfs.MarxanUnsolved<-function(x) {
	return(spfs.MarxanData(x@data))
}

#' @export
#' @describeIn spfs
`spfs<-.MarxanUnsolved`<-function(x,value) {
	stopifnot(length(value)==nrow(x@data@species) & is.numeric(value) & !any(is.na(value)))
	x@data@species$spf<-value
	return(x)
}

#' @export
#' @describeIn targets
targets.MarxanUnsolved<-function(x) {
	return(targets.MarxanData(x@data))
}

#' @export
#' @describeIn targets
`targets<-.MarxanUnsolved`<-function(x,value) {
	stopifnot(length(value)==nrow(x@data@species) & is.numeric(value) & !any(is.na(value)))
	x@data@species$target<-value
	return(x)
}

#' @export
#' @describeIn maxtargets
maxtargets.MarxanUnsolved<-function(x) {
	return(maxtargets.MarxanData(x@data))
}

#' @export
#' @describeIn maxtargets
`maxtargets<-.MarxanUnsolved`<-function(x,value) {
	stopifnot(length(value)==nrow(x@data@species) & is.numeric(value) & !any(is.na(value)))
	x@data@species$maxtargets<-value
	return(x)
}

#' @export
#' @describeIn sppids
sppids.MarxanUnsolved<-function(x) {
	return(sppids.MarxanData(x@data))
}

#' @export
#' @describeIn sppids
`sppids<-.MarxanUnsolved`<-function(x,value) {
	stopifnot(length(value)==nrow(x@data@species) & is.integer(value) & !any(is.na(value)))
	x@data@species$id<-value
	return(x)
}

#' @export
#' @describeIn puids
puids.MarxanUnsolved<-function(x) {
	return(puids.MarxanData(x@data))
}

#' @export
#' @describeIn puids
`puids<-.MarxanUnsolved`<-function(x,value) {
	stopifnot(length(value)==nrow(x@data@pu) & is.integer(value) & !any(is.na(value)))
	x@data@pu$id<-value
	return(x)
}


#' @export
#' @describeIn costs
costs.MarxanUnsolved<-function(x) {
	return(costs.MarxanData(x@data))
}

#' @export
#' @describeIn costs
`costs<-.MarxanUnsolved`<-function(x,value) {
	stopifnot(length(value)==nrow(x@data@pu) & is.numeric(value) & !any(is.na(value)))
	x@data@pu$costs<-value
	return(x)
}


#' @export
#' @describeIn inistatus
inistatus.MarxanUnsolved<-function(x) {
	return(inistatus.MarxanData(x@data))
}

#' @export
#' @describeIn inistatus
`inistatus<-.MarxanUnsolved`<-function(x,value) {
	stopifnot(length(value)==nrow(x@data@pu) & is.numeric(value) & !any(is.na(value)))	
	x@data@pu$status<-value
	return(x)
}

