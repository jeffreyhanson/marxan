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

#' Create a new "MarxanUnsolved" object
#'
#' This function creates a "MarxanUnsolved" object using "MarxanOpts" and "MarxanData" objects.
#'
#' @param opts "MarxanOpts" object.
#' @param data "MarxanData" object.
#' @return "MarxanUnsolved" object.
#' @export
#' @seealso \code{\link{MarxanOpts-class}}, \code{\link{MarxanData-class}}.
MarxanUnsolved<-function(opts, data) {
	return(new("MarxanUnsolved", opts=opts, data=data))
}

#' @rdname solve
#' @inheritParams solve
#' @export
solve.MarxanUnsolved=function(x, wd=tempdir(), seeds=sample.int(n=10000L, size=x@opts@NCORES), clean=TRUE, verbose=TRUE) {
	# check that Marxan is installed properly
	findMarxanExecutablePath()
	stopifnot(is.marxanInstalled())
	# check inputs are valid
	stopifnot(file.exists(wd))
	stopifnot(length(seeds)==x@opts@NCORES)
	if (is.null(x@data@boundary) && x@opts@BLM!=0) {
		if (!is.null(x@data@polygons)) {
			warning('The BLM is non-zero and the MarxanData object has no planning unit boundary data,\n boundary data is being calculated..')
			x@data@boundary<-calcBoundaryData(is.null(x@data@polygons))
		} else {
			stop('The BLM is non-zero and the MarxanData object has no boundary or polygon data')
		}
	}
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


#' @rdname basemap
#' @export
basemap.MarxanUnsolved<-function(x, basemap="none", grayscale=FALSE, force_reset=FALSE) {
	return(basemap.MarxanData(x@data, basemap, grayscale, force_reset))
}

#' @rdname spplot
#' @inheritParams spplot
#' @export
setMethod(
	"spplot",
	signature(obj='MarxanUnsolved'),
	function(obj, y=obj@data@species$id, var='amount', basemap="none", colramp="YlOrRd", alpha=ifelse(basemap=="none", 1, 0.7), grayscale=FALSE, force_reset=FALSE) {
		return(spplot(obj@data, y, var, basemap, colramp, alpha, grayscale, force_reset))
	}
)


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


#' Read Marxan input data and parameters
#'
#' This function saves "MarxanUnsolved" objects to disk.
#'
#' @param path "character" path for input file to load.
#' @param skipchecks "logical" should data integrity checks be skipped?
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
#' @inheritParams names
names.MarxanUnsolved<-function(x) {
	return(names.MarxanData(x@data))
}

#' @export
#' @rdname names
#' @inheritParams names
`names<-.MarxanUnsolved`<-function(x,value) {
	stopifnot(length(value)==nrow(x@data@species) & !anyDuplicated(value) & is.character(value) & !any(is.na(value)))
	x@data@species$name<-value
	return(x)
}


#' @export
#' @rdname spfs
#' @inheritParams spfs
spfs.MarxanUnsolved<-function(x) {
	return(spfs.MarxanData(x@data))
}

#' @export
#' @rdname spfs
#' @inheritParams spfs
`spfs<-.MarxanUnsolved`<-function(x,value) {
	stopifnot(is.numeric(value) & !any(is.na(value)))
	x@data@species$spf<-value
	return(x)
}

#' @export
#' @rdname targets
#' @inheritParams targets
targets.MarxanUnsolved<-function(x) {
	return(targets.MarxanData(x@data))
}

#' @export
#' @rdname targets
#' @inheritParams targets
`targets<-.MarxanUnsolved`<-function(x,value) {
	stopifnot(is.numeric(value) & !any(is.na(value)))
	x@data@species$target<-value
	return(x)
}

#' @export
#' @rdname maxtargets
maxtargets.MarxanUnsolved<-function(x) {
	return(maxtargets.MarxanData(x@data))
}

#' @export
#' @rdname maxtargets
`maxtargets<-.MarxanUnsolved`<-function(x,value) {
	stopifnot(is.numeric(value) & !any(is.na(value)))
	x@data@species$maxtargets<-value
	return(x)
}

#' @export
#' @rdname sppids
#' @inheritParams sppids
sppids.MarxanUnsolved<-function(x) {
	return(sppids.MarxanData(x@data))
}

#' @export
#' @rdname sppids
#' @inheritParams sppids
`sppids<-.MarxanUnsolved`<-function(x,value) {
	stopifnot(length(value)==nrow(x@data@species) & !anyDuplicated(value) & is.integer(value) & !any(is.na(value)))
	x@data@species$id<-value
	return(x)
}

#' @export
#' @rdname puids
#' @inheritParams puids
puids.MarxanUnsolved<-function(x) {
	return(puids.MarxanData(x@data))
}

#' @export
#' @rdname puids
#' @inheritParams puids
`puids<-.MarxanUnsolved`<-function(x,value) {
	stopifnot(length(value)==nrow(x@data@pu) & !anyDuplicated(value) & is.integer(value) & !any(is.na(value)))
	x@data@pu$id<-value
	return(x)
}


#' @export
#' @rdname costs
costs.MarxanUnsolved<-function(x) {
	return(costs.MarxanData(x@data))
}

#' @export
#' @rdname costs
`costs<-.MarxanUnsolved`<-function(x,value) {
	stopifnot(is.numeric(value) & !any(is.na(value)))
	x@data@pu$costs<-value
	return(x)
}


#' @export
#' @rdname inistatus
#' @inheritParams inistatus
inistatus.MarxanUnsolved<-function(x) {
	return(inistatus.MarxanData(x@data))
}

#' @export
#' @rdname inistatus
#' @inheritParams inistatus
`inistatus<-.MarxanUnsolved`<-function(x,value) {
	stopifnot(is.numeric(value) & !any(is.na(value)))	
	x@data@pu$status<-value
	return(x)
}

