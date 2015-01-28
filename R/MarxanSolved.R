#' @include RcppExports.R marxan-internal.R misc.R generics.R MarxanOpts.R MarxanData.R MarxanUnsolved.R MarxanResults.R
NULL


#' MarxanSolved: An S4 class to represent Marxan inputs and outputs
#'
#' This class is used to store Marxan input and output data in addition to input parameters.
#'
#' @slot opts "MarxanOpts" object used to store input parameters.
#' @slot data "MarxanData" object used to store input data.
#' @slot results "MarxanResults" object used to store results.
#' @export
#' @seealso \code{\link{MarxanOpts-class}}, \code{\link{MarxanData-class}}, \code{\link{MarxanResults-class}}.
setClass("MarxanSolved",
	representation(
		data="MarxanData",
		opts="MarxanOpts",
		results="MarxanResults"
	)
)

setClassUnion("MarxanUnsolvedOrSolved", c("MarxanSolved", "MarxanUnsolved"))


#' Create new "MarxanSolved" object
#'
#' This function creates a "MarxanSolved" object using a "MarxanUnsolved" object and a "MarxanResults" object.
#'
#' @param unsolved "MarxanUnsolved" object.
#' @param results "MarxanResults" object.
#' @return "MarxanSolved" object.
#' @export
#' @seealso \code{\link{MarxanSolved-class}}, \code{\link{MarxanResults-class}}.
MarxanSolved<-function(unsolved, results) {
	return(new("MarxanSolved", opts=unsolved@opts, data=unsolved@data, results=results))
}

#' @rdname solve
#' @inheritParams solve
#' @export
solve.MarxanSolved<-function(x, wd=tempdir(), seeds=sample.int(n=10000L, size=x@opts@NCORES), clean=TRUE, force_reset=FALSE) {
	if (!force_reset)
		stop("This object already has Marxan solutions. Use force_reset=TRUE to force recalculation of solutions.")
	return(solve(MarxanUnsolved(opts=x@opts,data=x@data), wd, seeds, clean))
}

#' @rdname selections
#' @inheritParams selections
#' @export
selections.MarxanSolved<-function(x, y=NULL) {
	return(selection.MarxanResults(x@results, y))
}

#' @rdname score
#' @inheritParams score
#' @export
score.MarxanSolved<-function(x, y=NULL) {
	return(score.MarxanResults(x@results, y))
}

#' @export
summary.MarxanSolved<-function(x) {
	return(summary.MarxanResults(x@results))
}

#' @export
print.MarxanSolved<-function(x) {
	cat("MarxanSolved object.\n")
	print.MarxanOpts(x@opts, FALSE)
	print.MarxanData(x@data, FALSE)
	print.MarxanResults(x@results, FALSE)
}

# ' @export
setMethod(
	'show',
	'MarxanSolved',
	function(object)
		print.MarxanSolved(object)
)


#' @rdname log
#' @inheritParams log
#' @export
log.MarxanSolved<-function(x) {
	log.MarxanResults(x@results)
}

#' @export
#' @rdname amountheld
amountheld.MarxanSolved<-function(x, y=NULL) {
	return(amountheld.MarxanResults(x@results, y))
}

#' @rdname occheld
#' @inheritParams occheld
#' @export
occheld.MarxanSolved<-function(x, y=NULL) {
	return(occheld.MarxanResults(x@results, y))
}

#' @rdname mpm
#' @inheritParams mpm
#' @export
mpm.MarxanSolved<-function(x, y=NULL) {
	return(mpm.MarxanResults(x@results, y))
}

#' @rdname sepacheived
#' @inheritParams sepacheived
#' @export
sepacheived.MarxanSolved<-function(x, y=NULL) {
	return(sepacheived.MarxanResults(x@results, y))
}


#' @rdname targetsmet
#' @inheritParams targetsmet
#' @export
targetsmet.MarxanSolved<-function(x, y=NULL) {
	return(targetsmet.MarxanResults(x@results, y))
}

#' @rdname pca
#' @inheritParams pca
#' @export
pca.MarxanSolved<-function(x, var='selections', ..., force_reset=FALSE) {
	return(dist.MarxanResults(x@results, var, ..., force_reset=force_reset))	
}

#' @rdname dist
#' @export
dist.MarxanSolved<-function(x, var='selections', method="bray", force_reset=FALSE) {
	return(dist.MarxanResults(x@results, var, method, force_reset=force_reset))
}

#' @rdname mds
#' @inheritParams mds
#' @export
mds.MarxanSolved<-function(x, var='selections', method="bray", ..., force_reset=FALSE) {
	return(mds.MarxanResults(x@results, var, method, ..., force_reset=force_reset))
}

#' @rdname hclust
#' @inheritParams hclust
#' @export
hclust.MarxanSolved<-function(x, type='mds', var='selections', ..., force_reset=FALSE) {
	return(hclust.MarxanResults(x@results, type='mds', var='selections', ..., force_reset=force_reset))
}

#' @rdname ordiplot
#' @inheritParams ordiplot
#' @export
ordiplot.MarxanSolved<-function(x, type='mds', var='selections', nbest=1, ..., force_reset=FALSE) {
	return(ordiplot.MarxanResults(x@results, type, var, nbest, ..., force_reset=force_reset))
}

#' @rdname dendrogram
#' @export
dendrogram.MarxanSolved<-function(x, type='mds', var='selections', nbest=1, ..., force_reset=FALSE) {
	return(dendrogram.MarxanResults(x@results, type, var, nbest, ..., force_reset=force_reset))
}

#' @rdname dotchart
#' @export
dotchart.MarxanSolved<-function(x, var="score", nbest=1, n=50) {
	dotchart.MarxanResults(x@results, var, nbest, n)
}

#' @rdname basemap
#' @export
basemap.MarxanSolved<-function(x, basemap="none", grayscale=FALSE, force_reset=FALSE) {
	return(basemap.MarxanData(x@data, basemap, grayscale, force_reset))
}

#' @rdname spplot
#' @inheritParams spplot
#' @export
setMethod(
	"spplot",
	signature(obj='MarxanSolved'),
	function(obj, y=obj@data@species$id, var='amount', basemap="none", colramp="YlOrRd", alpha=ifelse(basemap=="none", 1, 0.7), grayscale=FALSE, force_reset=FALSE) {
		return(spplot.MarxanData(obj@data, y, var, basemap, colramp, alpha, grayscale, force_reset))
	}
)


#' @export
#' @rdname update
update.MarxanSolved<-function(x, formula, solve=TRUE, force_reset=TRUE) {
	return(update.MarxanUnsolved(MarxanUnsolved(x@opts, x@data), formula, solve, force_reset))
}


#' @rdname plot
#' @inheritParams plot
#' @export
setMethod(
	"plot",
	signature(x="MarxanSolved",y="numeric"),
	function(x, y, basemap="none", colramp="Greens", lockedincol="#000000FF", lockedoutcol="#D7D7D7FF", alpha=ifelse(basemap=="none",1,0.7), grayscale=FALSE, force_reset=FALSE) {
		# check for issues
		stopifnot(alpha<=1 & alpha>=0)
		match.arg(colramp, rownames(brewer.pal.info))
		stopifnot(inherits(x@data@polygons, "PolySet"))	
		# get basemap data
		if (basemap!="none")
			basemap<-basemap.MarxanData(x@data, basemap, grayscale, force_reset)
		# main processing
		if (y==0)
			y<-x@results@best
		values<-x@results@selections[y,]
		cols<-character(length(values))
		cols[which(x@data@pu$status==2)]<-lockedincol
		cols[which(x@data@pu$status==3)]<-lockedoutcol
		cols[which(x@data@pu$status<2)]<-brewerCols(values[which(x@data@pu$status<2)], colramp, alpha, n=2)
		prettyGeoplot(
			x@data@polygons,
			cols,
			basemap,
			ifelse(y==x@results@best, paste0("Best Solution (",y,")"), paste0("Solution (",y,")")),
			categoricalLegend(c(lockedoutcol,brewerCols(c(0,1),colramp,alpha,n=2),lockedincol),c("Locked Out", "Not Selected", "Selected", "Locked In")),
			beside=FALSE
		)
	}
)

#' @rdname plot
#' @inheritParams plot
#' @export
setMethod(
	"plot",
	signature(x="MarxanSolved",y="missing"),
	function(x, y, basemap="none", colramp="PuBu", lockedincol="#000000FF", lockedoutcol="#D7D7D7FF", alpha=ifelse(basemap=="none",1,0.7), grayscale=FALSE, force_reset=FALSE) {
		# check for issues
		match.arg(basemap, c("none", "roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid"))
		stopifnot(alpha<=1 & alpha>=0)
		match.arg(colramp, rownames(brewer.pal.info))
		stopifnot(inherits(x@data@polygons, "PolySet"))
		# get basemap data
		if (basemap!="none")
			basemap<-basemap.MarxanData(x@data, basemap, grayscale, force_reset)	
		# main processing
		if (force_reset || !is.cached(x@results, "selectionfreqs")) {
			cache(x@results, "selectionfreqs", colMeans(x@results@selections))
		}
		values<-cache(x@results,"selectionfreqs")[which(x@data@pu$status<2)]
		cols<-brewerCols(rescale(cache(x@results,"selectionfreqs"),from=range(values),to=c(0,1)), pal=colramp, alpha=alpha)
		cols[which(x@data@pu$status==2)]<-lockedincol
		cols[which(x@data@pu$status==3)]<-lockedoutcol
		prettyGeoplot(
			x@data@polygons,
			cols,
			basemap,
			"Planning unit selection frequency",
			continuousLegend(values,colramp,posx=c(0.3, 0.4),posy=c(0.1, 0.9)),
			beside=TRUE
		)
	}
)

#' @rdname plot
#' @inheritParams plot
#' @export
setMethod(
	"plot",
	signature(x="MarxanSolved",y="MarxanSolved"),
	function(x, y, i=NULL, j=i, basemap="none", colramp=ifelse(is.null(i), "RdYlBu", "Accent"), xlockedincol="#000000FF", xlockedoutcol="#D7D7D7FF", ylockedincol="#FFFFFFFF", ylockedoutcol="#D7D7D7FF", alpha=ifelse(basemap=="none",1,0.7), grayscale=FALSE, force_reset=FALSE) {
		# check for issues
		stopifnot(alpha<=1 & alpha>=0)
		match.arg(colramp, rownames(brewer.pal.info))
		stopifnot(inherits(x@data@polygons, "PolySet"))
		stopifnot(is.comparable(x,y))
		# get basemap data
		if (basemap!="none")
			basemap<-basemap.MarxanData(x@data, basemap, grayscale, force_reset)	
		# main processing
		cols<-character(nrow(x@data@pu))
		if (is.null(i) || is.null(j)) {
			cols[which(x@data@pu$status==2)]<-xlockedincol
			cols[which(y@data@pu$status==2)]<-ylockedincol
			cols[which(x@data@pu$status==3)]<-xlockedoutcol
			cols[which(y@data@pu$status==3)]<-ylockedoutcol
		
			if (force_reset || !is.cached(x@results, "selectionfreqs"))
				cache(x@results, "selectionfreqs", colMeans(x@results@selections))
			xsc<-cache(x@results, "selectionfreqs")[which(nchar(cols)==0)]
			if (force_reset || !is.cached(y@results, "selectionfreqs"))
				cache(y@results, "selectionfreqs", colMeans(y@results@selections))
			ysc<-cache(y@results, "selectionfreqs")[which(nchar(cols)==0)]
			values<-xsc-ysc
			cols[which(nchar(cols)==0)]<-brewerCols(rescale(values,to=c(0,1)), colramp, alpha)
			prettyGeoplot(
				x@data@polygons,
				cols,
				basemap,
				"Difference in selection frequencies",
				fun<-continuousLegend(
					values,
					colramp,
					posx=c(0.3, 0.4),posy=c(0.1, 0.9),
					center=TRUE,
					endlabs=c('+X','+Y')
				),
				beside=TRUE
			)
		} else {
			if (i==0)
				i<-x@results@best
			if (j==0)
				j<-y@results@best
			cols2<-brewerCols(seq(0,1,length.out=4),colramp,alpha,n=4)
			
			cols[which(x@results@selections[i,]==1 & y@results@selections[j,]==0)]<-cols2[1]
			cols[which(x@results@selections[i,]==0 & y@results@selections[j,]==1)]<-cols2[2]
			cols[which(x@results@selections[i,]==1 & y@results@selections[j,]==1)]<-cols2[3]
			cols[which(x@results@selections[i,]==0 & y@results@selections[j,]==0)]<-cols2[4]

			cols[which(x@data@pu$status==2)]<-xlockedincol
			cols[which(y@data@pu$status==2)]<-ylockedincol
			cols[which(x@data@pu$status==3)]<-xlockedoutcol
			cols[which(y@data@pu$status==3)]<-ylockedoutcol
			
			xrepr<-ifelse(i==x@results@best, '(best)', paste0('(',i,')'))
			yrepr<-ifelse(i==y@results@best, '(best)', paste0('(',j,')'))

			prettyGeoplot(
				x@data@polygons,
				cols,
				basemap,
				paste0("Difference in X solution ",i,ifelse(i==x@results@best, " (best)", ""), " and Y solution ",j, ifelse(j==y@results@best, " (best)", "")),
				categoricalLegend(
					c(cols2,xlockedincol,ylockedincol,xlockedoutcol,ylockedoutcol),
					c("Selected in X",  "Selected in Y", "Both", "Neither", "Locked in X", "Locked in Y", paste("Locked out X"), paste("Locked out Y")),
					ncol=4
				),
				beside=FALSE
			)
		}
	}
)


#' @rdname is.comparable
#' @export
setMethod(
	f="is.comparable",
	signature(x="MarxanUnsolvedOrSolved", y="MarxanUnsolvedOrSolved"),
	function(x,y) {
		return(is.comparable(x@data, y@data))
	}
)

#' @rdname is.comparable
#' @export
setMethod(
	f="is.comparable",
	signature(x="MarxanData", y="MarxanUnsolvedOrSolved"),
	function(x,y) {
		return(is.comparable(x, y@data))
	}
)

#' @rdname is.comparable
#' @export
setMethod(
	f="is.comparable",
	signature(x="MarxanUnsolvedOrSolved", y="MarxanData"),
	function(x,y) {
		return(is.comparable(x@data, y))
	}
)


#' Read Marxan input data, parameters and results
#'
#' This function saves "MarxanSolved" objects to disk.
#'
#' @param path "character" path for input file to load.
#' @param skipchecks "logical" should data integrity checks be skipped?
#' @export
#' @seealso \code{\link{MarxanSolved}}, \code{\link{MarxanSolved-class}}, \code{\link{read.MarxanOpts}}, \code{\link{read.MarxanData}}, \code{\link{read.MarxanUnsolved}}.
read.MarxanSolved<-function(path, skipchecks=FALSE) {
	dir<-try(parseArg("OUTPUTDIR",readLines(path),error=FALSE), silent=TRUE)
	if (is.null(dir) || inherits(dir, 'try-error'))
		dir<-basename(path)
	MarxanSolved(
		read.MarxanUnsolved(path, skipchecks=skipchecks),
		read.MarxanResults(dir)
	)
}

#' Write Marxan input data, parameters and results
#'
#' This function saves "MarxanSolved" objects to disk.
#'
#' @param x "MarxanSolved" object to save.
#' @param dir "character" directory path for location to save data.
#' @export
#' @seealso \code{\link{MarxanSolved}}, \code{\link{MarxanSolved-class}}, \code{\link{write.MarxanOpts}}, \code{\link{write.MarxanData}}, \code{\link{write.MarxanUnsolved}}. 
write.MarxanSolved<-function(x, dir=getwd()) {
	## write opts
	write.MarxanOpts(x@opts, dir)
	## write data
	write.MarxanData(x@data, dir)
	## write results
	write.table(x@results@summary, file.path(dir, "output_sum.csv"), quote=FALSE, row.names=FALSE, sep=",")
	# save marxan solution matrix
	write.table(x@results@selections+0, file.path(dir, "output_solutionsmatrix.csv"), quote=FALSE, row.names=TRUE, sep=",", col.names=replace(colnames(x@results@selections), 1, paste0("SolutionsMatrix,",colnames(x@results@selections)[1])))
	# save log file
	writeLines(x@results@log, con=file.path(dir, "output_log.dat"))
	# save mv data
	paths<-paste0(dir, "/output_mv", formatC(seq_len(nrow(x@results@selections)), flag=0, width=5), ".csv")
	# set defaults if not present in @data
	tmplst<-list()
	defaultvals<-c("name"="","sepnum"=0,"targetocc"=0)
	for (i in c("name","sepnum","targetocc")) {
		if (is.null(x@data@species[[i]])) {
			tmplst[[i]]<-rep(defaultvals[i], nrow(x@data@species))
		} else {
			tmplst[[i]]<-x@data@species[[i]]
		}
	}	
	for (i in seq_len(nrow(x@results@selections))) {
		write.table(
			data.frame(
				'"Conservation Feature"'=x@data@species$id,
				'"Feature Name"'=rep("", nrow(x@data@species)),
				'"Target"'=x@data@species$target,
				'"Amount Held"'=x@results@amountheld[i,],
				'"Occurrence Target"'=tmplst$targetocc,
				'"Occurrences Held"'=x@results@occheld[i,],
				'"Separation Target"'=tmplst$sepnum,
				'"Separation Achieved"'=x@results@sepacheived[i,],
				'"Target Met"'=replace(rep("no", nrow(x@data@species)), which(x@results@targetsmet[i,]), "yes"),
				'"MPM"'=x@results@mpm[i,],
				check.names=FALSE
			),
			file=paths[i],
			quote=FALSE, 
			sep=",", 
			row.names=FALSE
		)
	}
}


#' @export
#' @rdname names
#' @inheritParams names
names.MarxanSolved<-function(x) {
	return(names.MarxanData(x@data))
}

#' @export
#' @rdname names
#' @inheritParams names
`names<-.MarxanSolved`<-function(x,value) {
	stopifnot(length(value)==nrow(x@data@species) & !anyDuplicated(value) & is.character(value) & !any(is.na(value)))
	x@data@species$name<-value
	return(x)
}


#' @export
#' @rdname spfs
#' @inheritParams spfs
spfs.MarxanSolved<-function(x) {
	return(spfs.MarxanData(x@data))
}

#' @export
#' @rdname spfs
#' @inheritParams spfs
`spfs<-.MarxanSolved`<-function(x,value) {
	stopifnot(is.numeric(value) & !any(is.na(value)))
	x@data@species$spf<-value
	return(x)
}

#' @export
#' @rdname targets
#' @inheritParams targets
targets.MarxanSolved<-function(x) {
	return(targets.MarxanData(x@data))
}

#' @export
#' @rdname targets
#' @inheritParams targets
`targets<-.MarxanSolved`<-function(x,value) {
	stopifnot(is.numeric(value) & !any(is.na(value)))
	x@data@species$target<-value
	return(x)
}

#' @export
#' @describeIn maxtargets
maxtargets.MarxanSolved<-function(x) {
	return(maxtargets.MarxanData(x@data))
}

#' @export
#' @describeIn maxtargets
`maxtargets<-.MarxanSolved`<-function(x,value) {
	stopifnot(is.numeric(value) & !any(is.na(value)))
	x@data@species$maxtargets<-value
	return(x)
}

#' @export
#' @rdname sppids
#' @inheritParams sppids
sppids.MarxanSolved<-function(x) {
	return(sppids.MarxanData(x@data))
}

#' @export
#' @rdname sppids
#' @inheritParams sppids
`sppids<-.MarxanSolved`<-function(x,value) {
	stopifnot(length(value)==nrow(x@data@species) & !anyDuplicated(value) & is.integer(value) & !any(is.na(value)))
	x@data@species$id<-value
	return(x)
}

#' @export
#' @rdname puids
#' @inheritParams puids
puids.MarxanSolved<-function(x) {
	return(puids.MarxanData(x@data))
}

#' @export
#' @rdname puids
#' @inheritParams puids
`puids<-.MarxanSolved`<-function(x,value) {
	stopifnot(length(value)==nrow(x@data@pu) & !anyDuplicated(value) & is.integer(value) & !any(is.na(value)))
	x@data@pu$id<-value
	return(x)
}


#' @export
#' @rdname costs
costs.MarxanSolved<-function(x) {
	return(costs.MarxanData(x@data))
}

#' @export
#' @rdname costs
`costs<-.MarxanSolved`<-function(x,value) {
	stopifnot(is.numeric(value) & !any(is.na(value)))
	x@data@pu$costs<-value
	return(x)
}


#' @export
#' @rdname inistatus
#' @inheritParams inistatus
inistatus.MarxanSolved<-function(x) {
	return(inistatus.MarxanData(x@data))
}

#' @export
#' @rdname inistatus
#' @inheritParams inistatus
`inistatus<-.MarxanSolved`<-function(x,value) {
	stopifnot(is.numeric(value) & !any(is.na(value)))	
	x@data@pu$status<-value
	return(x)
}


