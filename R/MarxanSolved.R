#' @include RcppExports.R marxan-internal.R misc.R generics.R MarxanOpts.R MarxanData.R MarxanUnsolved.R MarxanResults.R
NULL


#' MarxanSolved: An S4 class to represent Marxan inputs and outputs
#'
#' This class is used to store Marxan input and output data in addition to input parameters.
#'
#' @slot data "MarxanData" object used to store input data.
#' @slot opts "MarxanOpts" object used to store input parameters.
#' @slot results "MarxanResults" object used to store results.
#' @export
#' @seealso \code{\link{MarxanOpts-class}}, \code{\link{MarxanData-class}}, \code{\link{MarxanResults-class}}
setClass("MarxanSolved",
	representation(
		data="MarxanData",
		opts="MarxanOpts",
		results="MarxanResults"
	)
)

setClassUnion("MarxanUnsolvedOrSolved", c("MarxanSolved", "MarxanUnsolved"))



#' Create new MarxanSolved object
#'
#' This function creates a MarxanSolved object using a "MarxanUnsolved" object and a "MarxanResults" object.
#'
#' @param unsolved "MarxanUnsolved" object.
#' @param results "MarxanResults" object.
#' @return "MarxanUnsolved" object.
#' @export
#' @seealso \code{\link{MarxanUnsolved-class}}, \code{\link{MarxanResults-class}}
MarxanSolved<-function(MarxanUnsolved, MarxanResults) {
	return(new("MarxanUnsolved", opts=unsolved@opts, data=unsolved@data, reuslts=results))
}

#' @describeIn solve
#' @export
solve.MarxanSolved<-function(x, wd=tempdir(), seeds=sample.int(n=10000L, size=x@opts@NCORES), clean=TRUE, force_reset=FALSE) {
	if (!force_reset)
		stop("This object already has Marxan solutions. Use force_reset=TRUE to force recalculation of solutions.")
	return(solve(MarxanUnsolved(opts=x@opts,data=x@data), wd, seeds, clean))
}

#' @describeIn selection
#' @export
selection.MarxanSolved<-function(x, y="best") {
	return(selection.MarxanResults(x@results, y))
}

#' @describeIn score
#' @export
score.MarxanSolved<-function(x, y="best") {
	return(score.MarxanResults(x@results, y))
}

#' @describeIn summary
#' @export
summary.MarxanSolved<-function(x) {
	return(summary.MarxanResults(x@results))
}

#' @describeIn print
#' @export
print.MarxanSolved<-function(x) {
	cat("MarxanSolved object.\n")
	print.MarxanOpts(x@opts, FALSE)
	print.MarxanData(x@data, FALSE)
	print.MarxanResults(x@results, FALSE)
}

#' @export
# setMethod(
	# 'show',
	# 'MarxanSolved',
	# function(x, ...)
		# print.MarxanSolved(x, ...)
# )

#' @export
#' @describeIn names
names.MarxanSolved<-function(x) {
	return(names(x@data))
}

#' @describeIn log
#' @export
log.MarxanSolved<-function(x) {
	log.MarxanResults(x@results)
}

#' @describeIn amountheld
#' @export
amountheld.MarxanSolved<-function(x, y=NULL) {
	return(amountheld.MarxanResults(x@results, y))
}

#' @describeIn occheld
#' @export
occheld.MarxanSolved<-function(x, y=NULL) {
	return(occheld.MarxanResults(x@results, y))
}

#' @describeIn targetsmet
#' @export
targetsmet.MarxanSolved<-function(x, y=NULL) {
	return(targetsmet.MarxanResults(x@results, y))
}

#' @describeIn pca
#' @export
pca.MarxanSolved<-function(x, var='selections', ..., force_reset=FALSE) {
	return(dist.MarxanResults(x@results, var, ..., force_reset=force_reset))	
}

#' @describeIn dist
#' @export
dist.MarxanSolved<-function(x, var='selections', method="bray", force_reset=FALSE) {
	return(dist.MarxanResults(x@results, var, method, force_reset=force_reset))
}

#' @describeIn mds
#' @export
mds.MarxanSolved<-function(x, var='selections', method="bray", ..., force_reset=FALSE) {
	return(mds.MarxanResults(x@results, var, method, ..., force_reset=force_reset))
}

#' @describeIn hclust
#' @export
hclust.MarxanSolved<-function(x, type='mds', var='selections', ..., force_reset=FALSE) {
	return(mds.MarxanResults(x@results, type='mds', var='selections', ..., force_reset=force_reset))
}

#' @describeIn ordiplot
#' @export
ordiplot.MarxanSolved<-function(x, type='mds', var='selections', nbest=1, ..., force_reset=FALSE) {
	return(ordiplot.MarxanResults(x@results, type, var, nbest, ..., force_reset=force_reset))
}

#' @describeIn dendrogram
#' @export
dendrogram.MarxanSolved<-function(x, type='mds', var='selections', nbest=1, ..., force_reset=FALSE) {
	return(dendrogram.MarxanResults(x@results, type, var, nbest, ..., force_reset=force_reset))
}

#' @describeIn dotchart
#' @export
dotchart.MarxanSolved<-function(x, var="score", nbest=1) {
	dotchart.MarxanResults(x@results, var, nbest)
}

#' @describeIn basemap
#' @export
basemap.MarxanSolved<-function(x, basemap="none", alpha=1, grayscale=FALSE, xzoom=c(1,1), yzoom=c(1,1), force_reset=FALSE) {
	return(basemap.MarxanData(x@data, basemap, alpha, grayscale, xzoom, yzoom, force_reset))
}

#' @describeIn update
#' @export
update.MarxanSolved<-function(x, formula, evaluate=TRUE, force_reset=TRUE) {
	return(update.MarxanUnsolved(MarxanUnsolved(x@opts, x@data), formula, evaluate, force_reset))
}

#' @describeIn plot
#' @export
setMethod(
	"plot", 
	signature(x="MarxanSolved", y="character"),
	function(x, y, basemap="none", colramp="BuGn", alpha=1, grayscale=FALSE, xzoom=c(1,1), yzoom=c(1,1), force_reset=FALSE) {
		plot(x@data, y, basemap, colramp, alpha, grayscale, xzoom, yzoom, force_reset=force_reset)
	}
)

#' @describeIn plot
#' @export
setMethod(
	"plot",
	signature(x="MarxanSolved",y="numeric"),
	function(x, y, basemap="none", colramp="Reds", lockedincol="#000000FF", lockedoutcol="#D7D7D7FF", alpha=1, grayscale=FALSE, xzoom=c(1,1), yzoom=c(1,1), force_reset=FALSE) {
		# check for issues
		stopifnot(alpha<=1 & alpha>=0)
		match.arg(colramp, rownames(brewer.pal.info))
		stopifnot(inherits(x@data@polygons, "PolySet"))	
		# get basemap data
		if (basemap!="none")
			basemap<-basemap.MarxanData(x@data, basemap, alpha, grayscale, xzoom, yzoom, force_reset)
		# main processing
		if (y==0)
			y<-x@results@best
		values<-x@results@selections[y,]
		cols<-character(length(values))
		cols[which(x@data@pu$status==2)]<-lockedincol
		cols[which(x@data@pu$status==3)]<-lockedoutcol
		cols[which(x@data@pu$status<2)]<-brewerCols(values[which(x@data@pu$status<2)])
		prettyGeoplot(
			x@data@polygons,
			cols,
			basemap,
			ifelse(y==x@results@best, paste0("Best Solution (",y,")"), paste0("Solution (",y,")")),
			categoricalLegend(c(lockedoutcol,brewerCols(c(0,1),colramp),lockedincol),c("Locked Out", "Not Selected", "Selected", "Locked In"))
		)
	}
)

#' @describeIn plot
#' @export
setMethod(
	"plot",
	signature(x="MarxanSolved",y="missing"),
	function(x, y, basemap="none", colramp="PuBu", lockedincol="#000000FF", lockedoutcol="#D7D7D7FF", alpha=1, grayscale=FALSE, xzoom=c(1,1), yzoom=c(1,1), force_reset=FALSE) {
		# check for issues
		stopifnot(alpha<=1 & alpha>=0)
		match.arg(colramp, rownames(brewer.pal.info))
		stopifnot(inherits(x@data@polygons, "PolySet"))
		# get basemap data
		if (basemap!="none")
			basemap<-basemap.MarxanData(x@data, basemap, alpha, grayscale, xzoom, yzoom, force_reset)	
		# main processing
		if (force_reset || !is.cached(x, "selectionfreqs"))
			cache(x, "selectionfreqs", colMeans(x@results@selections))
		values<-cache(x,"selectionfreqs")[which(x@data@pu$status<2)]
		cols<-brewerCols(rescale(cache(x,"selectionfreqs"),from=range(values),to=c(0,1)))
		cols[which(x@data@pu$status==2)]<-lockedincol
		cols[which(x@data@pu$status==3)]<-lockedoutcol
		prettyGeoplot(
			x@data@polygons,
			cols,
			basemap,
			"Selection Frequencies",
			continuousLegend(values,colramp)	
		)
	}
)

#' @describeIn plot
#' @export
setMethod(
	"plot",
	signature(x="MarxanSolved",y="MarxanSolved"),
	function(x, y, i=NULL, j=i, basemap="none", colramp="Spectral", xlockedincol="#000000FF", xlockedoutcol="#D7D7D7FF", ylockedincol="#FFFFFFFF", ylockedoutcol="#D7D7D7FF", alpha=1, grayscale=FALSE, xzoom=c(1,1), yzoom=c(1,1), force_reset=FALSE) {
		# check for issues
		stopifnot(alpha<=1 & alpha>=0)
		match.arg(colramp, rownames(brewer.pal.info))
		stopifnot(inherits(x@data@polygons, "PolySet"))
		# get basemap data
		if (basemap!="none")
			basemap<-basemap.MarxanData(x@data, basemap, alpha, grayscale, xzoom, yzoom, force_reset)	
		# main processing
		cols<-character(nrow(x@data@pu))
		cols[which(x@data@pu$status==2)]<-xlockedincol
		cols[which(x@data@pu$status==2)]<-ylockedincol
		cols[which(x@data@pu$status==3)]<-xlockedoutcol
		cols[which(y@data@pu$status==3)]<-ylockedoutcol
		if (is.null(i) || is.null(j)) {
			if (force_reset || !is.cached(x, "selectionfreqs"))
				cache(x, "selectionfreqs", colMeans(x@results@selections))
			if (force_reset || !is.cached(y, "selectionfreqs"))
				cache(y, "selectionfreqs", colMeans(y@results@selections))
			values<-order(cache(x, "selectionfreqs")[which(nchar(cols)==0)] - cache(y, "selectionfreqs")[which(nchar(cols)==0)])
			cols[which(nchar(cols)==0)]<-brewer.pal(rescale(order(values),to=c(0,1)))
			prettyGeoplot(
				x@data@polygons,
				cols,
				basemap,
				"Difference between planning unit selection frequencies (perentile)",
				fun<-continuousLegend(
					seq(0,1,0.1),
					colramp
				)
			)
		} else {
			if (i==0)
				i<-x@results@best
			if (j==0)
				j<-y@results@best
			cols[which(nchar(cols)==0)]<-brewer.pal(rescale(x@results@selections[i,which(nchar(cols)==0)]-y@results@selections[j,which(nchar(cols)==0)],from=c(-1,1),to=c(0,1)))
			prettyGeoplot(
				x@data@polygons,
				cols,
				basemap,
				paste0("Difference in  selections for solution ",i,ifelse(i==x@results@best, " (best)", ""), " and ",j, ifelse(j==y@results@best, " (best)", "")),
				categoricalLegend(
					c( brewerCols(seq(0,1,0.25),colramp),xlockedincol,ylockedincol,xlockedoutcol,ylockedoutcol),
					c("x=1 & y=0", "x=1 & y=0", "x=0 & y=0", "x=0 & y=1", "x locked in", "y locked in", "x locked out", "y locked out")
				)
			)
		}
	}
)


#' describeIn is.comparable
#' @export
setMethod(
	f="is.comparable",
	signature(x="MarxanUnsolvedOrSolved", y="MarxanUnsolvedOrSolved"),
	function(x,y) {
		return(is.comparable(x@data, y@data))
	}
)

#' describeIn is.comparable
#' @export
setMethod(
	f="is.comparable",
	signature(x="MarxanData", y="MarxanUnsolvedOrSolved"),
	function(x,y) {
		return(is.comparable(x, y@data))
	}
)

#' describeIn is.comparable
#' @export
setMethod(
	f="is.comparable",
	signature(x="MarxanUnsolvedOrSolved", y="MarxanData"),
	function(x,y) {
		return(is.comparable(x@data, y))
	}
)