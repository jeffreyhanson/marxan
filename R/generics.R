

#' Solve Marxan object
#'
#' This function executes Marxan using the input parameter and data stored in a "MarxanUsolved" object, 
#' and returns a "MarxanSolved" object with outputs in it.
#'
#' @param x "MarxanUnsolved" or "MarxanSolved" object
#' @param wd "character" file path to a working directory, this is a temporary directory by default to avoid pollution.
#' @param seeds "integer" vector of seeds to initialise Marxan 's random number generator
#' @param clean "logical" delete files once processing completed?
#' @param force_reset "logical" should Marxan solutions be recalculated even if "MarxanSolved" object supplied?
#' @return Marxan object
#' @note This function is used to solve a MarxanUnsolved object that has all of its inputs generated. The marxan function (without lower case 'm') provides a more general interface for generating inputs and outputs for Marxan.
solve<-function(x, ...) {UseMethod('solve')}

#' Extract Solution Selection
#'
#' Extract selections for a given solution from a "MarxanResults or "MarxanSolved" object.
#'
#' @param x "MarxanResults or "MarxanSolved" object
#' @param y 'best' to return selection for best solution, 'all' to return all solutions, or "integer" to return y'th solution
#' @return "matrix" or "numeric" vector with planning units statuses depending on arguments
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanSolved-class}}, \code{\link{marxan}}
selection<-function(x, ...) {UseMethod('selection')}

#' Extract Solution Score
#'
#' Extract solution score from "MarxanResults" or "MarxanSolved" object.
#'
#' @param x "MarxanResults or "MarxanSolved" object
#' @param y 'best' to return best solution score, 'all' to return all scores, or "integer" to return score for y'th solution
#' @return "matrix" or "numeric" vector with solution scores depending on arguments
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanSolved-class}}, \code{\link{marxan}}
score<-function(x, ...) {UseMethod('score')}

#' Summary
#'
#' This function returns the summary table output by Marxan.
#'
#' @param x "MarxanSolved" or "MarxanResults" object
#' 
#' @return "data.frame" with solution information
#' @seealso \code{link{MarxanSolved-class}}, \code{link{MarxanResults}}, \code{\link{marxan}}
summary<-function(x, ...) {UseMethod('summary')}

#' Print
#'
#' This function returns information about a Marxan object.
#'
#' @param x any "MarxanOpts", "MarxanData", "MarxanResults", "MarxanUnsolved", or "MarxanSolved" object
#' 
#' @seealso \code{link{MarxanOpts-class}}, \code{link{MarxanData-class}}, \code{link{MarxanResults-class}}, \code{link{MarxanUnsolved-class}}, \code{link{MarxanSolved-class}}, 
print<-function(x, ...) {UseMethod('print')}

#' Log
#'
#' This function returns the log file associated with running Marxan.
#'
#' @param x "MarxanResults" or "MarxanSolved" object
#' 
#' @seealso \code{link{MarxanResults-class}}, \code{link{MarxanSolved}}, \code{\link{marxan}}
log<-function(x, ...) {UseMethod('log')}


#' Amount Held
#'
#' This function returns the amount held for all features in a solution.
#'
#' @param x "MarxanResults" or "MarxanSolved" object
#' @return "matrix" or "numeric" vector with solution scores depending on arguments
#' @seealso \code{link{MarxanResults-class}}, \code{link{MarxanSolved}}, \code{\link{marxan}}
amountHeld<-function(x, ...) {UseMethod('amountHeld')}


#' Occurrence Held
#'
#' This function returns the number of occurrences held for all features in a solution.
#'
#' @param x "MarxanResults" or "MarxanSolved" object
#
#' @return "matrix" or "numeric" vector with solution scores depending on arguments
#' @seealso \code{link{MarxanResults-class}}, \code{link{MarxanSolved}}, \code{\link{marxan}}
occHeld<-function(x, ...) {UseMethod('occHeld')}


#' Compare Marxan Objects
#'
#' This function checks objects to see if they share the same input data.
#'
#' @param x "MarxanData", "MarxanUnsolved", or "MarxanSolved" objects
#' @return "logical" are the objects based on the same data?
#' @seealso \code{link{MarxanData-class}}, \code{link{MarxanUnsolved-class}}, \code{link{MarxanSolved-class}}
setGeneric("is.comparable", function(x, y) standardGeneric("is.comparable"))
setMethod(
	f="is.comparable",
	signature(x="MarxanData", y="MarxanData"),
	function(x,y) {
		return(
			identical(x@pu$id, y@pu$id) &
			identical(x@species$id, y@species$id) &
			identical(x@species$name, y@species$name) &
			identical(x@polygons, y@polygons) &
			identical(x@boundary$id1, y@boundary$id1) &
			identical(x@boundary$id2, y@boundary$id2)
		)
	}
)
setMethod(
	f="is.comparable",
	signature(x="Marxan", y="Marxan"),
	function(x,y) {
		return(is.comparable(x@data, y@data))
	}
)
setMethod(
	f="is.comparable",
	signature(x="MarxanData", y="Marxan"),
	function(x,y) {
		return(is.comparable(x, y@data))
	}
)
setMethod(
	f="is.comparable",
	signature(x="Marxan", y="MarxanData"),
	function(x,y) {
		return(is.comparable(x@data, y))
	}
)

#' Principle Components Analysis for Marxan Solutions
#'
#' This function runs an principle components analysis on Marxan solutions using various characteristics.
#' Results cached to permit rapid display for plotting functions.
#'
#' @param x "MarxanSolved" object
#' @param var "character" should solutions be compared based on selections ('solutions'), or the amount held ('amountheld'), number of occurances ('occheld'), or whether the targets have been met for each species ('targetsmet')?
#' @param ... arguments to \code{\link[stats]{prcomp}}
#' @param force_reset "logical" should analysis be run even if it is stored in the cache?
#' @return "prcomp" object with Euclidean distances between rotated data for ordination plots
#' @seealso \code{link{MarxanSolved-class}}, \code{link{marxan}}, \code{link{ordiplot}}, \code{link{dendrogram}}
pca<-function(x) {UseMethod("pca")}


#' Dissimilarity Matrix for Marxan Solutions
#'
#' This function calculates a dissimilty matrix for Marxan solutions using various characteristics.
#' Results cached to permit rapid display for plotting functions.
#'
#' @param x "MarxanSolved" object
#' @param var "character" should solutions be compared based on selections ('selections'), or the amount held ('amountheld'), number of occurances ('occheld'), or whether the targets have been met for each species ('targetsmet')?
#' @param method "character" name of distance metric to use for calculating distance (see \code{link[vegan]{vegdist}})
#' @param force_reset "logical" should analysis be run even if it is stored in the cache?
#' @return "dist"  with dissimilarity indices
#' @seealso \code{link{MarxanSolved-class}}, \code{link{marxan}}, \code{link{ordiplot}}, \code{link{dendrogram}}
dist<-function(x) {UseMethod("dist")}
dist.default<-stats::dist

#' Metric Dimensional Scaling for Marxan Solutions
#'
#' This function runs an metric dimensional scaling (using \code{\link[vegan]{monoMDS}}) on Marxan solutions using various characteristics.
#' Results cached to permit rapid display for plotting functions.
#'
#' @param x "MarxanSolved" object
#' @param var "character" should solutions be compared based on selections ('solutions'), or the amount held ('amountheld'), number of occurances ('occheld'), or whether the targets have been met for each species ('targetsmet')?
#' @param method "character" name of distance metric to use for calculating distance (see \code{link[vegan]{vegdist}})
#' @param ... additional arguments to \code{\link[vegan]{monoMDS}}
#' @param force_reset "logical" should analysis be run even if it is stored in the cache?
#' @return "monoMDS" object
#' @seealso \code{link{MarxanSolved-class}}, \code{link{marxan}}, \code{link{ordiplot}}, \code{link{dendrogram}}
mds=function(x, ...) UseMethod("mds")

#' Hierarchical Clustering for Marxan Solutions
#'
#' This function runs an hierarchical clustering on (using \code{\link[fastcluster]{hclust}}) on Marxan solutions using various characteristics.
#' Results cached to permit rapid display for plotting functions.
#'
#' @param x "MarxanSolved" object
#' @param type "character" use metric dimensional scaling ('mds'), or principle components analysis ('pca'), or distance matrix('dist') for analysis?
#' @param  "character" should solutions be compared based on selections ('solutions'), or the amount held ('amountheld'), number of occurances ('occheld'), or whether the targets have been met for each species ('targetsmet')?
#' @param method "character" name of distance metric to use for calculating distance (see \code{link[vegan]{vegdist}})
#' @param ... arguments to \code{\link[vegan]{monoMDS}}, or \code{\link[stats]{prcomp}}
#' @param force_reset "logical" should analysis be run even if it is stored in the cache?
#' @return "hclust" object with an extra element named "phylo" that stores a "phylo" representation for pretty plotting
#' @seealso \code{link{MarxanSolved-class}}, \code{link{marxan}}, \code{link{ordiplot}}, \code{link{dendrogram}}
hclust=function(x, ...) UseMethod("mds")

#' Ordination Plot of Marxan Solutions
#'
#' This function makes an ordination plot to visualise the differences between solutions using dimension reduction techniques (ie. NMDS or PCA).
#' Numbers indicate solution ids. The size of the number on your screen indicates relative solution quality ('score').
#' Distance matrices and results from multivariate analyses are cached to permit rapid display.
#'
#' @param x "MarxanSolved" object
#' @param type "character" use 'mds' or 'pca' for analysis?
#' @param var "character" should solutions be compared based on selections ('selections'), or the amount held ('amountheld'), number of occurances ('occheld'), or whether the targets have been met for each species ('targetsmet')?
#' @param nbest "integer" color the n best solutions in "red"
#' @param ... arguments to \code{\link[vegan]{monoMDS}}, \code{\link[stats]{prcomp}}, \code{\link{mds.MarxanResults}}, and \code{\link{pca.MarxanResults}}. For instance, to use euclidean distances, use dist='euclidean'
#' @param force_reset "logical" should analysis be rerun even if it is stored in the cache?
#' @return "prcomp" or "monoMDS" object
#' @seealso \code{link{MarxanSolved-class}}, \code{link{marxan}}
ordiplot=function(x, ...) UseMethod("ordiplot")

#' Dendrogram of Marxan Solutions
#'
#' This function makes a dendrogram to visualise differences between Marxan solutions using hierarchical clustering using various characteristics. 
#' Clustering can be applied to distances between raw data, or to results from dimensional reduction analysis (ie. MDS and PCA).
#' Distance matrices and results from multivariate analyses are cached to permit rapid display.
#'
#' @param x "MarxanSolved" object
#' @param type "character" use 'nmds' or 'pca' or 'dist' for analysis?
#' @param var "character" should solutions be compared based on selections ('selections'), or the amount held ('amountheld'), number of occurances ('occheld'), or whether the targets have been met for each species ('targetsmet')?
#' @param nbest "integer" color the n best solutions in "red"
#' @param ... arguments to \code{\link[vegan]{monoMDS}}, \code{\link[stats]{prcomp}}, \code{\link{nmds.MarxanResults}}, and \code{\link{pca.MarxanResults}}
#' @param force_reset "logical" should analysis be rerun even if it is stored in the cache?
#' @return "prcomp" or "monoMDS" object
#' @seealso \code{link{MarxanResults-class}}, \code{link{MarxanSolved-class}}, \code{link{marxan}}, 
dendrogram=function(x, ...) UseMethod("dendrogram")

#' Dot Chart of Marxan Solutions
#'
#' This function makes a dot chart to visualise differences between Marxan solutions using summary variables. Size of dots indicate overall solution quality (ie. 'score').
#' Supported summary variables are:
#' \tabular{rrr} {
#' 	short name \tab full name \tab Description
#' 	'score' \tab 'Score' \tab quality of solution
#' 	'cost' \tab 'Cost' \tab total cost of solution
#' 	'npu' \tab 'Planning_Units' \tab number of planning units selected in prioritisation
#' 	'con' \tab 'Connectivity' \tab sum boundary length of all edges in selected planning units' that have neighbours
#' 	'confrac' \tab 'Connectivity_In_Fraction' \tab connectivity efficiency relative to total boundary length
#' 	'conin' \tab 'Connectivity_In' \tab sum boundary length of edges belonging to selected planning units' that have neighbours
#' 	'conout' \tab 'Connectivity_Out' \tab sum boundary length of edges belonging to selected planning units' that have neighbours
#'  'penalty' \tab 'Penalty' \tab total species penalty 
#'  'shortfall' \tab 'Shortfall' \tab total shortfall for species targets 
#'  'mv' \tab 'Missing_Values' \tab number of species that do not have their targets met
#' }
#' 
#' @param x "MarxanResults" or "MarxanSolved"
#' @param var "character" what variable should be used to compare solutions?
#' @param nbest "integer" color the n best solutions in "red"
#' @seealso \code{link{MarxanResults-class}}, \code{link{MarxanSolved-class}}, \code{link{marxan}}
dotchart=function(x, ...) UseMethod("dotchart")



#' Basemap 
#'
#' This function retrieves google map data for planning units. The google map data is cached to provide fast plotting capabilities.
#'
#' @param basemap "character" name of basemap to display. Valid names are "roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid".
#' @param grayscale "logical" should basemap be gray scale?
#' @param xzoom "numeric" zoom the geoplot in or out along the x-axis by this percent.
#' @param yzoom "numeric" zoom the geoplot in or out along the y-axis by this percent.
#' @param force_reset "logical" ignore data in cache? Setting this as ignore will make function slower but may avoid bugs in cache system.
#' @return "list" with google map data
#' @seealso \cite{\link[RgoogleMaps]{GetMap.bbox}}, \cite{\link{plot}}
basemap<-function(x, ...) {UseMethod("basemap")}


#' Plot Marxan Solutions
#'
#' This function makes a geoplot displaying Marxan solutions.
#'
#' @param x "MarxanData", "MarxanUnsolved", "MarxanSolved" object.
#' @param y See below for details:
#'	\itemize{ 
#' 	\item if "missing": function plots the selection frequency of planning units for all solutions.
#'	\item if "integer": function plots the selection for the n'th solution, use 0 to plot the best solution.
#'	\item if "character": set to name of species to display its density, or "all" for sum density of all species, or "rich" to get species richness in each planning unit.
#'	\item if "MarxanSolved" plots the difference in solutions:
#'	\item \itemize{
#'		\item if i is "NULL": differences in selection frequencies are plotted.
#'		\item if i is "integer" and j is "integer", plots differences selection status for solution i in x, and solution j in y. Set i or j to 0 to refer to the best solution in x or y respectively.
#'		}
#' }
#' @param colramp "character" name of colour palette (see \code{\link[RColorBrewer]{brewer.pal.info}}).
#' @param alpha "numeric" alpha value
#' @param basemap "character" name of basemap to display. Valid names are "roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid".
#' @param grayscale "logical" should basemap be gray scale?
#' @param xzoom "numeric" zoom the geoplot in or out along the x-axis by this percent.
#' @param yzoom "numeric" zoom the geoplot in or out along the y-axis by this percent.
#' @param force_reset "logical" ignore data in cache? Setting this as ignore will make function slower but may avoid bugs in cache system.
#' @note This function will return an error if spatial polygons were not supplied during the construction of the Marxan object. Furthermore, "MarxanData" and "MarxanUnsolved" objects can only be used to display species densities.
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanSolved-class}}, \code{\link{basemap}} \code{\link{marxan}}, \code{\link{dendrogram}}, \code{\link{dotchart}}, \code{\link{ordiplot}}
setGeneric("plot", function(x,y, ...) standardGeneric("plot")
setMethod(
	"plot", 
	signature(x="MarxanData", y="character"),
	function(x, y, basemap="none", colramp="BuGn", alpha=1, grayscale=FALSE, xzoom=c(1,1), yzoom=c(1,1), force_reset=FALSE) {
		# init
		stopifnot(alpha<=1 & alpha>=0)
		match.arg(y, c("sum", "rich", unique(x@y$name)))
		match.arg(colramp, rownames(brewer.pal.info))
		stopifnot(inherits(x@polygons, "PolyData"))
		# get basemap data
		if (basemap!="none")
			basemap<-basemap.MarxanData(x@data, basemap, alpha, grayscale, xzoom, yzoom, force_reset)
		# get colours for planning units
		values<-numeric(nrow(x@pu))
		if (y=="all") {
			sub<-data.table(x@puvspecies)
			sub<-sub[,sum(amount),by=pu]
			values[sub$pu]<-sub$V1
		} else if (y=="rich") {
			sub<-data.table(x@puvspecies)
			sub<-sub[,.N,by=pu]
			values[sub$pu]<-sub$V1
		} else {
			if (is.character(y))
				y<-x@species$id[which(x@species$name==y)]
			sub<-x@puvspecies[which(x@puvspecies==y),]
			values[sub$pu]<-sub$amount
		}
		# plot data
		switch(y,
			"all"={
				y<-"Total Species Occurrence"
			},
			"rich"={
				y<-"Species Richness"
			},
			"default"={
				y<-x@species$name[which(x@species$id==y)]
			}
		)
		# make legend
		prettyGeoplot(x@polygons, col=brewerCols(rescale(values, to=c(0,1))), basemap=basemap, main=y, legend=continuousLegend(values, colramp))
		return(invisible())
	}
)
setMethod(
	"plot", 
	signature(x="MarxanUnsolved", y="character"),
	function(x, y, basemap="none", colramp="BuGn", alpha=1, grayscale=FALSE, xzoom=c(1,1), yzoom=c(1,1), force_reset=FALSE) {
		plot(x@data, y, basemap, colramp, alpha, grayscale, xzoom, yzoom, force_reset=force_reset)
	}
)
setMethod(
	"plot", 
	signature(x="MarxanSolved", y="character"),
	function(x, y, basemap="none", colramp="BuGn", alpha=1, grayscale=FALSE, xzoom=c(1,1), yzoom=c(1,1), force_reset=FALSE) {
		plot(x@data, y, basemap, colramp, alpha, grayscale, xzoom, yzoom, force_reset=force_reset)
	}
)
setMethod(
	"plot",
	signature(x="MarxanSolved",y="numeric"),
	function(x, y, basemap="none", colramp="Reds", lockedincol="#000000FF", lockedoutcol="#D7D7D7FF", alpha=1, grayscale=FALSE, xzoom=c(1,1), yzoom=c(1,1), force_reset=FALSE) {
		# check for issues
		stopifnot(alpha<=1 & alpha>=0)
		match.arg(colramp, rownames(brewer.pal.info))
		stopifnot(inherits(x@data@polygons, "PolyData"))	
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
			col<-cols,
			basemap=basemap,
			main<-ifelse(y==x@results@best, paste0("Best Solution (",y,")"), paste0("Solution (",y,")"))
			fun<-categoricalLegend(c(lockedoutcol,brewerCols(c(0,1),colramp),lockedincol),c("Locked Out", "Not Selected", "Selected", "Locked In"))
		)
	}
)
setMethod(
	"plot",
	signature(x="MarxanSolved",y="missing"),
	function(x, y, basemap="none", colramp="PuBu", lockedincol="#000000FF", lockedoutcol="#D7D7D7FF", alpha=1, grayscale=FALSE, xzoom=c(1,1), yzoom=c(1,1), force_reset=FALSE) {
		# check for issues
		stopifnot(alpha<=1 & alpha>=0)
		match.arg(colramp, rownames(brewer.pal.info))
		stopifnot(inherits(x@data@polygons, "PolyData"))
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
			col=cols,
			basemap=basemap,
			main="Selection Frequencies",
			fun=continuousLegend(values,colramp)	
		)
	}
)
setMethod(
	"plot",
	signature(x="MarxanSolved",y="MarxanSolved"),
	function(x, y, i=NULL, j=i basemap="none", colramp="Spectral", xlockedincol="#000000FF", xlockedoutcol="#D7D7D7FF", ylockedincol="#FFFFFFFF", ylockedoutcol="#D7D7D7FF", alpha=1, grayscale=FALSE, xzoom=c(1,1), yzoom=c(1,1), force_reset=FALSE) {
		# check for issues
		stopifnot(alpha<=1 & alpha>=0)
		match.arg(colramp, rownames(brewer.pal.info))
		stopifnot(inherits(x@data@polygons, "PolyData"))
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
				col<-cols,
				basemap=basemap,
				main<-"Difference between planning unit selection frequencies (perentile)",
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
				col<-cols,
				basemap=basemap,
				main<-paste0("Difference in  selections for solution ",i,ifelse(i==x@results@best, " (best)", ""), " and ",j, ifelse(j==y@results@best, " (best)", ""))
				fun<-categoricalLegend(
					c( brewerCols(seq(0,1,0.25),colramp),xlockedincol,ylockedincol,xlockedoutcol,ylockedoutcol),
					c("x=1 & y=0", "x=1 & y=0", "x=0 & y=0", "x=0 & y=1", "x locked in", "y locked in", "x locked out", "y locked out")
				)
			)
		}
	}
)

#' Update Marxan Inputs
#'
#' This function lets you update Marxan objects to obtain a new object with altered inputs. This is particularly useful if you want to rerun a Marxan
#' analysis but after changing a few parameters. For example, you might decide that you need to rerun a Marxan analysis with a higher number of iterations, or with a different
#' boundary length modifier. In effect, this lets you take an existing "MarxanUnsolved"/"MarxanSolved" object, change specific inputs, and run a Marxan analysis using the tweaked inputs.
#'
#' @param x "MarxanOpts", "MarxanData", "MarxanUnsolved", or "MarxanSolved" object.
#' @param formula "formula" with update commands (see examples).
#' @param evaluate "logical" should solutions be generated for the new object?
#' @return "MarxanSolved" or "MarxanUnsolved" object depending on solve argument.
#' @seealso \code{link{MarxanSolved-class}}, \code{link{MarxanUnsolved-class}}, \code{link{marxan}}
update<-function(x, formula, evaluate) UseMethod("update")


