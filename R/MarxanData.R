#' @include RcppExports.R marxan-internal.R misc.R generics.R calcBoundaryData.R calcPuVsSpeciesData.R
NULL

#' MarxanData: An S4 class to represent Marxan input data
#'
#' This class is used to store Marxan input data.
#'
#' @slot polygons "PolySet" planning unit spatial data
#' @slot pu "data.frame" planning unit data; with "id", "cost", "status" columns
#' @slot species "data.frame" with species data; with "id", "target", "spf", and "name" columns
#' @slot puvspecies "data.frame" with data on species density in each planning unit; with "species", "pu", and "target" columns
#' @slot boundary "data.frame" with data on the shared boundary length of planning; with "id1", "id2", and "amount" columns 
#' @slot .cache "environment" used to store processing calculations
#' @export
setClass("MarxanData",
	representation(
		polygons="PolySetOrNULL",
		pu="data.frame",
		species="data.frame",
		puvspecies="data.frame",
		boundary="data.frame",
		.cache="environment"
	),
	validity=function(object) {
		### check column names of inputs		
		# pu
		if (!identical(names(object@pu), c("id","cost","status"))) {
			if (all(c("id","cost","status") %in% names(pu))) {
				object@pu<-object@speciesDataFrame[,c("id","cost","status"),drop=FALSE]
			} else {
				stop("pu must have column names: 'id', 'cost', and 'status'.")
			}
		}
		if (!is.finite.numeric.wholenumber(pu$id))
			stop("pu$id must be finite integer.")
		if (!is.finite.numeric(pu$cost))
			stop("pu$cost must be finite numeric.")
		if (!is.finite.numeric.wholenumber(pu$status))
			stop("pu$status must be finite integer.")
		if (!asym.setequal(pu$status, 0:3))
			stop("pu$status must have values as: 0, 1, 2, or 3.")

		# species
		if (!identical(names(object@species), c("id","target","spf","name"))) {
			if (all(c("id","target","spf","name") %in% names(species))) {
				object@species<-object@species[,c("id","target","spf","name"),drop=FALSE]
			} else {
				stop("species must have column names: 'id', 'target', 'spf', and 'name'.")
			}
		}
		if (!is.finite.numeric.wholenumber(object@species$id))
			stop("species$id must be finite integer.")
		if (!is.finite.numeric(object@species$target))
			stop("species$target must be finite numeric.")
		if (!is.finite.numeric(object@species$spf))
			stop("species$spf must be numeric.")
		if (!is.valid.character(object@species$name))
			stop("species$name must be character and not NA.")

		# puvspecies
		if (!identical(names(object@puvspecies), c("species","pu","amount"))) {
			if (all(c("species","pu","amount") %in% names(puvspecies))) {
				object@puvspecies=object@puvspecies[,c("species","pu","amount"),drop=FALSE]
			} else {
				stop("puvspecies must have column names: 'species', 'pu', and 'amount'.")
			}
		}
		if (!is.finite.numeric.wholenumber(object@puvspecies$species))
			stop("puvspecies$species must be finite integer.")
		if (!is.finite.numeric.wholenumber(object@puvspecies$pu))
			stop("puvspecies$pu must be finite integer.")
		if (!is.finite.numeric(object@puvspecies$amount))
			stop("puvspecies$amount must be finite numeric.")
		
		# boundary
		if (!identical(names(object@boundary), c("id1","id2","boundary"))) {
			if (all(c("id1","id2","boundary") %in% names(boundary))) {
				object@boundary<-object@boundary[,c("id1","id2","boundary"),drop=FALSE]
			} else {
				stop("boundary must have column names: 'id1', 'id2', and 'boundary'.")
			}
		}
		if (!is.finite.numeric.wholenumber(object@boundary$id1))
			stop("boundary$id1 must be finite integer.")
		if (!is.finite.numeric.wholenumber(object@boundary$id2))
			stop("boundary$id2 must be finite integer.")
		if (!is.finite.numeric.wholenumber(object@boundary$boundary))
			stop("boundary$boundary must be finite numeric.")
		
		# check that planning unit inputs are valid
		if (!asym.setequal(object@boundary$id1, object@pu$id))
			stop("boundary$id1 has numbers not present in pu$id.")
		if (!asym.setequal(object@boundary$id2, object@pu$id))
			stop("boundary$id2 has numbers not present in pu$id.")
		if (!asym.setequal(setdiff(object@puvspecies$pu, object@pu$id)))
			stop("puvspecies$pu has some values not in pu$id.")
		
		# check that species inputs are valid
		if (!asm.setequal(object@puvspecies$species, object@pu$id))
			stop("puvspecies$pu has some values not in pu$id.")	
			
	}
)
setMethod(
	"initialize", 
	"MarxanData", 
	function(.Object, .cache=new.env()) {
		callNextMethod(.Object, .cache=.cache)
	}
)

#' Create new MarxanData object
#'
#' This function creates a MarxanData using pre-processed data.
#'
#' @param pu "data.frame" planning unit data; with columns "id", "cost", and "status" columns
#' @param species species "data.frame" species data; with columns "id", "target", "spf", and "name" columns
#' @param puvspecies puvspecies "data.frame" pu vs. species data; with "species", "pu", and "amount" columns
#' @param boundary boundary "data.frame" with shared boundary data; with "id1", "id2", and "boundary" columns
#' @param polygons "PolySet" with planning unit data or "NULL" if geoplotting capabilities are not required
#' @return MarxanData object
#' @seealso \code{\link{read.MarxanData}}, \code{\link{write.MarxanData}}, \code{\link{format.MarxanData}}, \code{\link{MarxanData-class}}
#' @export
#' @examples
#' data(planningunits, species, speciesRanges)
#' x<-MarxanData(
#' 	planningunits@@data,
#' 	speciesTargets,
#'  calcPuVsSpeciesData(planningunits, species),
#'  calcBoundaryData(planningunits)
#' )
MarxanData=function(pu, species, puvspecies, boundary, polygons=NULL) {
	return(new("MarxanData", pu=pu, species=species, puvspecies=puvspecies, boundary=puvspecies, polygons=polygons))
}

#' Read Input Marxan Data from Disk
#'
#' This function loads Marxan input data from disk.
#'
#' @param path "character" file path for input parameter file
#' @return MarxanData object
#' @export
#' @seealso \code{\link{write.MarxanData}}, \code{\link{format.MarxanData}}, \code{\link{MarxanData}}, \code{\link{MarxanData-class}}
read.MarxanData=function(path, ...) {
	args<-readLines(path)
	dir<-parseArg("INPUTDIR",args)
	return(MarxanData(
		as.data.table(fread(file.path(dir,parseArg("PUNAME")), header=TRUE, sep=",", stringsAsFactors=FALSE, quote=FALSE)),
		as.data.table(fread(file.path(dir,parseArg("SPECNAME")), header=TRUE, sep=",", stringsAsFactors=FALSE, quote=FALSE)),
		as.data.table(fread(file.path(dir,parseArg("PUVSPRNAME")), header=TRUE, sep=",", stringsAsFactors=FALSE, quote=FALSE)),
		as.data.table(fread(file.path(dir,parseArg("BOUNDNAME")), header=TRUE, sep=",", stringsAsFactors=FALSE, quote=FALSE)),
		NULL
	))
}

#' Write Input Marxan Data to Disk
#'
#' This function saves MarxanData objects to disk.
#'
#' @param x "MarxanData" object to save
#' @param dir "character" directory path for location to save data
#' @export
#' @seealso \code{\link{read.MarxanData}}, \code{\link{format.MarxanData}}, \code{\link{MarxanData}}, \code{\link{MarxanData-class}}
write.MarxanData=function(x, dir=getwd(), ...) {
	write.table(x@species,row.names=FALSE,sep=",",quote=FALSE,file.path(dir,"spec.dat"))
	write.table(x@puvspecies,row.names=FALSE,sep=",",quote=FALSE,file.path(dir,"puvspr2.dat"))
	write.table(x@boundary,row.names=FALSE,sep=",",quote=FALSE,file.path(dir,"bound.dat"))
	write.table(x@pu,row.names=FALSE,sep=",",quote=FALSE,file.path(dir,"pu.dat"))
}

#' Format Data for Marxan
#'
#' This function prepares spatially explicit planning unit and species data for Marxan processing.
#' By default, raw data can be provided which will processed for Marxan. However, if particular Marxan input file have
#' already been generated, these can be supplied using the pu, species, puvspecies, or boundary arguments to avoid re-processing
#' these outputs.
#'
#' @param polygons "SpatialPolygons" with planning unit data
#' @param rasters "RasterLayer", "RasterStack", "RasterBrick" with species distribution data
#' @param targets "numeric" or "character" vector with absolute or percent-based targets
#' @param pu "data.frame" planning unit data; with columns "id", "cost", and "status" columns (optional)
#' @param species "data.frame" species data; with columns "id", "target", "spf", and "name" columns (optional)
#' @param puvspecies "data.frame" pu vs. species data; with "species", "pu", and "amount" columns (optional)
#' @param boundary "data.frame" with shared boundary data; with "id1", "id2", and "boundary" columns (optional)
#' @param verbose "logical" Should information on data pre-processing be displayed?
#' @return MarxanData object
#' @seealso \code{\link{read.MarxanData}}, \code{\link{write.MarxanData}}, \code{\link{MarxanData}}, \code{\link{MarxanData-class}}
#' @export
#' @examples
#' data(planningunits, species, speciesRanges)
#' x<-MarxanData(planningunits, speciesRanges, targets="10%")
#' y<-MarxanData(planningunits, speciesRanges, species=species)
#' stopifnot(identical(x,y))
format.MarxanData=function(polygons, rasters, targets="20%", spf=1, pu=NULL, species=NULL, puvspecies=NULL, boundary=NULL, ..., verbose=FALSE) {
	# set polygons
	geoPolygons<-polygons
	if (!identical(geoPolygons@proj4string, CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))) {
		if (verbose)
			cat('Projecting polygons to WGS1984 for rendering.\n')
		geoPolygons<-spTransform(geoPolygons, CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
	}
	geoPolygons<-rcpp_Polygons2PolySet(geoPolygons)
	# set pu
	if (is.null(pu)) {
		if (inherits(polygons, "SpatialPolygonsDataFrame")) {
			if (asym.setdiff(c('id','cost','status'),names(polygons@data)))
				stop("polygons@data does not have columns: 'id', 'cost', 'status'")
			pu<-polygons@data[,c('id','cost','status'),drop=FALSE]
		} else {
			warning("creating default with equal costs and status=0.")
			pu<-data.frame(id=seq_along(polygons@polygons), cost=1, status=0)
		}
	}
	# cache max possible targets
	if (inherits(speciesRasterData, c('RasterLayer','RasterStack', 'RasterBrick'))) {
		if (verbose)
			cat("Caching raster data to for percent-based targets.\n")
		tmp<-cellStats(speciesRasterData,'sum')
		names(tmp)<-speciesDataFrame$name
		cache(x, 'speciesMaxTargets', tmp)
	}
	# set species
	if (is.null(species)) {
		species<-data.frame(
			id=seq_len(names(rasters)),
			target=speciesTargets,
			spf=spf,
			name=names(rasters)
		)
	}
	# set boundary
	if (is.null(boundary)) {
		if (identical(polygons@proj4string, CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')))
			warning("creating boundary length data from polygons in WGS1984; consider supplying in an object a projected CRS.")
		if (verbose)
			cat("Calculating boundary data.")
		boundary<-calcBoundaryData(rcpp_Polygons2PolySet(polygons@polygons), ...)
	}
	# set puvspecies
	if (is.null(puvspecies)) {
		projPolygons=polygons
		if (!identical(geoPolygons@proj4string, rasters@crs)) {
			if (verbose)
				cat("Projecting polygons to rasters' CRS")
			projPolygons<-spTransform(projPolygons, rasters@crs)
		}
		if (verbose)
			cat("Calculating species density in planning units.")
		puvspecies<-calcPuVsSpeciesData(projPolygons, rasters, ...)
	}
	
	return(MarxanData(pu, species, puvpsecies, boundary, polygons))
}

#' @describeIn print
#' @export
print.MarxanData=function(x, header=TRUE) {
	if (header)
		cat("MarxanData object.\n")
	cat("Number of planning units:",nrow(x@pu),"\n")
	cat("Number of species:",nrow(x@species),"\n")
}

#' @describeIn basemap
#' @export
basemap.MarxanData<-function(x, basemap="none", alpha=1, grayscale=FALSE, xzoom=c(1,1), yzoom=c(1,1), force_reset=FALSE) {
	# fetch data from google or cache
	if (force_reset || !is.cached(x, google)) {
			stop("Marxan object is not associated with spatially explicit data for the planning units.")
		cache(x, deparse(c(basemap, range(x@polygons[["X"]])*xzoom, range(x@polygons[["Y"]])*yzoom, grayscale)), GetMap.bbox(range(x@polygons[["X"]])*xzoom, range(x@polygons[["Y"]])*yzoom, maptype=google, GRAYSCALE=grayscale))
	}
	return(cache(deparse(c(basemap, range(x@polygons[["X"]])*xzoom, range(x@polygons[["Y"]])*yzoom, grayscale))))
}

#' @describeIn update
#' @export
update.MarxanData<-function(x, formula, force_reset=TRUE) {
	if (force_reset)
		x$.cache<-new.env()
	ops<-llply(as.list(attr(terms(formula),"variables"))[-1L], eval)
	findInvalidMarxanOperations(ops)
	ops<-ops[which(laply(ops, inherits, "MarxanDataOperation"))]
	for (i in seq_along(ops)) {
		# get rows to apply update operation
		if (inherits(ops[[i]], "MarxanSpeciesOperation")) {
			if (is.character(ops[[i]]$x)) {
				ops[[i]]$row<-match(ops[[i]]$x, x@species$name)
			} else {
				ops[[i]]$row<-match(ops[[i]]$x, x@species$id)
			}
			for (j in seq_along(ops[[i]]$value))
				x@species[opts[[i]]$row,opts[[i]]$col[j]] <- opts[[i]]$value[j]			
		} else if (inherits(ops[[i]], "MarxanPuOperation")) {
			opts[[i]]$row<-match(ops[[i]]$id, x@pu$id)
			for (j in seq_along(ops[[i]]$value))
				x@pu[opts[[i]]$row,opts[[i]]$col[j]] <- opts[[i]]$value[j]
		} else {
			stop("Unrecognised update operation.")
		}
	}
	return(x)
}

#' @describeIn plot
#' @export
setMethod(
	"plot", 
	signature(x="MarxanData", y="character"),
	function(x, y, basemap="none", colramp="BuGn", alpha=1, grayscale=FALSE, xzoom=c(1,1), yzoom=c(1,1), force_reset=FALSE) {
		# init
		stopifnot(alpha<=1 & alpha>=0)
		match.arg(y, c("sum", "rich", unique(x@y$name)))
		match.arg(colramp, rownames(brewer.pal.info))
		stopifnot(inherits(x@polygons, "PolySet"))
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

#' @describeIn is.cached
setMethod(
	f="is.cached", 
	signature(x="MarxanData", name="character"), 
	function(x,name) {
		return(!is.null(x@.cache[[names]]))
	}
)

#' @describeIn cache
setMethod(
	f="cache", 
	signature(x="MarxanData", name="character", y="ANY"), 
	function(x, name, y) {
		x@.cache[[name]]=y
	}
)

#' @describeIn cache
setMethod(
	f="cache", 
	signature(x="MarxanData", name="character", y="missing"), 
	function(x, name, y) {
		return(x@.cache[[name]])
	}
)

#' @describeIn is.comparable
#' export
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