#' @include RcppExports.R marxan-internal.R misc.R generics.R calcBoundaryData.R calcPuVsSpeciesData.R
NULL

#' MarxanData: An S4 class to represent Marxan input data
#'
#' This class is used to store Marxan input data.
#'
#' @slot polygons "PolySet" planning unit spatial data or "NULL" if data not available.
#' @slot pu "data.frame" planning unit data; with "integer" 'id', "numeric" 'cost', "integer" 'status' columns.
#' @slot species "data.frame" with species data; with "integer" 'id', "numeric" 'target', "numeric" 'spf', and "character" 'name' columns.
#' @slot puvspecies "data.frame" with data on species density in each planning unit, with "integer" 'species', "integer" 'pu', and "numeric" 'target' columns. This "data.frame" is sorted in order of 'pu' column.
#' @slot puvspecies_spo "data.frame" with data in \code{puvspecies} ordered by 'species' column.
#' @slot boundary "data.frame" with data on the shared boundary length of planning; with "integer" 'id1', "integer" 'id2', and "numeric" 'amount' columns.
#' @slot skipchecks "logical" Skip data integrity checks? May improve speed for big data sets.
#' @slot .cache "environment" used to cache calculations.
#' @seealso \code{\link[PBSmapping]{PolySet}}.
#' @export
setClass("MarxanData",
	representation(
		polygons="PolySetOrNULL",
		pu="data.frame",
		species="data.frame",
		puvspecies="data.frame",
		puvspecies_spo="data.frame",
		boundary="data.frame",
		skipchecks="logical",
		.cache="environment"
	),
	validity<-function(object) {	
		if (!skipchecks)
		### check column names of inputs		
		# pu
		expect_named(object@pu, c("id","cost","status"), label='argument to pu')
		expect_is(object@pu$id, "integer", label='argument to pu$id')
		expect_false(any(duplicated(object@pu$id)), info='argument to pu$id contains duplicates')
		expect_is(object@pu$cost, "numeric", label='argument to pu$cost')
		expect_true(all(object@pu$status %in% 0:3), info='argument to pu$status must only contain values in 0:3')

		# species
		expect_true(all(c("id","spf","target") %in% names(object@species)), label='argument to species')
		expect_is(object@species$id, "integer", label='argument to species$id')
		expect_false(any(is.na(object@species$id)), info='argument to species$id must not contain any NA values')
		expect_false(any(duplicated(object@species$id)), info='argument to species$id contains duplicates')
		expect_is(object@species$spf, "numeric", label='argument to species$spf')
		expect_false(any(is.na(object@species$spf)), info='argument to species$spf must not contain any NA values')
		expect_is(object@species$target, "numeric", label='argument to species$target')
		expect_false(any(is.na(object@species$target)), "numeric", info='argument to species$target must not contain any NA values')
		if (!is.null(object@species$name)) {
			object@species$name<-as.character(object@species$name)
			expect_is(object@species$name, "character", label='argument to species$name')
			expect_false(any(is.na(object@species$name)), info='argument to species$name must not contain any NA values')
		}
		if (!is.null(object@species$targetocc)) {
			expect_is(object@species$targetocc, "integer", label='argument to species$targetocc')
			expect_false(any(is.na(object@species$targetocc)), info='argument to species$targetocc must not contain any NA values')
		}
		if (!is.null(object@species$sepnum)) {
			expect_is(object@species$sepnum, "integer", label='argument to species$sepnum')
			expect_false(any(is.na(object@species$sepnum)), info='argument to species$sepnum must not contain any NA values')
		}
		if (!is.null(object@species$sepdistance)) {
			expect_is(object@species$sepdistance, "numeric", label='argument to species$sepdistance')
			expect_false(any(is.na(object@species$sepdistance)), info='argument to species$sepdistance must not contain any NA values')
		}
		
		# puvspecies
		expect_named(object@puvspecies, c("species","pu","amount"), label='argument to puvspecies')
		expect_is(object@puvspecies$species, "integer", label='argument to puvspecies$species')
		expect_false(any(is.na(object@puvspecies$species)), info='argument to puvspecies$species must not contain any NA values')
		expect_is(object@puvspecies$pu, "integer", label='argument to puvspecies$pu')
		expect_false(any(is.na(object@puvspecies$pu)), info='argument to puvspecies$pu must not contain any NA values')
		expect_is(object@puvspecies$amount, "numeric", label='argument to puvspecies$amount')
		expect_false(any(is.na(object@puvspecies$amount)), info='argument to puvspecies$amount must not contain any NA values')

		# puvspecies_spo
		expect_named(object@puvspecies_spo, c("species","pu","amount"), label='argument to puvspecies_spo')
		expect_is(object@puvspecies_spo$species, "integer", label='argument to puvspecies_spo$species')
		expect_false(any(is.na(object@puvspecies_spo$species)), info='argument to puvspecies_spo$species must not contain any NA values')
		expect_is(object@puvspecies_spo$pu, "integer", label='argument to puvspecies_spo$pu')
		expect_false(any(is.na(object@puvspecies_spo$pu)), info='argument to puvspecies_spo$pu must not contain any NA values')
		expect_is(object@puvspecies_spo$amount, "numeric", label='argument to puvspecies_spo$amount')
		expect_false(any(is.na(object@puvspecies_spo$amount)), info='argument to puvspecies_spo$amount must not contain any NA values')

		# boundary
		expect_named(object@boundary, c("id1","id2","boundary"), label='argument to boundary')
		expect_is(object@boundary$id1, "integer", label='argument to boundary$id1')
		expect_false(any(is.na(object@boundary$id1)), info='argument to boundary$id1 must not contain any NA values')
		expect_is(object@boundary$id2, "integer", label='argument to boundary$id2')
		expect_false(any(is.na(object@boundary$id2)), info='argument to boundary$id2 must not contain any NA values')
		expect_is(object@boundary$boundary, "numeric", label='argument to boundary$boundary')
		expect_false(any(is.na(object@boundary$boundary)), "numeric", info='argument to boundary$boundary must not contain any NA values')
		
		# cross table dependencies
		expect_true(all(object@boundary$id1 %in% object@pu$id), info='argument to boundary$id1 must only contain values present in pu$id')
		expect_true(all(object@boundary$id2 %in% object@pu$id), info='argument to boundary$id2 must only contain values present in pu$id')
		expect_true(all(object@puvspecies$pu %in% object@pu$id), info='argument to puvspecies$pu must only contain values present in pu$id')
		expect_true(all(object@puvspecies_spo$pu %in% object@pu$id), info='argument to puvspecies_spo$pu must only contain values present in pu$id')
		expect_true(all(object@puvspecies$species %in% object@species$id), info='argument to puvspecies$species must only contain values present in species$id')			
		expect_true(all(object@puvspecies_spo$species %in% object@species$id), info='argument to puvspecies_spo$species must only contain values present in species$id')
		if (!is.null(object@species$sepdistance)) {
			expect_is(object@pu$xloc, "numeric", label='argument to pu$xloc')
			expect_false(any(is.na(object@pu$xloc)), info='argument to pu$xloc must not contain any NA values')				
			expect_is(object@pu$yloc, "numeric", label='argument to pu$yloc')
			expect_false(any(is.na(object@pu$yloc)), info='argument to pu$yloc must not contain any NA values')				
		}
		return(TRUE)
	}
)

#' @export
setMethod(
	"initialize", 
	"MarxanData", 
	function(.Object, pu, species, puvspecies, boundary, polygons, puvspecies_spo, skipchecks, .cache=new.env()) {
		callNextMethod(.Object, polygons=polygons, pu=pu, species=species, puvspecies=puvspecies, puvspecies_spo=puvspecies_spo,skipchecks=skipchecks,boundary=boundary, .cache=.cache)
	}
)

#' Create new MarxanData object
#'
#' This function creates a "MarxanData" object using pre-processed data.
#'
#' @param pu "data.frame" planning unit data; with "integer" 'id', "numeric" 'cost', "integer" 'status' columns.
#' @param species "data.frame" with species data; with "integer" 'id', "numeric" 'target', "numeric" 'spf', and "character" 'name' columns.
#' @param puvspecies "data.frame" with data on species density in each planning unit, with "integer" 'species', "integer" 'pu', and "numeric" 'target' columns. This "data.frame" is sorted in order of 'pu' column.
#' @param boundary "data.frame" with data on the shared boundary length of planning; with "integer" 'id1', "integer" 'id2', and "numeric" 'amount' columns.
#' @param polygons "PolySet" planning unit spatial data. Defaults to \code{NULL} so that Marxan large problems can still be handled.
#' @param puvspecies_spo "data.frame" with data in \code{puvspecies} ordered by 'species' column. Defaults to \code{NULL}, and will generate data from \code{puvspecies}.
#' @param skipchecks "logical" Skip data integrity checks? May improve speed for big data sets. Defaults to \code{FALSE}.
#' @param .cache "environment" used to cache calculations. Defaults to a new environment.
#' @note Generally, users are not encouraged to change arguments to \code{.cache}.
#' @return MarxanData object
#' @seealso \code{\link[PBSmapping]{PolySet}}, \code{\link{read.MarxanData}}, \code{\link{write.MarxanData}}, \code{\link{format.MarxanData}}, \code{\link{MarxanData-class}}.
#' @export
#' @examples
#' data(planningunits, species)
#' x<-MarxanData(
#' 	planningunits@@data,
#' 	data.frame(id=1L, target=12, spf=23, name='species1', stringsAsFactors=FALSE),
#'  calcPuVsSpeciesData(planningunits, species[[1]]),
#'  calcBoundaryData(planningunits),
#'  SpatialPolygons2PolySet(planningunits)
#' )
MarxanData<-function(pu, species, puvspecies, boundary, polygons=NULL, puvspecies_spo=NULL, skipchecks=FALSE, .cache=new.env()) {
	# convert factors to characters
	if (inherits(species$name, "factor"))
		species$name<-as.character(species$name)
	# replace species names in puvspecies integers
	if (!is.integer(puvspecies$species))
		puvspecies$species<-species$id[match(as.character(puvspecies$species),species$name)]
	# sort df by pu
	if (!skipchecks)
		puvspecies<-puvspecies[order(puvspecies$pu),]
	# create df sorted by species
	if (is.null(puvspecies_spo))
		puvspecies_spo<-puvspecies[order(puvspecies$species),]
	return(new("MarxanData", polygons=polygons, pu=pu, species=species, puvspecies=puvspecies, puvspecies_spo=puvspecies_spo,skipchecks=skipchecks,boundary=boundary, .cache=.cache))
}

#' Read Marxan input
#'
#' This function loads Marxan input data from disk.
#'
#' @param path "character" file path for input parameter file or directory containing Marxan files named 'spec.dat', 'pu.dat', 'bound.dat', 'puvspr.dat', and (optionally) 'puvspr_sporder.dat'.
#' @return "MarxanData" object
#' @export
#' @seealso \code{\link{write.MarxanData}}, \code{\link{format.MarxanData}}, \code{\link{MarxanData}}, \code{\link{MarxanData-class}}.
read.MarxanData<-function(path, skipchecks=FALSE) {
	expect_true(file.exists(path), info="argument to path does not exist.")
	if (nchar(tools::file_ext(path))>0) {
		# load from input file
		args<-readLines(path)
		path<-parseArg("INPUTDIR",args)
		pu.pth<-file.path(path,parseArg("PUNAME",args))
		species.pth<-file.path(path,parseArg("SPECNAME",args))
		puvspr.pth<-file.path(path,parseArg("PUVSPRNAME",args))
		puvspr_sporder.pth<-file.path(path,parseArg("MATRIXSPORDERNAME", args, error=FALSE))
		bound.pth<-file.path(path,parseArg("BOUNDNAME",args))
	} else {
		# load from directory
		pu.pth<-file.path(path,'pu.dat')
		species.pth<-file.path(path,'spec.dat')
		puvspr.pth<-file.path(path,'puvspr.dat')
		puvspr_sporder.pth<-file.path(path,'puvspr_sporder.dat')
		bound.pth<-file.path(path,'bound.dat')
	}
	# try and load puvspr_sporder if present
	if (length(puvspr_sporder.pth)==0 || !file.exists(puvspr_sporder.pth)) {
		puvspr_sporder.pth<-NULL
	} else {
		puvspr_sporder.pth<-fread(puvspr_sporder.pth, header=TRUE, sep=",", stringsAsFactors=FALSE, colClasses=c(species="integer", pu="integer", amount="numeric"), data.table=FALSE)
	}
	# return MarxanData object
	return(MarxanData(
		pu=fread(pu.pth, header=TRUE, sep=",", stringsAsFactors=FALSE, colClasses=c(id="integer", cost="numeric", status="integer"), data.table=FALSE),
		species=fread(species.pth, header=TRUE, sep=",", stringsAsFactors=FALSE, colClasses=c(id="integer", spf="numeric", target="numeric", name="character"), data.table=FALSE),
		puvspecies=fread(puvspr.pth, header=TRUE, sep=",", stringsAsFactors=FALSE, colClasses=c(species="integer", pu="integer", amount="numeric"), data.table=FALSE),
		boundary=fread(bound.pth, header=TRUE, sep=",", stringsAsFactors=FALSE, colClasses=c(id1="integer", id2="integer", boundary="numeric"), data.table=FALSE),
		polygons=NULL,
		puvspecies_spo=puvspr_sporder.pth,
		skipchecks=skipchecks
	))
}

#' Write Marxan input data
#'
#' This function saves "MarxanData" objects to disk.
#'
#' @param x "MarxanData" object to save.
#' @param dir "character" directory path for location to save data.
#' @export
#' @seealso \code{\link{read.MarxanData}}, \code{\link{format.MarxanData}}, \code{\link{MarxanData}}, \code{\link{MarxanData-class}}.
write.MarxanData<-function(x, dir=getwd(), ...) {
	write.table(x@species,row.names=FALSE,sep=",",quote=FALSE,file.path(dir,"spec.dat"))
	write.table(x@puvspecies,row.names=FALSE,sep=",",quote=FALSE,file.path(dir,"puvspr.dat"))
	write.table(x@puvspecies_spo,row.names=FALSE,sep=",",quote=FALSE,file.path(dir,"puvspr_sporder.dat"))
	write.table(x@boundary,row.names=FALSE,sep=",",quote=FALSE,file.path(dir,"bound.dat"))
	write.table(x@pu,row.names=FALSE,sep=",",quote=FALSE,file.path(dir,"pu.dat"))
}

#' Format data for Marxan
#'
#' This function prepares spatially explicit planning unit and species data for Marxan processing.
#' By default, raw data can be provided which be used to generate all Marxan inputs from scratch. However, if particular Marxan input file have
#' already been generated, these can be supplied to avoid re-processing data.
#'
#' @param polygons "SpatialPolygons" with planning unit data.
#' @param rasters "RasterLayer", "RasterStack", "RasterBrick" with species distribution data.
#' @param targets "numeric" vector for targets for each species (eg. 12), or "character" vector with percent-based targets (eg. '12\%'). Defaults to '20\%' for each species.
#' @param pu "data.frame" planning unit data; with "integer" 'id', "numeric" 'cost', "integer" 'status' columns. Default behaviour is to generate a table with all costs and statuses set to 1 and 0 (respectively).
#' @param species "data.frame" with species data; with "integer" 'id', "numeric" 'target', "numeric" 'spf', and "character" 'name' columns. Default behaviour is to base targets on \code{target} argument, set 'spf' to 1, and set 'names' use names of layers in \code{rasters} argument.
#' @param puvspecies "data.frame" pu vs. species data; with "species", "pu", and "amount" columns. Defaults to \code{NULL}, and will be calculated using \code{calcPuVsSpeciesData}.
#' @param puvspecies_spo "data.frame" with data in \code{puvspecies} ordered by 'species' column. Defaults to \code{NULL}, and will generate data from \code{puvspecies}.
#' @param boundary "data.frame" with data on the shared boundary length of planning; with "integer" 'id1', "integer" 'id2', and "numeric" 'amount' columns. Default behaviour is to calculate this using \code{calcBoundaryData}.
#' @param Additional arguments to \code{calcBoundaryData} and \code{calcPuVsSpeciesData}.
#' @seealso \code{\link{MarxanData-class}}, \code{\link{MarxanData}}, \code{\link{read.MarxanData}}, \code{\link{write.MarxanData}}.
#' @export
#' @examples
#' data(planningunits, species)
#' x<-MarxanData(planningunits, rasters=species, targets="10\%")
#' y<-MarxanData(planningunits, rasters=species)
#' stopifnot(identical(x,y))
format.MarxanData<-function(polygons, rasters, targets="20%", spf=rep(1, nlayers(rasters)), sepdistance=rep(0, nlayers(rasters)), sepnum=rep(0,nlayers(rasters)), targetocc=rep(0,nlayers(rasters)), pu=NULL, species=NULL, puvspecies=NULL, puvspecies_spo=NULL, boundary=NULL, ..., verbose=FALSE) {
	# init
	.cache<-new.env()
	# set polygons
	geoPolygons<-polygons
	if (!identical(geoPolygons@proj4string, CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))) {
		if (verbose)
			cat('Projecting polygons to WGS1984 for rendering.\n')
		geoPolygons<-spTransform(geoPolygons, CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
	}
	geoPolygons<-rcpp_Polygons2PolySet(geoPolygons@polygons)
	# set pu
	if (is.null(pu)) {
		validNames<-c('id','cost','status','xloc','yloc')
		if (inherits(polygons, "SpatialPolygonsDataFrame") & all(c('id','cost','status') %in% names(polygons@data))) {
			pu<-polygons@data[,validNames[which(validNames %in% names(polygons@data))],drop=FALSE]
		} else {
			warning("argument to polygons@data does not have 'id', 'cost', and 'status' columns, creating default with equal costs and status=0")
			pu<-data.frame(id=seq_along(polygons@polygons), cost=1, status=0)
		}
		# get xy coordinates 
		if (any(sepdistance!=0)) {
			if (!identical(polygons@proj4string, CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')))
				warning("argument to polygons is in WGS1984 CRS and is being used to calculate planning unit centroids for xloc/yloc values")
			centroids<-gCentroid(polygons, byid=TRUE)
			pu$xloc<-centroids@coords[,1]
			pu$yloc<-centroids@coords[,2]
		}
	}
	# cache max possible targets
	if (inherits(rasters, c('RasterLayer','RasterStack', 'RasterBrick'))) {
		if (verbose)
			cat("Caching raster data to for percent-based targets.\n")
		tmp<-cellStats(rasters,'sum')
		names(tmp)<-names(rasters)
		.cache$speciesMaxTargets=tmp
	}
	# set species
	if (is.null(species)) {
		species<-data.frame(
			id=seq_along(names(rasters)),
			target=targets,
			spf=spf,
			name=names(rasters),
			sepdistance=sepdistance,
			sepnum=sepnum,
			targetocc=targetocc
		)
		# remove unnecessary marxan columns
		species<-species[,sapply(species[,5:7], function(x) return(all(x==0)))]
	}
	# set boundary
	polyset<-rcpp_Polygons2PolySet(polygons@polygons)
	if (is.null(boundary)) {
		if (identical(polygons@proj4string, CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')))
			warning("creating boundary length data from polygons in WGS1984; consider supplying in an object a projected CRS.")
		if (verbose)
			cat("Calculating boundary data.")
		boundary<-calcBoundaryData(polyset, ...)
	}
	# set puvspecies
	if (is.null(puvspecies)) {
		projPolygons=polygons
		if (!identical(projPolygons@proj4string, rasters@crs)) {
			if (verbose)
				cat("Projecting polygons to rasters' CRS")
			projPolygons<-spTransform(projPolygons, rasters@crs)
		}
		if (verbose)
			cat("Calculating species density in planning units.")
		puvspecies<-calcPuVsSpeciesData(projPolygons, rasters, ...)
	}
	# set puvspecies_spo
	if (is.null(puvspecies_spo))
		puvspecies_spo<-puvspecies[order(puvspecies$species),]
	return(MarxanData(pu=pu, species=species, puvspecies=puvspecies, puvspecies_spo=puvspecies_spo, boundary=boundary, polygons=polyset, .cache=.cache))
}

#' @export
print.MarxanData<-function(x, header=TRUE) {
	if (header)
		cat("MarxanData object.\n")
	cat("Number of planning units:",nrow(x@pu),"\n")
	cat("Number of species:",nrow(x@species),"\n")
}

#' @export
setMethod(
	'show',
	signature(object='MarxanData'),
	function(object)
		print.MarxanData(object)
)


#' @describeIn basemap
#' @export
basemap.MarxanData<-function(x, basemap="hybrid", grayscale=FALSE, force_reset=FALSE) {
	callchar<-hashCall(match.call(), 1)
	match.arg(basemap, c("roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid"))	
	if (is.null(x@polygons))
	stop("Marxan object is not associated with spatially explicit data for the planning units.")
	# fetch data from google or cache
	if (force_reset || !is.cached(x, callchar)) {
		cache(x, callchar, GetMap.bbox(range(x@polygons[["X"]]), range(x@polygons[["Y"]]), destfile=paste0(tempfile(),'.png'), maptype=basemap, GRAYSCALE=grayscale))
	}
	return(cache(x, callchar))
}

#' @describeIn update
#' @export
update.MarxanData<-function(x, formula, force_reset=TRUE) {
	if (force_reset)
		x@.cache<-new.env()
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
			if (length(ops[[i]]$row)==0)
				stop('argument to x (',ops[[i]]$x,') is not a valid species id or name')
			for (j in seq_along(ops[[i]]$value))
				x@species[ops[[i]]$row,ops[[i]]$col[j]] <- ops[[i]]$value[j]			
		} else if (inherits(ops[[i]], "MarxanPuOperation")) {
			ops[[i]]$row<-match(ops[[i]]$id, x@pu$id)
			if (length(ops[[i]]$row)==0)
				stop('argument to x (',ops[[i]]$x,') is not a valid pu id')
			for (j in seq_along(ops[[i]]$value))
				x@pu[ops[[i]]$row,ops[[i]]$col[j]] <- ops[[i]]$value[j]
		} else {
			stop("Unrecognised update operation.")
		}
	}
	return(x)
}

#' Update Marxan species parameters
#'
#' This function is used in the formula argument of the update function to change species parameters of a "MarxanData", "MarxanUnsolved", or "MarxanSolved" object.
#'
#' @param x "numeric" species id or "character" species name.
#' @param name "character" new species name.
#' @param spf "numeric" new species penalty factor.
#' @param target "numeric" new target. 
#' @note Only specified parameters are changed, all unspecific parameters will remain unchanged.
#' @export
#' @return "MarxanSpeciesOperation" object.
#' @seealso \code{\link{MarxanOpts-class}}, \code{\link{MarxanUnsolved-class}}, \code{\link{MarxanSolved-class}} \code{\link{update}}, \code{\link{opt}}, \code{\link{pu}}.
#' @examples
#' spp(1, name='species1')
#' spp(2, spf=5)
spp<-function(x, name=NA, spf=NA, target=NA) {
	if (is.na(name) & is.na(spf) & is.na(target))
		stop("no arguments were specified to change values.")
	args<-structure(c(name,spf,target), .Names=c("name","spf","target"))
	args<-args[!is.na(args)]
	return(
		structure(
			list(
				x, 
				args,
				names(args)
			),
			.Names = c("x", "value", "col"),
			class = c("MarxanUpdateOperation", "MarxanSpeciesOperation",  "MarxanDataOperation")
		)
	)
}

#' Update Marxan planning unit parameters
#'
#' This function is used in the formula argument of the update function to change planning unit parameters of a "MarxanData", "MarxanUnsolved", or "MarxanSolved" object.
#'
#' @param id "integer" id of the planning unit.
#' @param cost "numeric" new cost value.
#' @param status "numeric" new status value.
#' @note Only specified parameters are changed, all unspecific parameters will remain unchanged.
#' @export
#' @return "MarxanPuOperation" object.
#' @seealso \code{\link{MarxanOpts-class}}, \code{\link{MarxanUnsolved-class}}, \code{\link{MarxanSolved-class}} \code{\link{update}}, \code{\link{opt}}, \code{\link{spp}}
#' @examples
#' pu(1, cost=3)
#' pu(1, status=1)
pu<-function(id, cost=NA, status=NA) {
	if (is.na(cost) & is.na(status))
		stop("no arguments were supplied to change values.")
	args<-structure(c(cost,status), .Names=c("cost","status"))
	args<-args[!is.na(args)]	
	return(
		structure(
			list(
				id, 
				args,
				names(args)
			),
			.Names = c("id", "value", "col"),
			class = c("MarxanUpdateOperation", "MarxanPuOperation",  "MarxanDataOperation")
		)
	)
}


#' @describeIn spplot
#' @export
spplot.MarxanData<-function(x, y, var='amount', basemap="none", colramp="YlOrRd", alpha=ifelse(basemap=="none", 1, 0.7), grayscale=FALSE, force_reset=FALSE) {
	# data checks
	if (!inherits(x@polygons, "PolySet"))
		stop("Spatial data for planning units not present in object")
	match.arg(var, c('amount','occ'))
	# pre-processing
	if (inherits(y, "character")) {
		if (y=="all") {
			y<-unique(x@species$id)
		} else {
			match.arg(y, x@species$name)
			y<-x@species$id[match(y, x@species$name)]
		}
	} else {
		stopifnot(y %in% x@species$id)
	}
	# get basemap
	if (basemap!="none")
		basemap<-basemap.MarxanData(x, basemap, grayscale, force_reset)
	# main processing
	rows<-which(x@puvspecies$species %in% y)
	pus<-x@puvspecies$pu[rows]
	if (var=='occ') {
		values<-rep(1, length(rows))
	} else {
		values<-x@puvspecies$amount[rows]
	}
	values<-rcpp_groupsum(pus, values)
	if (length(unique(values))>1) {
		cols<-brewerCols(rescale(values, to=c(0,1)), colramp, alpha)
	} else {
		cols<-brewerCols(rep(values[1], length(values)), colramp, alpha)
		values<-c(0,values[1])
	}
	plot(1,1)
	prettyGeoplot(
		x@polygons,
		cols,
		basemap,
		paste0("Species ", ifelse(y=='occ', 'occupancy', 'amount'), " in planning units"),
		continuousLegend(values,colramp,posx=c(0.3, 0.4),posy=c(0.1, 0.9)),
		beside=TRUE
	)
}

#' @describeIn plot
#' @export
setMethod(
	"plot", 
	signature(x="MarxanData", y="character"),
	function(x, y, basemap="none", colramp="BuGn", alpha=1, grayscale=FALSE, force_reset=FALSE) {
		# init
		stopifnot(alpha<=1 & alpha>=0)
		match.arg(y, c("sum", "rich", unique(x@y$name)))
		match.arg(colramp, rownames(brewer.pal.info))
		stopifnot(inherits(x@polygons, "PolySet"))
		# get basemap data
		if (basemap!="none")
			basemap<-basemap.MarxanData(x@data, basemap=basemap, grayscale=grayscale, force_reset=force_reset)
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
		return(!is.null(x@.cache[[name]]))
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

#' @export
#' @describeIn names
names.MarxanData<-function(x) {
	return(x@species$name)
}

#' @export
#' @describeIn names
`names<-.MarxanData`<-function(x,value) {
	stopifnot(length(value)==nrow(x@species) & is.character(value) & !any(is.na(value)))
	x@species$name<-value
	return(x)
}

#' @export
#' @describeIn spfs
spfs.MarxanData<-function(x) {
	return(x@species$spfs)
}

#' @export
#' @describeIn spfs
`spfs<-.MarxanData`<-function(x,value) {
	stopifnot(length(value)==nrow(x@species) & is.numeric(value) & !any(is.na(value)))
	x@species$spf<-value
	return(x)
}

#' @export
#' @describeIn targets
targets.MarxanData<-function(x) {
	return(x@species$target)
}

#' @export
#' @describeIn targets
`targets<-.MarxanData`<-function(x, value) {
	stopifnot(length(value)==nrow(x@species) & is.numeric(value) & !any(is.na(value)))
	x@species$target<-value
	return(x)
}

#' @export
#' @describeIn sppids
sppids.MarxanData<-function(x) {
	return(x@species$id)
}

#' @export
#' @describeIn sppids
`sppids<-.MarxanData`<-function(x, value) {
	stopifnot(length(value)==nrow(x@species) & is.integer(value) & !any(is.na(value)))
	x@species$id<-value
	return(x)
}

#' @export
#' @describeIn puids
puids.MarxanData<-function(x) {
	return(x@pu$ids)
}

#' @export
#' @describeIn puids
`puids<-.MarxanData`<-function(x, value) {
	stopifnot(length(value)==nrow(x@pu) & is.integer(value) & !any(is.na(value)))
	x@pu$id<-value
	return(x)
}

#' @export
#' @describeIn costs
costs.MarxanData<-function(x) {
	return(x@pu$costs)
}

#' @export
#' @describeIn costs
`costs<-.MarxanData`<-function(x, value) {
	stopifnot(length(value)==nrow(x@pu) & is.numeric(value) & !any(is.na(value)))
	x@pu$costs<-value
	return(x)
}


#' @export
#' @describeIn inistatus
inistatus.MarxanData<-function(x) {
	return(x@pu$status)
}

#' @export
#' @describeIn inistatus
`inistatus<-.MarxanData`<-function(x,value) {
	stopifnot(length(value)==nrow(x@pu) & is.integer(value) & !any(is.na(value)))	
	x@pu$status<-value
	return(x)
}





