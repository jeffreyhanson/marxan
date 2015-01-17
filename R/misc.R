#' @include RcppExports.R marxan-internal.R
NULL

#' Test if GDAL is installed on computer
#'
#' This function tests if gdal is installed on the computer.
#' If not, download it here: \code{\url{http://download.osgeo.org/gdal}}
#'
#' @return Logical. Is it installed?
#' @seealso \code{\link[gdalUtils]{gdal_setInstallation}}
#' @export
#' @examples
#' gdal_setInstallation()
is.gdalInstalled <- function() {
	gdal_setInstallation()
	return(!is.null(getOption("gdalUtils_gdalPath")))
}

#' Rasterize polygon data using GDAL
#'
#' This function converts a SpatialPolygonsDataFrame to raster format using GDAL.
#' It is expected to be faster than \code{\link[raster]{rasterize}} for large datasets.
#' However, it will be significantly slower for small datasets.
#'
#' @param x "SpatialPolygonsDataFrame" with polygon spatial data
#' @param y "RasterLayer" with dimensions, extent, and resolution to be used as a template
#' @param field "character" column name with values to burn into the output raster
#' @export
#' @return RasterLayer
#' @seealso \code{\link[raster]{rasterize}}, \code{\link{is.gdalInstalled}}
setGeneric('rasterize.gdal', function(x,y, ...) standardGeneric('rasterize.gdal'))
setMethod(
	'rasterize.gdal',
	signature(x="SpatialPolygonsDataFrame", y="RasterLayer"),
	function(x, y, field=NULL) {
		writeOGR(x, tempdir(), 'polys', driver='ESRI Shapefile', overwrite=TRUE)
		writeRaster(setValues(y, NA), file.path(tempdir(), 'rast.tif'), NAflag=-9999, overwrite=TRUE)
		return(gdal_rasterize(file.path(tempdir(), 'polys.shp'), file.path(tempdir(), 'rast.tif'), l="polys", a="id", output_Raster=TRUE)[[1]])
	}
)

#' Test if Marxan is installed on computer
#'
#' This function determines if Marxan is installed on the computer, and will update \code{\link[base]{options}} 
#'
#' @return "logical" is it installed?
#' @export
#' @examples
#'	is.marxanInstalled()
is.marxanInstalled<-function() {
	return(!is.null(options()$marxanExecutablePath) & file.exists(options()$marxanExecutablePath))
}

#' Find Marxan executable suitable for computer
#'
#' This function checks the computer's specifications and sets options('marxanExecutablePath') accordingly.
#' Marxan exectuables can be downloaded from \code{\link{marxanURL}}, and installed by unzipping the files contents, and copying them
#' into the /bin folder in this package's installation directory. 
#' If a suitable executable cannot be found, this function will fail and provide information. determines if Marxan is installed on the computer, and will update \code{\link[base]{options}} 
#'
#' @seealso \link{\code{is.marxanInstalled}}
#' @return Logical. Is it installed?
#' @export
#' @examples
#' # Marxan executable files should be copied to this directory
#' system.file("bin", package="marxan")
#' # Look for Marxan
#' \donttest{
#' findMarxanExecutablePath()
#' }
#' # Was Marxan found?
#' is.marxanInstalled()
findMarxanExecutablePath=function() {
	# if path already set then return it
	!is.null(options()$marxanExecutablePath)
		return(options()$marxanExecutablePath)
	# if path not set then set it
	if (.Platform$OS.type=="windows") {
		if (.Platform$r_arch=="x64") {
			path=list.files(system.file("bin", package="marxan"), "^marxan.*x64.exe$",full.names=TRUE)
		} else if (.Platform$r_arch=="i386") {
			path=system.file('bin/marxan.exe', package="marxan")
		} else {
			stop('Marxan will only run in 64bit or 32bit Windows environments.')
		}
	} else {
		if (.Platform$OS.type=="unix") {
			if (Sys.info()$Sys.info()[["sysname"]]=="Darwin") {
				if (Sys.info()$machine=="x86_64") {
					path=list.files(system.file("bin", package="marxan"), "^MarOpt.*Mac64",full.names=TRUE)
				} else if (Sys.info()$machine=="i686") {
					path=list.files(system.file("bin", package="marxan"), "^MarOpt.*Mac32",full.names=TRUE)
				}
			} else {
				if (Sys.info()$machine=="x86_64") {
					path=list.files(system.file("bin", package="marxan"), "^MarOpt.*Linux64",full.names=TRUE)
				} else if (Sys.info()$machine=="i686") {
					path=list.files(system.file("bin", package="marxan"), "^MarOpt.*Linux32",full.names=TRUE)
				}
			}
		} else {
			stop("Only Windows, Mac OSX, and Linux systems are supported.")
		}
	}
	# check that path is valid
	if (length(path)!=1 || !file.exists(path))
		stop(paste0("Marxan executable files not found.\nDownload marxan from ",marxanURL," and copy the files into:\n", system.file("bin", package="marxan")))
	options(marxanExecutablePath=path)
}

#' Update Marxan input parameters
#'
#' This function is used in the formula argument of the update function to change input parameters of a "MarxanOpts", "MarxanUnsolved", or "MarxanSolved" object.
#'
#' @param name "character" name of parameter to change.
#' @param value "numeric" new value.
#' @return "MarxanOptsOperation" object.
#' @export
#' @seealso \code{\link{MarxanOpts-class}}, \code{\link{MarxanUnsolved-class}}, \code{\link{MarxanSolved-class}} \code{\link{update}}, \code{\link{spp}}, \code{\link{pu}}
opt<-function(name, value) {
	return(
		structure(
			list(name,value),
			.Names = c("slot", "value"),
			class = c("MarxanUpdateOperation", "MarxanOptsOperation")
		)
	)
}

#' Update Marxan species parameters
#'
#' This function is used in the formula argument of the update function to change species parameters of a "MarxanData", "MarxanUnsolved", or "MarxanSolved" object.
#'
#' @param x "numeric" species id or "character" species name.
#' @param name "character" new species name.
#' @param spf "numeric" new species penalty factor.
#' @param target "numeric" new target. 
#' @note Set arguments 'name', 'spf', 'target' to NA (default) to keep the same.
#' @export
#' @return "MarxanSpeciesOperation" object.
#' @seealso \code{\link{MarxanOpts-class}}, \code{\link{MarxanUnsolved-class}}, \code{\link{MarxanSolved-class}} \code{\link{update}}, \code{\link{opt}}, \code{\link{pu}}
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
#' @note Set argument 'cost' or 'status' to NA (default) to keep the same.
#' @export
#' @return "MarxanPuOperation" object.
#' @seealso \code{\link{MarxanOpts-class}}, \code{\link{MarxanUnsolved-class}}, \code{\link{MarxanSolved-class}} \code{\link{update}}, \code{\link{opt}}, \code{\link{spp}}
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
			.Names = c("x", "value", "col"),
			class = c("MarxanUpdateOperation", "MarxanPuOperation",  "MarxanDataOperation")
		)
	)
}
