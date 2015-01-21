#' @include RcppExports.R marxan-internal.R misc.R
NULL

#' Solve Marxan object
#'
#' This function executes Marxan using the input parameter and data stored in a "MarxanUsolved" object, 
#' and returns a "MarxanSolved" object with outputs in it.
#'
#' @param x "MarxanUnsolved" or "MarxanSolved" object.
#' @param wd "character" file path to a working directory, this is a temporary directory by default to avoid pollution.
#' @param seeds "integer" vector of seeds to initialise Marxan 's random number generator.
#' @param clean "logical" delete files once processing completed?
#' @param force_reset "logical" should Marxan solutions be recalculated even if "MarxanSolved" object supplied?
#' @return "MarxanSolved" object
#' @note This function is used to solve a MarxanUnsolved object that has all of its inputs generated. The marxan function (without lower case 'm') provides a more general interface for generating inputs and outputs for Marxan.
#' @name solve
NULL

#' Extract solution score
#'
#' Extract solution score from "MarxanResults" or "MarxanSolved" object.
#'
#' @param x "MarxanResults or "MarxanSolved" object.
#' @param y "NULL" to return all scores, "integer" 0 to return score for best solution, "integer" value greater than 0 for \code{y}'th solution score.
#' @return "matrix" or "numeric" vector with solution score(s) depending on arguments.
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanSolved-class}}, \code{\link{marxan}}
#' @export
score<-function(x, ...) {UseMethod('score')}

#' Species penalty factors
#'
#' This function returns or assigns the species penalty factors for speces in a Marxan object.
#'
#' @param x any "MarxanData", "MarxanUnsolved", or "MarxanSolved" object.
#' @note This generic method does not work on "MaranResults" objects because they do not store this information.
#' @export
#' @seealso \code{\link{MarxanData-class}}, \code{\link{MarxanUnsolved-class}}, \code{\link{MarxanSolved-class}}
spfs<-function(x, ...) {UseMethod('spfs')}

#' @export
#' @rdname spfs
#' @inheritParams spfs
`spfs<-`<-function(x, ...) {UseMethod('spfs<-')}

#' Targets
#'
#' This function returns or assigns the targets for species in a Marxan object.
#'
#' @param x any "MarxanData", "MarxanUnsolved", or "MarxanSolved" object.
#' @param ... not used.
#' @note This generic method does not work on "MaranResults" objects because they do not store this information.
#' @export
#' @seealso \code{\link{MarxanOpts-class}}, \code{\link{MarxanData-class}}, \code{\link{MarxanUnsolved-class}}, \code{\link{MarxanSolved-class}}
targets<-function(x, ...) {UseMethod('targets')}

#' @rdname targets
#' @inheritParams targets
#' @export
`targets<-`<-function(x, ...) {UseMethod('targets<-')}


#' Maximum Targets
#'
#' This function returns or assigns the maximum targets for species in a Marxan object. Maximum targets are necessary for setting percent based targets.
#'
#' @param x any "MarxanData", "MarxanUnsolved", or "MarxanSolved" object.
#' @note This generic method does not work on "MaranResults" objects because they do not store this information.
#' @export
#' @seealso \code{\link{MarxanOpts-class}}, \code{\link{MarxanData-class}}, \code{\link{MarxanUnsolved-class}}, \code{\link{MarxanSolved-class}}
maxtargets<-function(x, ...) {UseMethod('targets')}

#' @export
`maxtargets<-`<-function(x, ...) {UseMethod('targets<-')}


#' Species identifiers
#'
#' This function returns or assigns the species ids for a Marxan object.
#'
#' @param x any "MarxanData", "MarxanUnsolved", or "MarxanSolved" object.
#' @note This generic method does not work on "MaranResults" objects because they do not store this information.
#' @export
#' @seealso \code{\link{MarxanOpts-class}}, \code{\link{MarxanData-class}}, \code{\link{MarxanUnsolved-class}}, \code{\link{MarxanSolved-class}}
sppids<-function(x, ...) {UseMethod('sppids')}


#' @export
#' @rdname sppids
#' @inheritParams sppids
`sppids<-`<-function(x, ...) {UseMethod('sppids<-')}


#' Planning unit identifiers
#'
#' This function returns or assigns the planning unit ids for a Marxan object.
#'
#' @param x any "MarxanOpts", "MarxanData", "MarxanUnsolved", or "MarxanSolved" object.
#' @note This generic method does not work on "MaranResults" objects because they do not store this information.
#' @export
#' @seealso \code{\link{MarxanOpts-class}}, \code{\link{MarxanData-class}}, \code{\link{MarxanUnsolved-class}}, \code{\link{MarxanSolved-class}}
puids<-function(x, ...) {UseMethod('puids')}

#' @rdname puids
#' @inheritParams puids
#' @export
`puids<-`<-function(x, ...) {UseMethod('puids<-')}


#' Planning unit costs
#'
#' This function returns or assigns the planing unit costs for a Marxan object.
#'
#' @param x any "MarxanOpts", "MarxanData", "MarxanUnsolved", or "MarxanSolved" object.
#' @param ... not used
#' @note This generic method does not work on "MaranResults" objects because they do not store this information.
#' @export
#' @seealso \code{\link{MarxanOpts-class}}, \code{\link{MarxanData-class}}, \code{\link{MarxanUnsolved-class}}, \code{\link{MarxanSolved-class}}
costs<-function(x, ...) {UseMethod('costs')}

#' @export
#' @rdname costs
`costs<-`<-function(x, ...) {UseMethod('costs<-')}

#' Planning unit initial status
#'
#' This function returns or assigns the planing unit initial statuses for a Marxan object.
#'
#' @param x any "MarxanData", "MarxanUnsolved", or "MarxanSolved" object.
#' @note This generic method does not work on "MaranOpts" or "MaranResults" objects because they do not store this information.
#' @export
#' @seealso \code{\link{MarxanOpts-class}}, \code{\link{MarxanData-class}}, \code{\link{MarxanUnsolved-class}}, \code{\link{MarxanSolved-class}}
inistatus<-function(x, ...) {UseMethod('inistatus')}

#' @export
#' @rdname inistatus
#' @inheritParams inistatus
`inistatus<-`<-function(object,value) {UseMethod('inistatus<-')}

#' Species names
#'
#' This function returns or assigns the species names for a Marxan object.
#'
#' @param x any "MarxanData", "MarxanUnsolved", or "MarxanSolved" object.
#' @note This generic method does not work on "MaranOpts" or "MaranResults" objects because they do not store this information.
#' @seealso \code{\link{MarxanOpts-class}}, \code{\link{MarxanData-class}}, \code{\link{MarxanUnsolved-class}}, \code{\link{MarxanSolved-class}}
#' @name names
NULL

#' Log
#'
#' This function returns the log file associated with running Marxan.
#'
#' @param x "MarxanResults" or "MarxanSolved" object.
#' @param ... not used.
#' @export
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanSolved}}, \code{\link{marxan}}
log<-function(x, ...) {UseMethod('log')}

#' @export
log.default<-base::log

#' Extract amount held for a solution
#'
#' This function returns the amount held for each species in a solution.
#'
#' @param x "MarxanResults" or "MarxanSolved" object.
#' @param y "NULL" to return all values, "integer" 0 to return values for best solution, "integer" value greater than 0 for \code{y}'th solution value.
#' @return "matrix" or "numeric" vector depending on arguments.
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanSolved}}, \code{\link{marxan}}
#' @export
amountheld<-function(x, ...) {UseMethod('amountheld')}


#' Extract occurrence held for a solution
#'
#' This function returns the number of occurrences held for each species in a solution.
#'
#' @param x "MarxanResults" or "MarxanSolved" object.
#' @param y "NULL" to return all values, "integer" 0 to return values for best solution, "integer" value greater than 0 for \code{y}'th solution value.
#' @return "matrix" or "numeric" vector depending on arguments.
#' @export
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanSolved}}, \code{\link{marxan}}
occheld<-function(x, ...) {UseMethod('occheld')}

#' Extract information on whether solutions have met targets
#'
#' This function reports whether a solution has met the targets for each species in a solution.
#'
#' @param x "MarxanResults" or "MarxanSolved" object.
#' @param y "NULL" to return all values, "integer" 0 to return values for best solution, "integer" value greater than 0 for \code{y}'th solution value.
#' @return "matrix" or "logical" vector depending on arguments.
#' @export
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanSolved}}, \code{\link{marxan}}
targetsmet<-function(x, ...) {UseMethod('targetsmet')}


#' Extract solution selection
#'
#' Extract selections for a given solution from a "MarxanResults or "MarxanSolved" object.
#'
#' @param x "MarxanResults or "MarxanSolved" object.
#' @param y "NULL" to return all solution selections, "integer" 0 to return selection for best solution, "integer" value greater than 0 for \code{y}'th solution selection.
#' @return "matrix" or "numeric" vector with planning units statuses depending on arguments.
#' @export
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanSolved-class}}, \code{\link{marxan}}.
selection<-function(x, ...) {UseMethod('selection')}

#' Extract minimum proportion met ratio
#'
#' Extract minimum proportion met (MPM) ratio from "MarxanResults" or "MarxanSolved" object.
#'
#' @param x "MarxanResults or "MarxanSolved" object.
#' @param y "NULL" to return all ratios, "integer" 0 to return ratios for best solution, "integer" value greater than 0 for \code{y}'th solution ratios.
#' @param ... not used.
#' @return "matrix" or "numeric" vector with solution ratios depending on arguments.
#' @export
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanSolved-class}}, \code{\link{marxan}}
mpm<-function(x, ...) {UseMethod('mpm')}

#' Extract separation achieved value
#'
#' Extract separation achieved value from "MarxanResults" or "MarxanSolved" object.
#'
#' @param x "MarxanResults or "MarxanSolved" object.
#' @param y "NULL" to return all values, "integer" 0 to return values for best solution, "integer" value greater than 0 for \code{y}'th solution values.
#' @return "matrix" or "numeric" vector with values depending on arguments.
#' @export
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanSolved-class}}, \code{\link{marxan}}
sepacheived<-function(x, ...) {UseMethod('sepacheived')}

#' Compare Marxan objects
#'
#' This function checks objects to see if they share the same input data.
#'
#' @param x "MarxanData", "MarxanUnsolved", or "MarxanSolved" objects.
#' @param y "MarxanData", "MarxanUnsolved", or "MarxanSolved" objects.
#' @return "logical" are the objects based on the same data?
#' @export
#' @seealso \code{\link{MarxanData-class}}, \code{\link{MarxanUnsolved-class}}, \code{\link{MarxanSolved-class}}
setGeneric("is.comparable", function(x, y) standardGeneric("is.comparable"))

#' Principle components Analysis on Marxan solutions
#'
#' This function runs an principle components analysis on Marxan solutions using various characteristics.
#' Results cached to permit rapid display for plotting functions.
#'
#' @param x "MarxanResults" or "MarxanSolved" object.
#' @param var "character" should solutions be compared based on selections ('solutions'), or the amount held ('amountheld'), number of occurances ('occheld'), or whether the targets have been met for each species ('targetsmet')?
#' @param ... arguments to \code{\link[stats]{prcomp}}.
#' @param force_reset "logical" should analysis be run even if it is stored in the cache?
#' @return "prcomp" object with Euclidean distances between rotated data for ordination plots.
#' @export
#' @seealso \code{\link{MarxanSolved-class}}, \code{\link{marxan}}, \code{\link{ordiplot}}, \code{\link{dendrogram}}
pca<-function(x, ...) {UseMethod("pca")}


#' Dissimilarity matrix for Marxan solutions
#'
#' This function calculates a dissimilty matrix for Marxan solutions using various characteristics.
#' Results cached to permit rapid display for plotting functions.
#'
#' @param x "MarxanResults" or "MarxanSolved" object.
#' @param var "character" should solutions be compared based on selections ('selections'), or the amount held ('amountheld'), number of occurances ('occheld'), or whether the targets have been met for each species ('targetsmet')?
#' @param method "character" name of distance metric to use for calculations (see \code{\link[vegan]{vegdist}})
#' @param force_reset "logical" should analysis be run even if it is stored in the cache?
#' @return "dist"  with dissimilarity indices.
#' @export
#' @seealso \code{\link{MarxanSolved-class}}, \code{\link{marxan}}, \code{\link{ordiplot}}, \code{\link{dendrogram}}
dist<-function(x,...) {UseMethod("dist")}

#' @export
dist.default<-stats::dist

#' Metric Dimensional Scaling for Marxan Solutions
#'
#' This function runs an metric dimensional scaling (using \code{\link[vegan]{monoMDS}}) on Marxan solutions using various characteristics.
#' Results cached to permit rapid display for plotting functions.
#'
#' @param x "MarxanResults" or "MarxanSolved" object.
#' @param var "character" should solutions be compared based on selections ('solutions'), or the amount held ('amountheld'), number of occurances ('occheld'), or whether the targets have been met for each species ('targetsmet')?
#' @param method "character" name of distance metric to use for calculating distance (see \code{\link[vegan]{vegdist}}).
#' @param ... additional arguments to \code{\link[vegan]{monoMDS}}.
#' @param force_reset "logical" should analysis be run even if it is stored in the cache?
#' @return "monoMDS" object.
#' @export
#' @seealso \code{\link{MarxanSolved-class}}, \code{\link{marxan}}, \code{\link{ordiplot}}, \code{\link{dendrogram}}
mds<-function(x, ...) UseMethod("mds")

#' Hierarchical Clustering for Marxan Solutions
#'
#' This function runs an hierarchical clustering on (using \code{\link[fastcluster]{hclust}}) on Marxan solutions using various characteristics.
#' Results cached to permit rapid display for plotting functions.
#'
#' @param x "MarxanResults" or "MarxanSolved" object.
#' @param type "character" use metric dimensional scaling ('mds'), or principle components analysis ('pca'), or distance matrix('dist') for analysis?
#' @param var "character" should solutions be compared based on selections ('solutions'), or the amount held ('amountheld'), number of occurances ('occheld'), or whether the targets have been met for each species ('targetsmet')?
#' @param ... arguments to \code{\link[vegan]{monoMDS}}, or \code{\link[stats]{prcomp}}.
#' @param force_reset "logical" should analysis be run even if it is stored in the cache?
#' @return "hclust" object with an extra element named "phylo" that stores a "phylo" representation for pretty plotting.
#' @export
#' @seealso \code{\link{MarxanSolved-class}}, \code{\link{marxan}}, \code{\link{ordiplot}}, \code{\link{dendrogram}}
hclust<-function(x, ...) UseMethod("hclust")

#' @export
#' @inheritParams hclust
#' @rdname hclust
hclust.default<-function(x, ...) {
	stats::hclust(d=x, ...)
}

#' Ordination plot of Marxan solutions
#'
#' This function makes an ordination plot to visualise the differences between solutions using dimension reduction techniques (ie. NMDS or PCA).
#' Numbers indicate solution ids. The size of the number on your screen indicates relative solution quality ('score').
#' Distance matrices and results from multivariate analyses are cached to permit rapid display.
#'
#' @param x "MarxanResults" or "MarxanSolved" object.
#' @param type "character" use 'mds' or 'pca' for analysis?
#' @param var "character" should solutions be compared based on selections ('selections'), or the amount held ('amountheld'), number of occurances ('occheld'), or whether the targets have been met for each species ('targetsmet')?
#' @param nbest "integer" color the n best solutions in "red"
#' @param ... arguments to \code{\link[vegan]{monoMDS}}, \code{\link[stats]{prcomp}}, \code{\link{mds.MarxanResults}}, and \code{\link{pca.MarxanResults}}. For instance, to use euclidean distances, use dist='euclidean'.
#' @param force_reset "logical" should analysis be rerun even if it is stored in the cache?
#' @return "prcomp" or "monoMDS" object.
#' @export
#' @seealso \code{\link{MarxanSolved-class}}, \code{\link{marxan}}.
ordiplot<-function(x, ...) UseMethod("ordiplot")

#' Plot species distribution in planning units
#'
#' This function plots the density of species in planning units in "Marxan" objects.
#'
#' @param x "MarxanData", "MarxanUnsolved", "MarxanSolved" object.
#' @param y "character" name of species, or "integer" species id.
#' @param var "character" \itemize{
#' \item if 'amount': sum density of \code{y} species in planning units is shown.
#' \item if 'occ': number of \code{y} species in planning units is shown.
#' }
#' @param basemap "character" name of google base map. Defaults to "none".
#' @param colramp "character" name of color ramp. See color palettes available in \code{\link[RColorBrewer]{brewer.pal.ino}}
#' @param alpha "numeric" between 0 and 1 describing how transparent the colors should be. Defaults to 1 if \code{basemap}='none', otherwise defaults to 0.7.
#' @param grayscale "logical" Shoould be basemap be gray scaled?
#' @param force_reset "logical" should base map image be downloaded even if it is stored in the cache?
#' @export
#' @seealso \code{\link{MarxanData-class}}, \code{\link{MarxanUnsolved-class}}, \code{\link{MarxanData-class}}, \code{\link[RgoogleMaps]{GetMap}}, \code{\link[RgoogleMaps]{PlotOnStaticMap}}
spplot<-function(x, ...) UseMethod("spplot")


#' Dendrogram of Marxan solutions
#'
#' This function makes a dendrogram to visualise differences between Marxan solutions using hierarchical clustering using various characteristics. 
#' Clustering can be applied to distances between raw data, or to results from dimensional reduction analysis (ie. MDS and PCA).
#' Distance matrices and results from multivariate analyses are cached to permit rapid display.
#'
#' @param x "MarxanResults" or "MarxanSolved" object.
#' @param type "character" use 'nmds' or 'pca' or 'dist' for analysis?
#' @param var "character" should solutions be compared based on selections ('selections'), or the amount held ('amountheld'), number of occurances ('occheld'), or whether the targets have been met for each species ('targetsmet')?
#' @param nbest "integer" color the n best solutions in "red".
#' @param ... arguments to \code{\link[vegan]{monoMDS}}, \code{\link[stats]{prcomp}}, \code{\link{nmds.MarxanResults}}, and \code{\link{pca.MarxanResults}}.
#' @param force_reset "logical" should analysis be rerun even if it is stored in the cache?
#' @return "prcomp" or "monoMDS" object.
#' @export
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanSolved-class}}, \code{\link{marxan}}
dendrogram<-function(x, ...) UseMethod("dendrogram")

#' Dot chart of Marxan solutions
#'
#' This function makes a dot chart to visualise differences between Marxan solutions using summary variables. Size of dots indicate overall solution quality (ie. 'score').
#' 
#' @param x "MarxanResults" or "MarxanSolved".
#' @param var "character" What variable should be used to compare solutions?
#' @param n "integer" Number of solutions to plot. Defaults to 50.
#' @param nbest "integer" Color the n best solutions in "red".
#' @note Supported variables are shown below.
#' \tabular{ccl}{
#' 	Short name \tab Full name \tab Description\cr
#' 	'score' \tab 'Score' \tab Quality of solution.\cr
#' 	'cost' \tab 'Cost' \tab Total cost of solution.\cr
#' 	'npu' \tab 'Planning_Units' \tab Number of planning units selected in prioritisation.\cr
#' 	'con' \tab 'Connectivity' \tab Sum boundary length of all edges in selected planning units' that have neighbours.\cr
#' 	'confrac' \tab 'Connectivity_In_Fraction' \tab Connectivity efficiency relative to total boundary length.\cr
#' 	'conin' \tab 'Connectivity_In' \tab Sum boundary length of edges belonging to selected planning units' that have neighbours.\cr
#' 	'conout' \tab 'Connectivity_Out' \tab Sum boundary length of edges belonging to selected planning units' that have neighbours.\cr
#'  'penalty' \tab 'Penalty' \tab Total species penalty.\cr
#'  'shortfall' \tab 'Shortfall' \tab Total shortfall for species targets.\cr
#'  'mv' \tab 'Missing_Values' \tab Number of species that do not have their targets met.\cr
#' }
#' @export
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanSolved-class}}, \code{\link{marxan}}
dotchart<-function(x, ...) UseMethod("dotchart")

#' @export
dotchart.default<-graphics::dotchart

#' Basemap 
#'
#' This function retrieves google map data for planning units. The google map data is cached to provide fast plotting capabilities.
#'
#' @param x "MarxanData", "MarxanUnsolved", "MarxanSolved" object.
#' @param basemap "character" name of base map to display. Valid names are "roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid".
#' @param grayscale "logical" should base map be gray scale?
#' @param force_reset "logical" ignore data in cache? Setting this as ignore will make function slower but may avoid bugs in cache system.
#' @return "list" with google map data.
#' @export
#' @seealso \cite{\link[RgoogleMaps]{GetMap.bbox}}, \cite{\link{plot}}
basemap<-function(x, ...) {UseMethod("basemap")}

#' Plot Marxan solutions
#'
#' This function makes a geoplot displaying Marxan solutions.
#'
#' @param x "MarxanData", "MarxanUnsolved", "MarxanSolved" object.
#' @param y See below for details:
#'	\itemize{ 
#' 	\item if "missing": function plots the selection frequency of planning units for all solutions.
#'	\item if "integer": function plots the selection for the n'th solution, use 0 to plot the best solution.
#'	\item if "character": set to name of species to display its density, or "all" for sum density of all species, or "rich" to get species richness in each planning unit.
#'	\item if "MarxanSolved" plots the difference in solutions: \itemize{
#'		\item if i is "NULL": differences in selection frequencies are plotted.
#'		\item if i is "integer" and j is "integer", plots differences selection status for solution i in x, and solution j in y. Set i or j to 0 to refer to the best solution in x or y respectively.
#'		}
#' }
#' @param colramp "character" name of colour palette (see \code{\link[RColorBrewer]{brewer.pal.info}}).
#' @param xlockedincol "character" color to plot locked in planning units in object \code{x}.
#' @param ylockedincol "character" color to plot locked in planning units in object \code{y}.
#' @param xlockedoutcol "character" color to plot locked out planning units in object \code{x}.
#' @param ylockedoutcol "character" color to plot locked out planning units in object \code{y}.
#' @param alpha "numeric" alpha value
#' @param basemap "character" name of basemap to display. Valid names are "roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid".
#' @param grayscale "logical" should basemap be gray scale?
#' @param force_reset "logical" ignore data in cache? Setting this as ignore will make function slower but may avoid bugs in cache system.
#' @note This function will return an error if spatial polygons were not supplied during the construction of the Marxan object.
#' @export
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanSolved-class}}, \code{\link{basemap}} \code{\link{marxan}}, \code{\link{dendrogram}}, \code{\link{dotchart}}, \code{\link{ordiplot}}, \code{\link{spplot}}
setGeneric("plot")

#' Update Marxan inputs
#'
#' This function lets you update Marxan objects to obtain a new object with altered inputs. This is particularly useful if you want to rerun a Marxan
#' analysis but after changing a few parameters. For example, you might decide that you need to rerun a Marxan analysis with a higher number of iterations, or with a different
#' boundary length modifier. In effect, this lets you take an existing "MarxanUnsolved"/"MarxanSolved" object, change specific inputs, and run a Marxan analysis using the tweaked inputs.
#'
#' @param x "MarxanOpts", "MarxanData", "MarxanUnsolved", or "MarxanSolved" object.
#' @param formula "formula" with update commands (see examples).
#' @param sovle "logical" should solutions be generated for the new object?
#' @return "MarxanSolved" or "MarxanUnsolved" object depending on solve argument.
#' @seealso \code{\link{MarxanSolved-class}}, \code{\link{MarxanUnsolved-class}}, \code{\link{marxan}}, \code{\link{opt}}, \code{\link{spp}}, \code{\link{pu}}
#' @name update
NULL

#' Test if hash is cached in a Marxan object
#'
#' Tests if hash is cached in Marxan object.
#' 
#' @param x "MarxanData" or "MarxanResults" object
#' @param name "character" hash.
#' @note caches are implemented using environments, the hash is used as the name of the object in the environment.
#' @return "logical" Is it cached?
#' @keywords internal
setGeneric("is.cached", function(x,name) standardGeneric("is.cached"))

#' Get and Set Cache Methods
#'
#' Getter and setter methods for caches in MarxanData and MarxanResults object.
#' 
#' @param x "MarxanData" or "MarxanResults" object
#' @param name "character" hash.
#' @param y if "ANY" this object gets cached with name, else if "missing" the object hashed at name gets returned.
#' @note caches are implemented using environments, the hash is used as the name of the object in the environment.
#' @return "any" or "NULL" depends on y argument.
#' @keywords internal
setGeneric("cache", function(x, name, y) standardGeneric("cache"))

