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
#' @return Marxan object
#' @export
#' @note This function is used to solve a MarxanUnsolved object that has all of its inputs generated. The marxan function (without lower case 'm') provides a more general interface for generating inputs and outputs for Marxan.
solve<-function(x, ...) {UseMethod('solve')}

#' Extract solution selection
#'
#' Extract selections for a given solution from a "MarxanResults or "MarxanSolved" object.
#'
#' @param x "MarxanResults or "MarxanSolved" object.
#' @param y 'best' to return selection for best solution, 'all' to return all solutions, or "integer" to return y'th solution.
#' @return "matrix" or "numeric" vector with planning units statuses depending on arguments.
#' @export
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanSolved-class}}, \code{\link{marxan}}.
selection<-function(x, ...) {UseMethod('selection')}

#' Extract solution score
#'
#' Extract solution score from "MarxanResults" or "MarxanSolved" object.
#'
#' @param x "MarxanResults or "MarxanSolved" object.
#' @param y 'best' to return best solution score, 'all' to return all scores, or "integer" to return score for y'th solution.
#' @return "matrix" or "numeric" vector with solution scores depending on arguments.
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanSolved-class}}, \code{\link{marxan}}
score<-function(x, ...) {UseMethod('score')}

#' Summary
#'
#' This function returns the summary table output by Marxan.
#'
#' @param x "MarxanSolved" or "MarxanResults" object.
#' @export
#' @return "data.frame" with solution information.
#' @seealso \code{\link{MarxanSolved-class}}, \code{\link{MarxanResults}}, \code{\link{marxan}}
summary<-function(x, ...) {UseMethod('summary')}

#' Print
#'
#' This function returns information about a Marxan object.
#'
#' @param x any "MarxanOpts", "MarxanData", "MarxanResults", "MarxanUnsolved", or "MarxanSolved" object.
#' 
#' @seealso \code{\link{MarxanOpts-class}}, \code{\link{MarxanData-class}}, \code{\link{MarxanResults-class}}, \code{\link{MarxanUnsolved-class}}, \code{\link{MarxanSolved-class}}, 
print<-function(x, ...) {UseMethod('print')}

#' Names
#'
#' This function returns the species names for a Marxan object.
#'
#' @param x any "MarxanOpts", "MarxanData", "MarxanUnsolved", or "MarxanSolved" object.
#' @note "MaranResults" objects do not store species names.
#' @seealso \code{\link{MarxanOpts-class}}, \code{\link{MarxanData-class}}, \code{\link{MarxanUnsolved-class}}, \code{\link{MarxanSolved-class}}
# names<-function(x, ...) {UseMethod('names')}

#' Log
#'
#' This function returns the log file associated with running Marxan.
#'
#' @param x "MarxanResults" or "MarxanSolved" object.
#' @export
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanSolved}}, \code{\link{marxan}}
log<-function(x, ...) {UseMethod('log')}


#' Extract amount held for a solution
#'
#' This function returns the amount held for each species in a solution.
#'
#' @param x "MarxanResults" or "MarxanSolved" object.
#' @return "matrix" or "numeric" vector depending on arguments.
#' @export
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanSolved}}, \code{\link{marxan}}
amountheld<-function(x, ...) {UseMethod('amountheld')}


#' Extract occurrence held for a solution
#'
#' This function returns the number of occurrences held for each species in a solution.
#'
#' @param x "MarxanResults" or "MarxanSolved" object.
#' @return "matrix" or "numeric" vector depending on arguments.
#' @export
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanSolved}}, \code{\link{marxan}}
occheld<-function(x, ...) {UseMethod('occheld')}

#' Extract information on whether solutions have met targets
#'
#' This function reports whether a solution has met the targets for each species in a solution.
#'
#' @param x "MarxanResults" or "MarxanSolved" object.
#' @return "matrix" or "logical" vector depending on arguments.
#' @export
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanSolved}}, \code{\link{marxan}}
targetsmet<-function(x, ...) {UseMethod('targetsmet')}

#' Compare Marxan objects
#'
#' This function checks objects to see if they share the same input data.
#'
#' @param x "MarxanData", "MarxanUnsolved", or "MarxanSolved" objects.
#' @return "logical" are the objects based on the same data?
#' @export
#' @seealso \code{\link{MarxanData-class}}, \code{\link{MarxanUnsolved-class}}, \code{\link{MarxanSolved-class}}
setGeneric("is.comparable", function(x, y) standardGeneric("is.comparable"))

#' Principle components Analysis on Marxan solutions
#'
#' This function runs an principle components analysis on Marxan solutions using various characteristics.
#' Results cached to permit rapid display for plotting functions.
#'
#' @param x "MarxanSolved" object.
#' @param var "character" should solutions be compared based on selections ('solutions'), or the amount held ('amountheld'), number of occurances ('occheld'), or whether the targets have been met for each species ('targetsmet')?
#' @param ... arguments to \code{\link[stats]{prcomp}}.
#' @param force_reset "logical" should analysis be run even if it is stored in the cache?
#' @return "prcomp" object with Euclidean distances between rotated data for ordination plots.
#' @export
#' @seealso \code{\link{MarxanSolved-class}}, \code{\link{marxan}}, \code{\link{ordiplot}}, \code{\link{dendrogram}}
pca<-function(x) {UseMethod("pca")}


#' Dissimilarity matrix for Marxan solutions
#'
#' This function calculates a dissimilty matrix for Marxan solutions using various characteristics.
#' Results cached to permit rapid display for plotting functions.
#'
#' @param x "MarxanSolved" object.
#' @param var "character" should solutions be compared based on selections ('selections'), or the amount held ('amountheld'), number of occurances ('occheld'), or whether the targets have been met for each species ('targetsmet')?
#' @param method "character" name of distance metric to use for calculating distance (see \code{\link[vegan]{vegdist}})
#' @param force_reset "logical" should analysis be run even if it is stored in the cache?
#' @return "dist"  with dissimilarity indices.
#' @export
#' @seealso \code{\link{MarxanSolved-class}}, \code{\link{marxan}}, \code{\link{ordiplot}}, \code{\link{dendrogram}}
dist<-function(x) {UseMethod("dist")}
dist.default<-stats::dist

#' Metric Dimensional Scaling for Marxan Solutions
#'
#' This function runs an metric dimensional scaling (using \code{\link[vegan]{monoMDS}}) on Marxan solutions using various characteristics.
#' Results cached to permit rapid display for plotting functions.
#'
#' @param x "MarxanSolved" object
#' @param var "character" should solutions be compared based on selections ('solutions'), or the amount held ('amountheld'), number of occurances ('occheld'), or whether the targets have been met for each species ('targetsmet')?
#' @param method "character" name of distance metric to use for calculating distance (see \code{\link[vegan]{vegdist}}).
#' @param ... additional arguments to \code{\link[vegan]{monoMDS}}.
#' @param force_reset "logical" should analysis be run even if it is stored in the cache?
#' @return "monoMDS" object.
#' @export
#' @seealso \code{\link{MarxanSolved-class}}, \code{\link{marxan}}, \code{\link{ordiplot}}, \code{\link{dendrogram}}
mds=function(x, ...) UseMethod("mds")

#' Hierarchical Clustering for Marxan Solutions
#'
#' This function runs an hierarchical clustering on (using \code{\link[fastcluster]{hclust}}) on Marxan solutions using various characteristics.
#' Results cached to permit rapid display for plotting functions.
#'
#' @param x "MarxanSolved" object
#' @param type "character" use metric dimensional scaling ('mds'), or principle components analysis ('pca'), or distance matrix('dist') for analysis?
#' @param  "character" should solutions be compared based on selections ('solutions'), or the amount held ('amountheld'), number of occurances ('occheld'), or whether the targets have been met for each species ('targetsmet')?
#' @param method "character" name of distance metric to use for calculating distance (see \code{\link[vegan]{vegdist}}).
#' @param ... arguments to \code{\link[vegan]{monoMDS}}, or \code{\link[stats]{prcomp}}.
#' @param force_reset "logical" should analysis be run even if it is stored in the cache?
#' @return "hclust" object with an extra element named "phylo" that stores a "phylo" representation for pretty plotting.
#' @export
#' @seealso \code{\link{MarxanSolved-class}}, \code{\link{marxan}}, \code{\link{ordiplot}}, \code{\link{dendrogram}}
hclust=function(x, ...) UseMethod("mds")

#' Ordination plot of Marxan solutions
#'
#' This function makes an ordination plot to visualise the differences between solutions using dimension reduction techniques (ie. NMDS or PCA).
#' Numbers indicate solution ids. The size of the number on your screen indicates relative solution quality ('score').
#' Distance matrices and results from multivariate analyses are cached to permit rapid display.
#'
#' @param x "MarxanSolved" object
#' @param type "character" use 'mds' or 'pca' for analysis?
#' @param var "character" should solutions be compared based on selections ('selections'), or the amount held ('amountheld'), number of occurances ('occheld'), or whether the targets have been met for each species ('targetsmet')?
#' @param nbest "integer" color the n best solutions in "red"
#' @param ... arguments to \code{\link[vegan]{monoMDS}}, \code{\link[stats]{prcomp}}, \code{\link{mds.MarxanResults}}, and \code{\link{pca.MarxanResults}}. For instance, to use euclidean distances, use dist='euclidean'.
#' @param force_reset "logical" should analysis be rerun even if it is stored in the cache?
#' @return "prcomp" or "monoMDS" object.
#' @export
#' @seealso \code{\link{MarxanSolved-class}}, \code{\link{marxan}}.
ordiplot=function(x, ...) UseMethod("ordiplot")

#' Dendrogram of Marxan solutions
#'
#' This function makes a dendrogram to visualise differences between Marxan solutions using hierarchical clustering using various characteristics. 
#' Clustering can be applied to distances between raw data, or to results from dimensional reduction analysis (ie. MDS and PCA).
#' Distance matrices and results from multivariate analyses are cached to permit rapid display.
#'
#' @param x "MarxanSolved" object.
#' @param type "character" use 'nmds' or 'pca' or 'dist' for analysis?
#' @param var "character" should solutions be compared based on selections ('selections'), or the amount held ('amountheld'), number of occurances ('occheld'), or whether the targets have been met for each species ('targetsmet')?
#' @param nbest "integer" color the n best solutions in "red".
#' @param ... arguments to \code{\link[vegan]{monoMDS}}, \code{\link[stats]{prcomp}}, \code{\link{nmds.MarxanResults}}, and \code{\link{pca.MarxanResults}}.
#' @param force_reset "logical" should analysis be rerun even if it is stored in the cache?
#' @return "prcomp" or "monoMDS" object.
#' @export
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanSolved-class}}, \code{\link{marxan}}
dendrogram=function(x, ...) UseMethod("dendrogram")

#' Dot chart of Marxan solutions
#'
#' This function makes a dot chart to visualise differences between Marxan solutions using summary variables. Size of dots indicate overall solution quality (ie. 'score').
#' Supported summary variables are:
#' \tabular{rrr} {
#' 	short name \tab full name \tab Description\cr
#' 	'score' \tab 'Score' \tab quality of solution.\cr
#' 	'cost' \tab 'Cost' \tab total cost of solution.\cr
#' 	'npu' \tab 'Planning_Units' \tab number of planning units selected in prioritisation.\cr
#' 	'con' \tab 'Connectivity' \tab sum boundary length of all edges in selected planning units' that have neighbours.\cr
#' 	'confrac' \tab 'Connectivity_In_Fraction' \tab connectivity efficiency relative to total boundary length.\cr
#' 	'conin' \tab 'Connectivity_In' \tab sum boundary length of edges belonging to selected planning units' that have neighbours.\cr
#' 	'conout' \tab 'Connectivity_Out' \tab sum boundary length of edges belonging to selected planning units' that have neighbours.\cr
#'  'penalty' \tab 'Penalty' \tab total species penalty.\cr
#'  'shortfall' \tab 'Shortfall' \tab total shortfall for species targets.\cr
#'  'mv' \tab 'Missing_Values' \tab number of species that do not have their targets met.\cr
#' }
#' 
#' @param x "MarxanResults" or "MarxanSolved".
#' @param var "character" what variable should be used to compare solutions?
#' @param nbest "integer" color the n best solutions in "red".
#' @export
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanSolved-class}}, \code{\link{marxan}}
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
#' @export
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanSolved-class}}, \code{\link{basemap}} \code{\link{marxan}}, \code{\link{dendrogram}}, \code{\link{dotchart}}, \code{\link{ordiplot}}
setGeneric("plot", function(x,y, ...) standardGeneric("plot"))


#' Update Marxan inputs
#'
#' This function lets you update Marxan objects to obtain a new object with altered inputs. This is particularly useful if you want to rerun a Marxan
#' analysis but after changing a few parameters. For example, you might decide that you need to rerun a Marxan analysis with a higher number of iterations, or with a different
#' boundary length modifier. In effect, this lets you take an existing "MarxanUnsolved"/"MarxanSolved" object, change specific inputs, and run a Marxan analysis using the tweaked inputs.
#'
#' @param x "MarxanOpts", "MarxanData", "MarxanUnsolved", or "MarxanSolved" object.
#' @param formula "formula" with update commands (see examples).
#' @param evaluate "logical" should solutions be generated for the new object?
#' @return "MarxanSolved" or "MarxanUnsolved" object depending on solve argument.
#' @export
#' @seealso \code{\link{MarxanSolved-class}}, \code{\link{MarxanUnsolved-class}}, \code{\link{marxan}}, \code{\link{opt}}, \code{\link{spp}}, \code{\link{pu}}
update<-function(x, formula, solve, ...) UseMethod("update")

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


#' Test if hash already assigned to an object in a cache
#'
#' Tests if hash is cached in Marxan object. Hashes are generated using function calls.
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

