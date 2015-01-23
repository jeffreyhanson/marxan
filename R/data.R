
#' Broad vegetation data
#'
#' "RasterLayer" data for the distribution of 63 vegetation classes in Tasmania, Australia. This data was sourced from the "Introduction to Marxan" course.
#'
#' @name tasinvis
#' @docType data
#' @usage data(tasinvis)
#' @format An object of class \code{"RasterLayer"}.
#' @keywords datasets
#' @seealso \code{\link{taspu}}
#' @examples
#' data(tasinvis)
#' plot(tasinvis)
"tasinvis"

#' Planning units for example reserve selection problem
#'
#' "SpatialPolygonsDataFrame" data for planning units in Tasmania, Australia. This data was sourced from the "Introduction to Marxan" course. The 'data' slot contains a 'data.frame' with three fields: 
#' \itemize{
#' 	\item The 'id' field contains "integer" planning unit ids.
#' 	\item The 'cost' field contains "numeric" unimproved land values to represent acquisition costs.
#' 	\item The 'status' field indicates the level of existing protection in each planning unit. Units with 50\% or more of their area contained in IUCN protected areas are associated with a value of 2; otherwise they are associated with a value of 0.
#' }
#'
#' @name taspu
#' @docType data
#' @usage data(taspu)
#' @format An object of class \code{"SpatialPolygonsDataFrame"}.
#' @keywords datasets
#' @seealso \code{\link{tasinvis}}
#' @examples
#' data(taspu)
#' plot(taspu)
"taspu"

