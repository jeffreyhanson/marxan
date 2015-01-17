#' General Marxan Function
#'
#' This is a general function to create Marxan objects from scratch and run the Marxan program to generate solutions.
#'
#' @param ... arguments are passed to MarxanData and MarxanOpts functions
#' @solve "logical" should the problem be solved using Marxan?
#' 
#' @return "MarxanSolved"  or "MarxanUnsolved"
#' @seealso \code{\link{MarxanOpts}}, \code{\link{MarxanData}}, \code{\link{MarxanResults}}, \code{\link{MarxanUnsolved}} 
marxan<-function(..., solve=TRUE) {
	x=new('Marxan', data=data, opts=opts, results=NULL)
	if (eval)
		x<-run(x, ...)
	return(x)
}

