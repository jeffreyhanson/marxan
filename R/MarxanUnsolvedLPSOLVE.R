#' @include RcppExports.R marxan-internal.R misc.R generics.R MarxanOpts.R MarxanOptsLpsolve.R MarxanData.R MarxanUnsolved.R
NULL

#' MarxanUnsolvedLpsolve: An S4 class to represent Marxan input parameter and data for lpsolve
#'
#' This class is used to store Marxan input parameters and data to generate solutions using the lpsolve program.
#'
#' @slot opts "MarxanOptsLpsolve" object used to store input parameters.
#' @slot data "MarxanData" object used to store input data.
#' @export
#' @seealso  \code{\link{MarxanOptsLpsolve-class}}, \code{\link{MarxanData-class}}.
setClass("MarxanUnsolvedLpsolve",
	contains="MarxanUnsolved"
	representation(
		opts="MarxanOptsLpsolve"
	)
)

#' Create a new "MarxanUnsolvedLpsolve" object
#'
#' This function creates a "MarxanUnsolvedLpsolve" object using "MarxanOptsLpsolve" and "MarxanData" objects.
#'
#' @param opts "MarxanOptsLpsolve" object.
#' @param data "MarxanData" object.
#' @return "MarxanUnsolvedLpsolve" object.
#' @export
#' @seealso \code{\link{MarxanUnsolvedLpsolve-class}}, \code{\link{MarxanOptsLpsolve-class}}, \code{\link{MarxanData-class}}.
MarxanUnsolvedLPSOLVE<-function(opts, data) {
	return(new("MarxanUnsolvedLpsolve", opts=opts, data=data))
}

#' @rdname solve
#' @inheritParams solve
#' @export
solve.MarxanUnsolvedLpsolve=function(x, algorithm="ILP", verbose=1, wd=tempdir(), seed=sample.int(n=10000L, size=1), clean=(wd==tempdir())) {
	## init
	# check argument validity
	match.arg(algorithm, c('ILP','LP'))
	match.arg(verbose, 1:3)
	switch(verbose,
		0={verbose<-'neutral'},
		1={verbose<-'normal'},
		2={verbose<-'detailed'},
		3={verbose<-'full'}
	)
	
	## prelim
	# prepare model
	writeLines(lpsolvemodel(x, algorithm), file=file.path(wd, 'marxan.lp'))
	mod<-read.lp(file.path(wd, 'marxan.lp'), type='lp')
	do.call('lp.control', append(list(lprec=mod, reset=FALSE),  as.list.MarxanOpts(x@opts))
	
	## main
	# solve model
	ret<-solve(mod)
	if (verbose>0)
		switch(ret,
			0={cat('optimal solution found\n')},
			1={cat('the model is sub-optimal\n')},
			2={cat('the model is infeasible\n')},
			3={cat('the model is unbounded\n')},
			4={cat('the model is degenerate\n')},
			5={cat('numerical failure encountered\n')},
			6={cat('process aborted\n')},
			7={cat('timeout\n')},
			8={cat('unknown error\n')},
			9={cat('the model was solved by presolve\n')},
			10={cat('the branch and bound routine failed\n')},
			11={cat('the branch and bound was stopped because of a break-at-first or break-at-value\n')},
			12={cat('a feasible branch and bound solution was found\n')},
			13={cat('no feasible branch and bound solution was found\n')}
		)
	
	## post
	# clean working files
	if (clean)
		unlink(file.path(wd, 'marxan.lp'))
	# return solved object
	return(
		new(
			paste0('MarxanSolvedLpsolve',algorithm)
				opts=x@opts
				data=x@data
				results=format.MarxanResults(get.variables(mod)[grep('^pu.*$',colnames(mod))],x@data)
		)
	)
}


