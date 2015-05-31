#' @include RcppExports.R marxan-internal.R misc.R generics.R MarxanOpts.R MarxanData.R
NULL

#' MarxanUnsolved: An S4 class to represent Marxan inputs
#'
#' This class is used to store Marxan input data and input parameters.
#'
#' @slot opts "MarxanOpts" object used to store input parameters.
#' @slot data "MarxanData" object used to store input data.
#' @export
#' @seealso  \code{\link{MarxanOpts-class}}, \code{\link{MarxanData-class}}.
setClass("MarxanUnsolved",
	representation(
		data="MarxanData",
		opts="MarxanOpts"
	)
)

#' Create a new "MarxanUnsolved" object
#'
#' This function creates a "MarxanUnsolved" object using "MarxanOpts" and "MarxanData" objects.
#'
#' @param opts "MarxanOpts" object.
#' @param data "MarxanData" object.
#' @return "MarxanUnsolved" object.
#' @export
#' @seealso \code{\link{MarxanOpts-class}}, \code{\link{MarxanData-class}}.
MarxanUnsolved<-function(opts, data) {
	return(new("MarxanUnsolved", opts=opts, data=data))
}


#' @export
print.MarxanUnsolved=function(x) {
	cat("MarxanUnsolved object.\n")
	print.MarxanOpts(x@opts, FALSE)
	print.MarxanData(x@data, FALSE)
}

#' @export
setMethod(
	'show',
	'MarxanUnsolved',
	function(object)
		print.MarxanUnsolved(object)
)


#' @rdname basemap
#' @export
basemap.MarxanUnsolved<-function(x, basemap="none", grayscale=FALSE, force_reset=FALSE) {
	return(basemap.MarxanData(x@data, basemap, grayscale, force_reset))
}

#' @rdname spplot
#' @inheritParams spplot
#' @export
setMethod(
	"spplot",
	signature(obj='MarxanUnsolved'),
	function(obj, y=obj@data@species$id, var='amount', basemap="none", colramp="YlOrRd", alpha=ifelse(basemap=="none", 1, 0.7), grayscale=FALSE, force_reset=FALSE) {
		return(spplot(obj@data, y, var, basemap, colramp, alpha, grayscale, force_reset))
	}
)


#' @export
#' @rdname update
update.MarxanUnsolved<-function(x, formula, solve=TRUE, force_reset=TRUE) {
	m<-MarxanUnsolved(
		opts=update.MarxanOpts(x@opts, formula),
		data=update.MarxanData(x@data, formula, force_reset)
	)
	if (solve)
		m<-solve.MarxanUnsolved(m)
	return(m)
}

#' @export
#' @rdname names
#' @inheritParams names
names.MarxanUnsolved<-function(x) {
	return(names.MarxanData(x@data))
}

#' @export
#' @rdname names
#' @inheritParams names
`names<-.MarxanUnsolved`<-function(x,value) {
	stopifnot(length(value)==nrow(x@data@species) & !anyDuplicated(value) & is.character(value) & !any(is.na(value)))
	x@data@species$name<-value
	return(x)
}


#' @export
#' @rdname spfs
#' @inheritParams spfs
spfs.MarxanUnsolved<-function(x) {
	return(spfs.MarxanData(x@data))
}

#' @export
#' @rdname spfs
#' @inheritParams spfs
`spfs<-.MarxanUnsolved`<-function(x,value) {
	stopifnot(is.numeric(value) & !any(is.na(value)))
	x@data@species$spf<-value
	return(x)
}

#' @export
#' @rdname targets
#' @inheritParams targets
targets.MarxanUnsolved<-function(x) {
	return(targets.MarxanData(x@data))
}

#' @export
#' @rdname targets
#' @inheritParams targets
`targets<-.MarxanUnsolved`<-function(x,value) {
	stopifnot(is.numeric(value) & !any(is.na(value)))
	x@data@species$target<-value
	return(x)
}

#' @export
#' @describeIn maxtargets
maxtargets.MarxanUnsolved<-function(x) {
	return(maxtargets.MarxanData(x@data))
}

#' @export
#' @describeIn maxtargets
`maxtargets<-.MarxanUnsolved`<-function(x,value) {
	stopifnot(is.numeric(value) & !any(is.na(value)))
	x@data@species$maxtargets<-value
	return(x)
}

#' @export
#' @rdname sppids
#' @inheritParams sppids
sppids.MarxanUnsolved<-function(x) {
	return(sppids.MarxanData(x@data))
}

#' @export
#' @rdname sppids
#' @inheritParams sppids
`sppids<-.MarxanUnsolved`<-function(x,value) {
	stopifnot(length(value)==nrow(x@data@species) & !anyDuplicated(value) & is.integer(value) & !any(is.na(value)))
	x@data@species$id<-value
	return(x)
}

#' @export
#' @rdname puids
#' @inheritParams puids
puids.MarxanUnsolved<-function(x) {
	return(puids.MarxanData(x@data))
}

#' @export
#' @rdname puids
#' @inheritParams puids
`puids<-.MarxanUnsolved`<-function(x,value) {
	stopifnot(length(value)==nrow(x@data@pu) & !anyDuplicated(value) & is.integer(value) & !any(is.na(value)))
	x@data@pu$id<-value
	return(x)
}

#' @export
#' @rdname costs
costs.MarxanUnsolved<-function(x) {
	return(costs.MarxanData(x@data))
}

#' @export
#' @rdname costs
`costs<-.MarxanUnsolved`<-function(x,value) {
	stopifnot(is.numeric(value) & !any(is.na(value)))
	x@data@pu$costs<-value
	return(x)
}

#' @export
#' @rdname inistatus
#' @inheritParams inistatus
inistatus.MarxanUnsolved<-function(x) {
	return(inistatus.MarxanData(x@data))
}

#' @export
#' @rdname inistatus
#' @inheritParams inistatus
`inistatus<-.MarxanUnsolved`<-function(x,value) {
	stopifnot(is.numeric(value) & !any(is.na(value)))	
	x@data@pu$status<-value
	return(x)
}

#' @export
gurobimoodel<-function(x, problem='ILP') {




















}


lpsolvemodel<-function(x, problem='ILP') {
	## calculations
	# prelim calcs
	softSpp<-which(is.finite(x@data@species$spf))
	firmSpp<-which(!is.finite(x@data@species$spf))
	edge<-which(x@data@boundary$id1==x@data@boundary$id2)
	notEdge<-which(x@data@boundary$id1!=x@data@boundary$id2)
	puFree<-which(x@data$pu$status<2)
	puLockedIn<-which(x@data@pu$status==2)
	puLockedOut<-which(x@data@pu$status==3)
	# decision variables
	sf.vars<-paste0('sf',x@data@species$id[softSpp])
	pu.vars<-paste0('pu',x@data@pu$id)
	puxorpu.vars<-paste0('bl_pu',x@data@boundary$id1[noEdge],'_pu',x@data@boundary$id2[notEdge])
			
	## parse model text
	# init
	mod.fun<-paste0(x@data@pu$cost[puFree], ' ', pu.vars[puFree])
	mod.constrs<-c()

	# solution boundary data
	if (!isTRUE(all.equal(x@opts@BLM,0))) {
		# lock pus in solution
		if (length(puLockedIn)>0) {
			mod.fun<-c(mod.fun,paste0(x@data@pu$cost[puLockedIn], ' ', pu.vars[puLockedIn]))
			mod.constrs<-c(
				mod.constrs,
				'/* lock pus in optimal solution */',
				paste0('pu',x@data@pu[puLockedIn],' = 1;')
			)
		}
		# lock pus out solution
		if (length(puLockedOut)>0) {
			mod.fun<-c(mod.fun,paste0(x@data@pu$cost[puLockedOut], ' ', pu.vars[puLockedOut]))
			mod.constrs<-c(
				mod.constrs,
				'/* lock pus out optimal solution */',
				paste0('pu',x@data@pu[puLockedOut],' = 0;')
			)
		}
		# exposed edge constraints
		if (length(edge)>0)
			mod.fun<-c(
				mod.fun,
				paste0(x@opts@BLM*x@boundary$boundary[edge], ' pu', x@boundary$id1[edge])
			)
		if (length(notEdge)>0) {
			mod.fun<-c(
				mod.fun,
				paste0(x@opts@BLM*x@boundary@boundary[notEdge], ' ', puxorpu.vars)
			)
			mod.constrs<-c(
				mod.constrs,
				'/* pu boundary constraints for ILP */',
				paste0(puxorpu.vars,' <= pu',boundDF$id1[notEdge],' + pu',boundDF$id2[notEdge]),
				paste0(puxorpu.vars,' >= pu',boundDF$id1[notEdge],' - pu',boundDF$id2[notEdge]),
				paste0(puxorpu.vars,' >= -pu',boundDF$id1[notEdge],' + pu',boundDF$id2[notEdge]),
				paste0(puxorpu.vars,' <= 2 - pu',boundDF$id1[notEdge],' + pu',boundDF$id2[notEdge])
			)
		}
	}

	# species with soft constraints
	if (length(softSpp)>0) {
		mod.fun<-c(paste0(speciesDF$spf[softSpp], ' ', sf.vars))
		mod.constrs<-c(mod.constrs,'/* soft constraints for species with finite spfs */')
		for (i in seq_along(softSpp)) {
			currRows<-which(x@data@puvspecies==x@data@species$id[softSpp[i]])
			currU<-sum(x@data@puvspecies$amount[currRows])+1
			currPUs<-paste0('pu',x@data@puvspecies$pu[currRows])
			currTarget<-x@data@species$target[i]-1e-5
			currRepr<-paste(paste0(x@data@puvspecies$amount[currRows],' ',currPUs), collapse' + ')
			mod.constrs<-c(
				mod.constrs,
				paste0(currRepr, ' + ', currTarget+1, ' ',sf.vars[i],' >= ', currTarget+1,';'),
				paste0(currRepr, ' + ',currU,' ',sf.vars[i],' <= ', currTarget+currU,';')
			)
		}
	}
	
	# species with hard constraints
	if (length(firmSpp)>0) {
		mod.constrs<-c(mod.constrs, '/* hard constraints for species with infinite spfs */')
		for (i in seq_along(firmSpp)) {
			currRows<-which(x@data@puvspecies==x@data@species$id[firmSpp[i]])
			currPUs<-paste0('pu',x@data@puvspecies$pu[currRows])
			currRepr<-paste(paste0(x@data@puvspecies$amount[currRows],' ',currPUs), collapse' + ')
			mod.constrs<-c(
				mod.constrs,
				paste0(currRepr, '>=', x@data@species$target[i],';')
			)
		}
	}

	# cost constraints
	if (!isTRUE(all.equal(x@COSTTHRESH, 0))) {
		currMaxCost<-paste0(x@data@pu$cost[puFree], ' ', pu.vars[puFree])
		if (!isTRUE(all.equal(x@opts@BLM,0))) {
			if (length(puLockedIn)>0)
				currMaxCost<-c(
					currMaxCost,
					paste0(x@data@pu$cost[puLockedIn], ' ', pu.vars[puLockedIn])
				)
			if (length(puLockedOut)>0)
				currMaxCost<-c(
					currMaxCost,
					paste0(x@data@pu$cost[puLockedOut], ' ', pu.vars[puLockedOut])
				)
		}
		mod.constrs<-c(
			mod.constrs,
			'/* pu cost constraints*/',
			paste0(paste(currRepr, collapse=' + '), '<=', x@opts@COSTTHRESH,';')
		)
	}
	
	# decision variable types
	switch(problem,
		'ILP'={
			mod.vtypes<-paste0('bin: ',paste(sf.vars, pu.vars, puxorpu.vars ,sep=' '))
		},
		'LP'={
			mod.constrs<-c(
				'/* pu decision variable limits */'
				paste0('pu',x@data@pu[puFree],' >= 0;'),
				paste0('pu',x@data@pu[puFree],' <= 1;'),
			)
			mod.vtypes<-paste0('sec: ',paste(sf.vars, pu.vars, puxorpu.vars ,sep=' '),';')
		}
	)
	# compile full model
	return(c(
		'/* marxan model */\n\n/* objective function */',
		paste0('min: ',paste(mod.fun, collapse=' + '),';'),
		mod.constrs,
		'/* decision variable types */',
		mod.vtypes,
		'/* end */'
	))
}
