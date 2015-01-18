#' @include misc.R marxan-internal.R generics.R
NULL

#' MarxanResults: An S4 class to represent Marxan results
#'
#' This class is used to store the results from a Marxan run.
#'
#' @slot summary "data.frame" with summary information on solutions.
#' @slot selections "matrix" with binary selections.
#' @slot amountheld "matrix" with the amount held for each species in each solution.
#' @slot occheld "matrix" with the number of occurrences for each species in each solution.
#' @slot targetsmet "matrix" indicating whether the targets have been met for each species in each solution.
#' @slot best "integer" with index of best solution.
#' @slot log "character" with Marxan log  file.
#' @export
#' @seealso \code{\link{MarxanResults}}, \code{\link{read.MarxanResults}}, 
setClass("MarxanResults",
	representation(
		summary="data.frame",
		selections="matrix",
		amountheld="matrix",
		occheld="matrix",
		targetsmet="matrix",
		best="integer",
		log='character',
		.cache='environment'
	)
)
setMethod(
	"initialize", 
	"MarxanResults", 
	function(.Object, summary, selections, amountheld, occheld, targetsmet, best, log, .cache=new.env()) {
		callNextMethod(.Object, summary=summary, selections=selections, amountheld=amountheld, occheld=occheld, targetsmet=targetsmet, best=best, log=log, .cache=.cache)
	}
)

#' Create MarxanResults object
#'
#' This function generates a MarxanResults object. Methods are provided to do this using a directory containing Marxan outputs or using objects already loaded into R. 
#'
#' @param dir directory from which to load Marxan data
#' @param summary "data.frame" with summary information on solutions
#' @param selections "matrix" with binary selections
#' @param amountheld "matrix" with the amount held for each species in each solution
#' @param occheld "matrix" with the number of occurrences for each species in each solution
#' @param best "integer" with index of best solution
#' @param log "character" with Marxan log file
#' @export
#' @return MarxanResults object
#' @seealso \code{\link{MarxanResults-class}} \code{\link{read.MarxanResults}}
MarxanResults=function(summary, selections, amountheld, occheld, targetsmet, log) {
	return(new("MarxanResults", summary=summary, selections=selections, amountheld=amountheld, occheld=occheld, targetsmet=targetsmet, best=which.max(summary$Score), log=log))
}

#' Read Marxan results from disk
#'
#' This function reads outputs from a directory after Marxan been run.
#'
#' @param dir "character" with file path to directory with Marxan output files.
#' @export
#' @return "MarxanResults" object
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanResults}}
read.MarxanResults=function(dir) {
	# check for valid inputs
	if (!file.exists(dir))
		stop('Directory does not exist.')
	# load data
	pths=list.files(dir, "^output_mv.*.csv$", full.names=TRUE)
	mvs<-rbind.fill(llply(pths, fread, header=TRUE,sep=",",stringsAsFactors=FALSE, data.table=FALSE))
	mvs$sol_id<-rep(seq_along(pths), each=nrow(mvs)/length(pths))
	setnames(mvs, c("Conservation Feature","Amount Held", "Occurrences Held", "Target Met"), c("Conservation.Feature","Amount.Held", "Occurrences.Held", "Target.Met"))
	mvs$Target.Met<-mvs$Target.Met=="yes"
	sumry<-fread(file.path(dir,'output_sum.csv'),header=TRUE,sep=",",stringsAsFactors=FALSE,data.table=FALSE)
	names(sumry)<-gsub(" ", ".", names(sumry))
	# create object
	return(MarxanResults(
		summary=sumry,
		selections=as.matrix(fread(file.path(dir,'output_solutionsmatrix.csv'),header=TRUE,sep=",",stringsAsFactors=FALSE))==1,
		amountheld=pivot(mvs$Conservation.Feature, mvs$sol_id, mvs$Amount.Held),
		occheld=pivot(mvs$Conservation.Feature, mvs$sol_id, mvs$Occurrences.Held),
		targetsmet=pivot(mvs$Conservation.Feature, mvs$sol_id, mvs$Target.Met),
		log=paste(readLines(file.path(dir,'output_log.dat')), collapse="\n")
	))
}

#' Merge Marxan results
#'
#' This function merges a list of MarxanResults into a single MarxanResults object.
#'
#' @param x "list" of "MarxanResults" objects.
#' @export
#' @return "MarxanResults" object
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanResults}}
merge.MarxanResults<-function(x) {
	x=MarxanResults(
		summary=ldply(x, slot, name="summary"),
		selections=do.call(rbind, llply(x, slot, name="selections")),
		amountheld=do.call(rbind, llply(x, slot, name="amountheld")),
		occheld=do.call(rbind, llply(x, slot, name="occheld")),
		targetsmet=do.call(rbind, llply(x, slot, name="targetsmet")),
		log=paste(laply(x, slot, name="log"), collapse="\n")
	)
	x@summary$Run_Number<-seq_len(nrow(x@summary))
	return(x)
}

#' @describeIn selection
#' @export
selection.MarxanResults<-function(x, y="best") {
	if (is.numeric(y))
		return(x@selections[y,])
	if (y=="best")
		return(x@selections[x@best,])
	if (y=="all")
		return(x@selections)
}

#' @describeIn score
#' @export
score.MarxanResults<-function(x, y="best") {
	if (is.numeric(y))
		return(x@summary$Score[y])
	if (y=="best")
		return(x@summary$Score[x@best])
	if (y=="all")
		return(x@summary$Score)
}

#' @describeIn summary
#' @export
summary.MarxanResults<-function(x) {
	return(x@results@summary)
}

#' @describeIn print
#' @export
print.MarxanResults<-function(x, header=TRUE) {
	if (header)
		cat("MarxanResults object.\n")
	cat("Number of solutions:",nrow(x@summary),"\n")
}

#' @export
# setMethod(
	# 'show',
	# 'MarxanResults',
	# function(x, ...)
		# print.MarxanResults(x, ...)
# )


#' @describeIn log
#' @export
log.MarxanResults=function(x) {
	cat(x@log)
}

#' @describeIn amountheld
#' @export
amountheld.MarxanResults<-function(x, y=NULL) {
	if (is.null(y))
		return(x@amountheld)
	if (y==0)
		return(x@amountheld[x@best,])
	return(x@amountheld[y,])
}

#' @describeIn occheld
#' @export
occheld.MarxanResults<-function(x, y=NULL) {
	if (is.null(y))
		return(x@occheld)
	if (y==0)
		return(x@occheld[x@best,])
	return(x@occheld[y,])
}

#' @describeIn targetsmet
#' @export
targetsmet.MarxanResults<-function(x, y=NULL) {
	if (is.null(y))
		return(x@targetsmet)
	if (y==0)
		return(x@targetsmet[x@best,])
	return(x@targetsmet[y,])
}

#' @describeIn pca
#' @export
pca.MarxanResults=function(x, var, ..., force_reset=FALSE) {
	# init
	match.arg(var, c("selections", "occheld", "amountheld", "targetsmet"))	
	callchar=hashCall(match.call())
	# check that pca not being applied to non-continuous data
	if (var=='selections' || var=='targetsmet')
		warning("Running a pca on binary data is not recommended; consider using a NMDS with method='bray'.")
	# run analysis
	if (force_reset || !is.cached(x, callchar)) {
		pca<-prcomp(slot(x, var), center=TRUE, scale=TRUE, ...)
		pca$predict<-predict(pca, slot(x,var), ...)
		pca$dist<-dist(pca$predict, method="euclidean")
		cache(x, callchar, pca)
	}
	return(cache(x, callchar))
}

#' @describeIn dist
#' @export
dist.MarxanResults=function(x, var="selections", method="bray", force_reset=FALSE) {
	# init
	callchar=hashCall(match.call())
	match.arg(var, c("selections", "occheld", "amountheld", "targetsmet"))	
	# cache distance matrix
	if (force_reset || !is.cached(callchar)) {
		cache(x, callchar, vegdist(x=slot(x,var), binary=var %in% c('selections', 'targetsmet'), method=method))
	}
	return(cache(x, callchar))
}

#' @describeIn mds
#' @export
mds.MarxanResults=function(x, var="selections", method="bray", ..., force_reset=FALSE) {
	# init
	match.arg(var, c("selections", "occheld", "amountheld", "targetsmet"))
	callchar=hashCall(match.call())
	# check that pca not being applied to non-continuous data
	if (var=='selections' || var=='targetsmet')
		warning("Running a pca on binary data is not recommended; consider using a NMDS with method='bray'.")
	# cache nmds matrix
	if (force_reset || !is.cached(callchar)) {
		cache(x,callchar, monoMDS(dist(x, var, method, force_reset), k=2, ...))
	}
	return(cache(x, callchar))
}

#' @describeIn hclust
#' @export
hclust.MarxanResults=function(x, type="mds", var="selections", ..., force_reset=FALSE) {
	# init
	callchar=hashCall(match.call())
	match.arg(type, c("dist", "pca", "mds"))
	match.arg(var, c("selections", "occheld", "amountheld", "targetsmet"))
	# main
	tmp<-do.call(type, list(x, var, ..., force_reset))
	if (!is.cached(callchar)) {
		switch(type,
			"dist"={tmp<-fastcluster::hclust(tmp)},
			"mds"={tmp<-fastcluster::hclust(tmp$points)},
			"pca"={tmp<-fastcluster::hclust(tmp$dist)}
		)
		tmp$phylo<-as.phylo(tmp)
		cache(x, callchar, tmp)
	}
	return(cache(x, callchar))
}

#' @describeIn ordiplot
#' @export
ordiplot.MarxanResults=function(x, type='mds', var='selections', nbest=1, ..., force_reset=FALSE) {
	match.arg(type, c("pca", "mds"))
	match.arg(var, c("selections", "occheld", "amountheld", "targetsmet"))	
	if (type=="pca") {
		tmp<-pca.MarxanResults(x, var, ..., force_reset)
		prettyPcaBiplot(x=tmp,size=to(x@summary$Score, from=c(1,2)),nbest=nbest, main=paste0("Solution biplot based on ",to.pretty.name(var)))
		return(tmp)
	} else if (type=="mds") {
		tmp<-mds.MarxanResults(x, var, ..., force_reset)
		prettyBiplot(x=tmp$points,size=to(x@summary$Score, from=c(1,2)),nbest=nbest,xlab="MDS1", ylab="MDS2", main=paste0("Solution biplot based on ",to.pretty.name(var)))
		return(tmp)
	}
}

#' @describeIn dendrogram
#' @export
dendrogram.MarxanResults=function(x, type='mds', var='selections', nbest=1, ..., force_reset=FALSE) {
	match.arg(type, c("pca", "mds", "dist"))
	match.arg(var, c("selections", "occheld", "amountheld", "targetsmet"))
	prettyDendrogram(
		hclust.MarxanResults(x@results, type, var, ..., force_reset)$phylo,
		rescale(x@summary$Score, to=c(1,2)),
		nbest, 
		paste0("Solution dendrogram based on ",to.pretty.name(var))
	)
}

#' @describeIn dotchart
#' @export
dotchart.MarxanResults<-function(x, var="score", nbest=1) {
	match.arg(var, 
		c(
			'score', 'Score',
			'cost', 'Cost',
			'npu','Planning_Units',
			'con', 'Connectivity',
			'confrac', 'Connectivity_In_Fraction' ,
			'conin','Connectivity_In',
			'conout',  'Connectivity_Out',
			'penalty', 'Penalty',
			'shortfall',  'Shortfall',
			'mv', 'Missing_Values'
		)
	)
	switch(var,
		'score'={var<-'Score'},
		'cost'={var<-'Cost'},
		'npu'={var<-'Planning_Units'},
		'con'={var<-'Connectivity'},
		'confrac'={var<-'Connectivity_In_Fraction'},
		'conin'={var<-'Connectivity_In'},
		'conout'={var<-'Connectivity_Out'},
		'penalty'={var<-'Penalty'},
		'shortfall'={var<-'Shortfall'},
		'mv'={var<-'Missing_Values'}
	)
	tmp<-order(summary[[var]])
	prettyDotchart(x@summary[[var]][tmp], pch=16, labels=tmp, main="Solution dotplot", xlab=gsub("_", " ", var, fixed=TRUE), ylab=tmp, pt.cex=rescale(x@summary[[var]][tmp], to=c(1,2)), color=replace(rep("black", nrow(x@summary)), seq_len(nrow(x@summary)) < nbest, "red"))	
}

#' @describeIn is.cached
setMethod(
	f="is.cached", 
	signature(x="MarxanResults", name="character"), 
	function(x,name) {
		return(!is.null(x@.cache[[names]]))
	}
)

#' @describeIn cache
setMethod(
	f="cache", 
	signature(x="MarxanResults", name="character", y="ANY"), 
	function(x, name, y) {
		x@.cache[[name]]=y
	}
)

#' @describeIn cache
setMethod(
	f="cache", 
	signature(x="MarxanResults", name="character", y="missing"), 
	function(x, name, y) {
		return(x@.cache[[name]])
	}
)



