#' @include misc.R marxan-internal.R generics.R
NULL

#' MarxanResults: An S4 class to represent Marxan results
#'
#' This class is used to store the results from a Marxan run.
#'
#' @slot summary "data.frame" with summary information on solutions (output_sum.dat).
#' @slot selections "matrix" with binary selections (output_solutionsmatrix.csv).
#' @slot amountheld "matrix" with the amount held for each species in each solution (collation of output_mv*.csv files).
#' @slot occheld "matrix" with the number of occurrences for each species in each solution (collation of output_mv*.csv files).
#' @slot sepacheived "matrix" indicating the separation achieved (collation of output_mv*.csv files).
#' @slot targetsmet "matrix" indicating whether the targets have been met for each species in each solution (collation of output_mv*.csv files).
#' @slot mpm "matrix" indicating minimum proportion met (MPM; collation of output_mv*.csv files).
#' @slot best "integer" with index of best solution.
#' @slot log "character" with Marxan log  file (otuput_log.dat). 
#' @export
#' @seealso \code{\link{MarxanResults}}, \code{\link{read.MarxanResults}}, \code{\link{write.MarxanResults}}.
setClass("MarxanResults",
	representation(
		summary="data.frame",
		selections="matrix",
		amountheld="matrix",
		occheld="matrix",
		sepacheived="matrix",
		targetsmet="matrix",
		mpm="matrix",
		best="integer",
		log='character',
		.cache='environment'
	)
)
setMethod(
	"initialize", 
	"MarxanResults", 
	function(.Object, summary, selections, amountheld, occheld, sepacheived, targetsmet, mpm, best, log, .cache=new.env()) {
		callNextMethod(.Object, summary=summary, selections=selections, amountheld=amountheld, occheld=occheld, sepacheived=sepacheived, targetsmet=targetsmet, mpm=mpm, best=best, log=log, .cache=.cache)
	}
)

#' Create MarxanResults object
#'
#' This function creates a new MarxanResults object.
#'
#' @param summary "data.frame" with summary information on solutions (output_sum.dat).
#' @param selections "matrix" with binary selections (output_solutionsmatrix.csv).
#' @param amountheld "matrix" with the amount held for each species in each solution (collation of output_mv*.csv files).
#' @param occheld "matrix" with the number of occurrences for each species in each solution (collation of output_mv*.csv files).
#' @param sepacheived "matrix" indicating the separation achieved (collation of output_mv*.csv files).
#' @param targetsmet "matrix" indicating whether the targets have been met for each species in each solution (collation of output_mv*.csv files).
#' @param mpm "matrix" indicating minimum proportion met (MPM; collation of output_mv*.csv files).
#' @param log "character" with Marxan log  file (otuput_log.dat). 
#' @export
#' @note "integer" slot 'best' is automatically calculated from \code{summary}.
#' @return MarxanResults object
#' @seealso \code{\link{MarxanResults-class}} \code{\link{read.MarxanResults}}
MarxanResults=function(summary, selections, amountheld, occheld, sepacheived, targetsmet, mpm, log) {
	return(new("MarxanResults", summary=summary, selections=selections, amountheld=amountheld, occheld=occheld, sepacheived=sepacheived, targetsmet=targetsmet, mpm=mpm, best=which.max(summary$Score), log=log))
}

#' Read Marxan results
#'
#' This function reads files output from Marxan.
#'
#' @param dir "character" with file path to directory containing Marxan output files.
#' @note This function assumes specific Marxan output files are present in the directory. If any of these are missing the function will crash.
#' @export
#' @return "MarxanResults" object
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanResults}}.
read.MarxanResults=function(dir) {
	# check for valid inputs
	if (!file.exists(dir))
		stop('Directory does not exist.')
	# load data
	pths<-list.files(dir, "^output_mv.*.csv$", full.names=TRUE)
	pths<-pths[which(basename(pths)!='output_mvbest.csv')]
	mvs<-rbind.fill(llply(pths, fread, header=TRUE,sep=",",stringsAsFactors=FALSE, data.table=FALSE))
	mvs$sol_id<-rep(seq_along(pths), each=nrow(mvs)/length(pths))
	setnames(mvs, c("Conservation Feature","Amount Held", "Occurrences Held", "Target Met", "Separation Achieved"), c("Conservation.Feature","Amount.Held", "Occurrences.Held", "Target.Met", "Separation.Achieved"))
	mvs$Target.Met<-mvs$Target.Met=="yes"
	sumry<-fread(file.path(dir,'output_sum.csv'),header=TRUE,sep=",",stringsAsFactors=FALSE,data.table=FALSE)
	names(sumry)<-gsub(" ", ".", names(sumry))
	# create object
	return(MarxanResults(
		summary=sumry,
		selections=as.matrix(fread(file.path(dir,'output_solutionsmatrix.csv'),header=TRUE,sep=",",stringsAsFactors=FALSE,drop=1))==1,
		amountheld=pivot(mvs$Conservation.Feature, mvs$sol_id, mvs$Amount.Held),
		occheld=pivot(mvs$Conservation.Feature, mvs$sol_id, mvs$Occurrences.Held),
		sepacheived=pivot(mvs$Conservation.Feature, mvs$sol_id, mvs$Separation.Achieved),
		targetsmet=pivot(mvs$Conservation.Feature, mvs$sol_id, mvs$Target.Met),
		mpm=pivot(mvs$Conservation.Feature, mvs$sol_id, mvs$MPM),
		log=paste(readLines(file.path(dir,'output_log.dat')), collapse="\n")
	))
}

#' Merge Marxan results
#'
#' This function merges a list of "MarxanResults" objects into a single MarxanResults object. 
#' It is used for collating Marxan results data when Marxan runs have been run in parallel.
#'
#' @param x "list" of "MarxanResults" objects.
#' @export
#' @return "MarxanResults" object
#' @seealso \code{\link{MarxanResults-class}}, \code{\link{MarxanResults}}.
merge.MarxanResults<-function(x) {
	x=MarxanResults(
		summary=ldply(x, slot, name="summary"),
		selections=do.call(rbind, llply(x, slot, name="selections")),
		amountheld=do.call(rbind, llply(x, slot, name="amountheld")),
		occheld=do.call(rbind, llply(x, slot, name="occheld")),
		sepacheived=do.call(rbind, llply(x, slot, name="sepacheived")),
		targetsmet=do.call(rbind, llply(x, slot, name="targetsmet")),
		mpm=do.call(rbind, llply(x, slot, name="mpm")),
		log=paste(laply(x, slot, name="log"), collapse="\n")
	)
	x@summary$Run_Number<-seq_len(nrow(x@summary))
	return(x)
}

#' @rdname selection
#' @inheritParams selection
#' @export
selection.MarxanResults<-function(x, y=NULL) {
	if (is.null(y))
		return(x@selections)
	if (y==0)
		return(x@selections[x@best,])
	return(x@selections[y,])
}


#' @rdname score
#' @inheritParams score
#' @export
score.MarxanResults<-function(x, y=NULL) {
	if (is.null(y))
		return(x@summary$Score)
	if (y==0)
		return(x@summary$Score[x@best])
	return(x@summary$Score[y])
}

#' @export
summary.MarxanResults<-function(x) {
	return(x@results@summary)
}

#' @export
print.MarxanResults<-function(x, header=TRUE) {
	if (header)
		cat("MarxanResults object.\n")
	cat("Number of solutions:",nrow(x@summary),"\n")
}

#' @export
setMethod(
	'show',
	'MarxanResults',
	function(object)
		print.MarxanResults(object)
)


#' @rdname log
#' @inheritParams log
#' @export
log.MarxanResults<-function(x) {
	cat(x@log)
}

#' @export
#' @rdname amountheld
amountheld.MarxanResults<-function(x, y=NULL) {
	if (is.null(y))
		return(x@amountheld)
	if (y==0)
		return(x@amountheld[x@best,])
	return(x@amountheld[y,])
}

#' @rdname occheld
#' @inheritParams occheld
#' @export
occheld.MarxanResults<-function(x, y=NULL) {
	if (is.null(y))
		return(x@occheld)
	if (y==0)
		return(x@occheld[x@best,])
	return(x@occheld[y,])
}

#' @rdname targetsmet
#' @inheritParams targetsmet
#' @export
targetsmet.MarxanResults<-function(x, y=NULL) {
	if (is.null(y))
		return(x@targetsmet)
	if (y==0)
		return(x@targetsmet[x@best,])
	return(x@targetsmet[y,])
}

#' @rdname mpm
#' @inheritParams mpm
#' @export
mpm.MarxanResults<-function(x, y=NULL) {
	if (is.null(y))
		return(x@mpm)
	if (y==0)
		return(x@mpm[x@best,])
	return(x@mpm[y,])
}

#' @rdname sepacheived
#' @inheritParams sepacheived
#' @export
sepacheived.MarxanResults<-function(x, y=NULL) {
	if (is.null(y))
		return(x@sepacheived)
	if (y==0)
		return(x@sepacheived[x@best,])
	return(x@sepacheived[y,])
}

#' @rdname pca
#' @inheritParams pca
#' @export
pca.MarxanResults=function(x, var, ..., force_reset=FALSE) {
	# init
	callchar=hashCall(match.call(), 1)
	match.arg(var, c("selections", "occheld", "amountheld", "targetsmet", "sepacheived", "mpm"))	
	if (ncol(slot(x, var))==1)
		stop('Marxan solutions must involve two or more species to be summarised using a PCA')
	tmp<-slot(x, var)[,apply(slot(x, var), 2, function(z){length(unique(z))>1})]
	if (ncol(tmp)<2)
		stop('Marxan solution ',var,' values are identical for (nearly) all species; PCA cannot be performed')
	if (length(unique(aaply(tmp, 1, paste, collapse=",", .drop=TRUE, .expand=FALSE)))==1)
		stop('All solutions have the same values for this variable.')
	# check that pca not being applied to non-continuous data
	if (var=='selections' || var=='targetsmet')
		warning("Running a pca on binary data is not recommended; consider using a NMDS with method='bray'.")
	# run analysis
	if (force_reset || !is.cached(x, callchar)) {
		pca<-prcomp(tmp, center=TRUE, scale=TRUE, ...)
		pca$predict<-predict(pca, tmp, ...)
		pca$dist<-dist(pca$predict, method="euclidean")
		cache(x, callchar, pca)
	}
	return(cache(x, callchar))
}

#' @rdname dist
#' @export
dist.MarxanResults<-function(x, var="selections", method="bray", force_reset=FALSE) {
	# init
	callchar=hashCall(match.call(), 1)
	match.arg(var, c("selections", "occheld", "amountheld", "targetsmet", "sepacheived", "mpm"))	
	if (length(unique(aaply(slot(x, var), 1, paste, collapse=",", .drop=TRUE, .expand=FALSE)))==1)
		stop('All solutions have the same values for this variable.')
	# cache distance matrix
	if (force_reset || !is.cached(x, callchar)) {
		cache(x, callchar, vegdist(x=slot(x,var), binary=var %in% c('selections', 'targetsmet'), method=method))
	}
	return(cache(x, callchar))
}

#' @rdname mds
#' @inheritParams mds
#' @export
mds.MarxanResults=function(x, var="selections", method="bray", ..., force_reset=FALSE) {
	# init
	callchar=hashCall(match.call(), 1)
	match.arg(var, c("selections", "occheld", "amountheld", "targetsmet", "sepacheived", "mpm"))	
	if (length(unique(aaply(slot(x, var), 1, paste, collapse=",", .drop=TRUE, .expand=FALSE)))==1)
		stop('All solutions have the same values for this variable.')	
	# cache nmds matrix
	if (force_reset || !is.cached(x, callchar)) {
		cache(x,callchar, monoMDS(dist.MarxanResults(x, var, method, force_reset=force_reset), k=2, ...))
	}
	return(cache(x, callchar))
}

#' @rdname hclust
#' @inheritParams hclust
#' @export
hclust.MarxanResults=function(x, type="dist", var="selections", ..., force_reset=FALSE) {
	# init
	callchar=hashCall(match.call(), 1)
	match.arg(type, c("dist", "pca", "mds"))
	match.arg(var, c("selections", "occheld", "amountheld", "targetsmet", "sepacheived", "mpm"))	
	if (length(unique(aaply(slot(x, var), 1, paste, collapse=",", .drop=TRUE, .expand=FALSE)))==1)
		stop('All solutions have the same values for this variable.')	
	# main
	tmp<-do.call(type, list(x, var, ..., force_reset=force_reset))
	if (!is.cached(x, callchar)) {
		switch(type,
			"dist"={tmp<-fastcluster::hclust(tmp)},
			"mds"={tmp<-fastcluster::hclust(stats::dist(tmp$points,'euclidean'))},
			"pca"={tmp<-fastcluster::hclust(stats::dist(tmp$dist, 'euclidean'))}
		)
		tmp$phylo<-as.phylo(tmp)
		cache(x, callchar, tmp)
	}
	return(cache(x, callchar))
}

#' @rdname ordiplot
#' @inheritParams ordiplot
#' @export
ordiplot.MarxanResults<-function(x, type='mds', var='selections', nbest=1, ..., force_reset=FALSE) {
	match.arg(type, c("pca", "mds"))
	match.arg(var, c("selections", "occheld", "amountheld", "targetsmet", "sepacheived", "mpm"))	
	if (length(unique(aaply(slot(x, var), 1, paste, collapse=",", .drop=TRUE, .expand=FALSE)))==1)
		stop('All solutions have the same values for this variable.')	
	if (type=="pca") {
		tmp<-pca.MarxanResults(x, var, ..., force_reset=force_reset)
		prettyPcaBiplot(x=tmp,size=rescale(x@summary$Score, to=c(0.8,2)),nbest=nbest, main=paste0("Solution biplot based on ",to.pretty.name(var)))
	} else if (type=="mds") {
		tmp<-mds.MarxanResults(x, var, ..., force_reset=force_reset)
		prettyBiplot(x=tmp$points,size=rescale(x@summary$Score, to=c(0.8,2)),nbest=nbest,xlab="MDS1", ylab="MDS2", main=paste0("Solution biplot based on ",to.pretty.name(var)))
	}
	return(invisible(tmp))
}

#' @rdname dendrogram
#' @export
dendrogram.MarxanResults=function(x, type='mds', var='selections', nbest=1, ..., force_reset=FALSE) {
	match.arg(type, c("pca", "mds", "dist"))
	match.arg(var, c("selections", "occheld", "amountheld", "targetsmet", "sepacheived", "mpm"))	
	if (length(unique(aaply(slot(x, var), 1, paste, collapse=",", .drop=TRUE, .expand=FALSE)))==1)
		stop('All solutions have the same values for this variable.')	
	tmp<-hclust.MarxanResults(x, type, var, ..., force_reset=force_reset)$phylo
	prettyDendrogram(
		tmp,
		rescale(x@summary$Score, to=c(0.8,2)),
		nbest, 
		paste0("Solution dendrogram based on ",to.pretty.name(var))
	)
	return(invisible(tmp))
}

#' @rdname dotchart
#' @export
dotchart.MarxanResults<-function(x, var="score", nbest=1, n=50) {
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
	n=min(nrow(x@summary),n)
	ord<-rev(order(x@summary[[var]])[seq_len(n)])
	tmp<-x@summary[ord,]
	cex<-rescale(tmp[[var]], to=c(0.8,2))
	prettyDotchart(
		x=tmp[[var]], 
		pch=16, 
		labels=ord,
		main="Solutions", 
		xlab=gsub("_", " ", var, fixed=TRUE), 
		pt.cex=cex, 
		lab.cex=0.8, 
		color=replace(rep("black", n), (n-seq_len(nbest))+1, "red")
	)
}

#' @describeIn is.cached
setMethod(
	f="is.cached", 
	signature(x="MarxanResults", name="character"), 
	function(x,name) {
		return(!is.null(x@.cache[[name]]))
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



