#' MarxanResults: An S4 class to represent Marxan results
#'
#' This class is used to store the results from a Marxan run.
#'
#' @slot summary "data.frame" with summary information on solutions
#' @slot selections "matrix" with binary selections
#' @slot amountheld "matrix" with the amount held for each species in each solution
#' @slot occheld "matrix" with the number of occurrences for each species in each solution
#' @slot best "integer" with index of best solution
#' @slot log "character" with Marxan log  file
#' @seealso \code{link{MarxanResults}}, \code{link{read.MarxanResults}}, 
setClass("MarxanResults",
	representation(
		summary="data.frame",
		selections="matrix",
		amountheld="matrix",
		occheld="matrix",
		log='character'
		.cache='environment'
	)
)
setMethod(
	"initialize", 
	"MarxanData", 
	function(.Object, .cache=new.env()) {
		callNextMethod(.Object, .cache=.cache)
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
#'
#' @return MarxanResults object
#' @seealso \code{link{MarxanResults-class}} \code{link{read.MarxanResults}}
MarxanResults=function(summary, selections, amounthend, occheld, log) {
	return(new("MarxanResults", summary=summary, selections=selections, amounthend=amountheld, occheld=occheld, best=which.max(summary$score), log=log))
}

#' Read Marxan results from disk
#'
#' This function reads outputs from a directory after Marxan been run.
#'
#' @param dir "character" with file path to directory with Marxan output files.
#' 
#' @return "MarxanResults" object
#' @seealso \code{link{MarxanResults-class}}, \code{link{MarxanResults}}
read.MarxanResults=function(dir) {
	# check for valid inputs
	if (!file.exists(dir))
		stop('Directory does not exist.')
	# load data
	mvs<-rbind.fill(laply(list.files(dir, "^output_mv.*.csv$", full.names=TRUE), fread, header=TRUE,sep=",",stringsAsFactors=FALSE,quote=FALSE))
	setnames(mvs, c("Conservation Feature","Amount Held", "Occurrences Held"), c("Conservation.Feature","Amount.Held", "Occurrences.Held"))
	sumry<-fread(file.path(x,'output_sum.csv'),header=TRUE,sep=",",stringsAsFactors=FALSE,quote=FALSE,data.table=FALSE)
	names(sumry)<-gsub(" ", ".", names(sumry))
	# create object
	return(MarxanResults.data.frame(
		sumry,
		as.matrix(fread(file.path(x,'output_solutionsmatrix.csv'),header=TRUE,sep=",",stringsAsFactors=FALSE,quote=FALSE)),
		as.matrix(dcast(mvs[,c("Conservation.Feature","Amount.Held"),with=FALSE], Conservation.Feature ~ Amount.Held))
		as.matrix(dcast(mvs[,c("Conservation.Feature","Occurrences.Held"),with=FALSE], Conservation.Feature ~ Occurrences.Held))
		paste(readLines(file.path(x,'output_log.csv')), collapse=TRUE)
	))
}

#' @descrbiein selection
selection.MarxanResults<-function(x, y="best") {
	if (is.numeric(y))
		return(x@selections[y,])
	if (y=="best")
		return(x@selections[x@best,])
	if (y=="all")
		return(x@selections)
}

#' @describein score
score.MarxanResults<-function(x, y="best") {
	if (is.numeric(y))
		return(x@summary$Score[y])
	if (y=="best")
		return(x@summary$Score[x@best])
	if (y=="all")
		return(x@summary$Score)
}

#' @describein summary
summary.MarxanResults<-function(x) {
	return(x@results@summary)
}

#' @describein print
print.MarxanResults<-function(x, header=TRUE) {
	if (header)
		cat("MarxanResults object.\n")
	cat("Number of solutions:",nrow(x@summary),"\n")
}

#' @describein log
log.MarxanResults=function(x) {
	cat(x@log)
}

#' @describein amountHeld
amountHeld.MarxanResults<-function(x, y="best") {
	if (is.numeric(y))
		return(x@amountheld[y,])
	if (y=="best")
		return(x@amountheld[x@best,])
	if (y=="all")
		return(x@amountheld)
	stop("y should be equal to 'best', 'all' or a an integer.")
}

#' @descrbiein occHeld
occHeld.MarxanResults<-function(x, y="best") {
	if (is.numeric(y))
		return(x@occHeld[y,])
	if (y=="best")
		return(x@occHeld[x@best,])
	if (y=="all")
		return(x@occHeld)
	stop("y should be equal to 'best', 'all' or a an integer.")
}

#' @describein pca
pca.MarxanResults=function(x, var, ..., force_reset=FALSE) {
	# init
	match.arg(var, c("selections", "occheld", "targetsmet"))	
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

#' @describein dist
dist.MarxanResults=function(x, var="selections", method="bray", force_reset=FALSE) {
	# init
	callchar=hashCall(match.call())
	match.arg(var, c("selections", "occheld", "targetsmet"))	
	# cache distance matrix
	if (force_reset || !is.cached(callchar)) {
		cache(x, callchar, vegdist(x=slot(x,var), binary=var %in% c('selections', 'targetsmet'), method=method))
	}
	return(cache(x, callchar))
}

#' @describein mds
mds.MarxanResults=function(x, var="selections", method="bray", ..., force_reset=FALSE) {
	# init
	match.arg(var, c("selections", "occheld", "targetsmet"))
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

#' @describein hclust
hclust.MarxanResults=function(x, type="mds", var="selections", ..., force_reset=FALSE) {
	# init
	callchar=hashCall(match.call())
	match.arg(type, c("dist", "pca", "mds"))
	match.arg(var, c("selections", "occheld", "targetsmet"))
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

#' @describein ordiplot
ordiplot.MarxanResults=function(x, type='mds', var='selections', nbest=1, ..., force_reset=FALSE) {
	match.arg(type, c("pca", "mds"))
	match.arg(var, c("selections", "occheld", "targetsmet"))	
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

#' @describein dendrogram
dendrogram.MarxanResults=function(x, type='mds', var='selections', nbest=1, ..., force_reset=FALSE) {
	match.arg(type, c("pca", "mds", "dist"))
	match.arg(var, c("selections", "occheld", "targetsmet"))
	prettyDendrogram(
		hclust.MarxanResults(x@results, type, var, ..., force_reset)$phylo,
		scores=rescale(x@summary$Score, to=c(1,2)),
		nbest=nbest, 
		main=paste0("Solution dendrogram based on ",to.pretty.name(var))
	)
}

#' @describein dotchart
dotchart.MarxanResults(x, var="score", nbest=1) {
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




