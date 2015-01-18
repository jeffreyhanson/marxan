#' @include RcppExports.R dependencies.R
NULL

# set union classes
suppressWarnings(setOldClass("PolySet"))
suppressWarnings(setClassUnion("PolySetOrNULL", c("PolySet", "NULL")))

# setOldClass("RasterBrick")
# setOldClass("RasterStack")
# setOldClass(".RasterBrickSparse")
suppressWarnings(setClassUnion("RasterStackOrBrick", c("RasterBrick", "RasterStack")))

# misc hidden functions
marxanURL="http://www.uq.edu.au/marxan/get-marxan-software"

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

is.finite.numeric.wholenumber <- function(x) {
	return(is.numeric(x) & !any(!is.finite(x)) & all(is.wholenumber(x)))
}

is.finite.numeric <- function(x) {
	return(is.numeric(x) & !any(!is.finite(x)))
}

is.valid.character <- function(x) {
	return(is.character(x) & all(!is.na(x)))
}

to.colors<-function(values, cols) {
	return(rgb(colorRamp(cols)(rescale(values)), maxColorValue=255))
}

to.pretty.name<-function(x) {
	return(
		switch(x,
			"selections"="planning unit selections",
			"amountheld"="amount of species in solutions",
			"occheld"="occurrences of species in solutions",
			"targetsmet"="targets met for species in solutions"
		)
	)
}

parseArg<-function(name, args, error=TRUE) {
	val=grep(name,args, value=TRUE)
	if (error & length(val)!=1L) {
		stop(paste("Cannot find argument",name,"in Marxan input file."))
	} else if (length(val)!=1L) {
		return(NULL)
	} else {
		return(strsplit(val, " ", fixed=TRUE)[[1L]][[2L]])
	}
}

asym.setequal <- function(x,y) {
	return(length(setdiff(x,y))==0)
}



### pretty plotting functions
prettyBiplot<-function(x,size,nbest,xlab,ylab,main) {
	plot(xlim=range(x[,1]), ylim=range(x[,2]), main=main, xlab=xlab, ylab=ylab)
	text(x=x[,1], y=y[,2], labels=seq_len(nrow(x)), col=replace(rep("black", nrow(x)), which(order(size,decreasing=TRUE)<=nbest), "red"), cex=size)
}

prettyPcaBiplot<-function(x, size, nbest, choices=1L:2L, scale=1, pc.biplot=FALSE, ...) {
    if (length(choices) != 2L) 
        stop("length of choices must be 2")
    if (!length(scores <- x$x)) 
        stop(gettextf("object '%s' has no scores", deparse(substitute(x))), 
            domain = NA)
    if (is.complex(scores)) 
        stop("biplots are not defined for complex PCA")
    lam <- x$sdev[choices]
    n <- NROW(scores)
    lam <- lam * sqrt(n)
    if (scale < 0 || scale > 1) 
        warning("'scale' is outside [0, 1]")
    if (scale != 0) 
        lam <- lam^scale
    else lam <- 1
    if (pc.biplot) 
        lam <- lam/sqrt(n)
    prettyPcaBiplotSub(t(t(scores[, choices])/lam), t(t(x$rotation[,choices]) * lam), size, nbest, ...)
}

prettyPcaBiplotSub<-function (x, y, size, nbest, var.axes = TRUE, col, cex = rep(par("cex"), 2), 
    xlabs = NULL, ylabs = NULL, expand = 1, xlim = NULL, ylim = NULL, 
    arrow.len = 0.1, main = NULL, sub = NULL, xlab = NULL, ylab = NULL, 
    ...)
{
    n <- nrow(x)
    p <- nrow(y)
    if (missing(xlabs)) {
        xlabs <- dimnames(x)[[1L]]
        if (is.null(xlabs)) 
            xlabs <- 1L:n
    }
    xlabs <- as.character(xlabs)
    dimnames(x) <- list(xlabs, dimnames(x)[[2L]])
    if (missing(ylabs)) {
        ylabs <- dimnames(y)[[1L]]
        if (is.null(ylabs)) 
            ylabs <- paste("Var", 1L:p)
    }
    ylabs <- as.character(ylabs)
    dimnames(y) <- list(ylabs, dimnames(y)[[2L]])
    if (length(cex) == 1L) 
        cex <- c(cex, cex)
    if (missing(col)) {
        col <- par("col")
        if (!is.numeric(col)) 
            col <- match(col, palette(), nomatch = 1L)
        col <- c(col, col + 1L)
    }
    else if (length(col) == 1L) 
        col <- c(col, col)
    unsigned.range <- function(x) c(-abs(min(x, na.rm = TRUE)), 
        abs(max(x, na.rm = TRUE)))
    rangx1 <- unsigned.range(x[, 1L])
    rangx2 <- unsigned.range(x[, 2L])
    rangy1 <- unsigned.range(y[, 1L])
    rangy2 <- unsigned.range(y[, 2L])
    if (missing(xlim) && missing(ylim)) 
        xlim <- ylim <- rangx1 <- rangx2 <- range(rangx1, rangx2)
    else if (missing(xlim)) 
        xlim <- rangx1
    else if (missing(ylim)) 
        ylim <- rangx2
    ratio <- max(rangy1/rangx1, rangy2/rangx2)/expand
    on.exit(par(op))
    op <- par(pty = "s")
    if (!is.null(main)) 
        op <- c(op, par(mar = par("mar") + c(0, 0, 1, 0)))
    plot(x, type = "n", xlim = xlim, ylim = ylim, col = col[1L], xlab = xlab, ylab = ylab, sub = sub, main = main, ...)
    text(x, xlabs, cex = size, col = replace(rep("black", nrow(x)), which(order(size,decreasing=TRUE)<=nbest), "red"), ...)
    par(new = TRUE)
    dev.hold()
    on.exit(dev.flush(), add = TRUE)
    plot(y, axes = FALSE, type = "n", xlim = xlim * ratio, ylim = ylim * 
        ratio, xlab = "", ylab = "", col = col[1L], ...)
    axis(3, col = col[2L], ...)
    axis(4, col = col[2L], ...)
    box(col = col[1L])
    text(y, labels = ylabs, cex = cex[2L], col = col[2L], ...)
    if (var.axes) 
        arrows(0, 0, y[, 1L] * 0.8, y[, 2L] * 0.8, col = col[2L], 
            length = arrow.len)
    invisible()
}

prettyDendrogram<-function(phy, scores, nbest=1, main) {
	tipInd=which(rank(scores)<=nbest)
	bestEdges=which.edge(phy, phy$tip.label[tipInd])
	edge.cols=replace(rep("black", length(phy$edge)), bestEdges, "red")
	edge.widths=replace(rep(1, length(phy$edge)), bestEdges, 2)
	tip.cols=replace(rep("black", length(phy$tip.label)),tipInd, "red")
	plot(phy, main=main, edge.color=edge.cols, tip.color=tip.cols, edge.width=edge.widths, direction="downwards")
	axisPhylo(2, las = 1)	
}

prettyDotchart<-function (x, labels = NULL, groups = NULL, gdata = NULL, cex = par("cex"), 
    pch = 21, gpch = 21, bg = par("bg"), color = par("fg"), gcolor = par("fg"), 
    lcolor = "gray", xlim = range(x[is.finite(x)]), main = NULL, 
    xlab = NULL, ylab = NULL, pt.cex, ...) 
{
    opar <- par("mai", "mar", "cex", "yaxs")
    on.exit(par(opar))
    par(cex = cex, yaxs = "i")
    if (!is.numeric(x)) 
        stop("'x' must be a numeric vector or matrix")
    n <- length(x)
    if (is.matrix(x)) {
        if (is.null(labels)) 
            labels <- rownames(x)
        if (is.null(labels)) 
            labels <- as.character(1L:nrow(x))
        labels <- rep_len(labels, n)
        if (is.null(groups)) 
            groups <- col(x, as.factor = TRUE)
        glabels <- levels(groups)
    }
    else {
        if (is.null(labels)) 
            labels <- names(x)
        glabels <- if (!is.null(groups)) 
            levels(groups)
        if (!is.vector(x)) {
            warning("'x' is neither a vector nor a matrix: using as.numeric(x)")
            x <- as.numeric(x)
        }
    }
    plot.new()
    linch <- if (!is.null(labels)) 
        max(strwidth(labels, "inch"), na.rm = TRUE)
    else 0
    if (is.null(glabels)) {
        ginch <- 0
        goffset <- 0
    }
    else {
        ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
        goffset <- 0.4
    }
    if (!(is.null(labels) && is.null(glabels))) {
        nmai <- par("mai")
        nmai[2L] <- nmai[4L] + max(linch + goffset, ginch) + 
            0.1
        par(mai = nmai)
    }
    if (is.null(groups)) {
        o <- 1L:n
        y <- o
        ylim <- c(0, n + 1)
    }
    else {
        o <- sort.list(as.numeric(groups), decreasing = TRUE)
        x <- x[o]
        groups <- groups[o]
        color <- rep_len(color, length(groups))[o]
        lcolor <- rep_len(lcolor, length(groups))[o]
        offset <- cumsum(c(0, diff(as.numeric(groups)) != 0))
        y <- 1L:n + 2 * offset
        ylim <- range(0, y + 2)
    }
    plot.window(xlim = xlim, ylim = ylim, log = "")
    lheight <- par("csi")
    if (!is.null(labels)) {
        linch <- max(strwidth(labels, "inch"), na.rm = TRUE)
        loffset <- (linch + 0.1)/lheight
        labs <- labels[o]
        mtext(labs, side = 2, line = loffset, at = y, adj = 0, 
            col = color, las = 2, cex = cex, ...)
    }
    abline(h = y, lty = "dotted", col = lcolor)
    points(x, y, pch = pch, col = color, bg = bg)
    if (!is.null(groups)) {
        gpos <- rev(cumsum(rev(tapply(groups, groups, length)) + 
            2) - 1)
        ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
        goffset <- (max(linch + 0.2, ginch, na.rm = TRUE) + 0.1)/lheight
        mtext(glabels, side = 2, line = goffset, at = gpos, adj = 0, 
            col = gcolor, las = 2, cex = cex, ...)
        if (!is.null(gdata)) {
            abline(h = gpos, lty = "dotted")
            points(gdata, gpos, pch = gpch, col = gcolor, bg = bg, 
                ...)
        }
    }
    axis(1)
    box()
    title(main = main, xlab = xlab, ylab = ylab, ...)
    invisible()
}

prettyGeoplot<-function(polygons, col, basemap, main, fun) {
	par(oma=c(0,0,3,0))
	mtext(side=3, line=0.5, cex=3, main)
	if (is.list(basemap)) {
		PlotPolysOnStaticMap(basemap, polygons, col=cols)
	} else {
		plot(polygons, col=cols)
	}
	fun()
	return(invisible())
}

### color function
brewerCols<-function(values, pal, alpha=1) {
	r<-colorRamp(brewer.pal(brewer.pal.info$maxcolors[which(rownames(brewer.pal.info)==pal)], pal), alpha=TRUE)(values)
	r[,4]=rescale(alpha, from=c(0,1), to=c(0,255))
	return(rgb(r, maxColorValue=255))
}


### automated legend functions
continuousLegend<-function(values, pal) {
	return(
		function() {
			posx=c(0.3, 0.4)
			posy=c(0.1, 0.9)
			xdiff=diff(par()$usr[1:2])
			ydiff=diff(par()$usr[3:4])
			rect(xleft=par()$usr[1]+(xdiff*posx[1]), ybottom=par()$usr[3]+(ydiff*posy[1]), xright=par()$usr[1]+(xdiff*posx[2]), ytop=par()$usr[4]+(ydiff*posy[2]))
			shape::colorlegend(zlim=range(values), col=brewerCols(seq(0,1,0.01), pal), zval=pretty(values), posx=posx, posy=posy, xpd=TRUE)
		}
	)
}

categoricalLegend<-function(col,labels) {
	return(
		function() {
			legend(bg="white", legend=labels, fill=col)	
		}
	)
}

# update functions
findInvalidMarxanOperations<-function(ops) {
	isinvalid<-!laply(ops, inherits, "MarxanUpdateOperation")
	if (any(isinvalid))
		stop(
			paste(
				paste(
					laply(
						as.list(attr(terms(formula),"variables"))[-1L][which(isinvalid)],
						deparse
					),
					collapse=" + "
				),
				"are invalid operations for a Marxan object."
			)
		)
}

updateOperation<-function(x, arg, value) {
	# parse command
	splt=strsplit(arg, ".", fixed=TRUE)
	if (splt[[1]][[1]]=="opts") {
		# check to see if command is valid
		if (!.hasSlot(x@opts, splt[[1]][[2]]))
			stop(paste(splt[[1]][[2]],"is not a valid Marxan parameter."))
		slot(x@opts, splt[[1]][[2]])=value
	} else if (splt[[1]][[1]]=="species") {
		# check to see if command is valid
		if (!asym.setequal(splt[[1]][[3]],names(x@data@speciesDataFrame))) {
			stop(paste(splt[[1]][[3]],"is not a valid column in speciesDataFrame"))
		}
		# parse command
		if (asym.setequal(splt[[1]][[2]],x@data@speciesDataFrame$id)) {
			x@data@speciesDataFrame[which(x@data@speciesDataFrame$id==splt[[1]][[2]]),splt[[1]][[3]]]=value
		} else if (asym.setequal(splt[[1]][[2]],x@data@speciesDataFrame$name)) {
			x@data@speciesDataFrame[which(x@data@speciesDataFrame$name==splt[[1]][[2]]),splt[[1]][[3]]]=value
		} else {
			stop(paste(splt[[1]][[2]],"is not a valid species id or name."))
		}
	} else if (splt[[1]][[1]]=="pu") {
		# check to see if command is valid
		if (!asym.setequal(splt[[1]][[2]],x@data@speciesDataFrame$id)) {
			stop(paste(splt[[1]][[2]],"is not a valid planning unit id."))
		}
		if (!asym.setequal(splt[[1]][[3]],names(x@data@puDataFrame))) {
			stop(paste(splt[[1]][[3]],"is not a valid column in puDataFrame"))
		}
		# parse command
		x@data@puDataFrame[which(x@data@puDataFrame$id==splt[[1]][[2]]),splt[[1]][[3]]]=value
	} else {
		stop(paste(splt[[1]][[1]],"is not valid."))
	}
	return(x)
}


# raster processing functions
zonalSum.RasterLayerInMemory <- function(polys, rast, speciesName) {
	tmp=rcpp_groupsum(getValues(polys),getValues(rast))
	return(data.frame(species=speciesName, pu=attr(tmp, "ids"), amount=c(tmp)))
}

zonalSum.RasterLayerNotInMemory <- function(bs, polys, rast, speciesName, ncores, registered) {
	if (registered & .Platform$OS.type=="windows")
		clusterExport(clust, c("bs","polys", "rast", "rcpp_groupsum"))
	tmp=rcpp_groupcombine(llply(seq_len(bs$n), .parallel=registered, function(i) {
		return(rcpp_groupsum(getValues(polys, bs$row[i], bs$nrows[i]), getValues(rast, bs$row[i], bs$nrows[i])))
	}))
	return(data.frame(species=speciesName, pu=attr(tmp, "ids"), amount=c(tmp)))
}

hashCall<-function(expr, skipargs=c(), env=parent.frame()) {
	for (i in seq_along(names(expr))[c(-1L, (skipargs*-1L)-1L)])
		if (inherits(expr[[i]], c("name")))
			expr[[i]]=eval(expr[[i]], envir=env)
	return(deparse(expr))
}


# Many thanks and all the street creds to user "Charles" on StackOverflow
# http://stackoverflow.com/questions/5307313/fastest-tall-wide-pivoting-in-r
pivot<-function(col, row, value) {
  col<-as.factor(col)
  row<-as.factor(row)
  mat<-array(dim = c(nlevels(row), nlevels(col)), dimnames = list(levels(row), levels(col)))
  mat[(as.integer(col) - 1L) * nlevels(row) + as.integer(row)] = value
  mat
}

