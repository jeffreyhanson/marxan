#' @include RcppExports.R dependencies.R
NULL

# set union classes
suppressWarnings(setOldClass("PolySet"))
suppressWarnings(setClassUnion("PolySetOrNULL", c("PolySet", "NULL")))

# misc hidden functions
marxanURL="http://www.uq.edu.au/marxan/get-marxan-software"


asym.setequal <- function(x,y) {
	return(length(setdiff(x,y))==0)
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



### pretty plotting functions
prettyBiplot<-function(x,size,nbest,xlab,ylab,main) {
	par()
	plot(1,1, xlim=range(x[,1]), ylim=range(x[,2]), main=main, xlab=xlab, ylab=ylab, type="n")
	text(x=x[,1], y=x[,2], labels=seq_len(nrow(x)), col=replace(rep("black", nrow(x)), which(as.numeric(factor(size*-1))<=nbest), "red"), cex=size)
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
    plot(x, type = "n", xlim = xlim, ylim = ylim, col = col[1L], xlab = xlab, ylab = ylab, sub = sub, ...)
	title(main, line=2.5)
    text(x, xlabs, cex = size, col = replace(rep("black", nrow(x)),which(as.numeric(factor(size*-1))<=nbest), "red"), ...)
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
	tipInd=which(as.numeric(factor(scores))<=nbest)
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
    xlab = NULL, ylab = NULL, pt.cex, lab.cex, ...) 
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
            col = color, las = 2, cex = lab.cex, ...)
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
    # title(main = main, xlab = xlab, ylab = ylab, ...)
    title(main = main, xlab = xlab, ylab = ylab)
    invisible()
}

prettyGeoplot<-function(polygons, col, basemap, main, fun, beside=TRUE) {
	# make layout
	defpar<-par(no.readonly = TRUE)
	par(mar=c(1,1,1,1),oma=c(0,0,0,0))
	if (beside) {
		layout(matrix(c(1,1,3,2),ncol=2,byrow=TRUE), widths=c(0.8,0.2), height=c(0.1,0.9))
	} else {
		layout(matrix(c(1,3,2),ncol=1,byrow=TRUE), widths=c(1), height=c(0.1,0.8,0.1))
	}
	# make title
	plot(1,1,type="n", xlim=c(-1,1), ylim=c(-1,1), axes=FALSE, xlab="", ylab="")
	mtext(side=1, line=-1.5, main, cex=1.5)
	# make legend
	plot(1,1,type="n", xlim=c(-1,1), ylim=c(-1,1), axes=FALSE, xlab="", ylab="")
	fun()
	# make geoplot
	if (is.list(basemap)) {
		PlotOnStaticMap(basemap)
		suppressWarnings(PlotPolysOnStaticMap(basemap, polygons, col=col, add=TRUE))
	} else {
		xdiff<-diff(range(polygons$X))
		plotPolys(polygons, col=col, axes=FALSE, xlab="", ylab="")
	}
	par(defpar)
	return(invisible())
}


### color function
brewerCols<-function(values, pal, alpha=1, n=NULL) {
	if (is.null(n))
		n<-brewer.pal.info$maxcolors[which(rownames(brewer.pal.info)==pal)]
	suppressWarnings(r<-colorRamp(brewer.pal(n, pal))(values))
	return(rgb(r, maxColorValue=255, alpha=rescale(alpha, from=c(0,1), to=c(0,255))))
}


### automated legend functions
continuousLegend<-function(values, pal, posx, posy, center=FALSE, endlabs=NULL) {
	return(
		function() {
			if (center) {
				vabs<-max(abs(range(values)))
				values<-seq(-vabs,vabs,length.out=100)
			}
			xdiff<-diff(par()$usr[1:2])
			ydiff<-diff(par()$usr[3:4])
			zvals<-pretty(values)
			zvals<-zvals[which(zvals>min(values) & zvals<max(values))]
			if (max(zvals)<1) {
				digit<-2
			} else {
				digit<-1
			}
			# rect(xleft=par()$usr[1]+(xdiff*posx[1]), ybottom=par()$usr[3]+(ydiff*posy[1]), xright=par()$usr[1]+(xdiff*posx[2]), ytop=par()$usr[4]+(ydiff*posy[2]), xpd=TRUE)
			shape::colorlegend(zlim=range(values), digit=digit, col=brewerCols(seq(0,1,length.out=100), pal), zval=zvals, posx=posx, posy=posy, xpd=TRUE)
			if (!is.null(endlabs)) {
				xcoord<-par()$usr[1] + mean(par()$usr[2:1]*posx*2.2)
				ycoords<-(par()$usr[3] + diff(par()$usr[3:4])*posy) + (diff(par()$usr[3:4]) * c(-0.02,0.02))
				text(x=rep(xcoord, 2), y=ycoords, rev(endlabs), cex=1.2, ad=c(0.5,0.5))
			}
		}
	)
}

categoricalLegend<-function(col,labels, ncol=1) {
	return(
		function() {
			if (ncol==1) {
				legend("top", bg="white", legend=labels, fill=col, bty="n", horiz=TRUE, cex=1.5)
			} else {
				legend(y=par()$usr[3]+(diff(par()$usr[3:4])*0.6), x=par()$usr[1]+(diff(par()$usr[1:2])*0.5), bg="white", legend=labels, fill=col, bty="n", ncol=ncol, cex=1.5, xjust=0.5, yjust=0.5, xpd=TRUE)
			}
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
		if (!asym.setequal(splt[[1]][[3]],names(x@data@pu))) {
			stop(paste(splt[[1]][[3]],"is not a valid column in pu"))
		}
		# parse command
		x@data@puDataFrame[which(x@data@pu$id==splt[[1]][[2]]),splt[[1]][[3]]]=value
	} else {
		stop(paste(splt[[1]][[1]],"is not valid."))
	}
	return(x)
}


# raster processing functions
zonalSum.RasterLayerInMemory <- function(polys, rast, speciesName) {
	tmp<-rcpp_groupsum(getValues(polys),getValues(rast))
	tmp<-data.frame(species=speciesName, pu=attr(tmp, "ids"), amount=c(tmp))
	return(tmp[which(tmp$amount>0),,drop=FALSE])
}

zonalSum.RasterLayerNotInMemory <- function(bs, polys, rast, speciesName, ncores, registered) {
	if (registered & .Platform$OS.type=="windows")
		clusterExport(clust, c("bs","polys", "rast", "rcpp_groupsum"))
	tmp<-rcpp_groupcombine(llply(seq_len(bs$n), .parallel=registered, function(i) {
		return(rcpp_groupsum(getValues(polys, bs$row[i], bs$nrows[i]), getValues(rast, bs$row[i], bs$nrows[i])))
	}))
	tmp<-data.frame(species=speciesName, pu=attr(tmp, "ids"), amount=c(tmp))
	return(tmp[which(tmp$amount>0),,drop=FALSE])
}

hashCall<-function(expr, skipargs=c(), env=parent.frame()) {
	expr<-expr[c((skipargs*-1L)-1L)]
	expr<-expr[which(names(expr)!="force_reset")]
	for (i in seq_along(names(expr)))
		if (inherits(expr[[i]], c("name")))
			expr[[i]]=eval(expr[[i]], envir=env)
	return(paste(deparse(expr), collapse=";"))
}


# Many thanks and all the street creds to user "Charles" on StackOverflow
# http://stackoverflow.com/questions/5307313/fastest-tall-wide-pivoting-in-r
pivot<-function(col, row, value) {
  col<-as.factor(col)
  row<-as.factor(row)
  mat<-array(dim = c(nlevels(row), nlevels(col)), dimnames = list(levels(row), levels(col)))
  mat[(as.integer(col) - 1L) * nlevels(row) + as.integer(row)] = value
  dimnames(mat)<-NULL
  return(mat)
}

## find gdal installation paths
# this function is basically the same as gdalUtils:::set_gdalInstallation but has some functions changed to be compatible with Ubuntu 12
findGdalInstallationPaths<-function (search_path = NULL, rescan = FALSE, ignore.full_scan = TRUE, verbose = FALSE) {
	gdal_python_utilities <- function(path) {
		if (missing(path))
			path <- gdal_path()
		sapply(path, list.files, pattern = "\\.py")
	}
	gdal_drivers <- function(path, verbose = FALSE) {
		if (missing(path)) 
			path <- gdal_path(checkValidity = TRUE)
		cmd <- file.path(path, "gdalinfo")
		cmd <- paste0("\"", cmd, "\"", " --formats")
		drivers_raw <- lapply(cmd, system, intern = TRUE)
		result <- vector(mode = "list", length(path))
		names(result) <- path
		for (i in seq_along(drivers_raw)) {
			drivers_raw[[i]] <- drivers_raw[[i]][-1]
			drivers = strsplit(drivers_raw[[i]], ":")
			driver_names = gsub("^ ", "", sapply(drivers, function(x) {x[2]}))
			driver_codes_perm = strsplit(sapply(drivers, function(x) {x[1]}), "\\(")
			driver_codes = gsub(" ", "", sapply(driver_codes_perm, function(x) {x[1]}), fixed = TRUE)
			driver_perm = gsub("\\)", "", sapply(driver_codes_perm, function(x) {x[2]}))
			r <- w <- u <- v <- s <- rep(FALSE, length(driver_perm))
			r[grep(driver_perm, pattern = "r")] <- TRUE
			w[grep(driver_perm, pattern = "w")] <- TRUE
			u[grep(driver_perm, pattern = "\\+")] <- TRUE
			v[grep(driver_perm, pattern = "v")] <- TRUE
			s[grep(driver_perm, pattern = "s")] <- TRUE
			result[[i]] <- data.frame(format_code = driver_codes, 
				read = r, write = w, update = u, virtualIO = v, 
				subdatasets = s, format_name = driver_names
			)
		}
		return(result[[1]])
	}
	gdal_version <- function(path, newerThan = NULL, verbose = FALSE) {
		if (missing(path))
			path <- gdal_path()
		cmd <- normalizePath(list.files(path, "^gdalinfo$|^gdalinfo\\.exe$", full.names = TRUE))
		cmd <- paste0("\"", cmd, "\"", " --version")
		result <- lapply(cmd, system, intern = TRUE)
		res <- sapply(result, length)
		if (sum(res) != length(result))
		  message("Probably broken install of gdal at '", paste0(path[which(res != 1)], collapse = "' and '"), "'")
		result <- result[res == 1]
		date <- version <- vector(mode = "list", length = length(result))
		for (i in seq_along(result)) {
			ingd <- strsplit(result[[i]], ",")[[1]]
			version[[i]] <- gsub(ingd[1], pattern = "GDAL ", replacement = "")
			ind <- grep(ingd, pattern = "releas")
			date[[i]] <- as.character(as.Date(gsub(ingd[ind], pattern = " released ", replacement = ""), format = "%Y/%m/%d"))
		}
		if (!is.null(newerThan)) {
			test <- try(as.Date(newerThan), silent = TRUE)
			if (!inherits(test, "try-error")) {
				datein <- lapply(date, as.Date)
				res <- sapply(datein, ">=", as.Date(newerThan))
			} else {
				version <- gsub(tolower(version), pattern = "[a-z]", replacement = "")
				res <- sapply(version, strsplit, "\\.")
				newerThan <- strsplit(newerThan, "\\.")[[1]]
				for (i in seq_along(res)) {
					difs <- as.numeric(res[[i]]) - as.numeric(newerThan)
					difs <- sign(difs)
					if (sum(difs == -1) == 0) {
						res[[i]] <- TRUE
					} else {
						if (difs[1] < 0) {
							res[[i]] <- FALSE
						} else if (difs[1] > 0) {
							res[[i]] <- TRUE
						} else if (difs[1] == 0) {
							if (difs[2] < 0) {
								res[[i]] <- FALSE
							} else if (difs[2] > 0) {
								res[[i]] <- FALSE
							}
							else {
								if (difs[3] >= 0) {
									res[[i]] <- TRUE
								}
								else if (difs[3] < 0) {
									res[[i]] <- FALSE
								}
							}
						}
					}
				}
			}
			names(res) <- path
			return(res)
		}
		result <- as.data.frame(cbind(path = path[res == 1], version = version, date = date), stringsAsFactors = FALSE)
		return(result)
	}
	correctPath <- function(x) {
		if (!is.null(x)) {
			if (.Platform$OS.type == "windows") {
				x <- shortPathName(x)
			} else {
				x <- path.expand(x)
			}
			x <- gsub(x, pattern = "\\\\", replacement = "/")
			ind <- substr(x, nchar(x), nchar(x)) != "/"
			x[ind] <- paste0(x[ind], "/")
		}
		return(x)
	}
	gdal_check_validity <- function(path) {
		checkValidity <- sapply(path, function(x) {
			cmd <- normalizePath(list.files(path = x, pattern = "^gdalinfo$|^gdalinfo\\.exe$", full.names = TRUE))
			if (length(cmd) == 0) {
				return(FALSE)
			} else {
			cmd <- paste0("\"", cmd, "\"", " --version")
			validity = length(try(gdal <- system(cmd, intern = TRUE), silent = TRUE))
			return(as.logical(validity))
			}
			}
		)
	}
  gdal_path <- function(search_path = NULL, ignore.options = FALSE, 
						ignore.which = FALSE, ignore.common = FALSE, ignore.full_scan = FALSE, 
						force_full_scan = FALSE, checkValidity, search_path_recursive = FALSE, 
						verbose = FALSE) {
		owarn <- getOption("warn")
		options(warn = -2)
		on.exit(options(warn = owarn))
		if (missing(checkValidity)) {
			if (is.null(getOption("gdalUtils_gdalPath"))) {
				checkValidity = TRUE
			} else {
				checkValidity = FALSE
			}
		}
		path <- NULL
		if (!force_full_scan) {
			if (!ignore.options) {
				if (verbose) 
					message("Checking the gdalUtils_gdalPath option...")
				option_paths <- unlist(sapply(getOption("gdalUtils_gdalPath"), function(x) return(x$path)))
				if (!is.null(option_paths) && checkValidity) {
					option_paths_check <- gdal_check_validity(option_paths)
					option_paths <- option_paths[option_paths_check]
				}
				path <- c(path, option_paths)
			}
			if (!is.null(search_path) && length(path) == 0) {
				if (verbose) 
					message("Checking the search path...")
				if (.Platform$OS == "unix") {
					search_paths <- list.files(path = search_path, pattern = "^gdalinfo$|^gdalinfo\\.exe$", recursive = search_path_recursive, full.names = TRUE)
				} else {
					search_paths <- list.files(path = search_path, 
								 pattern = "^gdalinfo$|^gdalinfo\\.exe$", 
								 recursive = search_path_recursive, full.names = TRUE)
				}
				if (length(search_paths) == 0) {
					search_paths <- NULL
				} else {
					search_paths <- normalizePath(dirname(search_paths))
				}
				if (!is.null(search_paths) && checkValidity) {
					search_paths_check <- gdal_check_validity(search_paths)
					search_paths <- search_paths[search_paths_check]
				}
				path <- c(path, search_paths)
			}
			if (!ignore.which && length(path) == 0) {
				if (verbose) 
					message("Checking Sys.which...")
				Sys.which_path <- dirname(Sys.which("gdalinfo"))
				if (Sys.which_path == "")
					Sys.which_path <- NULL
				if (!is.null(Sys.which_path) && checkValidity) {
					Sys.which_path_check <- gdal_check_validity(Sys.which_path)
					Sys.which_path <- Sys.which_path[Sys.which_path_check]
				}
				path <- c(path, Sys.which_path)
			}
			if (!ignore.common && length(path) == 0) {
				if (verbose) 
					message("Checking common locations...")
				if (.Platform$OS == "unix") {
					common_locations <- c("/usr/bin", "/usr/local/bin", "/Library/Frameworks/GDAL.framework", "/opt/local/bin")
				} else if (.Platform$OS == "windows") {
				common_locations <- c("C:\\Program Files", "C:\\Program Files (x86)", "C:\\OSGeo4W")
				}
				if (length(common_locations != 0)) {
					common_paths <- unlist(sapply(common_locations, 
						function(x) {
							if (.Platform$OS == "unix") {
								search_common_paths <- list.files(path = x, pattern = "^gdalinfo$|^gdalinfo\\.exe$", recursive = TRUE, full.names = TRUE)
							} else {
								search_common_paths <- list.files(path = x, pattern = "^gdalinfo$|^gdalinfo\\.exe$", recursive = TRUE, full.names = TRUE)
							}
							if (length(search_common_paths) == 0) {
								return(search_common_paths)
							} else {
								return(normalizePath(dirname(search_common_paths)))
							}
						}))
					if (length(common_paths) == 0)
						common_paths <- NULL
					if (!is.null(common_paths) && checkValidity) {
						common_paths_check <- gdal_check_validity(common_paths)
						common_paths <- common_paths[common_paths_check]
					}
					path <- c(path, common_paths)
				}
			}
			if (!ignore.full_scan && length(path) == 0) {
				force_full_scan = TRUE
			}
		}
		if (force_full_scan) {
			if (verbose) 
				message("Scanning your root-dir for available GDAL installations,... This could take some time...")
			if (.Platform$OS == "unix") {
				root_dir <- "/"
			}
			if (.Platform$OS == "windows") {
				root_dir <- "C:\\"
			}
			if (.Platform$OS == "unix") {
				search_full_path <- list.files(path = root_dir, pattern = "^gdalinfo$|^gdalinfo\\.exe$", recursive = TRUE, full.names = TRUE)
			} else {
				search_full_path <- list.files(path = root_dir, pattern = "^gdalinfo$|^gdalinfo\\.exe$", recursive = TRUE, full.names = TRUE)
			}
			if (length(search_full_path) == 0)  {
				search_full_path <- NULL
			} else {
				search_full_path <- normalizePath(dirname(search_full_path))
			}
			if (!is.null(search_full_path) && checkValidity) {
				search_full_path_check <- gdal_check_validity(search_full_path)
				search_full_path <- search_full_path[search_full_path_check]
			}
			path <- c(path, search_full_path)
		}
		if (length(path) == 0) {
			return(NULL)
		} else {
			return(correctPath(unique(path)))
		}
	}
	gdal_installation <- function(return_versions = TRUE, return_drivers = TRUE, 
									return_python_utilities = TRUE, sort_most_current = TRUE, 
									rescan = FALSE, search_path = NULL, ignore.full_scan = FALSE, 
									verbose = FALSE)
	{
		if (verbose) 
			message("Scanning for GDAL installations...")
		path <- gdal_path(ignore.options = rescan, search_path = search_path, ignore.full_scan = ignore.full_scan, verbose = verbose)
		if (is.null(path))
			return(NULL)
		gdal_installation_results <- lapply(path, function(x, return_drivers, return_python_utilities, return_versions) {
			result <- list(path = x)
			if (return_versions) {
				version <- gdal_version(x)
				result$version <- unlist(version$version)
				result$date <- unlist(version$date)
			}
			if (return_drivers) {
				result$drivers <- gdal_drivers(x)
			}
			if (return_python_utilities) {
				result$python_utilities <- gdal_python_utilities(x)
			}
			return(result)
		}, return_drivers = return_drivers, return_python_utilities = return_python_utilities, return_versions = return_versions)
		if (sort_most_current) {
			versions <- unlist(sapply(gdal_installation_results, function(x) return(x$date)))
			gdal_installation_results <- gdal_installation_results[order(as.Date(unlist(versions)), decreasing = TRUE)]
		}
		return(gdal_installation_results)
	}
	if (is.null(getOption("gdalUtils_gdalPath")))
		rescan = TRUE
	gdal_installation_out <- gdal_installation(search_path = search_path, 
												rescan = rescan, 
												ignore.full_scan = ignore.full_scan, 
												verbose = verbose
	)
	options(gdalUtils_gdalPath = gdal_installation_out)
	if (is.null(getOption("gdalUtils_gdalPath"))) {
		warning("No GDAL installation found. Please install 'gdal' before continuing:\n\t- www.gdal.org (no HDF4 support!)\n\t- www.trac.osgeo.org/osgeo4w/ (with HDF4 support RECOMMENDED)\n\t- www.fwtools.maptools.org (with HDF4 support)\n")
		if (ignore.full_scan) 
			warning("If you think GDAL is installed, please run:\ngdal_setInstallation(ignore.full_scan=FALSE)")
	} else {
		if (verbose) 
		message("GDAL version ", unlist(getOption("gdalUtils_gdalPath")[[1]]$version))
	}
}
findGdalInstallationPaths()


