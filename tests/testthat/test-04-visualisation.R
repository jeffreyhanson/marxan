context('04-visualisation')

# test statistical methods
test_that("Marxan visualisation methods don't work", {
	# generate data
	template<-disaggregate(raster(matrix(1:9, ncol=3), xmn=0, xmx=1, ymn=0, ymx=1, crs=CRS('+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ')),fact=5)
	polys<-rasterToPolygons(template, n=4, dissolve=TRUE)	
	species<-stack(llply(1:10, function(x) {setValues(template, round(runif(ncell(template))))}))
	# generate base objects
	md<-format.MarxanData(
		polygons=polys,
		rasters=species
	)
	mo<-MarxanOpts(NUMITNS=10L, NUMTEMP=4L)
	# generate unsolved object
	mu<-MarxanUnsolved(mo, md)
	ms<-solve(mu, clean=TRUE)
	ms2<-update(ms, ~opt(BLM=200))
	## stats functions
	# distances
	x<-dist(ms@results, var="selections")
	x<-dist(ms@results, var="selections")
	x<-dist(ms@results, var="selections", force_reset=TRUE)
	x<-dist(ms, var="amountheld")
	x<-dist(ms, var="amountheld")
	x<-dist(ms, var="amountheld", force_reset=TRUE)
	# nmds
	x<-mds(ms@results, var="selections")
	x<-mds(ms@results, var="selections")
	x<-mds(ms@results, var="selections", force_reset=TRUE)
	x<-mds(ms, var="amountheld")
	x<-mds(ms, var="amountheld")
	x<-mds(ms, var="amountheld", force_reset=TRUE)
	# pca
	x<-pca(ms@results, var="selections")
	x<-pca(ms@results, var="selections")
	x<-pca(ms@results, var="selections", force_reset=TRUE)
	x<-pca(ms, var="amountheld")
	x<-pca(ms, var="amountheld")
	x<-pca(ms, var="amountheld", force_reset=TRUE)
	# hclust
	x<-hclust(ms@results, var="selections")
	x<-hclust(ms@results, var="selections")
	x<-hclust(ms@results, var="selections", force_reset=TRUE)
	x<-hclust(ms, var="amountheld")
	x<-hclust(ms, var="amountheld")
	x<-hclust(ms, var="amountheld", force_reset=TRUE)
	## plotting functions
	# dendrogram
	dendrogram(ms@results, var="selections", nbest=1)
	dendrogram(ms, var="amountheld", nbest=2)
	# ordiplot
	ordiplot(ms@results, var="amountheld", nbest=1)
	ordiplot(ms@results, type='pca', var="amountheld", nbest=1)
	ordiplot(ms, var="amountheld", nbest=2)
	# dotchart
	dotchart(ms@results, var="score", nbest=1)
	dotchart(ms, var="con", nbest=2)
	# plot
	plot(ms)
	plot(ms, basemap="hybrid")
	plot(ms, 1, basemap="hybrid")
	plot(ms, 0, basemap="hybrid")
	plot(ms, ms2)
	plot(ms, ms2, i=0)
	plot(ms, ms2, i=0, j=1)
	plot(ms, ms2, basemap='hybrid')
	plot(ms, ms2, i=0, basemap='hybrid')
	plot(ms, ms2, i=0, j=1, basemap='hybrid')
})
