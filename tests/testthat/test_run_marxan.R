library(rgdal)
library(raster)
library(maptools)
library(plyr)
library(data.table)

# test MarxanOpts methods
test_that("MarxanOpts methods don't work", {
	opts1<-MarxanOpts()
	opts1<-update(opts1,~opt(BLM=5))
	opts2<-MarxanOpts(BLM=5)
	write.MarxanOpts(opts2, tempdir())
	opts3<-read.MarxanOpts(file.path(tempdir(), "input.dat"))
	expect_equal(opts1@BLM, opts2@BLM, opts3@BLM)
})

# test MarxanData methods
test_that("MarxanData methods don't work", {
	# make some data
	template<-disaggregate(raster(matrix(1:9, ncol=3), xmn=0, xmx=1, ymn=0, ymx=1, crs=CRS('+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ')),fact=5)
	polys<-rasterToPolygons(template, n=4, dissolve=TRUE)	
	species<-setValues(template, round(runif(ncell(template))))
	# construction method 1
	md1<-MarxanData(
		pu=data.frame(
			id=seq_len(nrow(polys@data)),
			cost=10,
			status=0
		),
		species=data.frame(id=1L, spf=1, target=100, name='spp'),
		puvspecies=calcPuVsSpeciesData(polys,species, id=1L),
		boundary=calcBoundaryData(polys)
	)
	write.MarxanData(md1, tempdir())
	md1@species[1,"spf"]=2
	md1@pu[5,"status"]=3
	# construction method 2
	md2<-read.MarxanData(tempdir())
	md2<-update(md2,~pu(5,status=3) + spp('spp',spf=2))
	# construction method 3
	md3<-format.MarxanData(
		polys,
		species,
		targets=100,
		spf=2
	)
	md3<-update(md2,~pu(5,status=3))
	# tests
	expect_equal(md1@pu,md2@pu)
	expect_equal(md3@pu,md2@pu)

	expect_equal(md1@species,md2@species)
	expect_equal(md1@species,md3@species)
	
	expect_equal(md1@puvspecies,md2@puvspecies)
	expect_equal(md1@puvspecies,md3@puvspecies)
	
	expect_equal(md1@puvspecies_spo,md2@puvspecies_spo)
	expect_equal(md1@puvspecies_spo,md3@puvspecies_spo)
	
	expect_equal(md1@boundary,md2@boundary)
	expect_equal(md1@boundary,md3@boundary)
})

# test MarxanUnsolved methods
test_that("MarxanUnsolved methods don't work", {
	# generate data
	template<-disaggregate(raster(matrix(1:9, ncol=3), xmn=0, xmx=1, ymn=0, ymx=1, crs=CRS('+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ')),fact=5)
	polys<-rasterToPolygons(template, n=4, dissolve=TRUE)	
	species<-setValues(template, round(runif(ncell(template))))
	# generate base objects
	md1<-MarxanData(
		pu=data.frame(
			id=seq_len(nrow(polys@data)),
			cost=10,
			status=0
		),
		species=data.frame(id=1L, spf=1, target=100, name='spp'),
		puvspecies=calcPuVsSpeciesData(polys,species, id=1L),
		boundary=calcBoundaryData(polys)
	)
	md2<-update(md1,~spp(1,spf=5) + pu(3, cost=2))
	mo1<-MarxanOpts()
	mo2<-MarxanOpts(NUMITNS=10)
	# generate objects
	mu1<-MarxanUnsolved(mo1, md1)
	mu1<-update(mu1, ~opt(NUMITNS=10) + spp(1,spf=5) + pu(3, cost=2), FALSE)
	mu2<-MarxanUnsolved(mo2, md2)
	# equality tests
	expect_equal(mu1@data@pu,mu2@data@pu)
	expect_equal(mu1@data@species,mu2@data@species)
	expect_equal(mu1@opts@NUMITNS,mu2@opts@NUMITNS)
})


# test MarxanUnsolved and MarxanSolved basic functions
test_that("MarxanUnsolved methods don't work", {
	# generate data
	template<-disaggregate(raster(matrix(1:9, ncol=3), xmn=0, xmx=1, ymn=0, ymx=1, crs=CRS('+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ')),fact=5)
	polys<-rasterToPolygons(template, n=4, dissolve=TRUE)	
	species<-setValues(template, round(runif(ncell(template))))
	# generate base objects
	md<-MarxanData(
		pu=data.frame(
			id=seq_len(nrow(polys@data)),
			cost=10,
			status=0
		),
		species=data.frame(id=1L, spf=1, target=100, name='spp'),
		puvspecies=calcPuVsSpeciesData(polys,species, id=1L),
		boundary=calcBoundaryData(polys)
	)
	mo<-MarxanOpts(NUMITNS=10)
	# generate unsolved object
	mu<-MarxanUnsolved(mo, md)
	# try getter methods
	print(mu)
	names(mu)
	# check that marxan can be found
	findMarxanExecutablePath()
	is.marxanInstalled()
	# try solving it using update function and with paralleling
	ms<-update(mu, ~opt(BLM=101) + pu(5, cost=30) + spp(1, spf=10) + opt(NCORES=2))
	# try results getters
	expect_identical(score(ms, 1), score(ms@results, 1))
	expect_identical(score(ms, 0), score(ms@results, 0))
	expect_identical(score(ms), score(ms@results))
	
	expect_identical(amountheld(ms, 1), amountheld(ms@results, 1))
	expect_identical(amountheld(ms, 0), amountheld(ms@results, 0))
	
	expect_identical(amountheld(ms), amountheld(ms@results))
	expect_identical(occheld(ms, 1), occheld(ms@results, 1))
	expect_identical(occheld(ms, 0), occheld(ms@results, 0))
	expect_identical(occheld(ms), occheld(ms@results))
	
	expect_identical(targetsmet(ms, 1), targetsmet(ms@results, 1))
	expect_identical(targetsmet(ms, 0), targetsmet(ms@results, 0))
	expect_identical(targetsmet(ms), targetsmet(ms@results))
	
	expect_identical(mpm(ms, 1), mpm(ms@results, 1))
	expect_identical(mpm(ms, 0), mpm(ms@results, 0))
	expect_identical(mpm(ms), mpm(ms@results))

	expect_identical(sepacheived(ms, 1), sepacheived(ms@results, 1))
	expect_identical(sepacheived(ms, 0), sepacheived(ms@results, 0))
	expect_identical(sepacheived(ms), sepacheived(ms@results))	
	
	# test read and writes
	write.MarxanSolved(ms, tempdir())
	ms2<-read.MarxanSolved(file.path(tempdir(), 'input.dat'))
	expect_equal(ms@results@summary, ms2@results@summary)
	expect_equal(ms@results@selections, ms2@results@selections)
	expect_equal(ms@results@mpm, ms2@results@mpm)
	expect_equal(ms@results@occheld, ms2@results@occheld)
	expect_equal(ms@results@amountheld, ms2@results@amountheld)
	expect_equal(ms@results@sepacheived, ms2@results@sepacheived)
})

