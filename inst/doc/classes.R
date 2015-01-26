## ----, results="hide", eval=FALSE----------------------------------------
#  library(marxan)
#  data(taspu, tasinvis)

## ----, results="hide", eval=FALSE----------------------------------------
#  # create new MarxanOpts object, with default parameters except BLM and NUMITNS
#  # note that NUMITNS is an integer and must be set using a number followed with an 'L'
#  opts1<-MarxanOpts(BLM=100, NUMITNS=10L)
#  
#  # write to disk to a temporary directory
#  write.MarxanOpts(opts1, tempdir())
#  
#  # show the resulting input.dat file
#  cat(paste(readLines(), collapse="\n"),"\n")
#  
#  # create new Marxanopts object by loading parameters from the input.dat
#  opts2<-read.MarxanOpts()

## ----, results="hide", eval=FALSE----------------------------------------
#  # show all parameters and their values
#  str(opts1)
#  str(opts2)
#  
#  # show BLM parameter with @ operator
#  # the @ operator is conceptually similar to the $ operator,
#  # but used for S4 classes and not S3 classes.
#  opts1@BLM
#  
#  # show PROP parameter with the slot function
#  slot(opts1, 'PROP')

## ----, results="hide", eval=FALSE----------------------------------------
#  # change BLM parameter with @ operator and show it
#  opts1@BLM
#  opts1@BLM<-100
#  opts1@BLM
#  
#  # change HEURTYPE parameter with slot operator
#  opts1@HEURTYPE
#  slot(opts1, 'HEURTYPE')<-5L
#  opts1@HEURTYPE
#  
#  # copy parameters in opts1,
#  # change the NCORES and PROP parameters,
#  # store results in opts3
#  opts1@NCORES
#  opts1@PROP
#  opts3<-update(opts1, ~opt(NCORES=2L, PROP=0.5))
#  opts1@NCORES
#  opts3@NCORES
#  opts1@PROP
#  opts3@PROP

## ----, results="hide", eval=FALSE----------------------------------------
#  ## create MarxanData object from pre-processed data
#  # make pre-processed data
#  pu.dat<-taspu@data
#  spec.dat<-data.frame(id=unique(getValues(tasinvis)),spf=1,target=100)
#  puvspr.dat<-calcPuVsSpeciesData(taspu, tasinvis)
#  bound.dat<-calcBoundaryData(taspu)
#  polyset<-SpatialPolygons2PolySet(taspu)
#  
#  # make MarxanData object
#  mdata1<-MarxanData(pu=pu.dat, species=spec.dat, puvspecies=puvspr.dat, boundary=bound.dat, polygons=polyset)
#  
#  ## create MarxanData object from raw data
#  # format.MarxanData is basically a wrapper for code shown above
#  mdata2<-format.MarxanData(taspu, tasinvis, targets=100, spf=1)
#  
#  ## create MarxanData object from data marxan files
#  # write mdata1 to temporary folder
#  write.MarxanData(mdata1, tempdir())
#  
#  # create new MarxanData object
#  mdata3<-read.MarxanData(tempdir())
#  
#  ## show structure of MarxanData object
#  str(mdata3)

## ----, results="hide", eval=FALSE----------------------------------------
#  # show first 20 rows of species data
#  head(mdata1@species)
#  head(slot(mdata1, 'species'))
#  
#  # show species targets
#  mdata1@species$target
#  slot(mdata1, 'species')$target
#  targets(mdata1)
#  
#  # change species spfs to 5
#  mdata1@species$spf<-5L
#  slot(mdata1, 'species')$spf=5L
#  spfs(mdata1)<-5L
#  
#  # copy data in mdata1,
#  # change target for species 1 to 10,
#  # store new MarxanData object in mdata2
#  data2<-update(mdata1, ~spp(1, target=10))

## ----, results="hide", eval=FALSE----------------------------------------
#  ## create new MarxanUnsolved object using existing objects
#  mu1<-MarxanUnsolved(mopts1, mdata1)
#  
#  ## create new MarxanUnsolved object by reading parameters and data from files
#  # write data to file
#  write.MarxanUnsolved(mu1, tempdir())
#  
#  # read data from file and store in new object
#  input.dat.path<-file.path(tempdir(), 'input.dat')
#  mu2<-read.MarxanUnsolved(input.dat.path)
#  
#  ## create new MarxanUnsvoled object by processing raw data
#  mu3<-marxan(taspus, tasinvis, targets='50%', solve=FALSE)
#  
#  ## show structure of MarxanUnsolved object
#  str(mu3)

## ----, results="hide", eval=FALSE----------------------------------------
#  # copy the data in mu3,
#  # then change the HEURTYPE parameter to 4,
#  # change the CLUMPTYPE parameter to 1,
#  # change the target for species 1 to 2,
#  # change the cost for planning unit 4 to 10,
#  # and store data in mu4
#  mu4<-update(mu3, ~opt(HEURTYPE=4, CLUMPTYPE=1) + spp(1, target=2) + pu(4, cost=10))

## ----, results="hide", eval=FALSE----------------------------------------
#  ## create MarxanResults object
#  # save MarxanUnsolved object to temporary directory
#  write.MarxanUnsolved(mu1, tempdir())
#  
#  # find correct MARXAN program file and store it in options()$marxanExecutablePath
#  findMarxanExecutablePath()
#  
#  # copy the MARXAN program file to the temporary directory
#  file.copy(options()$marxanExecutablePath, file.path(tempdir(), basename(options()$marxanExecutablePath)))
#  
#  # run MARXAN
#  system(paste0('"',file.path(tempdir(), basename(options()$marxanExecutablePath)),
#  	'" "',file.path(tempdir(), 'input.dat'),'"'))
#  
#  # reading MARXAN outputs and store then in a new MarxanResults object
#  mr1<-read.MarxanResults(tempdir())
#  
#  ## show structure for MarxanResults object
#  str(mr1)

## ----, results="hide", eval=FALSE----------------------------------------
#  # show summary data
#  mr1@summary
#  slot(mr1, 'summary')
#  summary(mr1)
#  
#  # show index for best solution
#  mr1@best
#  slot(mr1, 'best')
#  
#  # show log information
#  mr1@log
#  slot(mr1, 'log')
#  log(mr1)
#  
#  # access selections in all solutions
#  mr1@selections
#  slot(mr1, 'selections')
#  selections(mr1)
#  
#  # access selections for best solution
#  mr1@selections[mr1@best,]
#  slot(mr1, 'selections')[mr1@best,]
#  selections(mr1, 0)
#  
#  # access selections for third solution
#  mr1@selections[3,]
#  slot(mr1, 'selections')[3,]
#  selections(mr1, 3)

## ----, results="hide", eval=FALSE----------------------------------------
#  # generate a MarxanSolved object using the marxan function
#  ms1<-marxan(taspus, tasinvis, targets='50%', solve=FALSE)
#  
#  # solve a MarxanUnsolved object
#  ms2<-solve(mu1)
#  
#  # re-solve a MarxanSolved object
#  ms3<-solve(ms1, force_reset=TRUE)
#  
#  # update MarxanUnsolved object
#  ms4<-update(mu1, ~opt(HEURTYPE=2L) + spp(1, spf=5) + pu(4, cost=100))
#  
#  # update MarxanSolved object
#  ms5<-update(ms4, ~opt(HEURTYPE=2L) + spp(1, spf=5) + pu(4, cost=100))

