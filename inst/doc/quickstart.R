## ---- eval=FALSE---------------------------------------------------------
#  # load marxan R package
#  library(marxan)
#  
#  # load example data
#  data(taspu, tasinvis)

## ---- eval=FALSE---------------------------------------------------------
#  # make reserve systems
#  results<-marxan(taspu, tasinvis, targets="20%", NUMREPS=100L, BLM=0)
#  
#  # geoplot for best solution
#  plot(results, 0)
#  
#  # geoplot for selection frequencies
#  plot(results)

## ---- eval=FALSE---------------------------------------------------------
#  # plot distribution of vegetation class 5
#  spplot(results, 5, var='occ')
#  
#  # plot richness in planning units
#  spplot(results, var='occ')

## ---- eval=FALSE---------------------------------------------------------
#  # generate new portfolio of reserve systems
#  results2<-update(results, ~opt(BLM=500))

## ---- eval=FALSE---------------------------------------------------------
#  # geoplot comparing best solutions in each portfolio
#  plot(results, results2, i=0, j=0)
#  
#  # geoplot comparing selection frequencies
#  plot(results, results2, basemap='satellite', alpha=0.4)

## ---- eval=FALSE---------------------------------------------------------
#  # dotchart ranking solutions based on overall score
#  dotchart(results2, var='score')
#  
#  # dotchart ranking solutions based on connectivity
#  dotchart(results2, var='con')

## ---- eval=FALSE---------------------------------------------------------
#  # dendrogram showing how similar solutions are to each other based on selections
#  dendrogram(results2, type='dist', var='selections', nbest=10)
#  
#  # ordination plot show how similar solutions are to each other based on
#  # well they represent different features
#  ordiplot(results2, type='mds', var='amountheld', method='bray')

