## ----, eval=FALSE--------------------------------------------------------
#  # load marxan R package
#  library(marxan)
#  
#  # load example data
#  data(taspu, tasinvis)

## ----, eval=FALSE--------------------------------------------------------
#  # plot first 12 vegetation classes
#  # grey colors indicate absence
#  # green colors indicate presence
#  # try making the plotting window larger if
#  # you only see gray
#  plot(tasinvis)

## ----, eval=FALSE--------------------------------------------------------
#  # the attribute table for a shapefile is stored
#  # in the data slot which can be accessed with @data
#  # print first 20 rows of attribute table
#  head(taspu@data)
#  
#  # plot planning units
#  plot(taspu)
#  
#  # plot planning units,
#  # and colour by acquisition cost
#  spplot(taspu, 'cost')
#  
#  # plot planning units,
#  # and colour by present level of protection
#  # 0 = not already protected
#  # 2 = already protected
#  spplot(taspu, 'status')

## ----, eval=FALSE--------------------------------------------------------
#  # copy taspu
#  taspu2<-taspu
#  
#  # set costs
#  taspu2@data$cost<-1
#  
#  # set status values
#  # note the 'L' after the zero is used to indicate
#  # that we mean the integer zero and not the decimal
#  # place number zero
#  taspu2@data$status<-0L
#  
#  # show first 20 rows of taspu
#  # to check they are the same as before
#  head(taspu@data)
#  
#  # show first 20 rows of taspu2
#  # to check they are different
#  head(taspu2@data)

## ----, eval=FALSE--------------------------------------------------------
#  # argument to targets is level of protection
#  # argument to spf is the species penalty factor for each vegetation class
#  # argument to NCORES defines the number of threads for parallel processing
#  # argument to NUMREPS controls the number of solutions in our portfolio
#  # argument to BLM controls boundary length modifier
#  results<-marxan(taspu2, tasinvis, targets="20%", spf=1, NUMREPS=100L, NCORES=2L, BLM=0, lengthFactor=1e-5)

## ----, eval=FALSE--------------------------------------------------------
#  # histogram of proportion of vegetation classes adequately
#  # represented in each solution
#  # if many of the solutions adequately represented the classes
#  # most of the bins would be close to 1, whereas if
#  # the solutions failed to represent the classes most of the
#  # bins would be close to zero
#  hist(rowMeans(targetsmet(results)), freq=TRUE, xlim=c(0,1), las=1,
#  	main='Histogram of representation in portfolio',
#  	ylab='Frequency of solutions',
#  	xlab='Proportion of veg. classes adequately represented'
#  )

## ----, eval=FALSE--------------------------------------------------------
#  # geoplot distribution of vegetation class 5
#  spplot(results, 5, var='occ')
#  
#  # geoplot richness in planning units
#  # with a satellite base map
#  spplot(results, var='occ', basemap='satellite')

## ----, eval=FALSE--------------------------------------------------------
#  # copy the MARXAN parameters and pre-processed data in results,
#  # update the SPF parameter for all species,
#  # run MARXAN,
#  # load the solutions back into R,
#  # store the solutions in results2
#  results2<-update(results, ~spp(1:63, spf=rep(1,63)))

## ----, eval=FALSE--------------------------------------------------------
#  # get levels of representation in each portfolio
#  results.repr<-rowMeans(targetsmet(results))
#  results2.repr<-rowMeans(targetsmet(results2))
#  
#  # create 2 plotting areas in the one window
#  par(mfrow=c(1,2))
#  
#  # histogram of first portfolio
#  hist(results.repr, freq=TRUE, xlim=c(0,1), las=1,
#  	ylab='Frequency of solutions',
#  	xlab='Proportion of veg. classes adequately represented',
#  	main="Level of representation with SPF=1"
#  )
#  
#  # print best level of representation
#  print(max(results.repr))
#  
#  # histogram of second portfolio
#  # if you see a giant single rectangle this means
#  # all the solutions have the same level of representation
#  hist(results2.repr, freq=TRUE, xlim=c(0,1), las=1,
#  	ylab='Frequency of solutions',
#  	xlab='Proportion of veg. classes adequately represented',
#  	main="Level of representation with SPF=100"
#  )
#  
#  # print best level of representation
#  print(max(results2.repr))

## ----, eval=FALSE--------------------------------------------------------
#  # make a geoplot of the best solution
#  plot(results2, 0)
#  
#  # make a geoplot of the second solution,
#  # with kickass google map background and transparent colors
#  plot(results2, 2, basemap='satellite', alpha=0.4)
#  
#  # make a geoplot of planning unit selection frequencies,
#  # planning units with darker colours were more often
#  # selected for protection than those with lighter colours.
#  plot(results2, basemap='satellite', alpha=0.4)
#  
#  # make a geoplot of selection frequencies using different colours
#  # see Color Brewer (http://colorbrewer2.org/) for available
#  # colour ramps
#  plot(results2, colramp='YlGnBu')

## ----, eval=FALSE--------------------------------------------------------
#  # get planning unit ids
#  pu.ids<-taspu@data$id
#  
#  # get planning unit costs
#  pu.costs<-taspu@data$cost
#  
#  # get planning unit statuses
#  pu.status<-taspu@data$status
#  
#  # copy input parameters and data in results2,
#  # change planning unit costs and statuses
#  # rerun MARXAN,
#  # load outputs into R and store them in results3
#  results3<-update(results2, ~pu(pu.ids, cost=pu.costs, status=pu.status))

## ----, eval=FALSE--------------------------------------------------------
#  # geoplot showing differences between the best solution in each portfolio
#  plot(results2, results3, i=0, j=0)
#  
#  # geoplot showing differences between the third solution
#  # in results2 and the fifth solution in results3
#  plot(results2, results3, i=3, j=5)
#  
#  # geoplot showing difference in selection frequencies between the two objects
#  # white colors indicate that units are already in a protected area
#  # blue colours indicate that units were more often selected in results2
#  # red  colours indicate that units were more often selected in results3
#  plot(results2, results3)

## ----, eval=FALSE--------------------------------------------------------
#  ## generate list of portfolios with different BLMS
#  # make vector BLM parameters to use
#  blm.pars=c(0, 100, 250, 500, 750, 1000)
#  
#  # create list with different portfolio for each BLM
#  results4<-list()
#  for (i in seq_along(blm.pars)) {
#  	results4[[i]]<-update(results3, ~opt(BLM=blm.pars[i], NUMREPS=10L))
#  }
#  
#  ## extract data from portfolios
#  # create empty vectors to store values
#  cost<-c()
#  con<-c()
#  blm<-c()
#  
#  # extract values for best solutions
#  for (i in seq_along(blm.pars)) {
#  	cost<-append(cost, summary(results4[[i]])[["Cost"]])
#  	con<-append(con, summary(results4[[i]])[["Shortfall"]])
#  	blm<-append(blm, rep(blm.pars[i], nrow(summary(results4[[i]]))))
#  }
#  
#  ## plot trade-off between shortfall and connectivity
#  # get colours for legend
#  legend.cols<-c("#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C", "#F03B20", "#BD0026")
#  pt.cols<-legend.cols[match(blm, blm.pars)]
#  
#  # reset plotting window
#  par(mfrow=c(1,1))
#  
#  # plot trade-off data
#  # higher shortfall values means worse representation
#  # higher connectivity values mean more fragmentation
#  plot(cost~con, bg=pt.cols, col='black', ylab='Cost', xlab='Connectivity', pch=21,
#  	main='Trade-off between cost and connectivity')
#  abline(lm(cost~con))
#  
#  # add legend
#  legend("topright", legend=blm.pars, col='black', pt.bg=legend.cols, pch=21, title='BLM')

## ----, eval=FALSE--------------------------------------------------------
#  # make new solutions with BLM=0.0001
#  results5<-update(results3, ~opt(BLM=blm.pars[2]))
#  
#  # geoplot showing differences between the best solution in each portfolio
#  plot(results5, results3, i=0, j=0)
#  
#  # geoplot showing difference in selection frequencies between the two objects
#  # black colours indicate that units are already in a protected area
#  # blue colours indicate that units were more often selected in results4[[2]],
#  # and red colours indicate that they were often selected in results3
#  plot(results5, results3)

## ----, eval=FALSE--------------------------------------------------------
#  # make dotchart showing the score of each solution
#  # the score describes the overall value of the prioritisations based on our criteria
#  # the lower the value, the better the solution
#  # the best solution is coloured in red
#  dotchart(results5, var='score')
#  
#  # make dotchart showing the connectivity of the solutions
#  # solutions with lower values are more clustered
#  # solutions with higher values are more fragmented
#  # argument to n specifies the number of solutions to plot
#  # argument to nbest specifies number of solutions to colour in red
#  dotchart(results5, var='con', nbest=5, n=20)

## ----, eval=FALSE--------------------------------------------------------
#  ## dendrogram showing differences between solutions based on which planning units
#  ## were selected (using Bray-Curtis distances by default)
#  # the solutions are shown at the (bottom) tips of the tree.
#  # solutions that occupy nearby places in tree
#  # have similar sets of planning units selected.
#  # the best prioritisation is coloured in red.
#  dendrogram(results5, type='dist', var='selections')
#  
#  ## same dendrogram as above but with the best 10 prioritisations coloured in red
#  # if all the red lines connect together at the bottom of the dendrogram
#  # this means that all the best prioritisations are really similar to each other,
#  # but if they connect near the top of the dendrogram then this means that
#  # some of the best prioritisations have totally different sets of planning units
#  # selected for protection.
#  dendrogram(results5, type='dist', var='selections', nbest=10)
#  
#  ## ordination plot showing differences between solutions based on the number of units
#  ## occupied by each vegetation class (using MDS with Bray-Curtis distances)
#  # we can also use multivariate techniques to see how the solutions vary
#  # based on how well they represent different vegetation classes.
#  # the numbers indicate solution indices.
#  # solutions closer to each other in this plot have more
#  # similar levels of representation for the same species.
#  # the size of the numbers indicate solution quality,
#  # the bigger the number, the higher the solution score.
#  ordiplot(results5, type='mds', var='occheld', method='bray')
#  
#  # ordination plot showing differences between solutions based on the amount held
#  # by each vegetation class (using a principle components analysis)
#  # labels are similar to the previous plot.
#  # the arrows indicate the variable loadings.
#  ordiplot(results5, type='pca', var='amountheld')

