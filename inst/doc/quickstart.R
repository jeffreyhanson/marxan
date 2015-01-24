## ----load_deps, results="hide"-------------------------------------------
# load marxan R package
library(marxan)

# load example data
data(taspu, tasinvis)

## ----plot_data, results="hide", eval=FALSE-------------------------------
#  # plot planning units
#  plot(taspu)
#  
#  # plot vegetation data
#  plot(tasinvis)

## ----view_pu_data, results="hide", eval=FALSE----------------------------
#  # print data in the attribute table for first 20 planning units
#  head(taspu@data)
#  
#  # plot planning units with colours indicating cost
#  spplot(taspu, 'cost')
#  
#  # plot planning units with colors indicating status
#  # units with a status of 2 have most of their area in IUCN protected areas,
#  # otherwise they have a status of 0
#  spplot(taspu, 'status')

## ----run_marxan, results="hide", eval=FALSE------------------------------
#  # the NUMREPS=100L parameter tells marxan to generate 100 candidate reserve systems
#  # the BLM=0 parameter indicates that fragmented prioritisations incur no additional penalties
#  results<-marxan(taspu, tasinvis, targets="50%", NUMREPS=100L, BLM=0)

## ----make_geoplots, results="hide", eval=FALSE---------------------------
#  ## make a geoplot of the best solution
#  plot(results, 0)
#  
#  ## make a geoplot of the second solution
#  # let's also add a kickass google map background
#  # and make the planning unit colours transparent.
#  plot(results, 2, basemap='satellite', alpha=0.8)
#  
#  ## make a geoplot of planning unit selection frequencies
#  # planning units with darker colours were more often
#  # selected for protection than those with lighter colours.
#  plot(results, basemap='satellite', alpha=0.8)

## ----make_dotcharts, results="hide", eval=FALSE--------------------------
#  # make dotchart showing the score of each solution
#  # the score describes the overall value of the prioritisations based on our criteria
#  # the lower the value, the better the solution
#  # the best solution is coloured in red.
#  dotchart(results, var='score')
#  
#  # make a dotchart showing the connectivity of the solutions
#  # the connectivity describes how clustered the selected planning units are
#  # a prioritisation with lots of planning units close together will have a low value
#  # whereas a fragmented prioritisation will have a high value
#  # we can specify arguments to limit the plot to the solutions in with the top 20
#  # connectivity vlaues, and colour the best 5 in red.
#  dotchart(results, var='con', nbest=5, n=20)

## ----mv_analysis, results="hide", eval=FALSE-----------------------------
#  ## dendrogram showing differences between solutions based on which planning units
#  ## were selected (using Bray-Curtis distances by default)
#  # the solutions are shown at the tips of the tree.
#  # solutions that occupy nearby places in tree
#  # have similar sets of planning units selected.
#  # the best prioritisation is coloured in red.
#  dendrogram(results, type='dist', var='selections')
#  
#  ## same dendrogram as above but with the best 10 prioritisations coloured in red
#  # if all the red lines connect together at the bottom of the dendrogram
#  # this means that all the best prioritisations are really similar to each other,
#  # but if they connect near the top of the dendrogram then this means that
#  # some of the best prioritisations have totally different sets of planning units
#  # selected for protection.
#  dendrogram(results, type='dist', var='selections', nbest=10)
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
#  ordiplot(results, type='mds', var='occheld', method='bray')
#  
#  # ordination plot showing differences between solutions based on the amount held
#  # by each vegetation class (using a principle components analysis)
#  # labels are similar to the previous plot.
#  # the arrows indicate the variable loadings.
#  ordiplot(results, type='pca', var='amountheld')

## ----run_marxan_again, results="hide", eval=FALSE------------------------
#  # use the update function to copy the data in results,
#  # tweak the parameters, rerun marxan, and store the
#  # new solutions in results2.
#  results2<-update(results, ~opt(BLM=100))

## ----compare_solutions, results="hide", eval=FALSE-----------------------
#  ## geoplot showing differences between the best solution in
#  ## results and results2
#  plot(results, results2, i=0, j=0)
#  
#  ## geoplot showing differences between the third solution
#  ## in results and the fifth solution in results2
#  plot(results, results2, i=3, j=5)
#  
#  ## geoplot showing different in selection frequencies in the results and results2 objects
#  # blue colours indicate that units were more often selected in
#  # the scenario where we assumed equal costs and that Tasmania has no protected areas.
#  # red  colours indicate that untis were more often selected
#  # in the scenario where we used cost and protected area data
#  # to get more informed prioritisations.
#  plot(results, results2)

