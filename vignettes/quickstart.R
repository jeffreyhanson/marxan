## ----load_deps, results="hide"-------------------------------------------
# load marxan R package
library(marxan)

# load example data
data(taspu, tasinvis)

## ------------------------------------------------------------------------
# plot planning units
plot(taspu)

# plot vegetation data
plot(tasinvis)

## ------------------------------------------------------------------------
# print data in the attribute table for first 20 planning units
head(taspu@data)

# plot planning units with colours indicating cost
spplot(taspu, 'cost')

# plot planning units with colors indicating status
# units with a status of 2 have most of their area in IUCN protected areas,
# otherwise they have a status of 0
spplot(taspu, 'status')

## ------------------------------------------------------------------------
# the NUMREPS=100L parameter tells marxan to generate 100 candidate reserve systems
# the BLM=0 parameter indicates that fragmented prioritisations incur no additional penalties
results<-marxan(taspu, tasinvis, targets="50%", NUMREPS=100L, BLM=0)

## ------------------------------------------------------------------------
## make a geoplot of the best solution
plot(results, 0)

## make a geoplot of the second solution
# let's also add a kickass google map background
# and make the planning unit colours transparent.
plot(results, 2, basemap='satellite', alpha=0.8)

## make a geoplot of planning unit selection frequencies
# planning units with darker colours were more often
# selected for protection than those with lighter colours.
plot(results, basemap='satellite', alpha=0.8)

## ------------------------------------------------------------------------
# make dotchart showing the score of each solution
# the score describes the overall value of the prioritisations based on our criteria
# the lower the value, the better the solution
# the best solution is coloured in red.
dotchart(results, var='score')

# make a dotchart showing the connectivity of the solutions
# the connectivity describes how clustered the selected planning units are
# a prioritisation with lots of planning units close together will have a low value
# whereas a fragmented prioritisation will have a high value
# we can specify arguments to limit the plot to the solutions in with the top 20
# connectivity vlaues, and colour the best 5 in red.
dotchart(results, var='con', nbest=5, n=20)

