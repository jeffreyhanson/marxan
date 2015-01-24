## ------------------------------------------------------------------------
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

