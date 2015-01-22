marxan
============

#### This R package contains decision support tools for reserve selection using Marxan. It brings the entire Marxan workflow to R. Key features include the ability to prepare input data for Marxan, execute Marxan, and visualise Marxan solutions.

##### Installation

Users running the latest version of R (3.1.2) can ignore this step. Earlier versions of R (ie. 3.0.1, 3.0.2, or 3.1.1) may run into issues installing this package due to issues with the testthat package. In such cases, users should first install an archived version of the 'testthat' R package before trying to install the 'marxan' R package. The code below can be used to install 'testthat' for R version 3.0.2 and on [Marxan.net](http://marxan.net/rstudio/):

```
if (!require('devtools'))
	install.packages('devtools', repo='http://cran.rstudio.com', dep=TRUE)
install_url('http://cran.r-project.org/src/contrib/Archive/testthat/testthat_0.8.1.tar.gz')
```

To install the marxan R package, execute the following commands in R:

```
if (!require('devtools'))
	install.packages('devtools', repo='http://cran.rstudio.com', dep=TRUE)
devtools:::install_github('paleo13/marxan')
```

There have been reports of Linux and Mac OSX users having issues with installing the 'rgdal' and 'rgeos' packages. Users experiencing these issues can try running code below to install these packages:

```
## rgdal on ubuntu 12
# run this in the terminal
sudo apt-get install libgdal-dev
sudo apt-get install libproj-dev
# run this inside R
install.packages("rgdal")

## rgdal on Mac OSX Mavericks
# run this inside R
setRepositories(ind = c(1,6))
install.packages(c('rgeos', 'rgdal'))
```

##### Quick start guide

First, let's load the 'marxan' R package and some example data.

```
# load marxan R package
library(marxan)

# load example data
data(planningunits, species)
```

Funnily enough, the 'planningunits' object has our planning units, and the 'species' object has presence/absence data for some simulated species. Let's take a look at the data.

``` 
# plot planning units
plot(planningunits)

# plot species layers - each panel shows data for a different species
plot(species)
```

Now, let's make some reserve systems.

```
# format data, and run marxan with a low number of iterations so this example
# doesn't take too long
results<-marxan(planningunits, species, NUMITNS=10L, NUMTEMP=8L)


# note the L letters next to the numbers used to set NUMITNS and NUMTEMP:
# these tell R that you mean the integer 10 and not a decimal place number 10
#
# these L characters need to be used after numbers when specifying integer parameters,
# like NUMITNS, and NUMTEMP
```

Ok, so apparently it worked, but how can we visualise the solutions?

```
# make a geoplot of the best solution
plot(results, 0)

# make a geoplot of the second solution
plot(results, 2)

# make a geoplot of planning unit selection frequencies, 
# and slightly transparent colors with google map base map
plot(results, basemap='hybrid', alpha=0.7)
```

We have one hundred solutions. How do we compare them all? We could make dot charts that show various summary statistics.

```
# make dotchart of showing the solution scores of the best 50 solutions
dotchart(results, var='score')

# make dotchart of connectivity, showing best 20 solutions with best 5 five colored in red
dotchart(results, var='con', nbest=5, n=20)
```

How can we visualise the variation in the solutions? Are most of them the same but with a few small differences, or do the solutions tend to fall into two or three main groups?

Fortunately, statisticians solved this problem a long time ago. We can use ordination techniques to create a few variables that describe commonalities in the solutions, and visualise the main sources of variation in a few dimensions.

```
# dendrogram showing differences between solutions based on which planning units 
# were selected (using Bray-Curtis distances by default)
dendrogram(results, type='dist', var='selections')

# ordination plot showing differences between solutions based on the number of units
# occupied by each species (also using Euclidean distances)
ordiplot(results, type='mds', var='occheld', method='euclidean')

# ordination plot showing differences between solutions based on the amount held 
# by each species (using a principle compoenents analysis)
ordiplot(results, type='pca', var='amountheld')
```

Ok, so looking at these solutions we might decide that we need to change a few parameters and rerun Marxan. We can do this--efficiently--by 'updating' our 'results' Marxan object with new parameters, running Marxan again, and storing the results in a new object.

```
# change boundary length and rerun
results2<-update(results, ~opt(BLM=100))

# change the species penalty factor for species 1, lock out planning unit 1,
# reduce the BLM again, and use a different heuristic
results3<-update(results2, ~spp(1, spf=20, target=200) + opt(BLM=70, HEURTYPE=5L))
```

Finally, we can compare the solutions in different runs.

```
# geoplot showing different in selection frequencies in the 'results' and 'results2' objects
plot(results, results2)

# geoplot showing differences between the best solution in 'results2' and 
# the third solution in 'results3'
plot(results2, results3, i=0, j=3)
````


