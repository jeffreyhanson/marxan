marxan
============

#### This R package contains decision support tools for reserve selection using Marxan. It brings the entire Marxan workflow to R. Key features include the ability to prepare input data for Marxan, execute Marxan, and visualise Marxan solutions.

##### Installation instructions

To install this package, execute the following commands in R:

```

if (!require('devtools'))
	install.packages('devtools', repo='http://cran.rstudio.com', dep=TRUE)
library(devtools)
install_github('paleo13/marxan')

```

Next, download the [Marxan software](http://www.uq.edu.au/marxan/marxan-software), unzip it, and copy the executable files ('MarOpt_v243_Linux32', 'MarOpt_v243_Linux64', 'MarOpt_v243_Mac32', 'MarOpt_v243_Mac64', 'Marxan.exe', and 'Marxan_x64'), into the 'bin' folder where the R package was installed. This 'bin' folder can be found by running:

```
system.file("bin", package="marxan")
```

Finally, verify that R can find these files with:

```
findMarxanExecutablePath()
is.marxanInstalled(verbose=TRUE)
```

If everything works, you should see the message 'marxan R package successfully installed'. If not, try repeating the above steps. Failing that, lodge an issue [here](https://github.com/paleo13/marxan/issues).

##### Quick start guide

First, let's load some example data.

```
# load data
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
# format data and run marxan
results<-marxan(planningunits, species)
```

Ok, so apparently it worked, but how can we visualise the solutions?

```
# make a geoplot of the best solution
plot(results, 0)

# make a geoplot of the second solution
plot(results, 2)

# make a geoplot of planning unit selection frequencies
plot(results)
```

We have one hundred solutions. How can we see how they all compare? We could make dot charts.

```
# make dotchart of showing the solution scores of the best 50 solutions
dotchart(results, var='score')


# make dotchart of connectivity, showing best 20 solutions with best 5 five colored in red
dotchart(results, var='score', nbest=5, n=20)
```

How can we visualise the variation in the solutions? Are most of them the same but with a few small differences, or do the solutions tend to fall into one or two main groups?

Fortunately, statisticians solved this problem a long time ago. We can use dimension reducing techniques to find commonalities in the solutions, and reduce variation in the solutions to a manageable number of dimensions. 

```
# dendrogram showing differences between solutions based on which planning units were selected (using Bray-Curtis distances by default)
dendrogram(results, type='dist', var='selections')

# ordination plot showing differences between solutions based on the number of units occupied by each species (also using Euclidean distances)
ordiplot(results, type='mds', var='occhheld', method='euclidean')

# ordination plot showing differences between solutions based on the amount held by each species (using a principle compoenents analysis)
ordiplot(results, type='pca', var='amountheld')
```

Ok, so looking at these solutions we might decide that we need to change a few parameters and rerun Marxan. We can do this--efficiently--by 'updating' our 'results' Marxan object and storing the results in a new objects.

```
# change boundary length and rerun
results2<-update(results, ~opt(BLM=100))

# change the species penalty factor for species 1, lock out planning unit 1, and  reduce the BLM again
results3<-update(results2, ~opt(BLM=100) + spp(1, spf=3) + opt(BLM=70))

```

Finally, we can compare the solutions in different runs

```
# geoplot showing different in selection frequencies in the 'results' and 'results2' objects
plot(results, results2)

# geoplot showing differences between the best solution in 'results2' and the third solution in 'results3'
plot(results2, results3, i=0, j=3
````


