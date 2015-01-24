marxan quick start guide
============

First, let's load the 'marxan' R package and some example data.


```r
# load marxan R package
library(marxan)
```

```
## Loading required package: sp
## Loading required package: rgdal
## rgdal: version: 0.9-1, (SVN revision 518)
## Geospatial Data Abstraction Library extensions to R successfully loaded
## Loaded GDAL runtime: GDAL 1.11.1, released 2014/09/24
## Path to GDAL shared files: C:/R/R-3.1.2/library/rgdal/gdal
## GDAL does not use iconv for recoding strings.
## Loaded PROJ.4 runtime: Rel. 4.8.0, 6 March 2012, [PJ_VERSION: 480]
## Path to PROJ.4 shared files: C:/R/R-3.1.2/library/rgdal/proj
## Loading required package: raster
## 
## Attaching package: 'marxan'
## 
## The following objects are masked from 'package:stats':
## 
##     dist, hclust
## 
## The following object is masked from 'package:graphics':
## 
##     dotchart
```

```r
# load example data
data(taspu, tasinvis)
```

This data comes from the ['Introduction to Marxan'](http://marxan.net/courses.html). `taspu` is a `SpatialPolygonsDataFrame` object that contains our planning units, and `tasinvis` is a `RasterLayer` that contains distribution data for 63 broad vegetation classes. Let's take a look at the data.


```r
# plot planning units
plot(taspu)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
# plot vegetation data
plot(tasinvis)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-2.png) 

Each planning unit in the `taspu` object is associated with an id, an acquisition cost, and a value indicating if the most of the unit is already locked up in protected areas. This information is stored in the `data` slot.


```r
# print data in the attribute table for first 20 planning units
head(taspu@data)
```

```
##   id     cost status
## 2  1 60.24638      0
## 3  2 19.86301      0
## 4  3 59.68051      0
## 5  4 32.41614      0
## 6  5 26.17706      0
## 7  6 51.26218      0
```

```r
# plot planning units with colours indicating cost
spplot(taspu, 'cost')
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
# plot planning units with colors indicating status
# units with a status of 2 have most of their area in IUCN protected areas,
# otherwise they have a status of 0
spplot(taspu, 'status')
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-2.png) 

Now, let's make some reserve systems.


```r
# the NUMREPS=100L parameter tells marxan to generate 100 candidate reserve systems
# the BLM=0 parameter indicates that fragmented prioritisations incur no additional penalties
results<-marxan(taspu, tasinvis, NUMREPS=100L, BLM=0)
```

Well, that was easy. Apparently it worked? All we see is text. How can visualise these solutions and assess their quality? We can make some geoplots.











