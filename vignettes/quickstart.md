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















