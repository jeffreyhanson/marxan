marxan
============

#### This R package contains decision support tools for reserve selection using Marxan. It brings the entire Marxan workflow to R. Key features include the ability to prepare input data for Marxan, execute Marxan, and visualise Marxan solutions.


To install the marxan R package, execute the following commands in R:

```
if (!require('devtools'))
	install.packages('devtools', repo='http://cran.rstudio.com', dep=TRUE)
devtools:::install_github('paleo13/marxan')
```

Once this package has been installed, you can explore the functions of this package by reading through the vignettes. This package comes with a quick start guide, called 'quickstart', and a more in depth tutorial, called 'marxan'. You can access them via R by running the code below.

```
# open vignettes in web browsers
vignette('quickstart', package='marxan')
vignette('marxan', package='marxan')
```

**If this R package helped you, please cite it along with the main Marxan citation.**

Hanson, J.O. & Watts, M.E. 2015. marxan: Decision support tools for reserve selection in R using Marxan. R package version 1.0.0. URL https://github.com/paleo13/marxan.

Ball, I.R., Possingham, H.P., and Watts, M.E. 2009. Marxan and relatives: Software for spatial conservation prioritisation. Chapter 14: Pages 185-195 in Spatial conservation prioritisation:   Quantitative methods and computational tools. Eds Moilanen, A., Wilson, K.A., Possingham H.P. Oxford University Press, Oxford, UK. 

