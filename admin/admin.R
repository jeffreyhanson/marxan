# compile c++ attributes
library(Rcpp)
setwd('C:/Users/jeff/Documents/GitHub/marxan')
compileAttributes()

# document code
setwd('C:/Users/jeff/Documents/GitHub/marxan')
library(devtools)
library(roxygen2)
document()

# find obvious errors
setwd('C:/Users/jeff/Documents/GitHub/marxan')
library(devtools)
library(roxygen2)
load_all() 

# formal package tests
setwd('C:/Users/jeff/Documents/GitHub/marxan')
library(devtools)
library(roxygen2)
test()

# local install
library(devtools)
install('C:/Users/jeff/Documents/GitHub/marxan')

# install from github
library(devtools)
install_github('paleo13/marxan')

# make vignettes
library(devtools)
setwd('C:/Users/jeff/Documents/GitHub/marxan')
build_vignettes()

