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


# local from github
library(devtools)
install_github('paleo13/marxan')

# check vignettes
library(devtools)
setwd('C:/Users/jeff/Documents/GitHub/marxan')
build_vignettes()