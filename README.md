PELVIS
======
[![CRAN Version](http://www.r-pkg.org/badges/version/AnthropMMD)](https://cran.r-project.org/package=PELVIS)
[![pipeline status](https://gitlab.com/f-santos/pelvis/badges/master/pipeline.svg)](https://gitlab.com/f-santos/pelvis/-/commits/master)
[![coverage report](https://gitlab.com/f-santos/pelvis/badges/master/coverage.svg)](https://gitlab.com/f-santos/pelvis/-/commits/master)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/AnthropMMD)](https://cran.r-project.org/package=PELVIS)

## Using PELVIS as a web application
It is *not* the recommended way to use PELVIS, but it is possible to use it online as a web application: https://f-santos.shinyapps.io/pelvis/

This does not require anything else than a web browser and an access to the Internet (in particular, you do not even need to install R), but it might be slower, and the web app will not be updated in a regular basis. This way of using PELVIS should be reserved to non-R users who are not willing to install R on their computer. In any other case, it is better to use PELVIS as an R package, as detailed below.

## Installation of the R package PELVIS from GitLab

### Install prerequisites

1. Install the R package `remotes` by typing the following command line into the R console:
```r
install.packages("remotes")
```

2. Install build environment
    * **Windows:** Install latest version of *[Rtools](https://cran.r-project.org/bin/windows/Rtools/)*.
    * **OSX:** Install *[XCODE](https://developer.apple.com/xcode/)*

### Installing PELVIS

Run the following command in R:
```r        
remotes::install_git('https://gitlab.com/f-santos/pelvis.git')
```

## Installation of the R package PEVIS from CRAN

The latest stable version of PELVIS is also available on CRAN, and can be installed by typing the following command line into the R console :

```r
install.packages("PELVIS", dep = TRUE)
```
	
## Running PELVIS

To start the graphical interface, run the following commands into the R console:
```r
library(PELVIS)
start_pelvis()
```
	
For most devices, it will be advisable to decrease the zoom level in your web browser.
	
## Citing PELVIS

To cite the package in a scientific article, type:
```r
citation("PELVIS")
```

into the R console, after installing the package.
