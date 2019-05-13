PELVIS
======

## Installation of the R package PELVIS using *devtools*

#### Install prerequisites

1. Install *devtools* by typing the following command line into the R console:

		install.packages("devtools")

2. Install build environment
    * **Windows:** Install latest version of *[Rtools](https://cran.r-project.org/bin/windows/Rtools/)*. During the installation process, make sure to select *"Edit the system path"*.
    * **OSX:** Install *[XCODE](https://developer.apple.com/xcode/)*

#### Installing PELVIS

Run the following commands in R:
        
	library(devtools)
	install_git('https://gitlab.com/f.santos/pelvis.git')


## Installation of the R package PEVIS from CRAN

The latest stable version of PELVIS is also available on CRAN, and can be installed by typing the following command line into the R console :

	install.packages("PELVIS", dep=TRUE)
	
## Running PELVIS

To start the graphical interface, run the following commands into the R console:

	library(PELVIS)
	StartPELVIS()
	
For most devices, it will be advisable to decrease the zoom level in your web browser.
	
## Citing PELVIS

The users of PELVIS who are willing to cite the package in a scientific article can find citation information by typing:

	citation("PELVIS")

into the R console, after installing the package.
