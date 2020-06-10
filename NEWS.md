# PELVIS 2.0.1 (Release date: 2020-04-27)

## Minor changes
* UI: Fixed typo in `CArc` levels
* The argument `stringsAsFactors = TRUE` is now passed to `read.table()`, for compatibility with R 4.0.0.

# PELVIS 2.0.0 (Release date: 2019-07-22)

## Changes in dependencies
* `PELVIS` now depend on R 3.5.0 or greater.
* `covr` and `testthat` are now suggested.

## Major changes
* A new option to handle different confidence levels for the sex estimates has been added.
* Although still advisable, variable selection in logistic regression models is now optional. A choice between AIC and BIC can also be made.
* Some functions have been renamed or deprecated.

## Minor changes
* More examples added in documentation files
* All comments in R functions have been translated into English
* Small refinements in user interface for the R-shiny application
* Updated citation information

## Reliability
* Unit tests with `testthat`, and continuous integration with GitLab, are now used to improve the reliability of future updates.

# PELVIS 1.2.1 (Release date: 2019-05-13)

## Minor changes

* Updated documentation files
* A third dataset has been included in the package (`rightBonesDataBruzek`)

# PELVIS 1.2.0 (Release date: 2019-04-23)

## Data

* A second dataset has been included in the package (`CTscanDataBruzek`)

## UI changes

* Added banner/logo
* Added icons in UI buttons

## Other changes

* Improved comments in all R functions
* Updated documentation
* Change of LICENSE (GPL3 -> CeCILL 2.1)


# PELVIS 1.1.0 (Release date: 2019-03-11)

## Major changes

* Updated learning dataset (now includes 592 ossa coxae all belonging to distinct individuals)

## Minor improvements

* Improved readability of the results displayed in dataTables


# PELVIS 1.0.5 (Release date: 2018-10-30)

* First release on Gitlab

## Minor improvements

* Replacement of tables by dataTables (from R package 'DT') in the user interface.


# PELVIS 1.0.4 (Release date: 2018-08-05)

First release (on CRAN only).
