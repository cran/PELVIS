\name{CTscanDataBruzek}
\alias{CTscanDataBruzek}
\docType{data}
\title{
  Dataset including 198 virtually reconstructed ossa coxae
}
\description{
  This dataset includes 198 ossa coxae segmented from CT-scans. The
  eleven trichotomic traits are given for each os coaxe (possibly with
  missing values for incomplete bones), along with the geographical
  origin and known sex of the individual. When possible, the age and
  stature of the individual are also given. This dataset is used as a
  training sample for the logistic regression models implemented in
  PELVIS.
}
\usage{data(CTscanDataBruzek)}
\format{
  A data frame with 198 observations on the following 16 variables:
  \describe{
    \item{\code{Id}}{a factor with 198 levels (unique ID of each os coxae)}
    \item{\code{Indiv}}{a factor with 99 levels (ID of each individual to whom the bone belongs)}
    \item{\code{Sex}}{a factor with levels \code{F}, \code{M} (known sex)}
    \item{\code{Age}}{a numeric vector (age of the associated individual in years)}
    \item{\code{Side}}{a factor with levels \code{L}, \code{R} (left or right side)}
    \item{\code{PrSu1}}{an ordered factor with levels \code{f}, \code{i}, \code{m}}
    \item{\code{PrSu2}}{an ordered factor with levels \code{f}, \code{i}, \code{m}}
    \item{\code{PrSu3}}{an ordered factor with levels \code{f}, \code{i}, \code{m}}
    \item{\code{GrSN1}}{an ordered factor with levels \code{f}, \code{i}, \code{m}}
    \item{\code{GrSN2}}{an ordered factor with levels \code{f}, \code{i}, \code{m}}
    \item{\code{GrSN3}}{an ordered factor with levels \code{f}, \code{i}, \code{m}}
    \item{\code{CArc}}{an ordered factor with levels \code{F}, \code{0}, \code{M}}
    \item{\code{IsPu}}{an ordered factor with levels \code{F}, \code{0}, \code{M}}
    \item{\code{InfP1}}{an ordered factor with levels \code{f}, \code{i}, \code{m}}
    \item{\code{InfP2}}{an ordered factor with levels \code{f}, \code{i}, \code{m}}
    \item{\code{InfP3}}{an ordered factor with levels \code{f}, \code{i}, \code{m}}
  }
}
\references{
Santos, F., Guyomarc'h, P., Rmoutilova, R. and Bruzek, J. (2019) A
method of sexing the human os coxae based on logistic regressions and
Bruzek's nonmetric traits. \emph{American Journal of Physical
Anthropology} \bold{169}(3), 435-447. doi: 10.1002/ajpa.23855

Bruzek, J., Rmoutilova, R., Guyomarc'h, P., & Santos, F. (2019)
Supporting data for: A method of sexing the human os coxae based on
logistic regressions and Bruzek's nonmetric traits [Data
set]. Zenodo. http://doi.org/10.5281/zenodo.2589917
}
\concept{hip bone}
\concept{os coxae}
\concept{ossa coxae}
\concept{sex estimation}
\concept{biological anthropology}
