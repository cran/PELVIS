\name{refDataBruzek02}
\alias{refDataBruzek02}
\docType{data}
\title{
Learning dataset for logistic regression models
}
\description{
This dataset includes 592 ossa coxae from five population samples. The eleven trichotomic traits are given for each os coaxe (possibly with missing values for incomplete bones), along with the geographical origin and known sex of the individual. When possible, the age and stature of the individual are also given. This dataset is used as a training sample for the logistic regression models implemented in PELVIS.
}
\usage{data(refDataBruzek02)}
\format{
  A data frame with 592 observations on the following 17 variables:
  \describe{
    \item{\code{Id}}{a factor with 592 levels (unique ID of the individual to whom the bone belongs)}
    \item{\code{Orig}}{a factor with 5 levels (geographical origin)}
    \item{\code{Sex}}{a factor with levels \code{F}, \code{M} (known sex)}
    \item{\code{Age}}{a numeric vector (age of the associated individual in years)}
    \item{\code{Side}}{a factor with levels \code{L}, \code{R} (left or right side)}
    \item{\code{Stature}}{a numeric vector (in cm)}
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
\keyword{hip bone, os coxae, ossa coxae, sex estimation}
\references{
Santos, F., Guyomarc'h, P., Rmoutilova, R. and Bruzek, J. (2019) A method of sexing the human os coxae based on logistic regressions and Bruzek's nonmetric traits. \emph{American Journal of Physical Anthropology}. doi: 10.1002/ajpa.23855

Bruzek, J., Rmoutilova, R., Guyomarc'h, P., & Santos, F. (2019) Supporting data for: A method of sexing the human os coxae based on logistic regressions and Bruzek's nonmetric traits [Data set]. Zenodo. http://doi.org/10.5281/zenodo.2589917
}
