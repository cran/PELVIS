\name{refData}
\alias{refData}
\docType{data}
\title{
Learning dataset for logistic regression models
}
\description{
This dataset includes 1110 ossa coxa from five population samples. The eleven trichotomic traits are given for each os coaxe (possibly with missing values for incomplete bones), along with the geographical origin and known sex of the individual. When possible, the age and stature of the individual are also given.
}
\usage{data(refData)}
\format{
  A data frame with 1110 observations on the following 17 variables:
  \describe{
    \item{\code{Id}}{a factor with 592 levels (unique ID of the individual to whom the bone belongs)}
    \item{\code{Orig}}{a factor with 5 levels (geographical origin)}
    \item{\code{Sex}}{a factor with levels \code{F}, \code{M} (known sex)}
    \item{\code{Age}}{a numeric vector (age of the associated individual in years)}
    \item{\code{Side}}{a factor with levels \code{L}, \code{R} (left or right side)}
    \item{\code{Stature}}{a numeric vector (in cm)}
    \item{\code{PrSu1}}{a factor with levels \code{f}, \code{i}, \code{m}}
    \item{\code{PrSu2}}{a factor with levels \code{f}, \code{i}, \code{m}}
    \item{\code{PrSu3}}{a factor with levels \code{f}, \code{i}, \code{m}}
    \item{\code{GrSN1}}{a factor with levels \code{f}, \code{i}, \code{m}}
    \item{\code{GrSN2}}{a factor with levels \code{f}, \code{i}, \code{m}}
    \item{\code{GrSN3}}{a factor with levels \code{f}, \code{i}, \code{m}}
    \item{\code{CArc}}{a factor with levels \code{F}, \code{0}, \code{M}}
    \item{\code{IsPu}}{a factor with levels \code{F}, \code{0}, \code{M}}
    \item{\code{InfP1}}{a factor with levels \code{f}, \code{i}, \code{m}}
    \item{\code{InfP2}}{a factor with levels \code{f}, \code{i}, \code{m}}
    \item{\code{InfP3}}{a factor with levels \code{f}, \code{i}, \code{m}}
  }
}
\keyword{hip bone, os coxae, ossa coxa, sex estimation}
\references{
Santos, F., Guyomarc'h, P., Rmoutilova, R. and Bruzek, J. (Submitted to \emph{American Journal of Physical Anthropology}) A probabilistic approach for sex classification using visual traits of the human os coxae following Bruzek (2002)
}
