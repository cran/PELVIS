\name{dataframe_sexing}
\alias{dataframe_sexing}
\alias{sexingFromFile}
\title{Internal function for sexing several human ossa coxae using both
original and revised Bruzek's methods (2002, 2019)}
\description{Produces sex estimates from each of the ossa coxae
submitted by the user through the graphical user interface of the
R-Shiny application.}
\usage{
dataframe_sexing(data, ref, updateProgressBar = NULL, conf_level = 0.95,
strategy = c("BIC", "AIC", "None"), trace = 1)
}
\note{
  This is an internal function for the R-Shiny application implemented in PELVIS.
}
\arguments{
  \item{data}{A test dataset submitted by the user throught the graphical
    user interface. The predictive factors (i.e. the eleven trichotomic
    traits) should have the same headers and levels as in the reference
    dataset \sQuote{refData} included in PELVIS. An example of valid data
    file can be found on Zenodo:
    \url{https://doi.org/10.5281/zenodo.2586897} (its field separator is
    the semicolon ";").}
  \item{ref}{A learning dataset for logistic
    regression models, basically the dataset \sQuote{refDataBruzek02}
    included in PELVIS (or any other dataset with the same variables).}
  \item{updateProgressBar}{Internal option for the R-Shiny application.}
  \item{conf_level}{0.95 by default, confidence level needed to produce
    a sex estimate.}
  \item{strategy}{A choice of information criterion (\code{"BIC"} or
    \code{"AIC"}) for variable selection in logistic regression models,
    or \code{"None"} for no variable selection.}
  \item{trace}{See \code{MASS::stepAIC}.}
}
\value{
  A complete dataframe of results displayed through the R-Shiny application.
}
\references{
  Santos, F., Guyomarc'h, P., Rmoutilova, R. and Bruzek, J. (2019) A
  method of sexing the human os coxae based on logistic regressions and
  Bruzek's nonmetric traits. \emph{American Journal of Physical
  Anthropology}, \bold{169}(3), 435--447. doi: 10.1002/ajpa.23855
}
\author{
  Frédéric Santos, <frederic.santos@u-bordeaux.fr>
}
\concept{hip bone}
\concept{os coxae}
\concept{ossa coxae}
\concept{sex estimation}
\concept{biological anthropology}

