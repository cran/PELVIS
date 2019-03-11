\name{sexingFromFile}
\alias{sexingFromFile}
\title{Internal function for sexing several single human ossa coxarum using both original and revised Bruzek's methods (2002, 2018)}
\description{Produces sex estimates from each of the ossa coxarum submitted by the user through the graphical user interface of the R-Shiny application.}
\usage{
sexingFromFile(dat, ref, updateProgressBar=NULL)
}
\note{
This is an internal function for the R-Shiny application implemented in PELVIS.
}
\arguments{
  \item{dat}{A test dataset submitted by the user throught the graphical user interface. The predictive factors (i.e. the eleven trichotomic traits) should have the same headers and levels as in the reference dataset \sQuote{refData} included in PELVIS. An example of valid data file can be downloaded here: http://www.pacea.u-bordeaux.fr/IMG/csv/data_test_PELVIS.csv (its field separator is the semicolon ";").}
  \item{ref}{A learning dataset for logistic regression models, basically the dataset \sQuote{refData} included in PELVIS (or any other dataset with the same variables).}
  \item{updateProgressBar}{Internal option for the R-Shiny application.}
}
\value{
A complete dataframe of results displayed through the R-Shiny application.
}
\references{
Santos, F., Guyomarc'h, P., Rmoutilova, R. and Bruzek, J. (Submitted to \emph{American Journal of Physical Anthropology}) A method of sexing the human os coxae based on logistic regressions and Bruzek's nonmetric traits
}
\author{
Frédéric Santos, <frederic.santos@u-bordeaux.fr>
}
