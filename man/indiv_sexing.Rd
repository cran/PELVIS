\name{indiv_sexing}
\alias{indiv_sexing}
\alias{indivSexing}
\title{
  Internal function for sexing one single human os coxae using
  revised Bruzek's method (2019)
}
\description{
  Produces a statistical sex estimate from up to eleven
  characters observed on the human os coxae, following Bruzek (2019), and
  using logistic regression models.
}
\usage{
indiv_sexing(ref, new_ind, strategy = c("BIC", "AIC", "None"), trace = 1,
conf_level = 0.95)
}
\note{
  This is mostly an internal function for the R-Shiny application
  implemented in PELVIS.
}
\arguments{
  \item{ref}{A learning dataset for logistic regression models,
  basically the dataset \sQuote{refData} included in PELVIS (or any
  other dataset with the same variables).}
  \item{new_ind}{A new os coxae to be determined, with eleven observed
    traits (possibly with missing values).}
  \item{strategy}{A choice of information criterion (\code{"BIC"} or
    \code{"AIC"}) for variable selection in logistic regression models,
    or \code{"None"} for no variable selection.}
  \item{trace}{Passed to \code{MASS::stepAIC}.}
  \item{conf_level}{Required posterior probability threshold to produce
    a sex estimate.}
}
\value{
  A list with the following components:
  \item{PredictedSex}{One unique character value, \sQuote{F}, \sQuote{I}
    or \sQuote{M}: final sex estimate for the studied os coxae.}
  \item{PostProb}{Posterior probability for the individual to be a
    male.}
  \item{BestModel}{Best logistic regression model for the
    studied os coxae according to the BIC criterion.}
  \item{VariablesUsed}{Names of the variables (including part or all of
    the nonmissing traits for the studied os coaxe) used in this best
    model.}
  \item{cvRate}{Success rate in cross-validation. Cf. Santos et
    al. (2019) for more details about cross-validation here.}
  \item{cvIndet}{Rate of individuals remaining indeterminate using the
    best logistic regression model.}
}
\references{
  Santos, F., Guyomarc'h, P., Rmoutilova, R. and Bruzek, J. (2019) A
  method of sexing the human os coxae based on logistic regressions and
  Bruzek's nonmetric traits. \emph{American Journal of Physical
  Anthropology} \bold{169}(3), 435-447. doi: 10.1002/ajpa.23855
}
\author{
  Frédéric Santos, <frederic.santos@u-bordeaux.fr>
}
\examples{
data(refDataBruzek02)
# Pick the first individual of the reference dataset with its 11 traits, as an example:
individual <- refDataBruzek02[1, -c(1:6)]
individual
# Compute a sex estimate for this individual:
indiv_sexing(ref = refDataBruzek02, new_ind = individual)
}
\concept{hip bone}
\concept{os coxae}
\concept{ossa coxae}
\concept{sex estimation}
\concept{biological anthropology}
