\name{indivSexing}
\alias{indivSexing}
\title{Internal function for sexing one single human os coxae using revised Bruzek's method (2018)}
\description{Produces a statistical sex estimate from eleven characters observed on the human os coxae, following Bruzek (2018), and using logistic regression models.}
\usage{
indivSexing(ref, newInd)
}
\note{
This is an internal function for the R-Shiny application implemented in PELVIS, documented here for testing purposes only.
}
\arguments{
  \item{ref}{A learning dataset for logistic regression models, basically the dataset \sQuote{refData} included in PELVIS (or any other dataset with the same variables).}
  \item{newInd}{A new os coxae to be determined, with eleven observed traits (possibly with missing values).}
}
\value{
A list with the following components:
	\item{PredictedSex}{One unique character value, \sQuote{F}, \sQuote{I} or \sQuote{M}: final sex estimate for the studied os coxae.}
	\item{PostProb}{Posterior probability for the individual to be a male.}
	\item{BestModel}{Best logistic regression model for the studied os coxae according to the BIC criterion.}
	\item{VariablesUsed}{Names of the variables (including part or all of the nonmissing traits for the studied os coaxe) used in this best model.}
	\item{cvRate}{Success rate in cross-validation. Cf. Santos et al. (2018) for more details about cross-validation here.}
	\item{cvIndet}{Rate of individuals remaining indeterminate using the best logistic regression model.}
}
\references{
Santos, F., Guyomarc'h, P., Rmoutilova, R. and Bruzek, J. (Submitted to \emph{American Journal of Physical Anthropology}) Probabilistic approach for sex classification using visual traits of the human os coxae following Bruzek (2002)
}
\author{
Frédéric Santos, <frederic.santos@u-bordeaux.fr>
}
\examples{
data(refData)
# Pick the first individual of the reference dataset with its 11 traits, as an example:
individual <- refData[1, -c(1:6)]
individual
# Produce a sex estimate for this individual:
indivSexing(ref=refData, newInd=individual)
}
