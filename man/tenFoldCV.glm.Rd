\name{tenFoldCV.glm}
\alias{tenFoldCV.glm}
\title{Internal function for evaluating the performance of logistic regression models used in Bruzek's revised method (2018)}
\description{Error rate, and rate of individuals remaining indeterminate, using 10-fold cross validation for a given logistic regression model in the context of Bruzek's revised method.}
\usage{
tenFoldCV.glm(dat.glm, mod, seuil=0.95)
}
\note{
This is an internal function for the R-Shiny application implemented in PELVIS. Please do not use this function for classical validation of GLMs outside the context of this package.
}
\arguments{
  \item{dat.glm}{A data frame to evaluate the GLM.}
  \item{mod}{GLM model to be evaluated.}
  \item{seuil}{Threshold for sex determination, classically 0.95 following Bruzek's recommendations.}
}
\value{
A complete dataframe of results displayed through the R-Shiny application.
}
\references{
Santos, F., Guyomarc'h, P., Rmoutilova, R. and Bruzek, J. (Submitted to \emph{American Journal of Physical Anthropology}) Probabilistic approach for sex classification using visual traits of the human os coxae following Bruzek (2002)
}
\author{
Frédéric Santos, <frederic.santos@u-bordeaux.fr>
}
