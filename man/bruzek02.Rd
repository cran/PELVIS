\name{bruzek02}
\alias{bruzek02}
\title{
  Internal function for sexing the human os coxae using Bruzek's
  method (2002)
}
\description{
  Produces a single (and non-probabilistic) sex estimate from five
  characters observed on the human os coxae, following Bruzek (2002)
}
\usage{
bruzek02(x)
}
\note{
  This is mainly an internal function for the R-Shiny application
  implemented in PELVIS.
}
\arguments{
  \item{x}{A character vector of length 5, having three possible values:
  \sQuote{F}, \sQuote{0} or \sQuote{M}.}
}
\value{
  One unique character value, \sQuote{F}, \sQuote{I} or \sQuote{M},
  according to the majority rule exposed by Bruzek (2002).
}
\references{
  Bruzek, J. (2002) A method for visual determination of sex, using the
  human hip bone. \emph{American Journal of Physical Anthropology}
  \bold{117}, 157--168. doi: 10.1002/ajpa.10012}
\author{
  Frédéric Santos, <frederic.santos@u-bordeaux.fr>
}
\examples{
# Here we create manually an individual:
individual <- c(PrSu = "M", GrSN = "F",
                CArc = "F", InfP = "0", IsPu = "F")
individual
# Determination produced by Bruzek (2002): female individual.
bruzek02(individual)
}
