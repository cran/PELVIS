\name{metavar}
\alias{metavar}
\title{Internal function used to get the five main characters described by Bruzek (2002) from the eleven visual traits observed on the human os coaxe.}
\description{Converts a set of three visual traits to its correspoding main character (e.g. PrSu1, PrSu2 and PrSu3 are converted to PrSu).}
\usage{
metavar(x, threshold=2)
}
\note{
This is an internal function for the R-Shiny application implemented in PELVIS, documented here for testing purposes only.
}
\arguments{
  \item{x}{A vector of characters of length 3, having three possible values: \sQuote{f}, \sQuote{i} or \sQuote{m}.}
  \item{threshold}{For internal purposes only: threshold for the majority rule; set to 2 by default since x is supposed to be of length 3 (cf. Bruzek 2002)}
}
\value{
A unique character value among three possible values: \sQuote{F}, \sQuote{0} or \sQuote{M}.
}
\references{
Bruzek, J. (2002) A method for visual determination of sex, using the human hip bone. \emph{American Journal of Physical Anthropology} \bold{117}, 157--168. doi: 10.1002/ajpa.10012
}
\author{
Frédéric Santos, <frederic.santos@u-bordeaux.fr>
}
\examples{
# Here we create a set of three values for the traits observed on the preauricular surface:
prsu <- c(PrSu1="f", PrSu2="f", PrSu3="m")
prsu
# Final determination for the main character PrSu:
metavar(prsu)
}
