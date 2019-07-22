\name{add_metavars}
\alias{add_metavars}
\alias{addMetavars}
\title{Add the five Bruzek's main characters to a dataframe containing
  the eleven basic traits.}
\description{
  From a given dataset including the 11 visual traits exposed
by Bruzek (2002), this function adds three corresponding main characters
(PrSu, GrSN and InfP) based on the majority rule exposed in the original
article.
}
\usage{
add_metavars(dat)
}
\note{
  This is mainly an internal function for the R-Shiny application
  implemented in PELVIS.
}
\arguments{
  \item{dat}{A dataframe including the 11 visual traits described by Bruzek.}
}
\value{
  A dataframe including also the main characters derived from those visual traits.
}
\references{
  Bruzek, J. (2002) A method for visual determination of sex, using the
  human hip bone. \emph{American Journal of Physical Anthropology}
  \bold{117}, 157--168. doi: 10.1002/ajpa.10012}
\author{
  Frédéric Santos, <frederic.santos@u-bordeaux.fr>
}
\examples{
# Load a dataset:
data(CTscanDataBruzek)
# Visualize the traits:
head(CTscanDataBruzek)
# Add all the main Bruzek's characters:
complete <- add_metavars(CTscanDataBruzek)
head(complete)
}
