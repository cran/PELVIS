\name{StartPELVIS}
\alias{StartPELVIS}
\title{
An R-Shiny application for the sex estimation of the human os coxae
}
\description{
Launches a graphical user interface (GUI) allowing to use Bruzek's methods (2002, 2019) for sexing the human os coxae, based on eleven visual traits.
}
\usage{
StartPELVIS()
}
\note{
The R console is not available when the GUI is active. To exit the GUI, type Echap (on MS Windows systems) or Ctrl+C (on Linux systems) in the R console.

Regardless of the size and resolution of your screen, for convenience, it is advisable to decrease the zoom level of your web browser and/or to turn on fullscreen mode.
}
\details{
The R-Shiny application proposes two tabs:
\itemize{
\item \sQuote{Data input: manual editing} can be used for both data entry and sex classification. The eleven trichotomic traits are manually edited for each os coxae through the GUI, and the corresponding sex estimates are then produced.
\item \sQuote{Data input: from text file} is the classical way to get the sex estimates for a whole sample of ossa coxae correctly described in a file. PELVIS accepts .CSV or .TXT data files, but does not support .ODS or .XLS(X) files. The predictive factors (i.e. the eleven trichotomic traits) should have the same headers and levels as in the reference dataset \sQuote{refData} included in PELVIS. An example of valid data file can be found on Zenodo: \url{https://doi.org/10.5281/zenodo.2586897} (its field separator is the semicolon ";").
}
In both tabs, two sex estimates are given: the visual sex estimate from Bruzek (2002), and the probabilistic sex estimate from Santos, Guyomarc'h, Rmoutilova and Bruzek (2019). Depending on the traits possibly missing on the ossa coxae submitted to the program, the logistic regression models can use various subsets of best predictors (selected by BIC). The final subset of predictors used for each os coxae is given in the table of results.
}
\value{
The function returns no value by itself, but the results can be downloaded through the graphical interface. The table of results includes the following columns:
\itemize{
\item \sQuote{Sex estimate (Bruzek 2002)}: the visual sex estimate based on Bruzek's method (2002).
\item \sQuote{Statistical sex estimate (2019)}: a sex estimation based on a logistic regression model, following the method described in Santos, Guyomarc'h, Rmoutilova and Bruzek (2019, submitted).
\item \sQuote{Prob(M)} is the probability (obtained with the logistic regression model) that the individual is a man. According to tradition in biological anthropology, we have the following decsion rule: if Prob(M)>0.95 then the sex estimate is \sQuote{M}; if Prob(M)<0.05 then the sex estimate is \sQuote{F}; else the individual remains indeterminate (\sQuote{I}).
\item \sQuote{Prob(F)}, defined as 1-Prob(M), is the probability that the individual is a woman.
\item \sQuote{Selected predictors in LR model}: for a given individual, the sex estimation proceeds as follows. First, a complete model is built using all available (i.e., nonmissing) traits for this individual. Then, a classical stepwise model selection by BIC is performed, and the subset of the most useful traits is used to produce the final sex estimate. This column gives the traits used for each individual.
\item \sQuote{10-fold CV accuracy (\%)}: the rate of correct classification for the corresponding logistic regression model is estimated using a ten-fold cross-validation on the learning sample.
\item \sQuote{Indet. rate in CV (\%)}: the rate of individuals remaining indeterminate in cross-validation for the corresponding logistic regression model.
}
}
\references{
Bruzek, J. (2002) A method for visual determination of sex, using the human hip bone. \emph{American Journal of Physical Anthropology} \bold{117}, 157--168. doi: 10.1002/ajpa.10012

Santos, F., Guyomarc'h, P., Rmoutilova, R. and Bruzek, J. (2019) A method of sexing the human os coxae based on logistic regressions and Bruzek's nonmetric traits. \emph{American Journal of Physical Anthropology}. doi: 10.1002/ajpa.23855
}
\author{
Frédéric Santos, <frederic.santos@u-bordeaux.fr>
}
\examples{
if(interactive()){StartPELVIS()}
}
