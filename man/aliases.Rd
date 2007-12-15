\name{aliases}
\alias{aliases}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Alias structure for fractional factorial 2-level designs}
\description{
  Function to provide a human-friendly format for the alias structure of a 
  fractional factorial 2-level design
}
\usage{
aliases(fit, code = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{ a linear model object with only 2-level factors as x-variables;
              the function will return an error, if the model contains partially
              aliased effects (like interactions in a Plackett-Burman design 
              for most cases) }
  \item{code}{ if TRUE, requests that aliasing is given in code letters (A, B, C 
              etc.) instead of (potentially lengthy) variable names; 
              in this case, a legend is included in the output object.}
}
\value{
  A list with two elements:
  \item{legend }{links the codes to variable names, if \code{code=TRUE}.}
  \item{aliases }{is a list of vectors of aliased effects.}
}
\references{ 
    Box G. E. P, Hunter, W. C. and Hunter, J. S. (2005) 
    \emph{Statistics for Experimenters, 2nd edition}.
    New York: Wiley.
    }
\author{ Ulrike Groemping }
\seealso{ \code{\link{FrF2-package}} for information on the package, 
     \code{\link{alias}} for the built-in R-function, 
     \code{\link{IAPlot}} for effects plots }
\keyword{ design }

\examples{
    ### Injection Molding Experiment. Box et al. 1978.
    data(BM93.e3.data)  #from BsMD
    iMdat <- BM93.e3.data[1:16,2:10]  #only original experiment
    # make data more user-friendly
    colnames(iMdat) <- c("MoldTemp","Moisture","HoldPress","CavityThick",
             "BoostPress","CycleTime","GateSize","ScrewSpeed","y")
    # determine aliases with all 2-factor-interactions
    aliases(lm(y ~ (.)^2, data = iMdat))
    # coded version
    aliases(lm(y ~ (.)^2, data = iMdat), code=TRUE)
    # determine aliases with all 3-factor-interactions
    aliases(lm(y ~ (.)^3, data = iMdat), code=TRUE)
    # determine aliases for unaliased model
    aliases(lm(y ~ ., data = iMdat))
}
