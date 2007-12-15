\name{DanielPlot}
\alias{DanielPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Normal or Half-Normal Effects Plots }
\description{
  The function is modified from the same-name function in packge \pkg{BsMD}
  with the purpose of providing more usage comfort (correct effect sizes 
  in case of factors, automatic annotation, 
  automatic labelling of the most significant factors only).
}
\usage{
DanielPlot(fit, code = FALSE, autolab = TRUE, alpha = 0.05, faclab = NULL, 
       block = FALSE, datax = TRUE, half = FALSE, pch = "*", 
       cex.fac = par("cex.lab"), cex.lab = par("cex.lab"), 
       cex.pch = par("cex"), cex.legend = par("cex.lab"), 
       main = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{object of class \code{lm}. Fitted model from \code{lm} or \code{aov}.}
  \item{code}{logical. If \code{TRUE} labels \dQuote{A},\dQuote{B}, etc. are used 
        instead of the names of the coefficients (factors). A legend linking 
        codes to names is provided.}
  \item{autolab}{ If TRUE, only the significant factors according to the Lenth method
                  (significance level given by \code{alpha}) are labelled. }
  \item{alpha}{ significanc level for the Lenth method }
  \item{faclab}{NULL or list. 
    If \code{NULL}, point labels are automatically determined according to the 
    setting of \code{code} (i.e. A,B,C etc. for \code{code=TRUE}, natural effect 
    names otherwise) and \code{autolab} (i.e. all effects are labelled if \code{autolab=FALSE},
    only significant effects are labelled if \code{autolab=TRUE}).  
    Otherwise, \code{faclab} can be used for manual labelling of certain effects 
    and should be a list with \code{idx} (integer vector referring to 
    position of effects to be labelled) and \code{lab} (character vector of labels)
    components. 
    }
  \item{block}{logical. If \code{TRUE}, the first factor is labelled as \dQuote{BK} (block).}
  \item{datax}{logical. If \code{TRUE}, the x-axis is used for the factor effects the
                the y-axis for the normal scores. The opposite otherwise.}
  \item{half}{logical. If \code{TRUE}, half-normal plot of effects is display.}
  \item{pch}{numeric or character. Points character.}
  \item{cex.fac}{numeric. Factor label character size.}
  \item{cex.lab}{numeric. Labels character size.}
  \item{cex.pch}{numeric. Points character size.}
  \item{cex.legend}{numeric. Legend size in case of codes.}
  \item{main}{NULL or character. Title of plot. If NULL, automatic title is generated.}
  \item{\dots}{graphical parameters passed to \code{plot}.}
}

\details{
    The design has to be a fractional factorial 2-level design. 
    Effects (except for the intercept) are displayed in a normal or half-normal 
    plot with the effects in the x-axis by default. 
}

\value{
    The function returns an invisible data frame with columns: \code{x}, \code{y},
    \code{no} and \code{effect}, for the coordinates, the position numbers and 
    the effect names for plotted points.
}

\references{
    Box G. E. P, Hunter, W. C. and Hunter, J. S. (2005)
    \emph{Statistics for Experimenters, 2nd edition}.
    New York: Wiley.

    Daniel, C. (1959) 
    Use of Half Normal Plots in Interpreting Two Level Experiments.
    \emph{Technometrics} \bold{1}, 311--340.
    
    Daniel, C. (1976) 
    \emph{Application of Statistics to Industrial Experimentation.}
    New York: Wiley.

    Lenth, R.V. (1989) Quick and easy analysis of unreplicated factorials.
    \emph{Technometrics} \bold{31}, 469--473.
    
    Lenth, R.V. (2006) Lenth s Method for the Analysis of Unreplicated Experiments.
    To appear in \emph{Encyclopedia of Statistics in Quality and Reliability}, Wiley, New York.
    Downloadable at \url{http://www.wiley.com/legacy/wileychi/eqr/docs/sample_1.pdf}.
}
\author{Ernesto Barrios, modified by Ulrike Groemping.}
\seealso{\code{\link{qqnorm}}, \code{LenthPlot}, \code{BsMD-package} }
\keyword{design}
