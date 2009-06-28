\name{IAPlot}
\alias{MEPlot}
\alias{IAPlot}
\alias{intfind}
\alias{check}
\alias{remodel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Main Effects and Interaction Plots }
\description{
  Main effects plots and interaction plots are produced. The other documented 
  functions are not intended for users. 
}
\usage{
MEPlot(obj, main = paste("Main effects plot for", respnam), 
    pch = 15, cex.xax = par("cex.axis"), cex.yax = cex.xax, mgp.ylab = 4, 
    cex.title = 1.5, cex.main = par("cex.main"), 
    lwd = par("lwd"), abbrev = 3, select = NULL)

IAPlot(obj, main = paste("Interaction plot matrix for", respnam), 
    pch = c(15, 17), cex.lab = par("cex.lab"), cex = par("cex"), 
    cex.xax = par("cex.axis"), cex.yax = cex.xax, cex.title = 1.5, 
    lwd = par("lwd"), abbrev = 4, show.alias = FALSE)
         
intfind(i, j, mat)

check(obj)

remodel(obj)

}

\arguments{
  \item{obj}{ a linear model object with 2-level factors or numerical 
              2-level variables; 
              the structure must be such that effects are either fully 
              aliased or orthogonal, like in a regular fractional factorial 2-level 
              design}
  \item{main}{ overall title for the plot assembly }
  \item{pch}{ Plot symbol number \code{MEPlot}, 
              or vector of two plot symbol numbers for the lower 
              and higher level of the trace factor \code{iap}}
  \item{cex.xax}{ size of x-axis annotation, defaults to \code{cex.axis}-parameter }
  \item{cex.yax}{ size of y-axis annotation, defaults to cex.xax }
  \item{mgp.ylab}{horizontal placement of label of vertical axis in \code{MEPlot}}
  \item{cex.title}{multiplier for size of overall title 
              (cex.main is multiplied with this factor) } 
  \item{cex.main}{ size of individual plot titles in \code{MEPlot} } 
  \item{cex.lab}{ Size of variable names in diagonal panels of interaction plots
              produced by \code{IAPlot}.}
  \item{cex}{ size of plot symbols in interaction plots }
  \item{lwd}{ line width for plot lines and axes }
  \item{abbrev}{ number of characters shown for factor levels }
  \item{select}{ vector with position numbers of the main effects to be displayed; \cr
              default: all main effects; \cr
              will eventually also be implemented for function IAPlot;
              currently, the model must be reduced in order to omit effects 
              from IAPlot}
  \item{show.alias}{ if TRUE, the interaction plot shows the number 
              of the list entry from aliases(obj)\$aliases 
              (cf. \code{\link{aliases}}) in order to support immediate 
              diagnosis of which depicted interaction may be due to other 
              than the shown effect because of aliasing}
  \item{i}{integer, for internal use only}
  \item{j}{integer, for internal use only}
  \item{mat}{matrix, for internal use only}
}

\details{
  \code{MEPlot} produces plots of all main effects in the model,
  \code{IAPlot} produces plots of all interaction effects in the model,
  \code{intfind} is an internal function not directly useful for users.
  \code{check} is an internal function for checking whether the model complies 
              with assumptions (fractional factorial of 2-level factors 
              with full or no aliasing, not partial aliasing),
  \code{remodel} is an internal function that redoes factor values into -1 and 1 
              coding, regardless of the contrasts that have been used for the 
              original factors;
              numerical data are transformed by subtracting the mean and dividing 
              by half the range (max-min), which also transforms them to -1 and 1 
              coding in the 2-level case (and leads to an error otherwise)
}
\value{
  \code{MEPlot} and \code{IAPlot} are used for their side effects only. 
  
  The internal function \code{check} is used within other functions for checking 
           whether the model is a fractional factorial with 2-level factors and 
           no partial aliasing, as requested for the package to work.  
           It is applied to remodeled objects only and returns a logical.  
           If the returned value is FALSE, the calling function fails. 
              
  The internal function \code{intfind} returns an integer (length 1 or 0). 
           It is not useful for users.
  
  The internal function \code{remodel} is applied to a linear model object and 
  returns a list of two components:
      \item{model }{is the redone model with x-variables recoded to numeric 
              -1 and 1 notation and aov objects made into \dQuote{pure} lm objects}
      \item{labs }{is a list preserving the level information from original factors
               (levels are minus and plus for numerical variables)}

}

\references{ 
    Box G. E. P, Hunter, W. C. and Hunter, J. S. (2005)
    \emph{Statistics for Experimenters, 2nd edition}.
    New York: Wiley.
 }
\author{ Ulrike Groemping }

\seealso{ \code{\link{FrF2-package}} for examples }
\keyword{ design }
