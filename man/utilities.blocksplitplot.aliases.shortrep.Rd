\name{utilities.blocksplitplot.aliases.shortrep}
\alias{recalc.alias.block}
\alias{struc.aliased}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Utility functions for short representation of block and splitplot generators }
\description{
  Utility functions for short representation of block and splitplot generators, 
  not intended for direct use
}
\usage{
recalc.alias.block(dia, leg)
struc.aliased(struc, nk, order)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dia}{ a list of alias information from function aliases }
  \item{leg}{ a character vector of legend entries shaped as \code{A=name1} or \code{F1=name1}, 
        depending on whether there are up to 50 factors or more}
  \item{struc}{ an output from function \code{recalc.alias.block} }
  \item{nk}{ number of factors (<=50: Letters; >50: F1, F2, ...) }
  \item{order}{ 2 or 3 (aliasing order) }
}
\details{
   The functions are not intended for direct use. 
}
\value{The functions are not intended for direct use. 
Function \code{recalc.alias.block} outputs a character vector of alias relations, 
function \code{struc.aliased} structures these into a list with main effects, 
2fis and 3fis (the latter for \code{order=3} only). 
These functions are used by function \code{FrF2}.
}
\references{ 
   Groemping, U. (2011). Relative projection frequency tables for orthogonal arrays. 
      Report 1/2011, \emph{Reports in Mathematics, Physics and Chemistry} \url{http://www1.beuth-hochschule.de/FB_II/reports/welcome.htm}, 
      Department II, Beuth University of Applied Sciences, Berlin.
      
  Hedayat, A.S., Sloane, N.J.A. and Stufken, J. (1999) \emph{Orthogonal Arrays: Theory and Applications}, Springer, New York. 
}
\author{ Ulrike Groemping }
\note{ This package is currently under intensive development. Substantial changes are to be expected in the near future.}
\seealso{ See Also \code{\link[FrF2]{FrF2-package}} }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
\keyword{internal}