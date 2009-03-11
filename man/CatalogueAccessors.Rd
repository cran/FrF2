\name{CatalogueAccessors}
\alias{[.catlg}
\alias{res.catlg}
\alias{nruns.catlg}
\alias{nfac.catlg}
\alias{WLP.catlg}
\alias{nfree.2fis.catlg}
\alias{free.2fis.catlg}
\alias{all.2fis.free.catlg}
\alias{catlg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Catalogue file and accessor functions }
\description{
  Functions to select elements or extract information from design catalogues of class catlg
}
\usage{
res.catlg(catlg)
nruns.catlg(catlg)
nfac.catlg(catlg)
WLP.catlg(catlg)
nfree.2fis.catlg(catlg)
free.2fis.catlg(catlg)
all.2fis.free.catlg(catlg)
catlg
\method{[}{catlg}(catlg,i)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{catlg}{ Catalogue of designs of class \code{catlg} (cf. details section)}
  \item{i}{ vector of index positions or logical vector that can be used for indexing a \code{catlg} object }
}
\details{
  The class \code{catlg} is a named list of design entries, 
  currently containing the Chen, Sun and Wu (1993) 2-level designs only 
  (complete list of 2-level fractional factorials up to 32~runs, 
  complete list of resolution IV 2-level fractional factorials with 64~runs). 
  Each design entry is again a list with the following items:
  \itemize{
      \item{res}{resolution, numeric, i.e. 3 denotes resolution III and so forth }
      \item{nfac}{number of factors} 
      \item{nruns}{number of runs} 
      \item{gen}{column numbers of additional factors in Yates order} 
      \item{WLP}{word length pattern (starting with words of length 1, 
              i.e. the first two entries are 0 for all designs in \code{catlg})} 
      \item{nfree.2fis}{number of 2-factor interactions free of aliasing with main effects or other 2-factor interactions} 
      \item{free.2fis}{maxtrix of 2-factor interactions free of aliasing with main effects or other 2-factor interactions} 
      \item{all.2fis.free}{vector of factors with all 2-factor interactions free in the above sense}
      }
   Reference to factors in components \code{nfree.2fis} and \code{all.2fis.free} 
   is via their position number (element of (1:nfac)). 
}

\value{
  \code{[} selects a subset of designs based on \code{i}, which is again a list of class \code{catlg}, even if a single element is selected.
  \code{res}, \code{nruns}, \code{nfac} and \code{nfree.2fis} return a named vector,
  the remaining functions return a list.
}
\references{ 
Chen, J., Sun, D.X. and Wu, C.F.J. (1993) A catalogue of 2-level and 3-level orthogonal arrays. \emph{Int. Statistical Review} \bold{61}, 131-145.}
\author{ Ulrike Groemping }

\seealso{ See Also \code{\link{FrF2}}  }
\examples{
c8 <- catlg[nruns.catlg(catlg)==8]
nfree.2fis.catlg(c8)
free.2fis.catlg(c8)
all.2fis.free.catlg(c8)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
