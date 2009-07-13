\name{CatalogueAccessors}
\alias{[.catlg}
\alias{print.catlg}
\alias{res.catlg}
\alias{nruns.catlg}
\alias{nfac.catlg}
\alias{WLP.catlg}
\alias{nclear.2fis.catlg}
\alias{clear.2fis.catlg}
\alias{all.2fis.clear.catlg}
\alias{catlg}
\alias{block.catlg}
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
nclear.2fis.catlg(catlg)
clear.2fis.catlg(catlg)
all.2fis.clear.catlg(catlg)
catlg
\method{[}{catlg}(catlg,i)
\method{print}{catlg}(x, name="all", nruns="all", nfactors="all", 
                        res.min=3, MaxC2=FALSE, show=10, 
                        gen.letters=FALSE, show.alias=FALSE, ...)
block.catlg
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{catlg}{ Catalogue of designs of class \code{catlg} (cf. details section)}
  \item{i}{ vector of index positions or logical vector that can be used for indexing a \code{catlg} object }
  \item{x}{ an object of class \code{catlg} }
  \item{name}{ character vector of entry names from \code{x}; default \dQuote{all} means: no selection made }
  \item{nruns}{ numeric integer (vector), giving the run size(s) for entries of \code{x} to be shown; 
                default \dQuote{all} means: no selection made  }
  \item{nfactors}{ numeric integer (vector), giving the factor number(s) for entries of \code{x} to be shown; 
                default \dQuote{all} means: no selection made }
  \item{res.min}{ numeric integer giving the minimum resolution for entries of \code{x} to be shown }
  \item{MaxC2}{ logical indicating whether designs are ordered by minimum aberration (default, \code{MaxC2=FALSE}) 
                or by maximum number of clear 2fis (\code{MaxC2=TRUE})}
  \item{show}{ integer number indicating maximum number of designs to be shown; default is 10 }
  \item{gen.letters}{ logical indicating whether the generators should be shown as 
                column numbers (default, \code{gen.letters=FALSE}) or as generators with 
                factor letters (e.g. E=ABCD, \code{gen.letters=TRUE})}
  \item{show.alias}{ logical indicating whether the alias structure (up to 2fis) is to be printed }
  \item{\dots}{ further arguments to function \code{print} }
  \item{block.catlg}{ data frame with block generators for full factorial designs up to 256~runs, 
                taken from Sun, Wu and Chen (1997)}
}
\details{
  The class \code{catlg} is a named list of design entries. 
  Each design entry is again a list with the following items:
  \itemize{
      \item{res}{resolution, numeric, i.e. 3 denotes resolution III and so forth }
      \item{nfac}{number of factors} 
      \item{nruns}{number of runs} 
      \item{gen}{column numbers of additional factors in Yates order} 
      \item{WLP}{word length pattern (starting with words of length 1, 
              i.e. the first two entries are 0 for all designs in \code{catlg})} 
      \item{nclear.2fis}{number of clear 2-factor interactions (i.e. free of aliasing with main effects or other 2-factor interactions)} 
      \item{clear.2fis}{2x\code{nclear.2fis} matrix of clear 2-factor interactions 
              (clear to be understood in the above sense) } 
      \item{all.2fis.clear}{vector of factors with all 2-factor interactions clear in the above sense}
      }
  Reference to factors in components \code{clear.2fis} and \code{all.2fis.clear} 
  is via their position number (element of (1:nfac)). 
   
  The \code{print} function for class \code{catlg} gives a concise overview of selected designs in any design catalogue of class \code{catlg}. 
  It is possible to restrict attention to designs with certain run sizes, numbers of factors, and/or to request a minimum resolutions. 
  Designs are ordered in decreasing quality, where the default is aberration order, but number of clear 2fis can be requested alternatively. 
  The best 10 designs are displayed per default, but this number can be changed (by the \code{show} option. 
  Options \code{gen.letters} and \code{show.alias} influence the style and amount of printed output.
   
  The catalogue \code{catlg}, which is included with package \code{FrF2}, 
  is of class \code{catlg} and currently contains 
  \itemize{
  \item the Chen, Sun and Wu (1993) 2-level designs 
  (complete list of 2-level fractional factorials from 4 to 32~runs, 
  complete list of resolution IV 2-level fractional factorials with 64~runs). 
  Note that the Chen Sun Wu paper only shows a selection of the 64~run designs, the 
  complete catalogue has been obtained from Don Sun and is numbered according to 
  minimum aberration (lower number = better design); numbering in the paper is 
  not everywhere in line with this numbering
  
  \item a few \dQuote{good} resolution IV designs in 128 runs, for up to 24 factors 
  obtained by evaluating designs from the complete catalogue by Xu (2009, catalogue on his website),
  for 25 to 64 factors taken from Block and Mee (2005, with corrigendum 2006)
  
  \item the best (MA) resolution V design for each number of factors in 256 and 512 runs, 
  the best resolution VI design for each number of factors in 1024 runs, 
  the best resolution VII design for each number of factors  in 2048 runs 
  and the best resolution VIII design each number of factors in 4096 runs. 
  These are taken from the website of Xu (2009), where complete catalogues of these designs 
  are provided. 
  }
  
  It is planned to extend the availability of large designs in catalogue \code{catlg}, e.g. by 
  adding good resolution IV designs of 256 runs or other \dQuote{good designs} provided by 
  Mee (2009) and by Xu (2009). 
}

\value{
  \code{[} selects a subset of designs based on \code{i}, which is again a list of class \code{catlg}, even if a single element is selected.
  \code{res}, \code{nruns}, \code{nfac} and \code{nclear.2fis} return a named vector, 
  the \code{print} method does not return anything (i.e. it returns \code{NULL}), and 
  the remaining functions return a list.
}
\references{ 
Block, R. and Mee, R. (2005) Resolution IV Designs with 128 Runs \emph{Journal of Quality Technology} \bold{37}, 282-293.

Block, R. and Mee, R. (2006) Corrigenda \emph{Journal of Quality Technology} \bold{38}, 196.

Chen, J., Sun, D.X. and Wu, C.F.J. (1993) A catalogue of 2-level and 3-level orthogonal arrays. \emph{Int. Statistical Review} \bold{61}, 131-145.

Mee, R. (2009). \emph{A Comprehensive Guide to Factorial Two-Level Experimentation}. 
New York: Springer.
    
Sun, D.X., Wu, C.F.J. and Chen, Y.Y. (1997). 
Optimal blocking schemes for 2\^p and 2\^(n-p) designs. \emph{Technometrics} \bold{39}, 
298-307. 

Xu, H. (2009) Algorithmic Construction of Efficient Fractional Factorial Designs With Large Run Sizes. To appear in \emph{Technometrics}.
}
\author{ Ulrike Groemping }

\seealso{ See Also \code{\link{FrF2}}  }
\examples{
c8 <- catlg[nruns.catlg(catlg)==8]
nclear.2fis.catlg(c8)
clear.2fis.catlg(c8)
all.2fis.clear.catlg(c8)

## usage of print function for inspecting catalogued designs
## the first 10 resolution V+ designs in catalogue catlg
print(catlg, res.min=5)
## the 10 resolution V+ designs in catalogue catlg with the most factors
## (for more than one possible value of nfactors, MaxC2 does usually not make sense)
print(catlg, res.min=5, MaxC2=TRUE)

## designs with 12 factors in 64 runs (minimum resolution IV because 
## no resolution III designs of this size are in the catalogue)
## best 10 aberration designs
print(catlg, nfactors=12, nruns=64)
## best 10 clear 2fi designs
print(catlg, nfactors=12, nruns=64, MaxC2=TRUE)
## show alias structure
print(catlg, nfactors=12, nruns=64, MaxC2=TRUE, show.alias=TRUE)
## show best 20 designs
print(catlg, nfactors=12, nruns=64, MaxC2=TRUE, show=20)

## use vector-valued nruns 
print(catlg, nfactors=7, nruns=c(16,32))
## all designs (as show=100 is larger than available number of designs)
##    with 7 or 8 factors in 16 runs
print(catlg, nfactors=c(7,8), nruns=16, show=100)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
