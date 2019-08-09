\name{godolphin}
\alias{colpick}
\alias{blockgencreate}
\alias{phimax}
\alias{Xcalc}
\alias{blockgengroup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Functions in support of Godolphin's approach for blocking designs }
\description{
  Function colpick handles the creation of X matrices for blocking, 
  function phimax calculations the maximum number of clear 2fis from Godolphin's approach,
  function blockgencreate creates block generators for use in function FrF2, 
  and function Xcalc calculates an X matrix from its first k columns. 
  The functions are meant for expert users only.
}
\usage{
colpick(design, q, all = FALSE, select.catlg = catlg, 
      estimable = NULL, method = "VF2", sort = "natural",
      all0 = FALSE, quiet = FALSE)
phimax(n, q, profile = NULL)
blockgencreate(X, p = 0)
Xcalc(XI, gen)
blockgengroup(X, p = 0, num = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{design}{a character string that identifies a design in the cataloge specified 
       by option \code{select.catlg}, \cr
       OR a class \code{catlg} object with a single entry (of longer, only the first one is used), \cr
       OR an integer number of factors for which a full factorial is assumed.}
  \item{q}{the requested block size is \eqn{2^q}}
  \item{all}{if TRUE (default FALSE), all possible \bold{X} matrices are returned; 
       otherwise, \code{colpick} returns the first successful one (if \code{estimable} 
       is not NULL) or the best one (otherwise)}
  \item{select.catlg}{name of catalogue (not in quotes); 
        only relevant, if \code{design} is a character string}
  \item{estimable}{a specification of 2fis to be kept clear in the blocked design, 
       either as a character vector of pairs of factor letters (using the first 
       elements of `Letters`) or as a two-row matrix of pairs of factor numbers occurring 
       in 2fis)}
  \item{method}{character string identifying a subgraph isomorphism method
       (VF2 or LAD), see \code{\link{FrF2}}}
  \item{sort}{character string specifying a presort strategy for subgraph 
       isomorphism search, see \code{\link{FrF2}}}
  \item{all0}{per default (\code{all0=FALSE}), \bold{X} matrices are requested to be 
       free of all-zero columns. Set \code{all0} to TRUE for permitting aliasing 
       between blocks and factor main effects, i.e. for finding a suitable split-plot
       constructor. }
  \item{quiet}{if TRUE, the message about failure is suppressed (for using the function 
       inside other functions, like \code{FrF2}) }
  \item{profile}{profile to use for calculation 
       (NULL or integer vector of \eqn{2^q-1} elements that sum to \eqn{n}); 
       if NULL, the maximally balanced profile is used (which yields the overall maximum)}
  \item{n}{number of factors}
  \item{X}{a \eqn{q \times n}{q times n} \bold{X} matrix 
       with only 0/1 elements for block construction }
  \item{p}{the number of generated factors (among the n factors); 
       \eqn{2^(n-p)} is the number of runs in the experiment)}
  \item{XI}{a \eqn{q \times k}{q times k} \bold{X}_I matrix 
       with only 0/1 elements, 
       to be extended into a \eqn{q \times n}{q times n} \bold{X} matrix for 
       block construction, given the generators in \code{gen} (\eqn{k=n-p})}
  \item{gen}{generators for extending \code{XI}: \code{gen} can be\cr 
        a class \code{catlg} object (e.g. \code{catlg["7-2.1"]} or 
        \code{catlg[nruns(catlg)==32 & nfac(catlg)==7])}; each \code{nfac} 
        entry must be the sum of \eqn{k} and the length of the \code{gen} element)\cr
        a vector of Yates column numbers (e.g. \code{c(7, 27)})\cr
        a vector of defining contrasts (e.g. \code{c("ABC","ABDE")})
        a list of vectors of base column numbers}
  \item{num}{if TRUE (default FALSE), Yates column numbers are returned 
        instead of their character representations}
}

\details{
    These are the user-visible functions for the Godolphin (2019) approach to blocking. 
    This approach and its implementation are described in Groemping (2019). 
    Direct use of this functions is intended for expert use only.

    Function \code{colpick} is the main workhorse function for blocking larger situations 
    in function \code{\link{FrF2}} 
    (since version 2 of the package, it replaces the earlier approach 
    with function \code{\link{blockpick.big}}); it makes use of function 
    \code{blockgencreate}, and of the internal function \code{blockgengroup}. 
    
    Function \code{phimax} returns the maximum number of 2fis that can 
    be kept clear when blocking a full factorial design in \eqn{n} factors into 
    blocks of size \eqn{2^q}, given the specified profile.
    
    Function \code{blockgencreate} creates block generators from an \bold{X} matrix 
    for blocking a design in \eqn{2^(n-p)=2^k} runs into blocks of size \eqn{2^q}, 
    where \eqn{n} and \eqn{q} are derived from \code{X} as the number of columns and rows, 
    respectively. The generators are returned as a character vector that consists of 
    strings of base factor letters.
    
    Function \code{Xcalc} extends a \eqn{q\times k}{q times k} 
    matrix \bold{X}_I by \eqn{p=n-k} columns 
    (\bold{X}_II in Godolphin notatation) based 
    on the generators provided in \code{gen}.
    
    Function \code{blockgengroup} is internal only.
}

\value{
    Function \code{colpick} returns a list of at least two elements: \cr
    if \code{all} is FALSE, the list consists of the matrix \code{X}, the character vector 
    \code{clear.2fis} and possibly the integer vector \code{map}, \cr
    otherwise of list-valued elements \code{X_matrices}, \code{clearlist} and 
    \code{profiles} and again possibly the integer vector \code{map}.
    
    Function \code{phimax} returns a real number.
    
    Function \code{blockgencreate} returns a character vector of generators
    in terms of \code{Letters} combinations of the first $n-p$ factors.
    
    Function \code{Xcalc} returns a \eqn{q\times n}{\code{q} times \code{n}} 
    matrix (in case of a single generator) or a list of such matrices 
    (if \code{gen} is a class \code{catlg} object with more than one element).
    
    The internal function \code{blockgengroup} returns a character vector of all effects 
    (denoted as base column letter combinations) aliased with the block main effect, 
    or corresponding Yates column numbers.
}

\references{
Groemping, U. (2010). Creating clear designs: a graph-based algorithm and a 
   catalogue of clear compromise plans. 
   \emph{Reports in Mathematics, Physics and Chemistry}, \href{http://www1.beuth-hochschule.de/FB_II/reports/Report-2010-005.pdf}{Report 05/2010}, 
   Department II, Beuth University of Applied Sciences Berlin.
   (Preprint for IIE Transactions; IIE Transactions is available at 
   \url{http://www.tandfonline.com}.)
   
Godolphin, J. (2019). Construction of Blocked Factorial Designs to Estimate 
   Main Effects and Selected Two-Factor Interactions. *Unpublished report* 
   available at \url{https://arxiv.org/abs/1907.02373}.
   
Groemping, U. (2019). Blocking regular 2-level fractional factorial designs 
   with R, while maintaining estimability of selected two-factor interactions. 
   \emph{Reports in Mathematics, Physics and Chemistry}, \href{http://www1.beuth-hochschule.de/FB_II/reports}{technical report}, 
   Department II, Beuth University of Applied Sciences Berlin.

}
\examples{
phimax(7, 2)  ## 16 2fis can be clear, if 128 run full factorial is blocked 
              ## into 32 blocks of size 2^2=4
## estimable in standard letters
requ <- c("BA", "BC", "BD", "BE", "BF", "BG", "BH", "BJ")
## estimability requirement in factor names
fn <- Letters[15:23] ## P to X
requfn <- requ
requfn <- sapply(1:8, function(obj) gsub(Letters[obj], fn[obj], requfn[obj]))

## obtain X matrix for accommodating estimability requirement in 9-4.2
(aus <- colpick("9-4.2", 2, estimable=requ))
## obtain the same matrix manually with Xcalc
XI <- aus$X[,1:5]
## obtain the same matrix manually with Xcalc
all(Xcalc(XI, catlg["9-4.2"])==aus$X)
## inspect X matrices generated from XI
Xcalc(XI, catlg[nruns(catlg)==32 & nfac(catlg)==9 & res(catlg)>=4])

## factor permutation needed
aus$map
## calculate block generators
blockgencreate(aus$X, p=4)
## automatic creation from the design 9-4.2 uses these block generators
summary(FrF2(32, 9, blocks=8, estimable=requ, factor.names=fn, 
    alias.block.2fis = TRUE, select.catlg = catlg["9-4.2"]), 
    brief=TRUE)
## can also be reproduced manually (internal function invperm does the permuting)
summary(FrF2(design="9-4.2", blocks=blockgencreate(aus$X, p=4), 
    factor.names=fn[FrF2:::invperm(aus$map)], 
    alias.block.2fis = TRUE), 
    brief=TRUE)
}
\author{Ulrike Groemping}
\seealso{\code{\link[igraph]{plot.igraph}}, \code{\link[igraph]{tkplot}},
   \code{\link[igraph]{plot.common}} }
\keyword{design}
