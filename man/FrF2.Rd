\name{FrF2}
\alias{FrF2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to provide regular Fractional Factorial 2-level designs }
\description{
  Regular fractional factorial 2-level designs are provided. 
  Apart from obtaining the usual minimum aberration designs in a fixed number of runs, it is possible to 
  request highest number of free 2-factor interactions instead of minimum aberration or to 
  request the smallest design that fulfills certain requirements (e.g. resolution V with 8 factors).
}
\usage{
FrF2(nruns = NULL, nfactors = NULL, factor.names = if (!is.null(nfactors)) {
        if (nfactors <= 50) Letters[1:nfactors] else 
                             paste("F", 1:nfactors, sep = "")} else NULL, 
        default.levels = c(-1, 1), generators = NULL, resolution = NULL, 
        estimable = NULL, max.nfree2fis = FALSE, 
        randomize = TRUE, seed = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nruns}{ Number of runs, must be a power of 2, if given. 
  
  The number of runs can also be omitted. In that case, 
  the function looks for the smallest design of the requested resolution that 
  accomodates \code{nfactors} factors.}
  \item{nfactors}{ The number of 2-level factors to be investigated. 
      Can be omitted, if it is obvious from the \code{factor.names} 
      or \code{nruns} together with \code{generators}.}
  \item{factor.names}{ a character vector of factor names (length up to nfactors) 
      or a named list with names representing factor names and elements vectors 
      of length 2 with factor levels for each factor. Elements can be empty strings. 
      In this case, default levels are used for the respective factor. }
  \item{default.levels}{ default levels (vector of length 2) for all factors for 
      which no specific levels are given }
  \item{generators}{ There are \code{log2(nruns)} basic factors the full factorial 
        of which spans the design. The generators specify how the remaining factors 
        are to be allocated to interactions of these.
        
        Generators can be any of 
        
        a list of vectors with position numbers of basis factors (e.g. c(1,2,4) 
        stands for the interaction between first, second and fourth basis factor) 
        
        a vector of character representations of these interactions 
        (e.g. \dQuote{ABD} stands for the same interaction as above)
        
        a vector of columns numbers in Yates order (e.g. 11 stands for ABD)
        }
  \item{resolution}{ is the arabic numeral for the requested resolution of the design. 
        \code{FrF2} looks for a design with at least this resolution. 
        A design with resolution III (resolution=3) confounds main effects 
        with 2-factor interactions, a design with resolution IV confounds main 
        effects with three-factor interactions or 2-factor interactions with each other, 
        and designs with resolution V or higher are usually regarded as very strong, 
        because all 2-factor interactions are unconfounded with each other and 
        with main effects.}
  \item{estimable}{ Not yet implemented }
  \item{max.nfree2fis}{ logical, defaults to FALSE. 
       If TRUE, maximizing the number of free 2-factor interactions takes 
       precedence over minimizing aberration. 
       Resolution is always considered first.
       
       Most likely, features like this are going to change in the future.}
  \item{randomize}{ logical. If TRUE, the design is randomized. This is the default. }
  \item{seed}{ optional seed for the randomization process }
  \item{\dots}{ currently not used }
}
\details{
  The function works on the basis of the catalogued designs by Chen, Sun and Wu (1993), 
  which are available as \code{\link{catlg}} (a list object of class \code{catlg}). 
  Various improvements are planned for the coming weeks and months, among others 
  inclusion of replication / repeated measurement, blocking and split plot facilities 
  (based on further catalogues), and inclusion of four-level factors. Also, it is 
  planned to support specification of a model to be estimable, and input and output 
  possibilities will be improved.
  
  Please contact me with any suggestions you may have.
}
\value{
  Currently, value is a list with the following three elements
  \item{desnum }{Design matrix in -1/1 coding}
  \item{design }{data frame with levels as requested}
  \item{catentry }{list with one element, which is the entry of \code{catlg} 
       on which the design is based}
}
\references{Chen, J., Sun, D.X. and Wu, C.F.J. (1993) 
A catalogue of 2-level and 3-level orthogonal arrays. 
\emph{International Statistical Review} \bold{61}, 131-145. 
}
\author{ Ulrike Groemping }

\seealso{ See Also \code{\link{pb}} for non-regular fractional factorials according 
to Plackett-Burman and \code{\link{catlg}} for the Chen, Sun, Wu catalogue 
and some accessor functions.}
\examples{
## maximum resolution minimum aberration design with 4 factors in 8 runs
FrF2(8,4)
## the design with changed default level codes
FrF2(8,4, default.level=c("current","new"))
## the design with number of factors specified via factor names (standard level codes)
FrF2(8,factor.names=list(temp="",press="",material="",state=""))
## the design with changed factor names and factor-specific level codes
FrF2(8,4, factor.names=list(temp=c("min","max"),press=c("low","normal"),
     material=c("current","new"),state=c("new","aged")))
## three ways for custom specification of the same design
FrF2(8, generators = "ABC")
FrF2(8, generators = 7)
FrF2(8, generators = list(c(1,2,3)))
## more than one generator
FrF2(8, generators = c("ABC","BC"))
FrF2(8, generators = c(7,6))
FrF2(8, generators = list(c(1,2,3),c(2,3)))
## finding smallest design with resolution 5 in 7 factors
FrF2(nfactors=7, resolution=5)

## maximum resolution minimum aberration design with 9 factors in 32 runs
FrF2(32,9)[3]
## maximum number of free 2-factor interactions instead of minimum aberration
FrF2(32,9,max.nfree2fis=TRUE)[3]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
