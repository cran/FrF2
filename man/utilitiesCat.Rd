\name{utilitiesCat}
\alias{mult.gen}
\alias{mult.gen.a}
\alias{words.all}
\alias{alias3fi}
\alias{fac.design}
\alias{gen.check}
\alias{estimable.check}
\alias{mapcalc}
\alias{mapcalc.distinct}
\alias{map2design}
\alias{formula2matrix}
\alias{char2num}
\alias{check.subisomorphic.special}
\alias{check.subisomorphic.matrix}
\alias{getNext}
\alias{estimable}
\alias{Yates}
\alias{Letters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~ Internal utility functions and user-visible constants for working with regular fractional factorials ~~ }
\description{
  ~~ Internal functions, mainly for deriving words, alias structures and so forth from catalogue information for regular 2-level fractional factorials. 
  They need not be accessed for normal use of the package - some of them may be made user-usable for future versions of the package. 
  
  The constants Yates and Letters are user-visible, as they can be useful for expert users. ~~
}
\usage{
mult.gen(liste) 
mult.gen.a(string.vec,sep="")
words.all(k, gen, design = NULL, max.length=7, select.catlg = catlg)
alias3fi(k, gen, order = 3)
fac.design(nlevels=NULL, nfactors=NULL, factor.names = NULL, 
        replications=1, repeat.only = FALSE, randomize=TRUE, seed=NULL)
gen.check(k, gen)
estimable.check(estimable, nfac, factor.names)
estimable(estimable, nfac, nruns, clear=FALSE, res3=FALSE, max.time=60, 
            select.catlg=catlg, perm.start=1:nfac, perms=NULL, order = 3)
mapcalc(estimable, nfac, nruns, res3=FALSE)
mapcalc.distinct(estimable, nfac, nruns, res3=FALSE, max.time=60, 
        select.catlg=catlg, perm.start=1:nfac, perms=NULL)
map2design(map)
formula2matrix(formula)
char2num(char)
check.subisomorphic.special(estimable, nfac, hilf2, hilf3=NULL, 
    res3=FALSE, max.time=60, 
    perm.start=1:nfac, begin_time=Sys.time(), name=NA)
check.subisomorphic.matrix(estimable, nfac, hilf2, hilf3=NULL, 
    res3=FALSE, max.time=60, 
    begin_time=Sys.time(), name=NA, perms=perms)
getNext(perm)
Yates
Letters
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{liste}{ list of words in terms of vectors of factor numbers to be combined into one new word }
  \item{string.vec}{ vector of words in terms of factor letter combinations (e.g. \dQuote{ABC}) to be combined into one new word }
  \item{sep}{ character by which interaction terms are separated or concatenated; default is empty, i.e. ABC are separated into A, B and C; 
          for more than 50~factors, a colon is needed so that F1:F2:F127 can be separated into F1, F2 and F127, for example.}
  \item{k}{ number of factors spanning a full factorial with the desired number of runs (i.e. \code{log2(nruns)}) }
  \item{gen}{ generators in any admissible format (\code{gen.check} transforms any admissible format to the default needed for calculations)) }
  \item{design}{ character string that contains a name of a design from \code{select.catlg}, like in \code{\link{FrF2}} }
  \item{max.length}{ the maximum word length to be calculated for the words list; in large design, choosing this to be 4 or 5 may substantially speed things up! }
  \item{select.catlg}{ catalogue of fractional factorial 2-level designs of class \code{catlg} }
  \item{order}{ 2 or 3, order of interactions to be shown }
  \item{nlevels}{ number(s) of levels, vector with \code{nfactors} entries or single number }
  \item{nfactors}{ like in \code{\link{FrF2}} }
  \item{factor.names}{ like in \code{\link{FrF2}} }
  \item{replications}{ like in \code{\link{FrF2}} }
  \item{repeat.only}{ like in \code{\link{FrF2}} }
  \item{randomize}{ like in \code{\link{FrF2}} }
  \item{seed}{ like in \code{\link{FrF2}} }
  \item{estimable}{ like in \code{\link{FrF2}} }
  \item{nfac}{ number of factors, like \code{nfactors} in \code{\link{FrF2}} }
  \item{nruns}{ like in \code{\link{FrF2}} }
  \item{clear}{ like in \code{\link{FrF2}} }
  \item{res3}{ like in \code{\link{FrF2}} }
  \item{max.time}{ like in \code{\link{FrF2}} }
  \item{perm.start}{ like in \code{\link{FrF2}} }
  \item{perms}{ like in \code{\link{FrF2}} }
  \item{map}{ map of experiment factors to design factors, used in \code{estimable} }
  \item{formula}{ formula to be translated into matrix for \code{estimable} }
  \item{char}{ character vector to be translated into matrix for \code{estimable} }
  \item{hilf2}{ list of length four words }
  \item{hilf3}{ list of length three words, if there are any }
  \item{begin_time}{ timing option used for controlling overall run time, calculated }
  \item{name}{ name of design in catalogue, used in case of max.time end for 
               information where the stop occurred }
  \item{perm}{ current permutation, of which \code{getNext} determines the 
               next permutation in lexicographic order }
}
\details{
   \code{Yates} and \code{Letters} are user-visible constants that are useful in design construction: 
   
       \code{Yates} is a list of design column generators in Yates order (for 4096 runs), e.g. \code{Yates[1:8]} is identical to 
       
       \code{list(1,2,c(1,2),3,c(1,3),c(2,3),c(1,2,3))}.
       
       \code{Letters} is the vector of all capital and lower case letters of the Roman alphabet except \code{I} and \code{i}. 
       It is used for default factor names and has been obtained as 
       
       \code{Letters <- c(LETTERS[-9],letters[-9])}.
   
   \code{fac.design} is currently not exported but will eventually be part of another package 
   for general DoE functionality. It creates full factorial designs of all kinds, but has currently only 
   been tested for 2-level fractional factorials.
}
\value{
    \code{mult.gen} and \code{mult.gen.a} determine a new word as the product of the argument words (numeric vector or character string respectively), 
    \code{words.all} calculates a word list object with word length pattern, the word list itself and a max.length entry, 
    \code{alias3fi} calculates an alias pattern based on the structure of the design (less demanding than the word list approach.
    
    \code{fac.design} determines a general full factorial and will be made available for that purpose in the future more generally. 
    It returns a data frame of S3 class \code{\link{design}} with attributes attached. 
    
    \code{gen.check} produces a list of vectors with integers from 1 to k for denoting the generators, 
    which may have come as a vector of factor letter combinations or a vector of column numbers in Yates order,
    \code{estimable.check} determines a list including elements \code{estimable}, 
    a 2-row-matrix indicating estimable 2fis from the permissible inputs, and \code{nfac}, 
       the number of factors from input parameter \code{estimable}, 
    function \code{estimable} calculates the appropriate design if a certain effects have been requested to be estimable, and 
    all subsequent functions are auxiliary functions to function \code{estimable}.
    
   \code{Yates} and \code{Letters} are user-visible constants (cf. details section). 
}
\references{ 
  Hedayat, A.S., Sloane, N.J.A. and Stufken, J. (1999) \emph{Orthogonal Arrays: Theory and Applications}, Springer, New York. 
}
\author{ Ulrike Groemping }
\note{ This package is currently under intensive development. Substantial changes are to be expected in the near future.}
\seealso{ ~~See Also \code{\link{FrF2}} }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
