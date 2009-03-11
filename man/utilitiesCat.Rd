\name{utilitiesCat}
\alias{mult.gen}
\alias{mult.gen.a}
\alias{words.all}
\alias{alias3fi}
\alias{gen.check}
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
mult.gen.a(string.vec)
words.all(k, gen, max.length=7)
gen.check(k, gen)
alias3fi(k, gen)
Yates
Letters
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{liste}{ list of words in terms of vectors of factor numbers to be combined into one new word }
  \item{string.vec}{ vector of words in terms of factor letter combinations (e.g. \dQuote{ABC}) to be combined into one new word }
  \item{k}{ number of factors spanning a full factorial with the desired number of runs (i.e. \code{log2(nruns)}) }
  \item{gen}{ generators in any admissible format (\code{gen.check} transforms any admissible format to the default needed for calculations)) }
  \item{max.length}{ the maximum word length to be calculated for the words list; in large design, choosing this to be 4 or 5 may substantially speed things up! }
}
\details{
   \code{Yates} and \code{Letters} are user-visible constants that are useful in design construction: 
   
       \code{Yates} is a list of design column generators in Yates order (for 4096 runs), e.g. \code{Yates[1:8]} is identical to 
       
       \code{list(1,2,c(1,2),3,c(1,3),c(2,3),c(1,2,3))}.
       
       \code{Letters} is the vector of all capital and lower case letters of the Roman alphabet except \code{I} and \code{i}. 
       It is used for default factor names and has been obtained as 
       
       \code{Letters <- c(LETTERS[-9],letters[-9])}.
}
\value{
    \code{mult.gen} and \code{mult.gen.a} determine a new word as the product of the argument words (numeric vector or character string respectively), 
    \code{words.all} calculates a word list object with word length pattern, the word list itself and a max.length entry, 
   \code{gen.check} produces a list of vectors with integers from 1 to k for denoting the generators, which may have come as a vector of factor letter combinations or a vector of column numbers in Yates order,
   and \code{alias3fi} calculates an alias pattern based on the structure of the design (less demanding than the word list approach.
   
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
