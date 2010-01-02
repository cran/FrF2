\name{estimable.2fis}
\alias{estimable.2fis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Statistical and algorithmic aspects of requesting 2-factor interactions to be estimable in FrF2 }
\description{
  This help page documents the statistical and algorithmic details of requesting 2-factor interactions to be estimable in FrF2 
}
\details{
  The option \code{estimable} allows to specify 2-factor interactions (2fis) that 
  have to be estimable in the model. Per default, it is assumed that a resolution IV 
  model is intended, as it is normally not reasonable to allow main effects to be 
  aliased with other 2-factor interactions in this situation. There are two types of 
  estimability that are distinguished by the setting of option \code{clear} in 
  function \code{link{FrF2}}.
  
  Let us first consider the situation of designs of at least resolution IV. 
  With option \code{clear=TRUE}, \code{FrF2} searches for 
  a model for which all main effects and all 2fis given in \code{estimable} are 
  clear of aliasing with any other 2fis. This is a weaker requirement than resolution V, 
  because 2fis outside those specified in \code{estimable} may be aliased with 
  each other. But it is much stronger than what is done in case of \code{clear=FALSE}: 
  For the latter, \code{FrF2} searches for a design that has a distinct column in 
  the model matrix for each main effect and each interaction requested 
  in \code{estimable}. 
  
  Users can explicitly permit that resolution III designs are included in the 
  search of designs for which the specified 2fis are estimable (by the \code{res3=TRUE} option). 
  In case of \code{clear=TRUE}, this leads to the somewhat 
  strange situation that main effects can be aliased with 2fis from outside 
  \code{estimable} while 2fis from inside \code{estimable} are not aliased with 
  any main effects or 2fis. 
  
  With \code{clear=TRUE}, the algorithms compares the requirement set to 
  catalogued sets of clear 2fis by a graph isomorphism algorithm from R-package 
  \pkg{igraph}. The search is quite fast in this case.
  
  With \code{clear=FALSE}, the algorithm loops through the eligible designs from 
  \code{catlg.select} from good to worse (in terms of MA) and, for each design, loops 
  through all eligible permutations of the experiment factors from \code{perms}. 
  If \code{perms} is omitted, the permutations are looped through in lexicographic 
  order starting from 1:nfac or \code{perm.start}. Especially in this case, 
  run times of the search algorithm can be very long. 
  The \code{max.time} option allows to limit this run time. 
  If the time limit is reached, the final situation (catalogued design and 
  current permutation of experiment factors) is printed so that the user can 
  decide to proceed later with this starting point (indicated by \code{catlg.select} 
  for the catalogued design(s) to be used and \code{perm.start} for the current 
  permutation of experiment factors). 
  Note that - according to the structure of the catalogued designs and the lexicographic 
  order of checking permutations - the initial order of the factors has a strong influence 
  on the run time for larger or unlucky problems. For example, consider 
  an experiment in 32~runs and 11~factors, for six of which the pairwise interactions are to be estimable 
  (Example 1 in Wu and Chen 1992). \code{estimable} for this model can be specified as \cr
    \code{formula("~(F+G+H+J+K+L)^2")} \cr
  OR \cr
    \code{formula("~(A+B+C+D+E+F)^2")}.\cr
    The former runs a lot faster than the latter (I have not yet seen the latter finish 
  the first catalogued design, if \code{perms} is not specified). 
  The reason is that the latter needs more permutations of the experiment factors than 
  the former, since the factors with high positions 
  change place faster and more often than those with low positions. 
  
  For this particular design, it is very advisable to constrain the 
  permutations of the experiment factors to the different subset selections of six factors 
  from eleven, since permutations within the sets do not change the possibility of accomodating 
  a design. The required permutations for the second version of this example 
  can be obtained e.g. by the following code: 
  
  \code{perms.6 <- combn(11,6)}\cr
  \code{perms.full <- matrix(NA,ncol(perms.6),11)}\cr
  \code{for (i in 1:ncol(perms.6))}\cr
  \code{perms.full[i,] <- c(perms.6[,i],setdiff(1:11,perms.6[,i]))}\cr

  Handing perms.full to the procedure using the \code{perms} option makes the second version of the 
  requested interaction terms fast as well, since up to almost 40 Mio permutations of experiment 
  factors are reduced to at most 462. Thus, whenever possible, 
  one should try to limit the permutations necessary in case of \code{clear=FALSE}.
  
  In order to support relatively comfortable creation of distinct designs of some frequently-used types 
  of required interaction patterns, the function \code{\link{compromise}} has been 
  divised: it supports creation of the so-called compromise designs of classes 1 to 4. 
  The list it returns also contains a component \code{perms.full} that can be used as input 
  for the \code{perms} option.
  
  Please contact me with any suggestions for improvements.
}
\references{
Addelman, S. (1962). Symmetrical and asymmetrical fractional factorial plans. 
\emph{Technometrics} \bold{4}, 47-58.

Chen, J., Sun, D.X. and Wu, C.F.J. (1993) 
A catalogue of 2-level and 3-level orthogonal arrays. 
\emph{International Statistical Review} \bold{61}, 131-145. 

Ke, W., Tang, B. and Wu, H. (2005). Compromise plans with clear two-factor interactions. 
\emph{Statistica Sinica} \bold{15}, 709-715.

Wu, C.F.J. and Chen, Y. (1992) 
A graph-aided method for planning two-level experiments when certain interactions 
are important. 
\emph{Technometrics} \bold{34}, 162-175. 
}

\author{ Ulrike Groemping }

\seealso{ See Also \code{\link{FrF2}} for regular fractional factorials and 
\code{\link{catlg}} for the Chen, Sun, Wu catalogue of designs 
and some accessor functions}
\examples{
########## usage of estimable ###########################
  ## design with all 2fis of factor A estimable on distinct columns in 16 runs
  FrF2(16, nfactors=6, estimable = rbind(rep(1,5),2:6), clear=FALSE)
  FrF2(16, nfactors=6, estimable = c("AB","AC","AD","AE","AF"), clear=FALSE)
  FrF2(16, nfactors=6, estimable = formula("~A+B+C+D+E+F+A:(B+C+D+E+F)"), 
       clear=FALSE)
            ## formula would also accept self-defined factor names
            ## from factor.names instead of letters A, B, C, ...
            
  ## estimable does not need any other input
  FrF2(estimable=formula("~(A+B+C)^2+D+E"))

  ## estimable with factor names 
  ## resolution three must be permitted, as FrF2 first determines that 8 runs 
  ##     would be sufficient degrees of freedom to estimate all effects 
  ##     and then tries to accomodate the 2fis from the model clear of aliasing in 8 runs
  FrF2(estimable=formula("~one+two+three+four+two:three+two:four"), 
       factor.names=c("one","two","three","four"), res3=TRUE)
  ## clear=FALSE allows to allocate all effects on distinct columns in the 
  ##     8 run MA resolution IV design
  FrF2(estimable=formula("~one+two+three+four+two:three+two:four"), 
       factor.names=c("one","two","three","four"), clear=FALSE)

  ## 7 factors instead of 6, but no requirements for factor G
  FrF2(16, nfactors=7, estimable = formula("~A+B+C+D+E+F+A:(B+C+D+E+F)"), 
       clear=FALSE)
  ## larger design for handling this with all required effects clear
  FrF2(32, nfactors=7, estimable = formula("~A+B+C+D+E+F+A:(B+C+D+E+F)"), 
       clear=TRUE)
  ## 16 run design for handling this with required 2fis clear, but main effects aliased
  ## (does not usually make sense)
  FrF2(16, nfactors=7, estimable = formula("~A+B+C+D+E+F+A:(B+C+D+E+F)"), 
       clear=TRUE, res3=TRUE)

## example for necessity of perms, and uses of select.catlg and perm.start
## based on Wu and Chen Example 1
  \dontrun{
  ## runs per default about max.time=60 seconds, before throwing error with 
  ##        interim results
  ## results could be used in select.catlg and perm.start for restarting with 
  ##       calculation of further possibilities
  FrF2(32, nfactors=11, estimable = formula("~(A+B+C+D+E+F)^2"), clear=FALSE)
  ## would run for a long long time (I have not yet been patient enough)
  FrF2(32, nfactors=11, estimable = formula("~(A+B+C+D+E+F)^2"), clear=FALSE, 
       max.time=Inf)
  }
  ## can be easily done with perms, 
  ## as only different subsets of six factors are non-isomorphic
  perms.6 <- combn(11,6)
  perms.full <- matrix(NA,ncol(perms.6),11)
  for (i in 1:ncol(perms.6))
     perms.full[i,] <- c(perms.6[,i],setdiff(1:11,perms.6[,i]))
  FrF2(32, nfactors=11, estimable = formula("~(A+B+C+D+E+F)^2"), clear=FALSE, 
      perms = perms.full )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
