\name{block}
\alias{block}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Statistical and algorithmic aspects of blocking in FrF2 }
\description{
  This help page documents the statistical and algorithmic details of blocking in FrF2 
}
\details{
  Blocking is done with the purpose to balance the design with respect to a factor 
  that is known or strongly suspected to have an influence but is not in itself of interest, 
  and it is usually assumed that block factors do not interact with experimental factors. 
  Examples are batches of material that are not large enough to accomodate the complete 
  experiment so that e.g. half the experiment is done on the first batch and the other half 
  on the second batch (two blocks). The block factor should be orthogonal to the experimental 
  factors, at least to their main effects. Per default, it is also requested that the block 
  factor is orthogonal to the 2-factor interactions. This can be changed by the user, if no 
  such design can be found.

  Blocking is currently implemented for regular fractional factorial designs only.\cr
  There are two principal ways to handle blocked designs, manual definition 
  (i.e. the user specifies exactly which columns are to be used for which purpose) and automatic 
  definition. Each situation has its specifics. These are detailed below. For users with 
  not so much mathematical/statistical background, it will often be best to use the automatic way, 
  specifying the treatement factors of interest via \code{nfactors} or \code{factor.names} 
  and a single number for \code{blocks} or \code{WPs}. 
  Users with more mathematical background may want to use the manual definitions, perhaps 
  in conjunction with published catalogues of good block designs, or 
  after inspecting possibilities with functions \code{\link{blockpick}} or \code{\link{blockpick.big}}.
  
  \describe{
  \item{Manual definition of blocked designs for regular fractional factorials}{
  The user can start from a design with a number of factors and manually specify which factors or 
  interactions are to be used as block generators. If this route is chosen, \code{blocks} 
  can be a vector of factor names or factor letters, or of the same form as generators, except that 
  not only base factors but all factors can be used and single factors are permitted 
  (which would lead to resolution II designs if used in generators). For example, \cr
    \code{block = Letters[c(2,4,5)]} \cr
  or \cr
    \code{block = list(2,4,5)} \cr
  specify that the 2nd, 4th and 5th factor are to be used as block generators, while \cr
    \code{block = c("Day","Shift")}\cr
  indicates that the named factors \dQuote{Day} and \dQuote{Shift} specified in \code{factor.names} 
  are to be treated as blocking factors). In this case, the number of blocks or whole plots is calculated, 
  and a new factor with the default name \dQuote{Blocks} (in general the name chosen in 
  option \code{block.name}) is generated, which would for example contain as levels 
  the Day/Shift combinations. It is also possible to choose interaction effects rather than factors themselves 
  as block generators, e.g. \cr
    \code{block = c("ABCD","EFGH") } \cr
  or\cr
    \code{block = list(c(1,2,3,4),c(5,6,7,8)) }. \cr
  The chosen effects and all interactions between them generate the blocks. 
  CAUTION: If the user manually generates a blocked design, it is his/her responsibility to ensure a 
  good choice of design (e.g. by using a catalogued design from Bisgaard 1994, 
  Sun, Wu and Chen 1997, or Cheng and Wu 2002).
  }
    \item{Automatic definition of blocked designs for regular fractional factorials}{
  If the user only specifies the number of blocks required for the experiment, function \code{\link{FrF2}} 
  automatically generates the blocks. For full factorial designs, function \code{\link{FrF2}} uses 
  the Sun, Wu and Chen (1997) catalogue of blocked designs (implemented in function \code{\link{blockpick}}). 
  Otherwise, depending on the size of the problem, 
  function \code{\link{FrF2}} uses function \code{\link{blockpick}} or function \code{\link{blockpick.big}} 
  for finding an appropriate allocation of block generator columns: 
  Smaller problems \cr
  (\code{choose(nruns-1-nfactors,k.block) < 100000}) \cr
  are treated with \code{\link{blockpick}}. 
  
  The search for an appropriate blocked design starts with the overall best unblocked design 
  (in terms of aberration or MaxC2, if requested). 
  If this best design does not yield an adequate blocking possibility, the search continues with 
  the next best design and so forth. \cr
  For the smaller problems, function \code{\link{blockpick}} 
  looks for \code{k.block} independent subsets among the eligible columns of the design. 
  (The eligible columns are all columns of the Yates matrix that are neither occupied 
  by treatment main effects nor by 2fis among treatments (if \code{alias.block.2fis=FALSE}, 
  which is the default), or all columns of the Yates matrix that are not occupied by treatment main effects 
  (if \code{alias.block.2fis=TRUE}). Note that no effort is made to avoid aliasing with 2-factor interactions, 
  if \code{alias.block.2fis=TRUE} is chosen. 
  
  For the larger problems, 
  function \code{\link{blockpick.big}} permutes the k~base factors of candidate designs with \code{nfactors + k.block} factors 
  in search of a design the first \code{k.block}~factors of which can be used for block construction. In the latter case, any 
  specification of design (via options \code{design} or \code{generators}) is ignored. Note that function 
  \code{\link{blockpick.big}} is not guaranteed to find an existing blocked design.
  
  Sun, Wu and Chen (1997) provide a catalogue of blocked designs 
  with a few quality criteria, and they state that there is no single best design, but that the choice 
  depends on the situation. \code{FrF2} always comes up with one specific solution design. 
  Comparisons to the catalogued designs in Sun, Wu and Chen (1997) have shown that 
  the designs found in \code{FrF2} are often but not always isomorphic to the catalogued ones. 
  Differences do occur, especially if the base designs are resolution III, or if \code{\link{blockpick.big}} 
  has to be used. Users who want to be certain to use a \dQuote{best} blocked design should manually 
  implement a specific catalogued design or inspect several solutions from functions \code{\link{blockpick}} 
  \code{\link{blockpick.big}}. 
  }
  }
  
  Please contact me with any suggestions for improvements.
}
\references{
Bisgaard, S. (1994a). Blocking generators for small 2\^(k-p) designs. 
\emph{J. Quality Technology} \bold{26}, 288-294.

Chen, J., Sun, D.X. and Wu, C.F.J. (1993) 
A catalogue of 2-level and 3-level orthogonal arrays. 
\emph{International Statistical Review} \bold{61}, 131-145. 

Cheng, C.-S. and Tsai, P.-W. (2009). 
Optimal two-level regular fractional factorial block and split-plot designs. 
\emph{Biometrika} \bold{96}, 83-93.

Cheng, S.W. and Wu, C.F.J. (2002). 
Choice of optimal blocking schemes in 2-level and 3-level designs. 
\emph{Technometrics} \bold{44}, 269-277. 

Sun, D.X., Wu, C.F.J. and Chen, Y.Y. (1997). 
Optimal blocking schemes for 2\^p and 2\^(n-p) designs. \emph{Technometrics} \bold{39}, 
298-307. 

}
\author{ Ulrike Groemping }

\seealso{ See Also \code{\link{FrF2}} for regular fractional factorials, 
\code{\link{catlg}} for the Chen, Sun, Wu catalogue of designs 
and some accessor functions, 
and \code{\link{splitplot}} for the statistical aspects of split-plot designs.}
\examples{
########## automatic blocked designs ###################
## from a full factorial ##
FrF2(8,3,blocks=2)
## with replication
run.order(FrF2(8,3,blocks=2,wbreps=2))
run.order(FrF2(8,3,blocks=2,wbreps=2,repeat.only=TRUE))
run.order(FrF2(8,3,blocks=2,bbreps=2))
run.order(FrF2(8,3,blocks=2,bbreps=2,wbreps=2))

## automatic blocked design with fractions
FrF2(16,7,blocks=4,alias.block.2fis=TRUE)
## isomorphic non-catalogued design as basis
FrF2(16,gen=c(7,11,14),blocks=4,alias.block.2fis=TRUE)
## FrF2 uses blockpick.big and ignores the generator
FrF2(64,gen=c(7,11,14),blocks=16,alias.block.2fis=TRUE)

########## manual blocked design ####################
### example that shows why order of blocks is not randomized
### can of course be randomized by user, if appropriate
FrF2(32,9,blocks=c("Day","Shift"),alias.block.2fis=TRUE, 
    factor.names=list(Day=c("Wednesday","Thursday"), Shift=c("Morning","Afternoon"),
        F1="",F2="",F3="",F4="",F5="",F6="",F7=""), default.levels=c("current","new"))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
