\name{pb}
\alias{pb}
\alias{pb.list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to generate non-regular fractional factorial screening designs }
\description{
  The function generates Plackett-Burman designs and in some cases other screening 
  designs in run numbers that are a multiple of 4. These designs are particularly suitable for screening a 
  large number of factors, since interactions are not fully aliased with one main effect each 
  but partially aliased. (The designs in 8 and 64 runs are exceptions from this rule.)
}
\usage{
pb(nruns, nfactors = nruns - 1, factor.names = if (nfactors <= 50) 
     Letters[1:nfactors] else paste("F", 1:nfactors, sep = ""), 
     default.levels = c(-1, 1), boxtyssedal = TRUE, n12.taguchi = FALSE, 
     randomize = TRUE, seed = NULL, ...)

pb.list
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nruns}{ number of runs, must be a multiple of 4 }
  \item{nfactors}{ number of factors, default is nruns - 1, and it is recommended to retain this default. 
   It is possible to specify factor names for fewer factors, and the remaining columns will be named \code{e1}, \code{e2}, ... 
   They are useful for representing error in effects plots. }
  \item{factor.names}{ a character vector of factor names (length up to nfactors) or a named list with names representing factor names and elements vectors of length 2 with factor levels for each factor. Elements can be empty strings. In this case, default levels are used for the respective factor. }
  \item{default.levels}{ default levels (vector of length 2) for all factors for which no specific levels are given }
  \item{boxtyssedal}{ logical, relevant only for nruns=16. 
               If FALSE, the geometric (=standard) 16 run plan is used. 
               If TRUE, the proposal by Box and Tyssedal is used instead, which has the advantage (for screening) of aliasing each interaction with several main effects, like the other Plackett-Burman designs. }
  \item{n12.taguchi}{ logical, relevant only for nruns=12. If TRUE, the 12 run design is given in Taguchi order. }
  \item{randomize}{ logical. If TRUE, the design is randomized. This is the default. }
  \item{seed}{ optional seed for the randomization process }
  \item{\dots}{ currently not used }
}
\details{
  For most run numbers, designs from Plackett and Burman (1946) are used, and columns are filled from left to right. The generating rows for these designs can be found in \code{pb.list} (a 0 entry indicates that the design is constructed by a different method, e.g. doubling).
  
  For 12 runs, the isomorphic design by Taguchi can be requested. 
  For 16 runs, the default is to use the designs suggested by Box and Tyssedal (2001), which up to 14 factors do not suffer from perfect aliasing. 
  For 32 runs, a cyclic design with generator given in Samset and Tyssedal (1999) is used. 
  For 92 runs, a design is constructed according to the Williamson construction with matrices A, B, C and D from Hedayat and Stufken (1999), p. 160. 
  
  So far, designs up to 96~runs are covered. More and different ones may follow, since the package is currently under intensive development.
}
\value{
  Currently, the function returns a list of
  \item{desnum }{design matrix coded with -1 and 1 entries}
  \item{design }{data frame of design, making use of factor levels, if available}
  \item{origin }{showing FrF2.pb as the generator of the design}
}
\references{ 
  Box, G.E.P. and Tyssedal, J. (2001) Sixteen Run Designs of High Projectivity for Factor Screening. 
      \emph{Communications in Statistics - Simulation and Computation} \bold{30}, 217-228. 
      
  Hedayat, A.S., Sloane, N.J.A. and Stufken, J. (1999) \emph{Orthogonal Arrays: Theory and Applications}, Springer, New York. 
  
  Plackett, R.L.; Burman, J.P. (1946) The design of optimum multifactorial experiments. \emph{Biometrika} \bold{33}, 305-325.
  
  Samset, O.; Tyssedal, J. (1999) Two-level designs with good projection properties. \emph{Technical Report 12, 
  Department of Mathematical Sciences, The Norwegian University of Science and Technology, Norway}.  
  
  Williamson, J. (1946) Determinants whose elements are 0 and 1. \emph{American Mathematical Monthly} \bold{53}, 427-434.
}
\author{ Ulrike Groemping }

\seealso{ See Also \code{\link{FrF2}} for regular fractional factorial designs }
\examples{
   pb(12,randomize=FALSE)
   pb(12,randomize=FALSE,n12.taguchi=TRUE)
   pb(20,seed=29869)
   pb(16,factor.names=list(A="",B="",C="",D=c("min","max"),
          E="",F="",G="",H="",J=c("new","old")))
   pb(8,default.levels=c("current","new"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
