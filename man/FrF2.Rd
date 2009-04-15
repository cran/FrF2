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
        estimable = NULL, clear = TRUE, res3 = FALSE, max.time = 60, 
        select.catlg=catlg, perm.start=NULL, perms = NULL, 
        MaxC2 = FALSE, replications = 1, repeat.only = FALSE, 
        randomize = TRUE, seed = NULL, alias.info = 2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nruns}{ Number of runs, must be a power of 2, if given. 
  
      The number of runs can also be omitted. In that case, 
      if \code{resolution} is specified, the function looks for the smallest 
      design of the requested resolution that accomodates \code{nfactors} factors. 
      If the smallest possible design is a full factorial or not catalogued, 
      the function stops with an error.
      
      If estimable is specified and nruns omitted, 
      nruns becomes the size of the smallest design that 
      MIGHT accomodate the effects requested in \code{estimable}. 
      If this run size turns out to be too low, an error is thrown. 
      In that case, explicitly double the run size and retry. }
  \item{nfactors}{ The number of 2-level factors to be investigated. 
      Can be omitted, if it is obvious from the \code{factor.names}, 
      or \code{nruns} together with \code{generators}, or 
      \code{estimable}. If \code{estimable} is used for determining 
      run size, it is assumed that the largest main effect position number 
      occurring in all \code{estimable} coincides with \code{nfactors}. }
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
  \item{estimable}{ indicates the 2-factor interactions (2fis) that are to be estimable in 
        the design. Consult the details section for two different approaches of 
        requesting estimability, as indicated by the status of the \code{clear} option. 

        \code{estimable} can be 
        
        a numeric matrix with two rows, each column of which indicates one interaction,
        e.g. column 1 3 for interaction of the first with the third factor
        
        OR
        
        a character vector containing strings of length 2 with capital letters from \code{\link{Letters}} 
        for the first 25 factors and small letters for the last 25 (e.g. \code{c(\"AB\",\"BE\")} 
        
        OR
        
        a formula that contains an adequate model formula, e.g. 
        
        \code{formula(\"~A+B+C+D+E+(F+G+H+J+K+L)^2\")} 
        
        for a model with (at least) eleven factors. 
        The names of the factors used in the formula can be the same letters usable in 
        the character vector (cf. above, A the first factor, B the second etc.), 
        or they can correspond to the factor names from 
        \code{factor.names}. 
        }
  \item{clear}{ logical, indicating how estimable is to be used. See details. }
  \item{res3}{ logical; if TRUE, \code{estimable} includes resolution III designs 
        into the search for adequate designs; otherwise resolution IV and higher designs 
        are included only. }
  \item{max.time}{ maximum time for design search as requested by \code{estimable}, 
        in seconds (default 60); used only if clear=FALSE, 
        since the search can take a long time in complicated or unlucky situations;
        set max.time to Inf if you want to force a search over an extended period of 
        time; however, be aware that it may still take longer than feasible 
        (cf. also details section)}
  \item{select.catlg}{ applicable only for the case \code{\"clear = FALSE\"}.
        Provides a catalogue in which the search is to be conducted 
        }
  \item{perm.start}{ applicable only for the case \code{\"clear = FALSE\"}.
        Provides a start permutation for permuting experiment factors (numeric vector). 
        This is useful for the case that a previous search was not (yet) successful 
        because of a time limit, since the algorithm notifies the user about the 
        permutation at which it had to stop.}
  \item{perms}{ applicable only for the case \code{\"clear = FALSE\"}.
        Provides the matrix of permutations of experiment factors to be tried; each 
        row is a permutation. For example, for an 11-factor design with the 
        first six factors and their 2fis 
        estimable, it is only relevant, which of the eleven factors are to be allocated 
        to the first six experiment factors, and these as well as the other five factors can be 
        in arbitrary order. This reduces the number of required permutations from 
        about 40 Mio to 462. 
        It is recommended to use \code{perms} whenever possible, if \code{\"clear = FALSE\"}, 
        since this dramatically improves performance of the algorithm.
        
        It is planned to automatically generate perms for certain structures like 
        compromise designs in the future.}
  \item{MaxC2}{ is a logical and defaults to FALSE. If TRUE, 
        maximizing the number of free 2-factor interactions takes precedence 
        over minimizing aberration. Resolution is always considered first. 
        Most likely, features like this are going to change in the future. }
  \item{replications}{ positive integer number. Default 1 (i.e. each row just once). 
       If larger, each design run is executed replication times. 
       If \code{repeat.only}, repeated measurements 
       are carried out directly in sequence, i.e. no true replication takes place, 
       and all the repeat runs are conducted together. It is likely that the error 
       variation generated by such a procedure will be too small, so that average values 
       should be analyzed for an unreplicated design. 
       
       Otherwise (default), the full experiment is first carried out once, then 
       for the second replication and so forth. In case of randomization, 
       each such blocks is randomized separately. In this case, replication variance is 
       more likely suitable for usage as error variance 
       (unless e.g. the same parts are used for replication runs although build 
       variation is important).}
  \item{repeat.only}{ logical, relevant only if replications > 1. If TRUE, 
        replications of each run are grouped together 
       (repeated measurement rather than true replication). The default is 
       \code{repeat.only=FALSE}, i.e. the complete experiment 
       is conducted in \code{replications} blocks, and each run occurs in each block.  }
  \item{randomize}{ logical. If TRUE, the design is randomized. This is the default. 
       In case of replications, the nature of randomization depends on the setting of 
       option \code{repeat.only}.}
  \item{seed}{ optional seed for the randomization process }
  \item{alias.info}{ can be 2 or 3, gives the order of interaction effects for which 
       alias information is to be included in the \code{aliased} component of the 
       \code{design.info} element of the output object. }
  \item{\dots}{ currently not used }
}
\details{
  The function works on the basis of the catalogued designs by Chen, Sun and Wu (1993), 
  which are available as \code{\link{catlg}} (a list object of class \code{catlg}). 
  The function output is a data frame of class \code{design} and has attributes that can be accessed 
  by functions \code{\link{desnum}}, \code{\link{run.order}} and \code{\link{design.info}}.
  
  The option \code{estimable} allows to specify 2-factor interactions (2fis) that 
  have to be estimable in the model. Per default, it is assumed that a resolution IV 
  model is intended. With option \code{clear=TRUE}, \code{FrF2} searches for 
  a model for which all main effects and all 2fis given in \code{estimable} are 
  clear of aliasing with any other 2fis. This is a weaker requirement than resolution V, 
  because 2fis outside those specified in \code{estimable} may be aliased with 
  each other. But it is much stronger than what is done in case of \code{clear=FALSE}: 
  For the latter, \code{FrF2} searches for a design that has a distinct column in 
  the model matrix for each main effect and each interaction requested with 
  in \code{estimable}. Per default, resolution III designs are not included in the 
  search. If this default is overridden by the \code{res3=TRUE} option, resolution III 
  designs are included. In case of \code{clear=TRUE}, this leads to the somewhat 
  strange situation that main effects can be aliased with 2fis from outside 
  \code{estimable} while 2fis from inside \code{estimable} are not aliased with 
  any main effects or 2fis. 
  
  With \code{clear=FALSE}, the algorithm loops through the eligible designs from 
  \code{catlg.select} from good to worse (in terms of MA) and, for each design, loops 
  through all eligible permutations of the experiment factors from \code{perms}. 
  If \code{perms} is omitted, the permutations are looped through in lexicographic 
  order starting from 1:nfac or \code{perm.start}. Especially in this case, 
  run times of the search   algorithm can be very long. 
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
  (Example 1 in Wu and Chen 1992). \code{estimable} for this model can be specified as 
  
  \code{formula(\dQuote{~(F+G+H+J+K+L)^2})} 
  
  OR 
  
  \code{formula(\dQuote{~(A+B+C+D+E+F)^2})}.
  
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
  
  \code{perms.6 <- combn(11,6)}
  
  \code{perms.full <- matrix(NA,ncol(perms.6),11)}
  
  \code{for (i in 1:ncol(perms.6))}
  
  \code{perms.full[i,] <- c(perms.6[,i],setdiff(1:11,perms.6[,i]))}

  Handing perms.full to the procedure using the \code{perms} option makes the second version of the 
  requested interaction terms fast as well, since up to almost 40 Mio permutations of experiment 
  factors are reduced to at most 462. Thus, whenever possible, 
  one should try to limit the permutations necessary in case of \code{clear=FALSE}.
  
  Various improvements to function \code{FrF2} are planned for the coming weeks and months, among others 
  blocking and split plot facilities (based on further catalogues), and inclusion of four-level factors. 
  Also, input and output possibilities will be improved.
  
  Please contact me with any suggestions.
}
\value{
  Value is a data frame of S3 class \code{\link{design}} with attributes attached. 
  The data frame itself contains the design with levels coded as requested.
  The following attributes are attached to it: 
  \item{desnum }{Design matrix in -1/1 coding}
  \item{run.order }{two column matrix, first columns contains the run number in 
       standard order, second column the run number as randomized;
       useful for switching back and forth between actual and standard run 
       number}
  \item{design.info }{list with the entries 
  \itemize{
  \item{type }{ character string \dQuote{full factorial}, \dQuote{FrF2}, 
       \dQuote{FrF2.estimable} or \dQuote{FrF2.generators}, depending on the 
       type of design}
  \item{catlg.entry }{ for type \code{FrF2} only; 
       list with one element, which is the entry of \code{catlg} 
       on which the design is based}
  \item{gen.display }{ for type \code{FrF2.generators} only; 
       character vector of generators in the form D=ABC etc.}
  \item{aliased }{ alias structure of main effects, 2fis and possibly 3fis,
       depending on the choice of \code{alias.info}; 
       itself a list the two or three components main, fi2, and optionally fi3}
  \item{replication }{ option setting in call to \code{FrF2} }
  \item{repeat.only }{ option setting in call to \code{FrF2} }
  \item{randomize }{ option setting in call to \code{FrF2} }
  \item{seed }{ option setting in call to \code{FrF2} }
       }
       }
}
\references{
Chen, J., Sun, D.X. and Wu, C.F.J. (1993) 
A catalogue of 2-level and 3-level orthogonal arrays. 
\emph{International Statistical Review} \bold{61}, 131-145. 

Wu, C.F.J. and Chen, Y. (1992) 
A graph-aided method for planning two-level experiments when certain interactions 
are important. 
\emph{Technometrics} \bold{34}, 162-175. 


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
## the design with number of factors specified via factor names 
      ## (standard level codes)
FrF2(8,factor.names=list(temp="",press="",material="",state=""))
## the design with changed factor names and factor-specific level codes
FrF2(8,4, factor.names=list(temp=c("min","max"),press=c("low","normal"),
     material=c("current","new"),state=c("new","aged")))
## a full factorial
FrF2(8,3, factor.names=list(temp=c("min","max"),press=c("low","normal"),
     material=c("current","new")))
## a replicated full factorial (implicit by low number of factors)
FrF2(16,3, factor.names=list(temp=c("min","max"),press=c("low","normal"),
     material=c("current","new")))
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
## show design information instead of design itself
design.info(FrF2(32,9))
## maximum number of free 2-factor interactions instead of minimum aberration
## show design information instead of design itself
design.info(FrF2(32,9,MaxC2=TRUE))

## usage of replication
## shows run order instead of design itself
run.order(FrF2(8,4,replication=2,randomize=FALSE))
run.order(FrF2(8,4,replication=2,repeat.only=TRUE,randomize=FALSE))
run.order(FrF2(8,4,replication=2))
run.order(FrF2(8,4,replication=2,repeat.only=TRUE))

## usage of estimable
  ## design with all 2fis of factor A estimable on distinct columns in 16 runs
  FrF2(16, nfactors=6, estimable = rbind(rep(1,5),2:6), clear=FALSE)
  FrF2(16, nfactors=6, estimable = c("AB","AC","AD","AE","AF"), clear=FALSE)
  FrF2(16, nfactors=6, estimable = formula("~A+B+C+D+E+F+A:(B+C+D+E+F)"), 
       clear=FALSE)
            ## formula would also accept self-defined factor names
            ## from factor.names instead of letters A, B, C, ...
            
  ## estimable does not need any other input
  FrF2(estimable=formula("~(A+B+C)^2+D+E"))

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
