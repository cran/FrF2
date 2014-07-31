\name{CIG}
\alias{CIGstatic}
\alias{CIG}
\alias{gen2CIG}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Clear interactions graph from catlg entry }
\description{
  Function CIG creates a clear interactions graph (CIG) 
  from a catlg design (design name must be given). 
  Function CIGstatic allows to create a static graph from a dynamically-adjusted 
  one. 
}
\usage{
CIG(design, select.catlg=catlg, static=FALSE, layout=layout.auto, label="num", 
   plot=TRUE, ...)
CIGstatic(graph, id, label="num", xlim=c(-1,1), ylim=c(1,-1), ...)
gen2CIG(nruns, gen)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{design}{a character string that identifies a design in the cataloge specified 
       by option \code{select.catlg}, \cr
       OR a class \code{catlg} object with a single entry, \cr
       OR a formula with all main effects and the requested clear 2-factor 
       interactions, \cr
       OR a character vector of length more than one with two-letter combinations of 
       the clear 2-factor interactions,\cr
       OR a numeric two-row matrix with factor numbers of the clear 2-factor 
       interactions,\cr
       OR a character two-row matrix with factor names of the clear 2-factor 
       interactions.\cr
       The first two are for graphing design CIGs, the other ones for requirement 
       set CIGs.}
  \item{\dots}{further arguments to be passed to function \code{\link[igraph]{tkplot}}, 
      or graphical parameters to be passed to \code{plot}.}
  \item{select.catlg}{name of catalogue (not in quotes); 
        only relevant, if \code{design} is a character string}
  \item{static}{logical. If \code{TRUE}, a static graphic is produced, 
        otherwise an interactive graphic is created that can be modified 
        by moving aroung nodes; only relevant for \code{plot=TRUE}}
  \item{layout}{ignored for \code{static=FALSE};\cr
        possible values are two-column matrices with number of rows 
        equal to the number of vertices of the graph, or layout parameters for 
        function \code{\link[igraph]{plot.igraph}} as described in 
        \code{\link[igraph]{plot.common}} 
        and \code{\link[igraph]{layout}};\cr
        default: \code{layout.auto} (changed with version 1.6, was \code{layout.circle} 
        before}
  \item{label}{in effect for \code{catlg} object only (character design name or 
        class \code{catlg} object);\cr
        a character string that decides for numeric labels or 
        character labels; any string other than the default will invoke the 
        factor letters as labels}
  \item{plot}{a logical that decides whether a plot is requested (default: \code{TRUE});\cr
        plotting can be suppressed, if graph creation is desired for calculating 
        graph characteristics with functions from package \code{igraph} 
        (e.g. \code{\link[igraph:cliques]{clique.number}},
        \code{\link[igraph:cliques]{largest.cliques}}, 
        \code{\link[igraph:independent.vertex.sets]{independence.number}},
        \code{\link[igraph:degree]{degree}}) }
  \item{graph}{a graph object from package \code{igraph}, or a list whose first 
        element is such a graph object (like the output from function \code{CIG})}
  \item{id}{identification number of the interactive graph to be reproduced in 
        static form; this number can be found in the header line of the graphics window}
  \item{xlim}{horizontal axis limits}
  \item{ylim}{vertical axis limits (per default reversed in order to exactly 
        reproduce the interactive graph) }
  \item{nruns}{number of runs of the design to be graphed }
  \item{gen}{generator (vector of Yates matrix column numbers) }
}

\details{
    The design depicted in \code{CIG} has to be the name (character string) of a 
    regular fractional factorial 2-level design present in \code{select.catlg}. 
    
    Clear 2fis are depicted as edges in the graph. In the interactive graph, 
    users can change the layout manually or with the menus. For example, 
    the Reingold-Tilford layout can be chosen, with a root vertex specified; 
    this sometimes helps in identifying groups 
    of vertices that are not connected with each other. 
    
    Previous versions of package \code{igraph} used to internally number the 
    vertices from 0 to number of vertices -1, not from 1 to number of vertices. 
    This has been changed 
    in June 2012 (FrF2 adapted to this change with version 1.5).
    
    Function \code{CIGstatic} serves the purpose to statically create the current 
    interactively modified graph; 
    the usual annotation possibilities for plots are available.
    
    Function \code{gen2CIG} returns a graph object that can be plotted or otherwise 
    investigated with graph-related 
    functionality.
}

\value{
    For \code{plot=FALSE} or \code{plot=TRUE} with \code{static=TRUE}, 
    function \code{CIG} visibly (\code{plot=FALSE}) or invisibly (\code{plot=TRUE}) 
    returns a graph from package \code{igraph}.\cr
    For \code{plot=TRUE} with \code{static=FALSE}, 
    the function returns a list with the first element \code{graph} 
    the element \code{coords} with the coordinates of that graph. \cr
    Function \code{CIGstatic} works on the list produced by function \code{CIG} 
    by plotting the graph statically using the positioning from the 
    current interactive picture. 
    
    Function \code{gen2CIG} returns a clear interactions graph that can e.g. be 
    plotted with functions \code{plot} (\code{\link[igraph]{plot.igraph}}) or 
    \code{\link[igraph]{tkplot}}.
}

\references{
Groemping, U. (2010). Creating clear designs: a graph-based algorithm and a 
   catalogue of clear compromise plans. 
   \emph{Reports in Mathematics, Physics and Chemistry}, report 05/2010, 
   Department II, Beuth University of Applied Sciences Berlin.
   (Preprint for IIE Transactions; IIE Transactions is available at 
   \url{http://www.tandfonline.com}.)

}
\examples{
\dontrun{
ex.CIG <- CIG("9-4.2")
## play around with the dynamic graph until it looks right
## look up its id number in the title bar of the graph window and use it for id
par(xpd=TRUE)
CIGstatic(ex.CIG, id=1)
}

graph1 <- CIG("9-4.2", plot=FALSE)   ### create graph object from design name
### calculate graph properties
require(igraph)
degree(graph1)
clique.number(graph1)
independence.number(graph1)
largest.cliques(graph1)

graph2 <- gen2CIG(32, c(7,11,14,29))   ### create graph object from generator columns
### check isomorphism to graph1
graph.isomorphic(graph1,graph2)

\dontrun{
## use a CIG for manual design search
## requirement set: 
estim <- compromise(17,15:17)$requirement ## all interactions of factors 15 to 17 (P,Q,R)
## graph the requirement set CIG
CIG(estim)
FrF2(128, 17, estimable=estim)  ## will run for a very long time
## interrupt with ESC key, after a short period of waiting
FrF2.currentlychecked()   ## displays the design that is currently checked 
                          ## should be 17-10.2407
CIG("17-10.2407")
## clearly, using columns 1, 8 and 9 for factors 15:17 does the job
    ## sometimes, the design where the algorithm got stuck, does not provide a solution
    ## in that case, option select.catlg can be used for restricting the search designs 
    ##     to designs further down in the catalogue, in order to find the next candidate
    ##     until final success is reached
    ## e.g.
    which(names(catlg)=="17-10.2407")
    FrF2(128, 17, estimable=estim, select.catlg=catlg[2375:length(catlg)])  
    ## will run for a very long time
    ## interrupting after short waiting time yields
    FrF2.currentlychecked()   ## displays the design that is currently checked 
                              ## should be 17-10.4177

## note: in this example, option sort="high" would make the automatic search fast
## so that manual treatment is not needed!

}
}
\author{Ulrike Groemping}
\seealso{\code{\link[igraph]{plot.igraph}}, \code{\link[igraph]{tkplot}},
   \code{\link[igraph]{plot.common}} }
\keyword{design}
