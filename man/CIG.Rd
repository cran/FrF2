\name{CIG}
\alias{CIGstatic}
\alias{CIG}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Clear interactions graph from catlg entry }
\description{
  Function CIG creates a clear interactions graph (CIG) 
  from a catlg design (design name must be given). 
  Function CIGstatic allows to create a static graph from a dynamically-adjusted 
  one. 
}
\usage{
CIG(design, select.catlg=catlg, static=FALSE, layout=layout.circle, label="num", ...)
CIGstatic(graph, id, label="num", xlim=c(-1,1), ylim=c(1,-1), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{design}{a character string that identifies a design in the cataloge specified 
       by option \code{select.catlg}}
  \item{\dots}{further arguments to be passed to function plot, 
      or graphical parameters to be passed to \code{plot}.}
  \item{select.catlg}{name of catalogue (not in quotes)}
  \item{static}{logical. If \code{TRUE}, a static graphic is produced, 
        otherwise an interactive graphic is created that can be modified 
        by moving aroung nodes}
  \item{layout}{possible values are two-column matrices with number of rows 
        equal to the number of vertices of the graph, or layout parameters for 
        function \code{\link[igraph]{plot.igraph}} as described in \code{\link[igraph]{plot.common}} }
  \item{label}{a character string that decides for numeric labels or 
        character labels; any string other than the default will invoke the 
        factor letters as labels}
  \item{graph}{a graph object from package \code{igraph}, or a list whose first 
        element is such a graph object (like the output from function \code{CIG})}
  \item{id}{identification number of the interactive graph to be reproduced in 
        static form; this number can be found in the header line of the graphics window}
  \item{xlim}{horizontal axis limits}
  \item{ylim}{vertical axis limits (per default reversed in order to exactly reproduce the 
        interactive graph) }
}

\details{
    The design depicted in \code{CIG} has to be the name (character string) of a regular 
    fractional factorial 2-level design present in \code{select.catlg}. 
    
    Clear 2fis are depicted as edges in the graph. In the interactive graph, 
    users can change the layout manually or with the menus. For example, the Reingold-Tilford 
    layout can be chosen, with a root vertex specified; this sometimes helps in identifying groups 
    of vertices that are not connected with each other. 
    
    Note that internally the vertices are numbered from 
    0 to number of vertices -1, not from 1 to number of vertices as in the default labels. 
    These internal numbers are also used in the menus for modifying the graph (e.g. specify root 0, 
    if vertex 1 is to be made the root).
    
    Function CIGstatic serves the purpose to statically create the current interactively modified graph; 
    the usual annotation possibilities for plots are available.
}

\value{
    Function CIC returns a list with the first element \code{graph} a graph 
    from package \code{igraph} and in case of static=FALSE the element \code{coords} 
    with the coordinates of that graph. \cr
    Function CIGstatic plots the graph statically using the positioning from the 
    current interactive picture. 
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
}
\author{Ulrike Groemping}
\seealso{\code{\link[igraph]{plot.igraph}}, \code{\link[igraph]{tkplot}},\code{\link[igraph]{plot.common}} }
\keyword{design}
