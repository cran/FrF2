\name{CIG.catlg}
\alias{CIGstatic}
\alias{CIG.catlg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Clear interactions graph from catlg entry }
\description{
  Function CIG.catlg creates a clear interactions graph (CIG) 
  from a catlg design (the first design of the catalog). 
  Function CIGstatic allows to create a static graph from a dynamically-adjusted 
  one. 
}
\usage{
CIG.catlg(design, select.catlg=catlg, static=FALSE, layout=layout.circle, label="num", ...)
CIGstatic(graph, id, label="num", xlim=c(-1,1), ylim=c(1,-1), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{design}{an experimental design of class \code{\link[DoE.base:class-design]{design}} 
              with the \code{type} element of the \code{design.info} attribute 
              containing \dQuote{FrF2} or \dQuote{pb}\cr
              OR\cr
              object of class \code{lm}. Fitted model from \code{lm} or \code{aov}.}
  \item{\dots}{further arguments to be passed to function plot, 
      or graphical parameters to be passed to \code{plot}.}
  \item{select.catlg}{name of catlg (not in quotes). 
        If \code{TRUE} labels \dQuote{A},\dQuote{B}, etc. are used 
        instead of the names of the coefficients (factors). A legend linking 
        codes to names is provided.}
  \item{static}{logical. If \code{TRUE}, a static graphic is produced, 
        otherwise an interactive graphic is created}
  \item{layout}{possible values are two-column matrices with number of rows 
        equal to the number of vertices of the graph, or layout parameters for 
        function \code{\link[igraph]{plot.igraph}} as described in \code{\link[igraph]{plot.common}} }
  \item{label}{a character string that decides for numeric labels or 
        character labels; any string other than the default will invoke the 
        factor letters as labels}
  \item{graph}{a graph object from package \code{igraph}, or a list whose first 
        element is such a graph object}
  \item{id}{identification number of the interactive graph to be reproduced in 
        static form; this number can be found in the header line of the graphics window}
  \item{xlim}{horizontal axis limits}
  \item{ylim}{vertical axis limits (per default reversed in order to exactly reproduce the 
        interactive graph) }
}

\details{
    The design depicted in \code{CIG.catlg} has to be the name (character string) of a regular 
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
    Function CIC.catlg returns a list with the first element \code{graph} a graph 
    from package \code{igraph} and in case of static=FALSE the element \code{coords} 
    with the coordinates of that graph. \cr
    Function CIGstatic plots the graph statically using the positioning from the 
    current interactive picture. 
}

\references{

}
\author{Ulrike Groemping}
\seealso{\code{\link[igraph]{plot.igraph}}, \code{\link[igraph]{tkplot}},\code{\link[igraph]{plot.common}} }
\keyword{design}
