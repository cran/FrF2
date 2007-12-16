\name{FrF2-package}
\alias{FrF2-package}
\alias{FrF2}
\docType{package}
\title{
Package for analysing Fractional Factorial designs with 2-level factors
}
\description{
This package analyses Fractional Factorial designs with 2-level factors; it is 
meant for completely aliased designs only, i.e. e.g. not for analysing Plackett-
Burman designs with interactions. 

The package enables convenient main effects and interaction plots for all 
factors simultaneously and offers an enhanced DanielPlot function (modified from 
BsMD).  Furthermore, the alias structure for Fractional Factorial 2-level 
designs is output in a more readable format than with the built-in function 
alias. 
}
\details{
This package analyses Fractional Factorial designs with 2-level factors; it is 
meant for completely aliased designs only, i.e. e.g. not for analysing Plackett-
Burman designs with interactions. 

The package works on linear models and enables convenient main effects and 
interaction plots (functions \code{MEPlot} and \code{IAPlot}) similar to those 
offered by Minitab software for all factors simultaneously, even though especially the 
interactions are often aliased, i.e. the model is typically singular.  
Furthermore, the function \code{DanielPlot} from package \pkg{BsMD} has been 
modified to automatically label effects significant according to the 
Lenth-criterion, and to provide more usage comfort to the analyst.  
Finally, the function \code{aliases} determines the alias structure of a 
Fractional Factorial 2-level design in a format more suitable for human readers 
than the output from the built-in function \code{alias}. 
}
\author{
Ulrike Groemping

Maintainer: Ulrike Groemping <groemping@tfh-berlin.de>
}
\references{
    Box G. E. P, Hunter, W. C. and Hunter, J. S. (2005) 
    \emph{Statistics for Experimenters, 2nd edition}.
    New York: Wiley.
    
    Daniel, C. (1959) 
    Use of Half Normal Plots in Interpreting Two Level Experiments.
    \emph{Technometrics}, \bold{1}, 311-340.
    
    Lenth, R.V. (1989) Quick and easy analysis of unreplicated factorials.
    \emph{Technometrics}, \bold{31}, 469-473.
    
}
\keyword{ design }
\seealso{
\code{BsMD-package}, \code{\link{MEPlot}}, \code{\link{IAPlot}}, 
\code{\link{DanielPlot}}, \code{\link{aliases}}
}
\examples{
    ### Injection Molding Experiment. Box et al. 1978.
    require(FrF2)
    data(BM93.e3.data)  #from BsMD
    iMdat <- BM93.e3.data[1:16,2:10]  #only original experiment
    # make data more user-friendly
    colnames(iMdat) <- c("MoldTemp","Moisture","HoldPress","CavityThick","BoostPress",
             "CycleTime","GateSize","ScrewSpeed", "y")
    # linear model with all main effects and 2-factor interactions
    iM.lm <- lm(y ~ (.)^2, data = iMdat)
    # determine aliases
    aliases(iM.lm)
    # coded version
    aliases(iM.lm, code=TRUE)
    # normal plot of effects, default is autolabel with alpha=0.05
    DanielPlot(iM.lm)  
    DanielPlot(iM.lm,code=TRUE)
    DanielPlot(iM.lm,code=TRUE,alpha=0.5)
    # half normal plot of effects
    DanielPlot(iM.lm,code=TRUE,alpha=0.5,half=TRUE)
    # main effects plots
    MEPlot(iM.lm)
    # interaction plots
    IAPlot(iM.lm)
    # interaction plots with attention drawn to aliases
    IAPlot(iM.lm, show.alias=TRUE)
    # alias groups corresponding to interaction plots
    aliases(iM.lm)$aliases[9:15]
}
