\name{FrF2-package}
\alias{FrF2-package}
\docType{package}
\title{
Fractional Factorial designs with 2-level factors
}
\description{
This package creates regular and non-regular Fractional Factorial designs. 
Furthermore, analysis tools for Fractional Factorial designs with 2-level factors 
are offered (main effects and interaction plots for all factors simultaneously, 
cube plot for looking at the simultaneous effects of three factors, full or half normal plot, 
alias structure in a more readable format than with the built-in function alias). 

The package works together with packages DoE.base and DoE.wrapper.
}
\details{
The package is currently subject to intensive development; most key functionality is now included. 
Some changes to input and output structures may still occur.
Please contact me, if you have suggestions.

This package designs and analyses Fractional Factorial experiments with 2-level factors. 
Regular (function \code{FrF2}) and non-regular (function \code{pb}) 2-level fractional factorial 
designs can be generated. 
Regular fractional factorials can be generated as blocked or split-plot designs, and hard-to-change 
factors can be specified in order to keep the number of level changes low.
Analysis facilities work for completely aliased designs only, 
i.e. e.g. not for analysing Plackett-Burman designs with interactions. 

Functions \code{fac.design}, \code{fractionate} or \code{oa.design} from 
Chambers and Hastie (1993) have been used as role models e.g. for 
the option \code{factor.names} or for outputting a data frame with attributes. 
However, S compatibility has not been considered in devising this package.

In terms of analysis, the package works on linear models and enables convenient main effects and 
interaction plots (functions \code{MEPlot} and \code{IAPlot}) similar to those 
offered by Minitab software for all factors simultaneously, even though especially the 
interactions are often aliased, i.e. the model is typically singular. 
For the (rare) case of suspected three-factor-interactions, function 
\code{cubePlot} displays a cube with corners labeled with the (modeled) 
means of three factors simultaneously.
Furthermore, the function \code{DanielPlot} from package \pkg{BsMD} has been 
modified to automatically label effects significant according to the 
Lenth-criterion, and to provide more usage comfort to the analyst.  
Finally, the function \code{aliases} determines the alias structure of a 
Fractional Factorial 2-level design in a format more suitable for human readers 
than the output from the built-in function \code{alias}. 

CAUTION: Note that the analysis facilities currently do not make use of the design 
information attached to the design. For example, an effects plot function will treat 
a linear model based on a split-plot-design in the same way as if the design were a 
standard fractional factorial.
}
\author{
Ulrike Groemping

Maintainer: Ulrike Groemping <groemping@bht-berlin.de>
}
\references{
    Box G. E. P, Hunter, W. C. and Hunter, J. S. (2005) 
    \emph{Statistics for Experimenters, 2nd edition}.
    New York: Wiley.
    
    Chambers, J.M. and Hastie, T.J. (1993). \emph{Statistical Models in S}, 
    Chapman and Hall, London.
     
    Chen, J., Sun, D.X. and Wu, C.F.J. (1993) 
    A catalogue of 2-level and 3-level orthogonal arrays. 
    \emph{International Statistical Review} \bold{61}, 131-145.

    Daniel, C. (1959) 
    Use of Half Normal Plots in Interpreting Two Level Experiments.
    \emph{Technometrics}, \bold{1}, 311-340.
    
    Hedayat, A.S., Sloane, N.J.A. and Stufken, J. (1999) 
    \emph{Orthogonal Arrays: Theory and Applications}, Springer, New York. 
    
    Lenth, R.V. (1989) Quick and easy analysis of unreplicated factorials.
    \emph{Technometrics}, \bold{31}, 469-473.
  
    Mee, R. (2009). \emph{A Comprehensive Guide to Factorial Two-Level Experimentation}. 
    New York: Springer.
    
    Montgomery, D.C. (2001). Design and Analysis of Experiments (5th ed.). Wiley, New York.

    Plackett, R.L.; Burman, J.P. (1946) The design of optimum multifactorial 
     experiments. \emph{Biometrika} \bold{33}, 305-325.
}
\keyword{ array }
\keyword{ design }
\seealso{
The key design generating functions: \code{\link{FrF2}}, \code{\link{pb}}\cr
S3 class \code{\link[DoE.base:class-design]{design}}\cr 
Related packages: 
\code{\link[DoE.base:DoE.base-package]{DoE.base}}, 
\code{\link[DoE.wrapper:DoE.wrapper-package]{DoE.wrapper}}, 
\code{\link[BsMD:BsMD-package]{BsMD}};\cr 
Graphical analysis functions: \code{\link{MEPlot}}, \code{\link{IAPlot}}, \code{\link{cubePlot}}, 
\code{\link{DanielPlot}}\cr
Analysis of alias structure for linear models of \code{FrF2} designs: \code{\link{aliases}}\cr
}
\examples{
    ### for examples on design generation, cf. functions pb and FrF2
  
    ### Injection Molding Experiment. Box et al. 1978.
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
    # cube plot of three factors
    # (not very useful for this model, for demonstration only)
      ## per default, modeled means are shown
      ## this does not make a difference here, since the main effect of 
      ## ScrewSpeed is confounded with the MoldTemp:HoldPress:BoostPress
      ## interaction, so that the three-factor-interaction is indirectly included 
      ## in the modeled means
      cubePlot(iM.lm, "MoldTemp", "HoldPress", "BoostPress")
      ## modeled means without a three-factor interaction
      cubePlot(lm(y ~ (MoldTemp+HoldPress+BoostPress)^2, data = iMdat), 
         "MoldTemp", "HoldPress", "BoostPress")
      ## modeled=FALSE reverts to showing the apparent three-factor interaction
      cubePlot(lm(y ~ (MoldTemp+HoldPress+BoostPress)^2, data = iMdat), 
         "MoldTemp", "HoldPress", "BoostPress", modeled=FALSE)
      ## cubePlot also works on raw data
      cubePlot(iMdat$y, iMdat$MoldTemp, iMdat$HoldPress, iMdat$BoostPress)
}
