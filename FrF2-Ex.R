pkgname <- "FrF2"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('FrF2')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("BsProb.design")
### * BsProb.design

flush(stderr()); flush(stdout())

### Name: BsProb.design
### Title: Bayesian posterior probabilities from Box and Meyer method
### Aliases: BsProb.design
### Keywords: design

### ** Examples

   ### there are several success stories and recommendations for this method
   ### in the simulated example here (not fabricated, 
   ###         it was the first one that came to my mind), 
   ### the method goes wrong, at least when using mInt=2 (the default, because 
   ###         Daniel plots work quite well for pure main effects models):
   ### active factors are A to E (perhaps too many for the method to work),
   ### the method identifies F, J, and L with highest probability 
   ### (but is quite undecided)
   plan <- pb(12)
   dn <- desnum(plan)
   set.seed(8655)
   y <- dn%*%c(2,2,2,2,3,0,0,0,0,0,0) + dn[,1]*dn[,3]*2 - dn[,5]*dn[,4] + rnorm(12)/10
   plan.r <- add.response(plan, response=y)
   plot(BsProb.design(plan.r), code=FALSE)
   plot(BsProb.design(plan.r, mInt=1), code=FALSE) ## much better!
   ### For comparison: A Daniel plot does not show any significant effects according 
   ### to Lenths method, but makes the right effects stick out
   DanielPlot(plan.r, half=TRUE, alpha=1)



cleanEx()
nameEx("CIG")
### * CIG

flush(stderr()); flush(stdout())

### Name: CIG
### Title: Clear interactions graph from catlg entry
### Aliases: CIGstatic CIG
### Keywords: design

### ** Examples

## Not run: 
##D ex.CIG <- CIG("9-4.2")
##D ## play around with the dynamic graph until it looks right
##D ## look up its id number in the title bar of the graph window and use it for id
##D par(xpd=TRUE)
##D CIGstatic(ex.CIG, id=1)
## End(Not run)

graph1 <- CIG("9-4.2", plot=FALSE)   ### create graph object from design name
### calculate graph properties
degree(graph1)
clique.number(graph1)
independence.number(graph1)
largest.cliques(graph1)




cleanEx()
nameEx("CatalogueAccessors")
### * CatalogueAccessors

flush(stderr()); flush(stdout())

### Name: CatalogueAccessors
### Title: Catalogue file and accessor functions
### Aliases: [.catlg print.catlg res nruns nfac WLP nclear.2fis clear.2fis
###   dominating res.catlg nruns.catlg nfac.catlg WLP.catlg
###   nclear.2fis.catlg clear.2fis.catlg all.2fis.clear.catlg
###   dominating.catlg catlg block.catlg
### Keywords: array design

### ** Examples

c8 <- catlg[nruns(catlg)==8]
nclear.2fis(c8)
clear.2fis(c8)
all.2fis.clear.catlg(c8)

## usage of print function for inspecting catalogued designs
## the first 10 resolution V+ designs in catalogue catlg
print(catlg, res.min=5)
## the 10 resolution V+ designs in catalogue catlg with the most factors
## (for more than one possible value of nfactors, MaxC2 does usually not make sense)
print(catlg, res.min=5, MaxC2=TRUE)

## designs with 12 factors in 64 runs (minimum resolution IV because 
## no resolution III designs of this size are in the catalogue)
## best 10 aberration designs
print(catlg, nfactors=12, nruns=64)
## best 10 clear 2fi designs
print(catlg, nfactors=12, nruns=64, MaxC2=TRUE)
## show alias structure
print(catlg, nfactors=12, nruns=64, MaxC2=TRUE, show.alias=TRUE)
## show best 20 designs
print(catlg, nfactors=12, nruns=64, MaxC2=TRUE, show=20)

## use vector-valued nruns 
print(catlg, nfactors=7, nruns=c(16,32))
## all designs (as show=100 is larger than available number of designs)
##    with 7 or 8 factors in 16 runs
print(catlg, nfactors=c(7,8), nruns=16, show=100)

## the irregular resolution V arrays from package DoE.base (from version 0.9-17)
## designs can be created from them using function oa.design
## Not run: 
##D ## not run in case older version of DoE.base does not have these
##D length3(L128.2.15.8.1)
##D length4(L128.2.15.8.1)  ## aliasing of 2fis with block factor
##D length4(L128.2.15.8.1[,-16])
##D 
##D length3(L256.2.19)
##D length4(L256.2.19)
##D 
##D ##length3(L2048.2.63) 
##D ##length4(L2048.2.63) do not work resource wise
##D ## but the array is also resolution V (but irregular)
## End(Not run)



cleanEx()
nameEx("FrF2-package")
### * FrF2-package

flush(stderr()); flush(stdout())

### Name: FrF2-package
### Title: Fractional Factorial designs with 2-level factors
### Aliases: FrF2-package
### Keywords: array design

### ** Examples

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
    ## plotting functions also work directly on designs, 
    ## if these have been generated from functions FrF2 or pb:
      plan <- FrF2(16, 7)
      plan <- add.response(plan, rnorm(16))
      MEPlot(plan)
      IAPlot(plan)
      DanielPlot(plan)
      



cleanEx()
nameEx("FrF2")
### * FrF2

flush(stderr()); flush(stdout())

### Name: FrF2
### Title: Function to provide regular Fractional Factorial 2-level designs
### Aliases: FrF2
### Keywords: array design

### ** Examples

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
## alias structure for three generators that differ only by sign
design.info(FrF2(16,generators=c(7,13,15),randomize=FALSE))$aliased
design.info(FrF2(16,generators=c(7,-13,15),randomize=FALSE))$aliased
design.info(FrF2(16,generators=c(-7,-13,-15),randomize=FALSE))$aliased
## finding smallest design with resolution 5 in 7 factors
FrF2(nfactors=7, resolution=5)
## same design, but with 12 center points in 6 positions
FrF2(nfactors=7, resolution=5, ncenter=12, center.distribute=6)


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


## Not run: 
##D ## examples below do work, but are repeated in the 
##D ## respective method's separate help file and are therefore prevented 
##D ## from running twice
##D 
##D ########## automatic blocked designs ###################
##D ## from a full factorial ##
##D FrF2(8,3,blocks=2)
##D ## with replication
##D run.order(FrF2(8,3,blocks=2,wbreps=2))
##D run.order(FrF2(8,3,blocks=2,wbreps=2,repeat.only=TRUE))
##D run.order(FrF2(8,3,blocks=2,bbreps=2))
##D run.order(FrF2(8,3,blocks=2,bbreps=2,wbreps=2))
##D 
##D ## automatic blocked design with fractions
##D FrF2(16,7,blocks=4,alias.block.2fis=TRUE,factor.names=c("MotorSpeed", "FeedMode","FeedSizing","MaterialType",
##D                   "Gain","ScreenAngle","ScreenVibLevel"))
##D ## isomorphic non-catalogued design as basis
##D FrF2(16,gen=c(7,11,14),blocks=4,alias.block.2fis=TRUE)
##D ## FrF2 uses blockpick.big and ignores the generator
##D FrF2(64,gen=c(7,11,14),blocks=16,alias.block.2fis=TRUE)
##D 
##D ########## manual blocked design ####################
##D ### example that shows why order of blocks is not randomized
##D ### can of course be randomized by user, if appropriate
##D FrF2(32,9,blocks=c("Day","Shift"),alias.block.2fis=TRUE, 
##D     factor.names=list(Day=c("Wednesday","Thursday"), Shift=c("Morning","Afternoon"),
##D         F1="",F2="",F3="",F4="",F5="",F6="",F7=""), default.levels=c("current","new"))
##D 
##D ########## hard to change factors ####################
##D ## example from Bingham and Sitter Technometrics 19999
##D ## MotorSpeed, FeedMode,FeedSizing,MaterialType are hard to change
##D BS.ex <- FrF2(16,7,hard=4,
##D      factor.names=c("MotorSpeed", "FeedMode","FeedSizing","MaterialType",
##D                   "Gain","ScreenAngle","ScreenVibLevel"), 
##D      default.levels=c("-","+"),randomize=FALSE)
##D design.info(BS.ex)
##D BS.ex
##D ## NOTE: the design has 8 whole plots.
##D ## If randomize=FALSE is used like here, the first hard-to-change factors 
##D ## do not always change between whole plots. 
##D ## A conscious and honest decision is required whether this is 
##D ##    acceptable for the situation at hand!
##D ## randomize=TRUE would cause more changes in the first four factors.
##D 
##D ########## automatic generation for split plot ##########
##D ## 3 control factors, 5 noise factors, control factors are whole plot factors
##D ## 8 plots desired in a total of 32 runs
##D ## Bingham Sitter 2003
##D BS.ex2a <- FrF2(32, 8, WPs=8, nfac.WP=3, 
##D       factor.names=c(paste("C",1:3,sep=""), paste("N",1:5,sep="")),randomize=TRUE)
##D 
##D ## manual generation of this same design
##D BS.ex2m <- FrF2(32, 8, generators=c("ABD","ACD","BCDE"),WPs=8, WPfacs=c("C1","C2","C3"), nfac.WP=3, 
##D       factor.names=c(paste("C",1:3,sep=""),paste("N",1:5,sep="")),randomize=TRUE)
##D 
##D ## design with few whole plot factors
##D ## 2 whole plot factors, 7 split plot factors
##D ## 8 whole plots, i.e. one extra WP factor needed
##D BSS.cheese.exa <- FrF2(32, 9, WPs=8, nfac.WP=2, 
##D       factor.names=c("A","B","p","q","r","s","t","u","v"))
##D design.info(BSS.cheese.exa)
##D ## manual generation of the design used by Bingham, Schoen and Sitter
##D ## note that the generators include a generator for the 10th spplitting factor
##D     ## s= ABq, t = Apq, u = ABpr and v = Aqr, splitting factor rho=Apqr
##D BSS.cheese.exm <- FrF2(32, gen=list(c(1,2,4),c(1,3,4),c(1,2,3,5),c(1,4,5),c(1,3,4,5)), 
##D       WPs=8, nfac.WP=3, WPfacs=c(1,2,10),
##D       factor.names=c("A","B","p","q","r","s","t","u","v","rho"))
##D design.info(BSS.cheese.exm)
##D 
##D ########## usage of estimable ###########################
##D   ## design with all 2fis of factor A estimable on distinct columns in 16 runs
##D   FrF2(16, nfactors=6, estimable = rbind(rep(1,5),2:6), clear=FALSE)
##D   FrF2(16, nfactors=6, estimable = c("AB","AC","AD","AE","AF"), clear=FALSE)
##D   FrF2(16, nfactors=6, estimable = formula("~A+B+C+D+E+F+A:(B+C+D+E+F)"), 
##D        clear=FALSE)
##D             ## formula would also accept self-defined factor names
##D             ## from factor.names instead of letters A, B, C, ...
##D             
##D   ## estimable does not need any other input
##D   FrF2(estimable=formula("~(A+B+C)^2+D+E"))
##D 
##D   ## estimable with factor names 
##D   ## resolution three must be permitted, as FrF2 first determines that 8 runs 
##D   ##     would be sufficient degrees of freedom to estimate all effects 
##D   ##     and then tries to accomodate the 2fis from the model clear of aliasing in 8 runs
##D   FrF2(estimable=formula("~one+two+three+four+two:three+two:four"), 
##D        factor.names=c("one","two","three","four"), res3=TRUE)
##D   ## clear=FALSE allows to allocate all effects on distinct columns in the 
##D   ##     8 run MA resolution IV design
##D   FrF2(estimable=formula("~one+two+three+four+two:three+two:four"), 
##D        factor.names=c("one","two","three","four"), clear=FALSE)
##D 
##D   ## 7 factors instead of 6, but no requirements for factor G
##D   FrF2(16, nfactors=7, estimable = formula("~A+B+C+D+E+F+A:(B+C+D+E+F)"), 
##D        clear=FALSE)
##D   ## larger design for handling this with all required effects clear
##D   FrF2(32, nfactors=7, estimable = formula("~A+B+C+D+E+F+A:(B+C+D+E+F)"), 
##D        clear=TRUE)
##D   ## 16 run design for handling this with required 2fis clear, but main effects aliased
##D   ## (does not usually make sense)
##D   FrF2(16, nfactors=7, estimable = formula("~A+B+C+D+E+F+A:(B+C+D+E+F)"), 
##D        clear=TRUE, res3=TRUE)
##D 
##D ## example for necessity of perms, and uses of select.catlg and perm.start
##D ## based on Wu and Chen Example 1
##D   \dontrun{
##D   ## runs per default about max.time=60 seconds, before throwing error with 
##D   ##        interim results
##D   ## results could be used in select.catlg and perm.start for restarting with 
##D   ##       calculation of further possibilities
##D   FrF2(32, nfactors=11, estimable = formula("~(A+B+C+D+E+F)^2"), clear=FALSE)
##D   ## would run for a long long time (I have not yet been patient enough)
##D   FrF2(32, nfactors=11, estimable = formula("~(A+B+C+D+E+F)^2"), clear=FALSE, 
##D        max.time=Inf)
##D   }
##D   ## can be easily done with perms, 
##D   ## as only different subsets of six factors are non-isomorphic
##D   perms.6 <- combn(11,6)
##D   perms.full <- matrix(NA,ncol(perms.6),11)
##D   for (i in 1:ncol(perms.6))
##D      perms.full[i,] <- c(perms.6[,i],setdiff(1:11,perms.6[,i]))
##D   FrF2(32, nfactors=11, estimable = formula("~(A+B+C+D+E+F)^2"), clear=FALSE, 
##D       perms = perms.full )
## End(Not run)



cleanEx()
nameEx("FrF2Large")
### * FrF2Large

flush(stderr()); flush(stdout())

### Name: FrF2Large
### Title: Function to provide large (at least 8192 runs) regular
###   Fractional Factorial designs that are not necessarily optimal,
###   especially large resolution V designs.
### Aliases: FrF2Large nrunsV
### Keywords: array design

### ** Examples

## numbers of runs needed for resolution V designs in different numbers of factors 
nrunsV(8)
nrunsV(18)
needed <- nrunsV(27)
needed
nrunsV(65)
nrunsV(71)

## Not run: 
##D plan <- FrF2Large(nrunsV(75),75)
##D summary(plan)
## End(Not run)



cleanEx()
nameEx("StructurePickers")
### * StructurePickers

flush(stderr()); flush(stdout())

### Name: StructurePickers
### Title: Functions to find split-plot or left-adjusted designs
### Aliases: splitpick leftadjust
### Keywords: array design

### ** Examples

## leftadjusting MA design from table 6.22 in BHH2, 9 factors, 32 runs
## NOTE: nevertheless not as well left-adjusted as the isomorphic design 9-4.1 from catlg
leftadjust(5,c(30,29,27,23))
## with option early=4 (i.e. 4 columns as early as possible are requested)
leftadjust(5,c(30,29,27,23),early=4)
leftadjust(5,catlg$'9-4.1'$gen,early=4)

## look for a split plot design in 32 runs with 7 factors, 
##       3 of which are whole plot factors, 
##       and 8 plots
splitpick(5,catlg$'7-2.1'$gen,nfac.WP=3,k.WP=3)



cleanEx()
nameEx("add.center")
### * add.center

flush(stderr()); flush(stdout())

### Name: add.center
### Title: Function to add center points to a 2-level fractional factorial
### Aliases: add.center
### Keywords: design array

### ** Examples

  ## purely technical example 
  plan <- FrF2(8,5, factor.names=c("one","two","three","four","five"))
  add.center(plan, 6)
  add.center(plan, 6, distribute=1)
  add.center(plan, 6, distribute=6)
  add.center(plan, 6, distribute=4)
  
  ## very artificial analysis example
    plan <- FrF2(8,4, factor.names=list(one=c(0,10),two=c(1,3),three=c(25,32),four=c(3.7,4.8)))
  ## add some response data
    y <- c(2+desnum(plan)%*%c(2,3,0,0) +
       1.5*apply(desnum(plan)[,c(1,2)],1,"prod") + rnorm(8))
       ## the "c()" makes y into a vector rather than a 1-column matrix
    plan <- add.response(plan, y)
  ## analysing this design provides an impression
    MEPlot(lm(y~(.)^2, plan))
    IAPlot(lm(y~(.)^2, plan))
    DanielPlot(lm(y~(.)^2,plan), half=TRUE, alpha=0.2)
  ## tentative conclusion: factors one and two do something
  ## wonder whether the model with one and two and their interaction is sufficient
  ## look at center points (!!! SHOULD HAVE BEEN INCLUDED FROM THE START,
  ##      but maybe better now than not at all)
  ## use distribute=1, because all center points are run at the end
    planc <- add.center(plan, 6, distribute=1)
      ## conduct additional runs for the center points
        y <- c(y, c(2+desnum(planc)[!iscube(planc),1:4]%*%c(2,3,0,0) +
             1.5*apply(desnum(planc)[!iscube(planc),][,c(1,2)],1,"prod") + rnorm(6)))
  ## add to the design
    planc <- add.response(planc, y, replace=TRUE)
  ## sanity check: repeat previous analyses for comparison, with the help of function iscube()
    MEPlot(lm(y~(.)^2, planc, subset=iscube(planc)))
    IAPlot(lm(y~(.)^2, planc, subset=iscube(planc)))
    DanielPlot(lm(y~(.)^2, planc, subset=iscube(planc)), half=TRUE, alpha=0.2)
  ## quick check whether there a quadratic effect is needed: is the cube indicator significant ?
    summary(lm(y~(.)^2+iscube(planc), planc))
        ## (in this unrealistic example, the quadratic effect is dominating everything else;
        ## with an effect that strong in practice, it is likely that
        ## one would either have expected a strong non-linearity before conducting the experiment, 
        ## OR that the effect is not real but the result of some stupid mistake
  ## alternatively, the check can be calculated per hand (cf. e.g. Montgomery, Chapter 11):
    ((mean(planc$y[iscube(planc)])-mean(planc$y[!iscube(planc)]))^2*8*6/(8+6)/var(y[!iscube(planc)]))
    ## must be compared to the F-quantile with 1 degree of freedom
    ## is the square of the t-value for the cube indicator in the linear model
   


cleanEx()
nameEx("aliases")
### * aliases

flush(stderr()); flush(stdout())

### Name: aliases
### Title: Alias structure for fractional factorial 2-level designs
### Aliases: aliases aliasprint print.aliases
### Keywords: design

### ** Examples

    ### Injection Molding Experiment. Box et al. 1978.
    data(BM93.e3.data)  #from BsMD
    iMdat <- BM93.e3.data[1:16,2:10]  #only original experiment
    # make data more user-friendly
    colnames(iMdat) <- c("MoldTemp","Moisture","HoldPress","CavityThick",
             "BoostPress","CycleTime","GateSize","ScrewSpeed","y")
    # determine aliases with all 2-factor-interactions
    aliases(lm(y ~ (.)^2, data = iMdat))
    # coded version
    aliases(lm(y ~ (.)^2, data = iMdat), code=TRUE)
    # determine aliases with all 3-factor-interactions
    aliases(lm(y ~ (.)^3, data = iMdat), code=TRUE)
    # show condensed form
    aliases(lm(y ~ (.)^3, data = iMdat), code=TRUE, condense=TRUE)
    # determine aliases for unaliased model
    aliases(lm(y ~ ., data = iMdat))



cleanEx()
nameEx("block")
### * block

flush(stderr()); flush(stdout())

### Name: block
### Title: Statistical and algorithmic aspects of blocking in FrF2
### Aliases: block
### Keywords: array design

### ** Examples

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




cleanEx()
nameEx("blockpick")
### * blockpick

flush(stderr()); flush(stdout())

### Name: blockpick
### Title: Function to show potential block assignments
### Aliases: blockpick blockpick.big
### Keywords: array design

### ** Examples

## look at possibilities for running a 32 run design with 6 factors in 8 blocks
## running this without alias.block.2fis=TRUE throws an error: not possible
## Not run: blockpick(k=5,design="6-1.1",k.block=3)
## the 8th to 10th design have more clear 2fis than the earlier ones
blockpick(k=5,design="6-1.1",k.block=3,alias.block.2fis=TRUE)
## function FrF2 can be used to manually accomodate this 
des32.6fac.8blocks.MaxC2 <- FrF2(32,6,blocks=c(3,12,21))
summary(des32.6fac.8blocks.MaxC2)
## automatic block generation leads to more aliased 2fis
summary(FrF2(32,6,blocks=8,alias.block.2fis=TRUE))

## look at possibilities for blocking design 7-3.1 from Chen, Sun, Wu catalogue
blockpick(4,design="7-3.1",k.block=2,alias.block.2fis=TRUE)

## big design
## running this throws an error on many machines because of too little memory
## Not run: blockpick(6,design="7-1.2",k.block=5,alias.block.2fis=TRUE)
## for obtaining a design for this scenario with blockpick.big, 
## the number of factors must be increased to 7+k.block=12
## designs 12-6.1 and 12-6.2 dont do it, 12-6.3 does
bpb <- blockpick.big(6,design="12-6.3",k.block=5,alias.block.2fis=TRUE)
bpb
## based on the result of blockpick.big, a blocked design can be obtained as follows:
des64.7fac.32blocks <- FrF2(64,gen=bpb$gen[1,], blocks = as.list(1:5), 
   alias.block.2fis=TRUE)
str(des64.7fac.32blocks)
## if the seven factors are to be named A,...,G:
des64.7fac.32blocks <- FrF2(64,gen=bpb$gen[1,], blocks = as.list(1:5), 
   alias.block.2fis=TRUE, factor.names=c(paste("b",1:5,sep=""),Letters[1:7]))
str(des64.7fac.32blocks)




cleanEx()
nameEx("compromise")
### * compromise

flush(stderr()); flush(stdout())

### Name: compromise
### Title: Function to support estimability requests for compromise designs
### Aliases: compromise
### Keywords: array design

### ** Examples

## seven factors two of which are in group G1
C1 <- compromise(7, c(2,4), class=1)
C1$perms.full  ## the same for all classes
C1$requirement
C2 <- compromise(7, c(2,4), class=2)
C2$requirement
C3 <- compromise(7, c(2,4), class=3)
C3$requirement
C4 <- compromise(7, c(2,4), class=4)
C4$requirement

## Not run: 
##D ########## usage of estimable ###########################
##D   ## design with with BD clear in 16 runs
##D   FrF2(16,7,estimable = C1$requirement)
##D   ## design with BD estimable on a distinct column in 16 runs (any design will do,
##D   ##    if resolution IV!!!
##D   FrF2(16,7,estimable = C1$requirement, clear=FALSE, perms=C1$perms.full)
##D   ## all four classes, mostly clear, for 32 runs
##D   FrF2(32,7,estimable = C1$requirement)
##D   FrF2(32,7,estimable = C2$requirement)   ## requires resolution V
##D          ## as clear class 2 compromise designs do not exist due to Ke et al. 2005
##D   FrF2(32,7,estimable = C2$requirement, clear=FALSE, perms=C2$perms.full)
##D   FrF2(32,7,estimable = C3$requirement)
##D   FrF2(32,7,estimable = C4$requirement)
##D   ## two additional factors H and J that do not show up in the requirement set
##D   FrF2(32,9,estimable = C3$requirement)
##D   ## two additional factors H and J that do not show up in the requirement set
##D   FrF2(32,9,estimable = C3$requirement, clear=FALSE)
##D   ## note that this is not possible for distinct designs in case perms is needed,
##D   ## because perms must have nfactors columns
## End(Not run)



cleanEx()
nameEx("estimable.2fis")
### * estimable.2fis

flush(stderr()); flush(stdout())

### Name: estimable.2fis
### Title: Statistical and algorithmic aspects of requesting 2-factor
###   interactions to be estimable in FrF2
### Aliases: estimable.2fis
### Keywords: array design

### ** Examples

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

## example for necessity of perms for the clear=FALSE case
## based on Wu and Chen Example 1
  ## Not run: 
##D   ## runs per default about max.time=60 seconds, before throwing error with 
##D   ##        interim results
##D   ## results could be used in select.catlg and perm.start for restarting with 
##D   ##       calculation of further possibilities
##D   FrF2(32, nfactors=11, estimable = formula("~(A+B+C+D+E+F)^2"), clear=FALSE)
##D   ## would run for a long long time (I have not yet been patient enough)
##D   FrF2(32, nfactors=11, estimable = formula("~(A+B+C+D+E+F)^2"), clear=FALSE, 
##D        max.time=Inf)
##D   
## End(Not run)
  ## can be easily done with perms, 
  ## as only different subsets of six factors are non-isomorphic
  perms.6 <- combn(11,6)
  perms.full <- matrix(NA,ncol(perms.6),11)
  for (i in 1:ncol(perms.6))
     perms.full[i,] <- c(perms.6[,i],setdiff(1:11,perms.6[,i]))
  ## function compromise will calculate the necessary perms entries automatically
  compromise(11,1:6)$perms.full
  FrF2(32, nfactors=11, estimable = formula("~(A+B+C+D+E+F)^2"), clear=FALSE, 
      perms = perms.full )



cleanEx()
nameEx("fold.design")
### * fold.design

flush(stderr()); flush(stdout())

### Name: fold.design
### Title: Function to create a foldover for 2-level fractional factorials
### Aliases: fold.design
### Keywords: design array

### ** Examples

  ## create resolution III design
  plan <- FrF2(8,5, factor.names=c("one","two","three","four","five"))
  ## add some resonse data
  y <- c(2+desnum(plan)%*%c(2,3,0,0,0) +
     1.5*apply(desnum(plan)[,c(1,2)],1,"prod") + rnorm(8))
     ## the "c()" makes y into a vector rather than a 1-column matrix
  plan <- add.response(plan, y)
  DanielPlot(lm(y~(.)^2,plan), alpha=0.2, half=TRUE)
  ## alias information
  design.info(plan)
  ## full foldover for dealiasing all main effects
  plan <- fold.design(plan)
  design.info(plan)
  ## further data, shifted by -2
  y <- c(y, desnum(plan)[9:16,1:5]%*%c(2,3,0,0,0) +
     1.5*apply(desnum(plan)[9:16,c(1,2)],1,"prod") + rnorm(8))
  plan <- add.response(plan, y, replace=TRUE)
  linmod <- lm(y~(.)^2,plan)
  DanielPlot(linmod, alpha=0.2, half=TRUE)
  MEPlot(linmod)
  IAPlot(linmod)
  
  ## fold on factor a only (also removes main effect aliasing here)
  plan <- FrF2(8,5, factor.names=c("one","two","three","four","five"))
  aliasprint(plan)
  plan <- fold.design(plan, columns=1)
  aliasprint(plan)
  
  ## fold a Plackett-Burman design with 11 factors
  plan <- pb(12)
  fold.design(plan)
   


cleanEx()
nameEx("pb")
### * pb

flush(stderr()); flush(stdout())

### Name: pb
### Title: Function to generate non-regular fractional factorial screening
###   designs
### Aliases: pb pb.list
### Keywords: array design

### ** Examples

   pb(12,randomize=FALSE)
   pb(12,randomize=FALSE,n12.taguchi=TRUE)
   pb(20,seed=29869)
   pb(16,factor.names=list(A="",B="",C="",D=c("min","max"),
          E="",F="",G="",H="",J=c("new","old")))
   pb(8,default.levels=c("current","new"))
   test <- pb(40) ## design created by doubling the 20 run design
   pb(12, ncenter=6) ## 6 center points with default placement
   
   ## Not run: 
##D    ## note: designs in 40, 56, 64, 88, and 96 runs are resolution IV,
##D    ## if the number of factors is up to nruns/2 - 1, e.g.:
##D    plan1 <- pb(40, 19)
##D    length3(plan1)  ## 0 generalized words of length 3
##D    length4(plan1)  ## 228 generalized words of length 4
##D    ## they can be made resolution IV by oldver=TRUE for 
##D    ## nfactors=nruns/2, e.g.:
##D    plan2 <- pb(40, 20)
##D    plan3 <- pb(40, 20, oldver=TRUE)
##D    length3(plan2)  ## 9 generalized words of length 3
##D    length3(plan3)  ## 0 generalized words of length 3
##D    length4(plan3)  ## 285 generalized words of length 4
##D    
##D    ## note: designs in 52, 76, and 100 runs are almost resolution IV,
##D    ## if the number of factors is up to nruns/2 - 1, e.g.:
##D    plan4 <- pb(52, 25)
##D    GR(plan4)       ## generalized resolution 3.92
##D    
##D    ## note: versions >1.3 avoid complete and heavy aliasing of triples of factors 
##D    ## for up to nruns-2 factors for 40, 52, 56, 64, 76, 88, 92 and 96 runs
##D    ## (the same for 100 runs, which were not implemented before version 1.3)
##D    plan5 <- pb(40, 38)
##D    plan6 <- pb(40, 38, oldver=TRUE)
##D    GR(plan5)       ## generalized resolution 3.4
##D    GR(plan6)       ## generalized resolution 3
##D    plan7 <- pb(52, 50)
##D    plan8 <- pb(52, 50, oldver=TRUE)
##D    GR(plan7)       ## generalized resolution 3.62
##D    GR(plan8)       ## generalized resolution 3.15
##D    
## End(Not run)
   


cleanEx()
nameEx("splitplot")
### * splitplot

flush(stderr()); flush(stdout())

### Name: splitplot
### Title: Statistical and algorithmic aspects of split-plot designs in
###   FrF2
### Aliases: splitplot
### Keywords: array design

### ** Examples

########## hard to change factors ####################
## example from Bingham and Sitter Technometrics 19999
## MotorSpeed, FeedMode,FeedSizing,MaterialType are hard to change
BS.ex <- FrF2(16,7,hard=4,
     factor.names=c("MotorSpeed", "FeedMode","FeedSizing","MaterialType",
                  "Gain","ScreenAngle","ScreenVibLevel"), 
     default.levels=c("-","+"),randomize=FALSE)
design.info(BS.ex)
BS.ex
## NOTE: the design has 8 whole plots.
## If randomize=FALSE is used like here, the first hard-to-change factors 
## do not always change between whole plots. 
## A conscious and honest decision is required whether this is 
##    acceptable for the situation at hand!
## randomize=TRUE would cause more changes in the first four factors.

########## automatic generation for split plot ##########
## 3 control factors, 5 noise factors, control factors are whole plot factors
## 8 plots desired in a total of 32 runs
## Bingham Sitter 2003
BS.ex2a <- FrF2(32, 8, WPs=8, nfac.WP=3, 
      factor.names=c(paste("C",1:3,sep=""), paste("N",1:5,sep="")),randomize=TRUE)

## manual generation of this same design
BS.ex2m <- FrF2(32, 8, generators=c("ABD","ACD","BCDE"),WPs=8, WPfacs=c("C1","C2","C3"), nfac.WP=3, 
      factor.names=c(paste("C",1:3,sep=""),paste("N",1:5,sep="")),randomize=TRUE)

## design with few whole plot factors
## 2 whole plot factors, 7 split plot factors
## 8 whole plots, i.e. one extra WP factor needed
BSS.cheese.exa <- FrF2(32, 9, WPs=8, nfac.WP=2, 
      factor.names=c("A","B","p","q","r","s","t","u","v"))
design.info(BSS.cheese.exa)
## manual generation of the design used by Bingham, Schoen and Sitter
## note that the generators include a generator for the 10th spplitting factor
    ## s= ABq, t = Apq, u = ABpr and v = Aqr, splitting factor rho=Apqr
BSS.cheese.exm <- FrF2(32, gen=list(c(1,2,4),c(1,3,4),c(1,2,3,5),c(1,4,5),c(1,3,4,5)), 
      WPs=8, nfac.WP=3, WPfacs=c(1,2,10),
      factor.names=c("A","B","p","q","r","s","t","u","v","rho"))
design.info(BSS.cheese.exm)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
