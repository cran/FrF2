FrF2 <- function(nruns=NULL, nfactors=NULL, 
                 factor.names = if(!is.null(nfactors)) {if(nfactors<=50) Letters[1:nfactors] else paste("F",1:nfactors,sep="")} else NULL, 
                 default.levels = c(-1,1), 
                 generators=NULL, resolution=NULL, estimable=NULL, max.nfree2fis=FALSE, 
                 randomize=TRUE, seed=NULL, ...){
    ## check nruns
    if (!is.null(nruns)){
       k <- round(log2(nruns))
       if (!2^k==nruns) stop("nruns must be a power of 2.")}
    ## check factor specifications
    if (is.null(factor.names) & is.null(nfactors) & (is.null(nruns) | is.null(generators)) ) 
         stop("The number of factors must be specified either nfactors, via factor.names or via nruns together with generators.")
    if (!is.null(factor.names) & !(is.character(factor.names) | is.list(factor.names)) ) stop("factor.names must be a character vector or a list.")
    if (is.null(nfactors)) {if (!is.null(factor.names)) nfactors <- length(factor.names)
                          else nfactors <- length(generators)+k}
    if (!nfactors==floor(nfactors)) stop("nfactors must be an integer number.")
    if (!is.null(factor.names) & !length(factor.names)==nfactors) stop("There must be nfactors factor names, if any.")
    if (!is.null(estimable)) warning("estimable is not yet implemented and has been ignored.")
    ## check factor level specifications
    if (!((is.character(default.levels) | is.numeric(default.levels)) & length(default.levels)==2) ) 
                 stop("default.levels must be a vector of 2 levels.")
    if (is.list(factor.names)){ if (any(factor.names=="")) factor.names[which(factor.names=="")] <- list(default.levels)}
        else {hilf <- vector("list",nfactors)
              names(hilf) <- factor.names
              hilf[1:nfactors]<-list(default.levels)
              factor.names <- hilf}

    if (!is.null(nruns)){
       ## find FrF2 for given number of runs
       if (nfactors<=k) stop(paste("You can accomodate ",nfactors," factors in ", 
              nruns, " runs without fractionating. \n Function FrF2 is for fractional factorials only."))
       if (nfactors>nruns-1) stop("You can accomodate at most ",nruns-1," factors in a FrF2 design with ",nruns," runs." )
       g <- nfactors - k  ## number of generators needed
       if (!is.null(generators)) {
              generators <- gen.check(k, generators)
              if (!length(generators)==g) stop("This design in ", nruns, " runs with ", nfactors," factors requires ", g, " generators.")
              }
       if (is.null(generators)) cand <- catlg[nruns.catlg(catlg)==nruns & nfac.catlg(catlg)==nfactors]
                                 
       else {  res <- NA; nfree.2fis<-NA; free.2fis<-NULL;all.2fis.free<-NA
               if (g<10) wl <- words.all(k, generators,max.length=6)
               else if (g<15) wl <- words.all(k, gen,max.length=5)
               else if (g<20) wl <- words.all(k, gen,max.length=4)
               WLP <- wl$WLP
               res <- min(as.numeric(names(WLP)[which(WLP>0)]))
               if (res==Inf) {if (g<10) res="7+"
               else if (g<15) res="6+"
               else if (g<20) res="5+"}

               cand <- list(custom=list(res=res, nfac=nfactors, nruns=nruns, 
                    gen=sapply(generators,function(obj) which(sapply(Yates[1:(nruns-1)],function(obj2) identical(obj,obj2)))), 
                    WLP=WLP, nfree.2fis=nfree.2fis, free.2fis=free.2fis, all.2fis.free=all.2fis.free))
                   ## needs to be list of list, because access later is always via cand[1]
               class(cand) <- c("catlg","list")
           }
    }
    else {
       ## find smallest FrF2 that fulfills the requirements regarding resolution/estimability
       if (is.null(resolution) & is.null(estimable)) stop("At least one of nruns or resolution or estimable must be given.")
       if (!is.null(resolution)) {
                 if (!(resolution==floor(resolution) & resolution>=3)) stop("resolution must be an integer number >=3.")
                 cand <- catlg[which(res.catlg(catlg)>=resolution & nfac.catlg(catlg)==nfactors)]
       }
       else cand <- catlg[which(nfac.catlg(catlg)==nfactors)]   
           ## no prior restriction by resolution
       #if (!is.null(estimable)) ## select those candidate designs with the requested effects unconfounded, if possible
       ## not yet implemented
       if (length(cand)==0) stop("There is no design with the requested combination of nfactors and resolution.")
    }
    if (max.nfree2fis) cand <- cand[which.max(sapply(cand[which(sapply(cand, function(obj) obj$res)==max(sapply(cand, function(obj) obj$res)))], 
              function(obj) obj$nfree.2fis))]
    ### so far, only output parameters of selected design
    ## generators and estimable models not yet implemented
    ### creation of actual design requires further work on replication
    if (is.null(nruns)) {nruns <- cand[[1]]$nruns 
                         k <- log2(nruns)
                         g <- nfactors - k}
    destxt <- "expand.grid(c(-1,1)"
    for (i in 2:k) destxt <- paste(destxt,",c(-1,1)",sep="")
    destxt <- paste("as.matrix(",destxt,"))",sep="")
    desmat <- eval(parse(text=destxt))
    rownames(desmat) <- 1:nruns
    if (randomize) {
        if (!is.null(seed)) set.seed(seed)
        desmat <- desmat[sample(nruns),]
    }
    for (i in 1:g) desmat <- cbind(desmat, apply(desmat[,unlist(Yates[cand[[1]]$gen[[i]]])],1,prod))
    colnames(desmat) <- names(factor.names)
    desdf <- data.frame(desmat)
    for (i in 1:nfactors) desdf[,i] <- des.recode(desdf[,i],"-1=factor.names[[i]][1];1=factor.names[[i]][2]") 
    aus <- list(desnum=desmat,design=desdf,catentry=cand[1])
    aus
} 