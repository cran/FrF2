FrF2 <- function(nruns=NULL, nfactors=NULL, 
                 factor.names = if(!is.null(nfactors)) {if(nfactors<=50) Letters[1:nfactors] else paste("F",1:nfactors,sep="")} else NULL, 
                 default.levels = c(-1,1), 
                 generators=NULL, resolution=NULL, estimable=NULL, clear=TRUE, res3=FALSE, max.time=60, 
                 select.catlg=catlg, perm.start=NULL, perms=NULL, MaxC2=FALSE, 
                 replications=1, repeat.only=FALSE, 
                 randomize=TRUE, seed=NULL, alias.info=2, ...){
    if (!is.null(nruns)){
       k <- round(log2(nruns))
       if (!2^k==nruns) stop("nruns must be a power of 2.")
       if (nruns < 8 | nruns > 64) stop("less than 8 or more than 64 runs are currently not covered by FrF2.")
       }
    if (!(is.logical(clear) | is.logical(FALSE) | is.logical(MaxC2) | is.logical(repeat.only) | is.logical(randomize) ))
       stop("clear, res3, MaxC2, repeat.only and randomize must be logicals (TRUE or FALSE).")
    if (!is.numeric(max.time)) stop("max.time must be a positive maximum run time for searching a design with estimable given and clear=FALSE.")
    if (!(is.null(resolution) | is.null(estimable))) stop("You can only specify resolution OR estimable.")
    if (!(is.null(resolution) | is.null(nruns))) warning("resolution is ignored, if nruns is given.")
    ## check factor specifications
    if (is.null(factor.names) & is.null(nfactors) & (is.null(nruns) | is.null(generators)) & is.null(estimable)) 
         stop("The number of factors must be specified either nfactors, via factor.names, via estimable, or via nruns together with generators.")
    if (!is.null(factor.names) & !(is.character(factor.names) | is.list(factor.names)) ) stop("factor.names must be a character vector or a list.")
    if (is.null(nfactors)) {if (!is.null(factor.names)) nfactors <- length(factor.names)
                          else if (!is.null(generators)) nfactors <- length(generators)+k}
    if (!is.null(estimable)) {
                if (!is.null(generators)) stop("You cannot combine estimable with generators.")
              estimable <- estimable.check(estimable, nfactors, factor.names)
              if (is.null(nfactors)) nfactors <- estimable$nfac
              estimable <- estimable$estimable
              if (is.null(nruns)) {
                 ## make even
                 nruns <- nfactors+ncol(estimable)+1 + (nfactors+ncol(estimable)+1)%%2
                 if (!isTRUE(all.equal(log2(nruns) %% 1,0))) nruns <- 2^(floor(log2(nruns))+1)
                 k <- round(log2(nruns))
                 if (k<3) stop("Please specify nruns and/or nfactors. Calculated values are unreasonable.")
           }
                if (is.null(perm.start)) perm.start <- 1:nfactors
                    else if (!is.numeric(perm.start)) 
                         stop ("perm.start must be NULL or a numeric permutation vector of length nfactors.")
                if (!all(sort(perm.start)==1:nfactors)) 
                         stop ("perm.start must be NULL or a numeric permutation vector of length nfactors.")
                if (!is.null(perms)) {
                   if (!is.matrix(perms) | !is.numeric(perms)) stop("perms must be a numeric matrix.")
                   if (!ncol(perms)==nfactors) stop ("matrix perms must have nfactors columns.")
                   if (any(apply(perms,1,function(obj) any(!sort(obj)==1:nfactors)))) 
                        stop("Each row of perms must be a permutation of 1:nfactors.")
                   }
    }
          ## from here on, estimable is a numeric matrix with two rows
    if (!nfactors==floor(nfactors)) stop("nfactors must be an integer number.")
    if (!is.null(factor.names) & !length(factor.names)==nfactors) stop("There must be nfactors factor names, if any.")
    if (is.null(factor.names)) if(nfactors<=50) factor.names <- Letters[1:nfactors] else factor.names <- paste("F",1:nfactors,sep="")
    
    if (!alias.info %in% c(2,3)) stop("alias.info can be 2 or 3 only.")
    ## check factor level specifications
    if (!((is.character(default.levels) | is.numeric(default.levels)) & length(default.levels)==2) ) 
                 stop("default.levels must be a vector of 2 levels.")
    if (is.list(factor.names)){ if (any(factor.names=="")) factor.names[which(factor.names=="")] <- list(default.levels)}
        else {hilf <- vector("list",nfactors)
              names(hilf) <- factor.names
              hilf[1:nfactors]<-list(default.levels)
              factor.names <- hilf}
    ## from here on, factor.names is a named list

    
    ff <- FALSE  ## indicator whether full factorial

    if (!is.null(nruns)){
       ## find FrF2 for given number of runs
       ## or estimable given (here, number of runs has already been calculated if necessary)
       if (nfactors<=k) {
                    ### full factorial
                    if (nfactors==k) aus <- fac.design(2, k, factor.names=factor.names, 
                             replications=replications, repeat.only=repeat.only, randomize=randomize, seed=seed)
                    else aus <- fac.design(2, nfactors, factor.names=factor.names, 
                             replications=replications*2^(k-nfactors), repeat.only=repeat.only, randomize=randomize, seed=seed)
                    ff <- TRUE
              }
       else {
       ### fractional factorial
       if (nfactors>nruns-1) stop("You can accomodate at most ",nruns-1," factors in a FrF2 design with ",nruns," runs." )
       g <- nfactors - k  ## number of generators needed
       if (!is.null(generators)) {
              generators <- gen.check(k, generators)
              if (!length(generators)==g) 
              stop("This design in ", nruns, " runs with ", nfactors," factors requires ", g, " generators.")
              }
       if (is.null(generators)) {
            if (!(is.null(estimable) | ff)) {
                      desmat <- estimable(estimable, nfactors, nruns, 
                           clear=clear, res3=res3, max.time=max.time, select.catlg=select.catlg, 
                           perm.start=perm.start, perms=perms, order=alias.info )
                      design.info <- list(type="FrF2.estimable", map=desmat$map, aliased=desmat$aliased, clear=clear, res3=res3)
                      desmat <- desmat$design
                  }
            else cand <- select.catlg[nruns.catlg(catlg)==nruns & nfac.catlg(catlg)==nfactors]
            }
       else {  res <- NA; nclear.2fis<-NA; clear.2fis<-NULL;all.2fis.clear<-NA
               if (g<10) wl <- words.all(k, generators,max.length=6)
               else if (g<15) wl <- words.all(k, generators,max.length=5)
               else if (g<20) wl <- words.all(k, generators,max.length=4)
               WLP <- wl$WLP
               res <- min(as.numeric(names(WLP)[which(WLP>0)]))
               if (res==Inf) {if (g<10) res="7+"
               else if (g<15) res="6+"
               else if (g<20) res="5+"}

               cand <- list(custom=list(res=res, nfac=nfactors, nruns=nruns, 
                    gen=sapply(generators,function(obj) which(sapply(Yates[1:(nruns-1)],function(obj2) identical(obj,obj2)))), 
                    WLP=WLP, nclear.2fis=nclear.2fis, clear.2fis=clear.2fis, all.2fis.clear=all.2fis.clear))
                   ## needs to be list of list, because access later is always via cand[1]
               class(cand) <- c("catlg","list")
           }
           } ## end of full or fractional factorial for given nruns or estimable
    }
    else {
       ## nruns not given and not already assigned for estimable
       ## find smallest FrF2 that fulfills the requirements regarding resolution/estimability
       if (is.null(resolution) & is.null(estimable)) stop("At least one of nruns or resolution or estimable must be given.")
       if (!is.null(resolution)) {
                 if (!(resolution==floor(resolution) & resolution>=3)) stop("resolution must be an integer number >=3.")
                 cand <- catlg[which(res.catlg(catlg)>=resolution & nfac.catlg(catlg)==nfactors)]
       }
       else cand <- catlg[which(nfac.catlg(catlg)==nfactors)]   
           ## no prior restriction by resolution
       #if (!is.null(estimable)) ## select those candidate designs with the requested effects unconfounded, if possible
       if (length(cand)==0) stop("Only a full factorial or a design with more than 64 runs grants the requested combination of nfactors and resolution.")
    }
    ## standard FrF2 situations
    if (MaxC2 & is.null(estimable) & is.null(generators) & !ff) 
           cand <- cand[which.max(sapply(cand[which(sapply(cand, function(obj) obj$res)==max(sapply(cand, function(obj) obj$res)))], 
              function(obj) obj$nclear.2fis))]

    ### creation of actual design 
    ## full factorial done already, therefore excluded
    if (!ff){
    if (is.null(nruns)) {nruns <- cand[[1]]$nruns 
                         k <- round(log2(nruns))
                         g <- nfactors - k}
    if (is.null(estimable)){
       destxt <- "expand.grid(c(-1,1)"
       for (i in 2:k) destxt <- paste(destxt,",c(-1,1)",sep="")
       destxt <- paste("as.matrix(",destxt,"))",sep="")
       desmat <- eval(parse(text=destxt))
    }
    rownames(desmat) <- 1:nruns

      rand.ord <- rep(1:nruns,replications)
      if (replications > 1 & repeat.only) rand.ord <- rep(1:nruns,each=replications)
      if (randomize & !is.null(seed)) set.seed(seed)
      if (randomize & !repeat.only) for (i in 1:replications) 
                  rand.ord[((i-1)*nruns+1):(i*nruns)] <- sample(nruns)
      if (randomize & repeat.only) rand.ord <- rep(sample(1:nruns), each=replications)

      desmat <- desmat[rand.ord,]

    if (is.null(estimable))
    for (i in 1:g) desmat <- cbind(desmat, apply(desmat[,unlist(Yates[cand[[1]]$gen[[i]]])],1,prod))
    
    colnames(desmat) <- names(factor.names)
    orig.no <- rownames(desmat)
    rownames(desmat) <- 1:(nruns*replications)
    desdf <- data.frame(desmat)
    for (i in 1:nfactors) desdf[,i] <- des.recode(desdf[,i],"-1=factor.names[[i]][1];1=factor.names[[i]][2]") 
    if (is.null(estimable) & is.null(generators) & !ff) 
         design.info <- list(type="FrF2", catlg.entry=cand[1], aliased = alias3fi(k,cand[1][[1]]$gen,order=alias.info))
    if (!is.null(generators)) {
         names(generators) <- Letters[(k+1):nfactors]
         gen.display <- paste(Letters[(k+1):nfactors],sapply(generators,function(obj) paste(Letters[obj],collapse="")),sep="=")
         design.info <- list(type="FrF2.generators", generators=gen.display, aliased = alias3fi(k,generators,order=alias.info))
         }
    aus <- desdf
    attr(aus,"desnum") <- desmat
    attr(aus,"run.order") <- cbind("run.no.in.std.order"=rand.ord,"run.no"=1:nrow(desmat))
    attr(aus,"design.info") <- c(design.info, replications=replications, repeat.only=repeat.only,
      randomize=randomize, seed=seed)
      }# end of excluding full factorial
    class(aus) <- c("design","data.frame")
    aus
} 