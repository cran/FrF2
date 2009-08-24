### n=100 noch einbauen
### can foldover techniques improve on projectivity here ?
###        for k<=n/2 with folded-over designs: use last n/2 columns (check, but seems to be OK, stated by Samset Tyssedal 1999
### pb in screen umbenennen ?

### references Plackett and Burman 1946, Box and Tyssedal 2001 and 1996, Samset and Tyssedal 1999 (with warning against mistakes, if checked)
###     Hedayat and Stufken 1999, Williamson 1944

## Plackett Burman generating vectors and matrices, according to 1946 article
##    c(-1,-1,-1,-1,1,-1,1,-1,1,1,1,-1,1,1,-1,-1,-1,1,1,1,1,1,-1,-1,1,1,-1,1,-1,-1,1)
##    for n=16, Box-Tyssedal constructions are used per default
##        !!! apparently, Samset and Tyssedal 1999 are wrong about projectivity 4 of first 9 columns in large screen ?
##    for n=32 replaced by non-geometric one, which is a cyclic design
##        with projectivity 3
##        with generator obtained from Samset and Tyssedal (1999)
##    for n=92, Williamson type design has been added (normalized to (omitted) +1 first column and -1 last row)
##              Williamson generating vectors from Hedayat and Stufken (1999), p. 160
##    for doubled designs, doubling column is moved to last column, since it is worst for projectivity of design with many factors 
##              (without it, it's projectivity 3, otherwise not)
##              If only half of the factors are used: last n/2 columns are of projectivity 4!!!


## function for generating screening designs
## according to Plackett and Burman, Taguchi (L12, same as PB, but different order of rows and columns), and Box and Tyssedal for 16 runs
pb <- function(nruns,nfactors=nruns-1, 
                 factor.names = if(nfactors<=50) Letters[1:nfactors] else paste("F",1:nfactors,sep=""), 
                 default.levels = c(-1,1), ncenter=0, center.distribute=NULL,
                 boxtyssedal=TRUE, n12.taguchi=FALSE, 
                 replications=1, repeat.only=FALSE, 
                 randomize=TRUE,seed=NULL, ...){
  creator <- sys.call()
    ## check validity of center point options
        if (!is.numeric(ncenter)) stop("ncenter must be a number")
        if (!length(ncenter)==1) stop("ncenter must be a number")
        if (!ncenter==floor(ncenter)) stop("ncenter must be an integer number")
        if (is.null(center.distribute)){
          if (!randomize) center.distribute <- min(ncenter,1)
             else center.distribute <- min(ncenter, 3)}
        if (!is.numeric(center.distribute)) stop("center.distribute must be a number")
        if (!center.distribute==floor(center.distribute)) stop("center.distribute must be an integer number")
        if (center.distribute > min(ncenter,nruns+1))
            stop("center.distribute can be at most min(ncenter, nruns+1)")
        if (randomize & center.distribute==1) warning("running all center point runs together is usually not a good idea.")
  ## error checks
  if (default.levels[1]==default.levels[2]) stop("Both default levels are identical.")
  if (nruns == 8) {
      if (nfactors>4) warning("Plackett-Burman designs in 8 runs coincide with regular fractional factorials. 
          For screening more than four factors, you may want to consider increasing the number of runs to 12. 
          Make sure to take the alias structure into account for interpretation!")
      else {
           aus <- FrF2(nruns=8, nfactors=nfactors, factor.names=factor.names,
                  default.levels=default.levels, replications=replications, repeat.only=repeat.only,
                  randomize=randomize,seed=seed, ...)
          warning("The design has been constructed with function FrF2 in order to ensure 
                resolution IV and show alias information.")
           }
      }
  if (!isTRUE(all.equal((nruns/4) %% 1,0))) stop("Plackett-Burman designs require that nruns is a multiple of 4.")
  if (is.null(nfactors)) stop("nfactors must be given.")
  if (!nfactors==floor(nfactors)) stop("nfactors must be an integer number.")
  if (!((is.character(default.levels) | is.numeric(default.levels)) & length(default.levels)==2) ) 
                 stop("default.levels must be a vector of 2 levels.")
  if (!is.null(factor.names) & !(is.character(factor.names) | is.list(factor.names)) ) stop("factor.names must be a character vector or a list.")
  if (length(factor.names)>nfactors) stop("You have specified more than nfactors factors in factor.names.")
  if (length(factor.names) < nfactors) {
            n.error <- nfactors-length(factor.names)
            hilf <- rep("",n.error)
            names(hilf) <- paste("e",1:n.error,sep="")
            if (!is.list(factor.names)) factor.names <- c(factor.names,names(hilf))
            else factor.names <- c(factor.names, as.list(hilf))
            }
  if (is.list(factor.names)){ 
        if (is.null(names(factor.names))){
            if (nfactors<=50) names(factor.names) <- Letters[1:nfactors]
            else names(factor.names) <- paste("F", 1:nfactors, sep="")
        } 
        if(any(factor.names=="")) factor.names[which(factor.names=="")] <- list(default.levels)
      }
        else {hilf <- vector("list",nfactors)
              names(hilf) <- factor.names
              hilf[1:nfactors]<-list(default.levels)
              factor.names <- hilf}
  ## from now on, factor.names is a list with all elements vectors
      if (ncenter > 0) if(any(is.na(sapply(factor.names,"is.numeric"))))
       stop("Center points are implemented for experiments with all factors quantitative only.")

  gen <- pb.list[[2]][[which(pb.list[[1]]==nruns)]]
  if (nruns==12 & n12.taguchi) sel <- taguchi(12)[,1:nfactors]
  else {
  if ((nruns==16) & boxtyssedal) sel <- boxtyssedal(nruns,nfactors)
  else {
  sel <- gen
  if (!length(gen)==nruns-1){ 
    if (nruns==28) sel <- rbind(sel,gen[,c(19:27,1:18)],gen[,c(10:27,1:9)],rep(-1,27))
    else if (nruns==52) sel <- rbind(sel,cbind(rep(c(1,-1),5),gen[2:11,c(42:51,2:41)]),
                 cbind(rep(c(1,-1),5),gen[2:11,c(32:51,2:31)]),
                 cbind(rep(c(1,-1),5),gen[2:11,c(22:51,2:21)]),
                 cbind(rep(c(1,-1),5),gen[2:11,c(12:51,2:11)]),
                 rep(-1,51))
    else if (nruns==76) {
                 sel <- matrix(0,76,75)
                 sel[1:3,]<-gen
                 for (i in 1:36) sel[(2+i*2):(3+i*2),] <- cbind(c(1,-1),gen[2:3,c((75-2*i+1):75,2:(75-2*i))])
                 sel[76,] <- rep(-1,75)
    }
    else if (nruns==92) {
          a<-c(1,1,1,-1,1,1,1,-1,1,-1,-1,-1,-1,-1,-1,1,-1,1,1,1,-1,1,1)
          b<-c(1,1,1,-1,-1,-1,1,1,-1,1,-1,1,1,-1,1,-1,1,1,-1,-1,-1,1,1)
          c<-c(1,-1,1,1,-1,1,1,-1,-1,1,1,1,1,1,1,-1,-1,1,1,-1,1,1,-1)
          d<-c(1,1,-1,-1,-1,1,-1,-1,-1,1,-1,1,1,-1,1,-1,-1,-1,1,-1,-1,-1,1)
          sel <- normalize.col.first(williamson(circ.mat(a),circ.mat(b),circ.mat(c),circ.mat(d)))
          sel <- normalize.row.last(sel)
    }
    else if (nruns %in% c(40,56,64,88,96)) 
           sel <- normalize.row.last(double.des(desnum(pb(nruns/2,randomize=FALSE))))[,1:nfactors]
    else
    stop(paste("Design for", nruns, "runs not yet implemented.","\n")) 
    }
  else sel <- rbind(circ.mat(sel),rep(-1,nruns-1))[,1:nfactors]
  }
  }

      rand.ord <- rep(1:nruns,replications)
      if (replications > 1 & repeat.only) rand.ord <- rep(1:nruns,each=replications)
      if (randomize & !is.null(seed)) set.seed(seed)
      if (randomize & !repeat.only) for (i in 1:replications) 
                  rand.ord[((i-1)*nruns+1):(i*nruns)] <- sample(nruns)
      if (randomize & repeat.only) rand.ord <- rep(sample(1:nruns), each=replications)

    colnames(sel) <- names(factor.names)
    rownames(sel) <- 1:nruns
    sel <- sel[rand.ord,]
    orig.no <- orig.no.rp <- rownames(sel)
    if (replications>1) {
        if (repeat.only) orig.no.rp <- paste(orig.no.rp,rep(1:replications,nruns),sep=".")
        else orig.no.rp <- paste(orig.no.rp,rep(1:replications,each=nruns),sep=".")
        }
    rownames(sel) <- 1:(nruns*replications)
    quant <- rep(FALSE,nfactors)
    desdf <- data.frame(sel)
    for (i in 1:nfactors) {
        desdf[,i] <- des.recode(desdf[,i],"-1=factor.names[[i]][1];1=factor.names[[i]][2]") 
        quant[i] <- is.numeric(desdf[,i])
        desdf[,i] <- factor(desdf[,i],levels=factor.names[[i]]) 
        contrasts(desdf[,i]) <- contr.FrF2(2)
        }
    if (nruns>8 | nfactors>4){ 
      aus <- desdf
          ## otherwise, aus has already been defined earlier
      rownames(aus) <- rownames(sel) <- 1:nrow(aus)
  
      attr(aus,"desnum") <- sel
      attr(aus,"run.order") <- data.frame("run.no.in.std.order"=orig.no,"run.no"=1:nrow(sel),"run.no.std.rp"=orig.no.rp)
      attr(aus,"design.info") <- list(type="pb", 
           nruns=nruns, nfactors=nfactors, factor.names=factor.names,
           replications=replications, repeat.only=repeat.only,
           randomize=randomize, seed=seed, creator=creator)
      class(aus) <- c("design","data.frame")
    }
    if (ncenter > 0) aus <- add.center(aus, ncenter, distribute=center.distribute)
    aus
}

taguchi <- function(nruns){
  if (nruns==12) 
   sel <- L12 <- rbind(c(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1),
      c(-1,-1,-1,-1,-1,+1,+1,+1,+1,+1,+1),
      c(-1,-1,+1,+1,+1,-1,-1,-1,+1,+1,+1),
      c(-1,+1,-1,+1,+1,-1,+1,+1,-1,-1,+1),
      c(-1,+1,+1,-1,+1,+1,-1,+1,-1,+1,-1),
      c(-1,+1,+1,+1,-1,+1,+1,-1,+1,-1,-1),
      c(+1,-1,+1,+1,-1,-1,+1,+1,-1,+1,-1),
      c(+1,-1,+1,-1,+1,+1,+1,-1,-1,-1,+1),
      c(+1,-1,-1,+1,+1,+1,-1,+1,+1,-1,-1),
      c(+1,+1,+1,-1,-1,-1,-1,+1,+1,-1,+1),
      c(+1,+1,-1,+1,-1,+1,-1,-1,-1,+1,+1),
      c(+1,+1,-1,-1,+1,-1,+1,-1,+1,+1,-1))
  sel
}

boxtyssedal <- function(nruns,k){
   ### some effects perfectly aliased, many partially aliased
   if (nruns==16 & k<=12) 
      sel <- rbind(c(-1,-1,-1,-1,-1,-1,-1,-1,+1,-1,-1,-1,+1,+1,+1),
                   c(+1,-1,-1,-1,+1,+1,+1,-1,-1,+1,-1,-1,-1,-1,+1),
                   c(-1,+1,-1,-1,+1,+1,-1,+1,-1,-1,+1,-1,-1,+1,-1),
                   c(+1,+1,-1,-1,-1,-1,+1,+1,-1,-1,-1,+1,+1,-1,-1), 
                   c(-1,-1,+1,-1,+1,-1,+1,+1,+1,+1,+1,-1,+1,-1,-1),
                   c(+1,-1,+1,-1,-1,+1,-1,+1,+1,+1,-1,+1,-1,+1,-1),
                   c(-1,+1,+1,-1,-1,+1,+1,-1,+1,-1,+1,+1,-1,-1,+1),
                   c(+1,+1,+1,-1,+1,-1,-1,-1,-1,+1,+1,+1,+1,+1,+1),
                   c(-1,-1,-1,+1,-1,+1,+1,+1,-1,+1,+1,+1,+1,+1,+1),
                   c(+1,-1,-1,+1,+1,-1,-1,+1,+1,-1,+1,+1,-1,-1,+1),
                   c(-1,+1,-1,+1,+1,-1,+1,-1,+1,+1,-1,+1,-1,+1,-1),
                   c(+1,+1,-1,+1,-1,+1,-1,-1,+1,+1,+1,-1,+1,-1,-1),
                   c(-1,-1,+1,+1,+1,+1,-1,-1,-1,-1,-1,+1,+1,-1,-1),
                   c(+1,-1,+1,+1,-1,-1,+1,-1,-1,-1,+1,-1,-1,+1,-1),
                   c(-1,+1,+1,+1,-1,-1,-1,+1,-1,+1,-1,-1,-1,-1,+1),
                   c(+1,+1,+1,+1,+1,+1,+1,+1,+1,-1,-1,-1,+1,+1,+1))
        ## N = AB = CE = DF
        ## O = AC = BE = DG
        ## P = AE = BC = DH
        ## ABCE
        ## ABDF
        ## ACDG
        ## BDEG
        ## ADEH
        ## BCDH
   if (nruns==16 & k>12) 
      sel <- rbind(c(-1,-1,-1,-1,-1,-1,+1,+1,+1,+1,+1,+1,-1,-1,+1),
                   c(+1,-1,-1,-1,+1,+1,+1,-1,+1,-1,+1,-1,+1,-1,-1),
                   c(-1,+1,-1,-1,+1,+1,-1,+1,+1,-1,-1,+1,-1,+1,-1),
                   c(+1,+1,-1,-1,-1,-1,-1,-1,-1,-1,+1,+1,+1,+1,+1),
                   c(-1,-1,+1,-1,+1,-1,+1,+1,-1,-1,-1,-1,+1,+1,+1),
                   c(+1,-1,+1,-1,-1,+1,-1,+1,-1,+1,-1,+1,+1,-1,-1),
                   c(-1,+1,+1,-1,-1,+1,+1,-1,-1,+1,+1,-1,-1,+1,-1),
                   c(+1,+1,+1,-1,+1,-1,-1,-1,+1,+1,-1,-1,-1,-1,+1),
                   c(-1,-1,-1,+1,-1,+1,-1,-1,+1,+1,-1,-1,+1,+1,+1),
                   c(+1,-1,-1,+1,+1,-1,+1,-1,-1,+1,-1,+1,-1,+1,-1),
                   c(-1,+1,-1,+1,+1,-1,-1,+1,-1,+1,+1,-1,+1,-1,-1),
                   c(+1,+1,-1,+1,-1,+1,+1,+1,-1,-1,-1,-1,-1,-1,+1),
                   c(-1,-1,+1,+1,+1,+1,-1,-1,-1,-1,+1,+1,-1,-1,+1),
                   c(+1,-1,+1,+1,-1,-1,-1,+1,+1,-1,+1,-1,-1,+1,-1),
                   c(-1,+1,+1,+1,-1,-1,+1,-1,+1,-1,-1,+1,+1,-1,-1),
                   c(+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1))
        ## P = AB = CE = DF
        ## Box and Tyssedal (2001); Samset and Tyssedal: last column together with first 8 is of projectivity 4
                                    ## however, this seems to be wrong!
                                    ##if (k==9) sel <- sel[,c(1:8,15)]  ## projectivity 4
   if (k>14) cat("Screening 15 factors in 16 runs involves perfect aliasing of \n pairwise interactions of the first six factors with the last factor.\n")
       sel[,1:k]
}
