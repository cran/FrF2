add.center <- function(design, ncenter, distribute=NULL, ...){
    if (!"design" %in% class(design)) stop("design must be of class design")
    di <- design.info(design)
    if (!(substr(di$type,1,4)=="FrF2" | substr(di$type,1,2)=="pb"))
       stop("center points only available for FrF2 and pb type designs")
    if (di$type=="FrF2.splitplot") 
       stop("currently, center points for splitplot designs are not supported")
    if (di$type=="FrF2.param") 
       stop("currently, center points for long version parameter designs are not supported")
    if (length(grep("center",di$type))>0)
       stop("design has center points already")
    if (!is.numeric(ncenter)) stop("ncenter must be a number")
    if (!length(ncenter)==1) stop("ncenter must be a number")
    if (!ncenter==floor(ncenter)) stop("ncenter must be an integer number")
    if (is.null(distribute)){
      if (!di$randomize) distribute <- 1
         else distribute <- min(ncenter, 3)}
    if (!is.numeric(distribute)) stop("distribute must be a number")
    if (!distribute==floor(distribute)) stop("distribute must be an integer number")
    if (distribute < 1 | distribute > min(di$nruns+1, ncenter))
       stop("distribute must be at least 1 and at most min(ncenter, nruns+1 of the design)")
    if (di$randomize & distribute==1) warning("running all center point runs together is usually not a good idea.")
    
    if (any(is.na(sapply(factor.names(design),"is.numeric"))))
       stop("Center points are implemented for experiments with all factors quantitative only.")
       

    design <- qua.design(design, quantitative="all")
    di <- design.info(design)
    clevels <- sapply(factor.names(design), "mean")
    di$ncube <- di$nruns
    di$ncenter <- ncenter
    if (!is.null(di$blocksize)) di$nruns <- di$nruns + di$nblocks*ncenter else
    di$nruns <- di$nruns+ncenter
    
    desnum <- desnum(design)
    ro <- run.order(design)
    if (!all(ro$run.no == sort(ro$run.no)) & (di$replications>0 | !is.null(di$blocksize))) 
        stop("center points cannot be added to designs with replications or blocks that are not in original order")
    hilf <- undesign(design)
    if (!is.null(di$blocksize)) if (di$bbreps==1) block.contrasts <- contrasts(hilf[,di$block.name])
        ## for bbreps>1, the block factor is currently not an R factor but a character vector only
    
    ## positions for adding center points (added after these positions)
    ## !!! blocks, replications and repeated measurements to be treated correctly
    if (!is.null(di$blocksize)){
         ## distribute within blocks
        if (distribute==1)
            addpos <- di$blocksize 
        else
            addpos <- c(0,round((1:(distribute-1))*di$blocksize/(distribute-1)))
    }
    else{
        if (distribute==1) 
             addpos <- di$ncube 
        else
          addpos <- c(0,round((1:(distribute-1))*di$ncube/(distribute-1)))
    }
    ## determine numbers of center points to be added at each position
        nrest <- ncenter%%distribute
        if (nrest==0) nadd <- rep(round(ncenter/distribute),distribute)
        if (nrest==1) nadd <- c(1,rep(0,distribute-1))+rep(floor(ncenter/distribute),distribute)
        if (nrest==2) nadd <- c(1,rep(0,distribute-2),1)+rep(floor(ncenter/distribute),distribute)
        if (nrest>2) nadd <- rep(floor(ncenter/distribute),distribute) + sample(c(rep(1,nrest),rep(0,distribute-nrest)))
        ## repeated measurements also for center points (assuming they are done for measurement accuracy)
        if (di$repeat.only) if (!is.null(di$blocksize)) nadd <- nadd*di$wbreps else nadd <- nadd*di$replications

    ## split vector for design data frame
        if (!is.null(di$blocksize)){
          if (di$wbreps==1 | di$repeat.only)
             together <- paste(rep(1:(di$nblocks*di$bbreps),each=di$blocksize*di$wbreps),
                          rep(cut(1:di$blocksize,unique(c(0,addpos))),each=di$wbreps,di$nblocks*di$bbreps),sep=".")
          else
             together <- paste(rep(1:(di$nblocks*di$bbreps*di$wbreps),each=di$blocksize),
                          rep(cut(1:di$blocksize,unique(c(0,addpos))),di$nblocks*di$bbreps*di$wbreps),sep=".")
             }
        else {if (di$replications==1 | (di$replications > 1 & di$repeat.only)) 
                together <- cut(rep(1:di$ncube,each=di$replications), unique(c(0,addpos)))
              else together <- paste(rep(1:di$replications,each=di$ncube),rep(cut(1:di$ncube,unique(c(0,addpos))),di$replications),sep=".")
                }
        ## the following command prevents split from reordering the data
        together <- factor(together, levels=unique(together))
   ## split the data frame
        getrennt <- split(hilf,together)
        ro$run.no.in.std.order <- as.character(ro$run.no.in.std.order)
        ro.getrennt <- split(ro,together)
        blockid <- rep("1", length(getrennt))

        von <- cumsum(c(1,lapply(getrennt, "nrow")))[-(length(getrennt)+1)]
        bis <- cumsum(lapply(getrennt, "nrow"))
        if (!is.null(di$blocksize)) blockid <- sapply(getrennt, function(obj) as.character(obj[1,di$block.name]))
        if (di$replications > 1 & is.null(di$blocksize) & !di$repeat.only) 
              blockid <- rep(1:di$replications, each=max(1,distribute-1))
        if (!is.null(di$blocksize)) if (di$wbreps > 1 & !di$repeat.only)
              blockid <- paste(blockid, rep(1:di$wbreps,each=max(1,distribute-1), times=di$nblocks*di$bbreps),sep=".")
    ## create center point data frames to be interleaved
    #    if (!is.null(di$nblocks)) no.center.groups <- distribute*di$nblocks*di$bbreps else 
    #        no.center.groups <- distribute*di$replications
        centers <- vector("list")
        ros <- vector("list")
    
        ## !!! ??? still have to provide row names for identifying runs
    
        ## centers and ros for WITHIN each block
        more <- setdiff(colnames(hilf), c(di$block.name,names(di$factor.names)))
            ## columns other than design factors, e.g. responses
        for (i in 1:distribute){
              cnext <- data.frame(matrix(clevels, ncol=di$nfactors, nrow=nadd[i], 
                                                 byrow=TRUE, dimnames=list(NULL, names(di$factor.names))))
              if (length(more)>0) cnext <- cbind(cnext, matrix(NA,nrow=nadd[i],ncol=length(more),dimnames=list(NULL,more)))
              centers <- c(centers, list(cnext))
              ros <- c(ros, list(data.frame(run.no.in.std.order=rep(0,nadd[i]),run.no=rep(0,nadd[i]),run.no.std.rp=as.character(rep(0,nadd[i])))))
              }

    ## interleave
         ## first center point entry of first (replication) block
          if (!is.null(di$blocksize)) 
              new <- cbind(matrix(hilf[1,di$block.name],nrow=nadd[1],ncol=1, dimnames=list(NULL, di$block.name)),centers[[1]])
          else new <- centers[[1]]
          ronew <- ros[[1]] 
          blockids <- unique(blockid)
          if (distribute==1){ 
            ## center points appended at end of each (replication) block
            if (length(blockids)==1){
               ## one (replication) block only
               new <- rbind(hilf, new)
               ronew <- rbind(ro, ronew)
               }
            else{
               ## more than one (replication) block
               ## as distribute equals 1, blockids == blockid
               new <- rbind(getrennt[[1]], new)
               ronew <- rbind(ro.getrennt[[1]], ronew)
               for (i in 2:length(blockids)){
                  if (!is.null(di$blocksize)) 
                  new <- rbind(new, getrennt[[i]], 
                      cbind(matrix(getrennt[[i]][1,di$block.name],nrow=nadd[1],ncol=1, dimnames=list(NULL, di$block.name)),centers[[1]]))
                  else new <- rbind(new, getrennt[[i]], centers[[1]])
                  ronew <- rbind(ronew, ro.getrennt[[i]], ros[[1]])
               }
              }
            }
            else{
              ### distribute > 1
              for (i in 1:length(blockids)){
              iblock <- which(blockid==blockids[i])
              if (i>1){ 
                  ## append first center points series of new block
                  ## for first block done already
                  if (!is.null(di$blocksize)) 
                  new <- rbind(new, 
                      cbind(matrix(getrennt[[iblock[1]]][1,di$block.name],nrow=nadd[1],ncol=1, dimnames=list(NULL, di$block.name)),centers[[1]]))
                  else new <- rbind(new, centers[[1]])
                  ronew <- rbind(ronew, ros[[1]])
                  }
              for (j in 1:length(iblock)){
                  ## append next data with subsequent center points for the current (replication) block 
                  if (!is.null(di$blocksize)) 
                  new <- rbind(new, getrennt[[iblock[j]]],
                      cbind(matrix(getrennt[[iblock[j]]][1,di$block.name],nrow=nadd[j+1],ncol=1, dimnames=list(NULL, di$block.name)),centers[[j+1]]))
                  else new <- rbind(new, getrennt[[iblock[j]]], centers[[j+1]])
                  ronew <- rbind(ronew, ro.getrennt[[iblock[j]]], ros[[j+1]])
                  }
              }
              }
        if (!is.null(di$blocksize)){ 
          if (exists("block.contrasts")) contrasts(new[,di$block.name]) <- block.contrasts
          desnum <- model.matrix(formula(paste("~",paste(c(di$block.name,names(di$factor.names)),collapse="+"))),data=new)[,-1]
        }
        else
        desnum <- model.matrix(formula(paste("~",paste(names(di$factor.names),collapse="+"))),data=new)[,-1]
        if (length(setdiff(colnames(new),c(di$block.name, names(di$factor.names))))>0){ 
               anhaeng <- as.matrix(new[,setdiff(colnames(new),c(di$block.name, names(di$factor.names))),drop=FALSE])
               storage.mode(anhaeng) <- "numeric"
               desnum <- cbind(desnum, anhaeng)
               }
     rownames(new) <- rownames(desnum) <- ronew$run.no <- 1:nrow(new)
     class(new) <- c("design","data.frame")
     desnum(new) <- desnum
     
     run.order(new) <- ronew
     di$type <- paste(di$type,"center",sep=".")
     design.info(new) <- di
     new
}