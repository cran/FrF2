`IAPlot` <-
function(obj, main=paste("Interaction plot matrix for",respnam), pch=c(15,17), 
    cex.lab=par("cex.lab"), cex=par("cex"), cex.xax=par("cex.axis"), 
    cex.yax=cex.xax, cex.title = 1.5, abbrev=4, show.alias=FALSE, ...){
    # obj     a linear model
    # pch     plot characters used
    # cex     plot character size
    # cex.lab size of variable labels in diagonal panels
    # cex.xax size of annotation for x-axis
    # cex.yax size of annotation for y-axis
    # abbrev  number of characters for factor levels in diagonal panel
    # show.alias  show number of effect in each panel, 
    #         in order to allow immediate judgment which effects are aliased
    
   ### base everything on model recoded to -1 and 1 numerics
   obj <- remodel(obj)
   ## extract labels for factor levels to desired length (abbrev)
   labs <- lapply(obj$labs,function(sp) substr(sp,1,abbrev))
   ## reduce obj list to its linear model component
   obj <- obj$model
   mod <- obj$model
   respnam <- colnames(mod)[attr(attr(mod,"terms"),"response")]
   ## for some reason, this does not work when using obj instead of mod
   
   if (!check(obj))
     stop("This routine is applicable for 2-level factorial designs without partial aliasing only.")
   
   ## prepare for simple access of important quantities
   term.ord <- attr(terms(obj),"order")
   nmain <- length(which(term.ord==1))
   nint <- length(which(term.ord==2))
   intcol <- attr(terms(obj),"intercept")
   mm<-model.matrix(obj)
   ## omit intercept, if present
   if (intcol > 0) mm <- mm[,-intcol]
   coefs <- coef(obj)
   if (intcol > 0) coefs <- coefs[-intcol] 

 
   terms1 <- colnames(mm)[which(term.ord==1)]
   terms2 <- colnames(mm)[which(term.ord==2)]
   ## used for easy prediction
   addnam <- setdiff(colnames(obj$model), terms1)
   names <- c(terms1, addnam)
   
   ## create matrix for plots
   predmat <- matrix(rep(0,4*nint),4,nint)
   varnums <- matrix(rep(0,2*nint),2,nint)
   colnames(predmat) <- terms2
   
   ## extract aliasing information
   ## for optional annotation of plots
   al <- aliases(obj)$aliases
   if (is.null(al)) exist <- names(coefs) 
   else exist <- sapply(al, function(sp) sp[1])
   alnum <- rep(0,length(coefs))
   names(alnum) <- names(coefs)
   
   ## calculate alias numbers for aliasing information
   ## and replace NA coefficients for aliased effects 
   ## with the appropriately signed replacement from "master"
   for (i in 1:length(coefs)){
      if (!is.na(coefs[i])) alnum[i] <- which(exist==names(coefs)[i])
      else{ 
          hilf <- grep(names(coefs)[i],al)
          coefs[i] <- coefs[exist[hilf]]
          alnum[i] <- hilf
          if (substr(al[[hilf]][grep(names(coefs[i]),al[[hilf]])],1,1)=="-")
             coefs[i] <- -coefs[i]
      }
   }
   ## re-include intercept coefficient
   if (intcol==1) coefs <- c(obj$coefficients[intcol], coefs)
   else if (intcol < length(coefs)) 
       coefs <- c(coefs[1:(intcol-1)],obj$coefficients[intcol])
   else coefs <- c(coefs, obj$coefficients[intcol])
   
   ## calculate model-based predicted values for all interactions
   for (i in 1:nint){
      dat <- matrix(rep(0,4*nmain),4,nmain)
      inam <- strsplit(terms2[i],":")[[1]]
      ml <- inam[1]
      mr <- inam[2]
      ## first in terms1
      m1 <- min(grep(ml,terms1,fixed=TRUE),grep(mr,terms1,fixed=TRUE))
      ## second in terms1
      m2 <- max(grep(ml,terms1,fixed=TRUE),grep(mr,terms1,fixed=TRUE))
      ## file in varnums matrix for later use
      varnums[,i] <- c(m1,m2)  
      dat[,m1] <- c(-1,1,-1,1)
      dat[,m2] <- c(-1,-1,1,1)
      colnames(dat) <- terms1
      ## allow for things like variables excluded by "-" etc.
      dat <- cbind(dat, matrix(rep(0,length(addnam)*4),4,length(addnam)))
      colnames(dat) <- names
      dat <- as.data.frame(dat)
      modmat <- model.matrix(lm(terms(obj),dat))
      predmat[,i] <- modmat %*% coefs
   }

   ## save old parameters for restoring them later
   omfrow <- par("mfrow")
   omar <- par("mar")
   ooma <- par("oma")
   ## determine axis limits and graphics parameters
   ax <- pretty(c(min(predmat),max(predmat)))
   par(mfcol=c(nmain,nmain), mar=c(1, 1, 1, 1) + 0.1, oma=c(3,5,4,0))
   # i is column index, j is row index
   for (i in 1:nmain){
     for (j in 1:nmain){
         ## interaction terms (regardless whether in model or not)
         ww <- c(paste(terms1[i],terms1[j],sep=":"), 
               paste(terms1[j],terms1[i],sep=":"))
         ## column of alnum corresponding to this ww 
         ## (if any, otherwise integer(0))
         hilf <- which(names(alnum) %in% ww)
         
         ## handle diagonal panels
          if (i==j){ plot(c(-1),min(ax)+5/6*(max(ax)-min(ax)),
                          ylim=c(min(ax),max(ax)),xlim=c(-1.1,1.1),
                          axes=FALSE,xlab="",ylab=respnam,
                          col="red", pch=pch[1], cex=cex)
                box(which="figure")
                points(c(-1),min(ax)+1/6*(max(ax)-min(ax)),pch=pch[2], cex=cex)
                text(c(-1,-1),min(ax)+c(5,1)*(max(ax)-min(ax))/6,
                      labs[[i]],pos=4, xpd=NA)
                text(0,(max(ax)+min(ax))/2, terms1[i],adj=0.5,xpd=TRUE, 
                    cex=cex.lab, col="blue")
                ## axes for corner panels
                if (i==1) axis(2, at = ax, labels = ax, cex.axis=cex.yax, 
                      outer=TRUE)
                if (j==nmain) axis(1, at = c(-1,1), labels = labs[[j]], 
                    cex.axis=cex.xax, outer=TRUE)
                }
          if (i<j) {
              sp <- intfind(i,j,varnums)
              if (is.null(sp)) {
                    plot(c(-10,10),c(-1,1), axes=FALSE, xlab="",
                       xlim=c(-1.1,1.1),ylim=c(min(ax),max(ax)))
                       ## skip panel
                    box(which="figure")
                    }
              else {plot(c(-1,1),predmat[c(1,2),sp],ylab=respnam,xlab=terms1[j],
                     type="b", xlim=c(-1.1,1.1),ylim=c(min(ax),max(ax)), 
                     axes=FALSE, col="red",
                     lty=3, pch=pch[1], cex=cex)
                    box(which="figure")
                    lines(c(-1,1),predmat[c(3,4),sp])
                    points(c(-1,1),predmat[c(3,4),sp],pch=pch[2], cex=cex)
                    }       
              ## axes for border panels
                if (i==1) axis(2, at = ax, labels = ax, cex.axis=cex.yax, outer=TRUE)
                if (j==nmain) axis(1, at = c(-1,1), labels = labs[[i]], 
                    cex.axis=cex.xax, outer=TRUE)
          }
          if (i>j) {
               sp <- intfind(j,i,varnums)
              if (is.null(sp)) {
                    plot(c(-10,10),c(-1,1), axes=FALSE, xlab="",
                       xlim=c(-1.1,1.1),ylim=c(min(ax),max(ax)))## skip panel
                    box(which="figure")
                    }
              else {plot(c(-1,1),predmat[c(1,3),sp],ylab=respnam,xlab=terms1[j],
                       type="b", xlim=c(-1.1,1.1),ylim=c(min(ax),max(ax)), axes=FALSE, 
                       col="red", lty=3, pch=pch[1], cex=cex)
                    box(which="figure")
                    lines(c(-1,1),predmat[c(2,4),sp])
                    points(c(-1,1),predmat[c(2,4),sp],pch=pch[2], cex=cex)}
         }
         if (show.alias) numact <- if (length(hilf)>0)
             text(0,max(ax),alnum[hilf],xpd=TRUE)
   }
   }
   title(main, line=1.5, outer=TRUE, cex.main=cex.title*par("cex.main"))
   par(mfrow=omfrow,mar=omar,oma=ooma)
}

