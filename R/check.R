`check` <-
function(obj){
     check<-TRUE
     ## check whether completely aliased only
     ## works for numeric -1 and 1 coded data only
     mm <- model.matrix(remodel(obj)$model)
     if (any(colnames(mm)=="(Intercept)")) 
             mm <- mm[,-which(colnames(mm)=="(Intercept)")]
      ## is not necessary any more, as remodel is run before calling check
      ##     ranges <- sapply(mm, function(sp){
      ##          hilf <- range(sp)
      ##           (max(hilf)-min(hilf))/2
      ##         })
      ##     mm <- scale(mm, scale=ranges)
     if (!all(t(mm)%*%mm %in% c(0,nrow(mm),-nrow(mm)))) check <- FALSE 
     check
   }

