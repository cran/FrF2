`remodel` <-
function(obj){   
     ## obj is a linear model object
     ## check whether as many coefficients as terms plus intercept
     ## i.e. two-level factors only
     if (length(coef(obj)) > length(attr(terms(obj),"term.labels")) + 
         sign(attr(terms(obj),"intercept"))) 
         stop("Only 2-level-designs are covered by this routine.")
     mod <- obj$model
     term.ord <- attr(terms(obj),"order")
     nmain <- length(which(term.ord==1))
     intcol <- attr(attr(mod,"terms"),"intercept")
     respnam <- colnames(mod)[attr(attr(mod,"terms"),"response")]
     labs <- lapply(vector("list",nmain),function(sp){c("-","+")})
     xmod <- mod[,rowSums(attr(attr(mod,"terms"),"factors"))>0]
       # numeric -1 1 columns instead of factors 
       # because otherwise prediction difficult for intermediate (0) level
     if (any(sapply(xmod,"is.factor"))) {
       hilf <- 0
       for (i in 1:length(xmod)) {
           hilf <- hilf+1
           if (is.factor(xmod[[i]])) { 
              labs[[hilf]] <- levels(xmod[[i]])
              xmod[[i]] <- 2*(as.numeric(xmod[[i]])-1.5)
           }
           }
       mod[,colnames(xmod)] <- xmod
       obj <- lm(terms(obj),data=data.frame(mod[,respnam],mod))
       }
       ## return list with linear model for further calculations
       ## and labels for annotating plots
       list(model=obj,labs=labs)
}

