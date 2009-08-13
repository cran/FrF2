print.aliases <- function(x, ...){
 ### probably no longer necessary
   if (!"aliases" %in% class(x)) stop("applicable for class aliases only")
   als <- which(sapply(x$aliases,"length")>1)
   if (length(als)>0){
      alprint <- list(legend=x$legend, aliases=x$aliases[als])
      alprint$aliases <- t(t(sapply(alprint$aliases, "paste", collapse=" = ")))
      colnames(alprint$aliases) <- ""
      rownames(alprint$aliases) <- rep("",nrow(alprint$aliases))
      if(!is.null(alprint$legend)) names(alprint$legend)<-rep("", length(alprint$legend))
      }
   else alprint <- "no aliasing in the model"
   if (is.null(alprint$legend)) print(alprint$aliases) else
   print(alprint)
}