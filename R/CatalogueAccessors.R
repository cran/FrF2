
"[.catlg" <- function(catlg,i){
       class(catlg) <- "list"
       aus <- catlg[i]
       if (!is.list(aus[1])) aus <- list(aus)
       class(aus) <- c("catlg","list")
       aus}
## same function for "[[.catlg" would destroy functionality of lapply

res.catlg <- function(catlg) sapply(catlg, function(obj) obj$res)
nclear.2fis.catlg <- function(catlg) sapply(catlg, function(obj) obj$nclear.2fis)
clear.2fis.catlg <- function(catlg) lapply(catlg, function(obj) obj$clear.2fis)
all.2fis.clear.catlg <- function(catlg) lapply(catlg, function(obj) obj$all.2fis.clear)
nfac.catlg <- function(catlg) sapply(catlg, function(obj) obj$nfac)
nruns.catlg <- function(catlg) sapply(catlg, function(obj) obj$nruns)
WLP.catlg <- function(catlg) lapply(catlg, function(obj) obj$WLP)

