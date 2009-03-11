## Plackett Burman generating vectors and matrices, according to 1946 article
## are in pb.list (in workspace sysdata.rda)
## most functions in here are used for extending the pb designs with entries 0 in this list
## (double.des, Williamson etc.)

## function for doubling designs
## normalize.row.last is applied later to ensure that last row is "-1"
double.des <- function(des){
   zeil <- nrow(des)
   ### +/- column unconventionally in the end instead of middle position!!!
   rbind(cbind(des,des,rep(1,zeil)),cbind(des,-des,rep(-1,zeil)))
}

circ.mat <- function(gen){
   n <- length(gen)+1
   sel <- gen
   for (i in 1:(n-2))
    sel <- c(sel,gen[(n-i):(n-1)],gen[1:(n-i-1)])
    matrix(sel,n-1,n-1,byrow=1)
}

williamson <- function(A,B,C,D){rbind(
  cbind(A, B, C, D),
  cbind(-B, A, D, -C),
  cbind(-C, -D, A, B),
  cbind(-D, C, -B, A))
}

normalize.col.first <- function(mat){
    hilf <- mat[,1]==-1
    mat[hilf,] <- -mat[hilf,]
    mat[,-1]
}

normalize.row.last <- function(mat){
    hilf <- mat[nrow(mat),]==1
    mat[,hilf] <- -mat[,hilf]
    mat
}

gen.check <- function(k,gen){   if (!is.list(gen)) {
                 if (!(is.numeric(gen) | is.character(gen))) 
                      stop("gen must be a list of generator vectors, a vector of column numbers or a character vector of generators.")
                 if (is.character(gen)) gen <- lapply(strsplit(gen,""), function(obj) which(Letters %in% obj))
                 else {
                 if (any(!gen==floor(gen))) stop("All entries in gen must be integer numbers.")
                 if (any(2^(0:(k-1)) %in% gen)) stop("This design is of resolution II and is not permitted in package FrF2.")
                 if (min(gen)<1 | max(gen)>2^k-1) stop("Column numbers in gen must be in the range of 3 to 2^k-1.")
                 gen <- Yates[gen]}
              }
              if (any(sapply(gen,function(obj) any(obj<1 | obj>k | !floor(obj)==obj) )))
                   stop(paste("All generators must contain integer numbers from 1 to", k, 
                     "\n or letters from",Letters[1],"to", Letters[k], "only."))
              gen
              }


"[.catlg" <- function(catlg,i){
       class(catlg) <- "list"
       aus <- catlg[i]
       if (!is.list(aus[1])) aus <- list(aus)
       class(aus) <- c("catlg","list")
       aus}
## same function for "[[.catlg" would destroy functionality of lapply

res.catlg <- function(catlg) sapply(catlg, function(obj) obj$res)
nfree.2fis.catlg <- function(catlg) sapply(catlg, function(obj) obj$nfree.2fis)
free.2fis.catlg <- function(catlg) sapply(catlg, function(obj) obj$free.2fis)
all.2fis.free.catlg <- function(catlg) sapply(catlg, function(obj) obj$all.2fis.free)
nfac.catlg <- function(catlg) sapply(catlg, function(obj) obj$nfac)
nruns.catlg <- function(catlg) sapply(catlg, function(obj) obj$nruns)
WLP.catlg <- function(catlg) sapply(catlg, function(obj) obj$WLP)


des.recode <- function (var, recodes, as.factor.result) 
{
    recode.list <- rev(strsplit(recodes, ";")[[1]])
    is.fac <- is.factor(var)
    if (missing(as.factor.result)) 
        as.factor.result <- is.fac
    result <- var
    if (is.fac) 
        result <- as.character(result)

    for (term in recode.list) {
            set <- eval(parse(text = strsplit(term, "=")[[1]][1]))
            target <- eval(parse(text = strsplit(term, "=")[[1]][2]), envir=parent.frame(), enclos=sys.frame(0))
            for (val in set) {
                if (is.na(val)) 
                  result[is.na(var)] <- target
                else result[var == val] <- target
        }
    }
    if (is.fac) result <- as.factor(result)
    result
}
