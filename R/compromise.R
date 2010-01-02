compromise <- function(nfactors, G1, class=3){
    if (!class %in% c(1,2,3,4)) stop("class must be an integer from 1 to 4")
    if (!is.numeric(G1)) stop("G1 must be numeric")
    if (length(G1)==0) stop("At least one effect must be in group 1")
    if (length(G1)==nfactors) stop("Use a resolution V design")
    if (length(G1)==1 & class==1) stop("no estimable effects required!")
    if (!is.numeric(nfactors)) stop("nfactors must be numeric")
    if (!all(G1%%1==0)) stop("G1 must consist of integer values")
    if (!all(nfactors%%1==0)) stop("nfactors must be integer")
    if (!length(unique(G1))==length(G1)) stop("non-unique values in G1")
    if (!nfactors>=max(G1)) stop("G1 must not contain numbers larger than nfactors")
    if (!min(G1)>=1) stop("G1 must not contain numbers smaller than 1")
    G2 <- setdiff(1:nfactors,G1)
    if (length(G2)==0) stop("At least one effect must be in group 2")
    
    perms <- combn(nfactors, length(G1))
    perms.full <- matrix(NA, ncol(perms), nfactors)
    for (i in 1:ncol(perms))
        perms.full[i,] <- c(perms[,i],setdiff(1:nfactors,perms[,i])) 
    perms.full <- perms.full[,invperm(c(G1,G2)),drop=FALSE]
    perms.full <- perms.full[ord(perms.full),,drop=FALSE]
    if (length(G1)>1)
    requirement <- apply(matrix(Letters[G1[combn(length(G1),2)]],nrow=2),2,"paste",collapse="")
    else requirement <- character(0)
    if (class==3)
    requirement <- c(requirement, outer(Letters[G1],Letters[G2],FUN=function(X,Y) paste(pmin(X,Y),pmax(X,Y),sep="")))
    if (class==2 & length(G2)>=2){
    requirement <- c(requirement, apply(matrix(Letters[G2[combn(length(G2),2)]],nrow=2),
                            2,"paste",collapse=""))
                            }
    if (class==4)
    requirement <- c(outer(Letters[G1],Letters[G2],FUN=function(X,Y) paste(pmin(X,Y),pmax(X,Y),sep="")))
    list(perms.full=perms.full, requirement=requirement, class=class)
}
