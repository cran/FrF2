BsProb.design <- function(design, mFac = NULL, response=NULL, select=NULL, mInt = 2, p = 0.25, g = 2,
    ng = 1, nMod = 10){
 ## adapted from BsMD
    if (!"design" %in% class(design)) stop("This function is for class design objects only.")
    di <- design.info(design)
    if (!(length(grep("FrF2",di$type))>0 |
           length(grep("pb",di$type))>0)) {
           if (!(di$type=="full factorial" & all(di$nlevels==2)))
        stop("The design obj must be of a type containing FrF2 or pb.")
        }
   if (is.null(di$response.names)) stop("The design must have at least one response.")
   if (is.null(mFac)) mFac <- di$nfactors   ## modified later, if select is used
    
   if (!is.null(select)){
       ## take care of selecting only part of the interactions
       if (!is.numeric(select)) stop("select must be numeric")
       if (!all(floor(select)==select)) stop("select must contain integer numbers")
       if (any(select<1 | select>di$nfactors)) stop("select must contain numbers betweeen 1 and ", di$nfactors, " only")
       select <- unique(select)
       if (length(select)<2) stop("at least 2 factors must be selected")
       faclab <- names(di$factor.names)[select]
       if (mFac>length(select)) mFac <- length(select)
       }
    else faclab <- names(di$factor.names)
    
    X <- desnum(design)[,faclab]
    if (!is.null(response)){
      if (!response %in% di$response.names)
        stop("Requested response is not a response variable in design.")
        }
    else response <- di$response.names[1] ## use first response variable per default
    y <- unlist(design[,response])
    ## for the original BsMD version, where blocks might be included
    blk <- 0
    
    rownames(X) <- rownames(X, do.NULL = FALSE, prefix = "r")
    storage.mode(X) <- "double"
    Y <- as.double(y)
    N <- as.integer(nrow(X))
    COLS <- as.integer(ncol(X) - blk)
    BLKS <- as.integer(blk)
    MXFAC <- as.integer(max(1, mFac))
    MXINT <- as.integer(mInt)
    PI <- as.double(p)
    if (length(g) == 1) {
        INDGAM <- as.integer(0)
        GAMMA <- as.double(g)
        NGAM <- as.integer(1)
        INDG2 <- as.integer(0)
        GAM2 <- as.double(0)
    }
    else {
        if (ng == 1) {
            INDGAM <- as.integer(0)
            GAMMA <- as.double(c(g[1], g[2]))
            NGAM <- as.integer(1)
            INDG2 <- as.integer(1)
            GAM2 <- as.double(g[2])
        }
        else {
            INDGAM <- as.integer(1)
            GAMMA <- as.double(seq(min(g), max(g), length = ng))
            NGAM <- as.integer(ng)
            INDG2 <- as.integer(0)
            GAM2 <- as.double(0)
        }
    }
    NTOP <- as.integer(nMod)
    mdcnt <- as.integer(0)
    ptop <- as.double(rep(0, NTOP))
    sigtop <- as.double(rep(0, NTOP))
    nftop <- as.integer(rep(0, NTOP))
    jtop <- matrix(0, nrow = NTOP, ncol = MXFAC)
    dimnames(jtop) <- list(paste("M", seq(NTOP), sep = ""), paste("x",
        seq(MXFAC), sep = ""))
    storage.mode(jtop) <- "integer"
    del <- as.double(0)
    sprob <- as.double(rep(0, (COLS + 1)))
    names(sprob) <- c("none", faclab)
    pgam <- as.double(rep(0, NGAM))
    prob <- matrix(0, nrow = (1 + COLS), ncol = NGAM)
    dimnames(prob) <- list(c("none", paste("x", 1:COLS, sep = "")),
        seq(NGAM))
    storage.mode(prob) <- "double"
    ind <- as.integer(-1)
    lst <- .Fortran("bm", X, Y, N, COLS, BLKS, MXFAC, MXINT,
        PI, INDGAM, INDG2, GAM2, NGAM, GAMMA, NTOP, mdcnt, ptop,
        sigtop, nftop, jtop, del, sprob, pgam, prob, ind, PACKAGE = "BsMD")
    names(lst) <- c("X", "Y", "N", "COLS", "BLKS", "MXFAC", "MXINT",
        "PI", "INDGAM", "INDG2", "GAM2", "NGAM", "GAMMA", "NTOP",
        "mdcnt", "ptop", "sigtop", "nftop", "jtop", "del", "sprob",
        "pgam", "prob", "ind")
    invisible(structure(lst, class = c("BsProb", class(lst))))
}