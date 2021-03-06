block.check <- function (k, blocks, nfactors, factor.names=Letters[1:nfactors]) 
{
    ## generators based on factors
    ## or list of single factor numbers
    if (is.list(blocks)){
      if (any(sapply(blocks, function(obj) any(obj < 1 | obj > nfactors | 
        !floor(obj) == obj)))) 
        stop(paste("All block generators must contain integer numbers from 1 to", 
            nfactors, "\n or letters from", Letters[1], "to", Letters[nfactors], 
            "only."))}
    else {
        ## number of blocks
        if (is.numeric(blocks) & length(blocks)==1) {
            if (!(2^round(log2(blocks)))==blocks) 
              stop ("The number of blocks must be an integer power of 2.")
            return(blocks)
        }
        ## invalid blocks
        if (!(is.numeric(blocks) | is.character(blocks))) 
            stop("blocks must be the number of blocks, a list of generator vectors, a character vector of block generators, 
                   a numeric vector of column numbers of the Yates matrix, or 
                   a character vector of factor names.")
        ## Yates column numbers
        if (is.numeric(blocks)) {
            if (any(!blocks == floor(blocks))) 
                stop("All entries in blocks must be integer numbers.")
            if (min(blocks) < 1 | max(blocks) > 2^k - 1) 
                stop("Column numbers in blocks must be in the range of 1 to 2^k-1.")
            blocks <- Yates[blocks]
            ## now a list
        }
        ## factor names or character block generators (ABC etc.)
        if (is.character(blocks)) {
            ## factor names are made into numeric vector
            ## block generators are made into list
            hilf <- factor.names
            if (is.list(hilf)) hilf <- names(hilf)
            if (all(blocks %in% hilf)) blocks <- as.list(which(hilf %in% blocks))
            else
            blocks <- lapply(strsplit(blocks, ""), function(obj) which(Letters %in% 
                obj))
            }
    }
    ## now blocks is a list (a number of blocks was returned above)
    ## must be checked for singularity outside of block.check
    blocks
}

blockfull <- function(block.gen, k, des.gen=NULL){
  ## function for obtaining the generating columns for 
  ## all block contrasts
  ## block.gen is a list of vectors of design column numbers
  ##    or a numeric vector of Yates column numbers
  ##    or a vector of character strings like "ABD", "CF" etc
  ## des.gen is a vector of Yates column numbers, 
  ##    including those for the base factors
  if (is.list(block.gen) && is.null(des.gen)) 
    stop("for list-valued block.gen, ", 
         "des.gen is needed (vector of Yates column numbers)")
  k.block <- length(block.gen)
  if (is.character(block.gen)) 
    block.gen <- sapply(block.gen, function(obj) which(names(Yates) == obj))
  if (is.list(block.gen))
    block.gen <- sapply(block.gen, function(obj) 
      as.intBase(paste(rowSums(do.call(cbind,
             lapply(obj, function(obj2) 
             digitsBase(des.gen[obj2],2,k))))%%2, collapse="")))

  ## now, blockgen is a vector of Yates column numbers
  aus <- block.gen
  if (k.block > 1)
    for (i in 2:k.block){
    sel <- DoE.base:::nchoosek(k.block, i)
    aus <- c(aus, 
             sapply(1:ncol(sel), 
                    function(obj) 
                      as.intBase(  ## convert to Yates column number
                        paste(     ## convert to character string
                          rowSums( ## obtain interaction pattern of combination
                            do.call(cbind,
                                    lapply(sel[,obj], function(obj2) 
                                      digitsBase(block.gen[obj2],2,k)  
                                      ## obtain binary for Yates column number
                                    ))
                          )%%2, 
                          collapse="")
                      )
             ))
  }
  aus
}
