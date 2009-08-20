find.gen <- function(design,...){
    if (!"design" %in% class(design))
        stop("find.gen works on class design objects only")
    di <- design.info(design)
    if ((length(grep("full factorial",di$type))>0 & all(di$nlevels==2)))
         return("full factorial")
    if (!(substr(di$type,1,4)=="FrF2"))
        stop("this is not a regular (fractional) factorial 2-level design")
  #  if (length(grep("blocked", di$type))>0 | length(grep("splitplot", di$type))>0)
  #      stop("find.gen does not work for blocked or splitplot designs")
    if (length(grep("splitplot", di$type))>0)
        stop("find.gen does not work for splitplot designs")
   gen <- NULL

   ## extract and remove old generator information
   ## also in form catlg.entry (or base.design)
   if (!is.null(di$catlg.entry)){
      gen <- di$catlg.entry[[1]]$gen
      if (is.null(di$base.design) & !is.null(di$catlg.entry)) di$base.design <- names(di$catlg.entry)
      di$catlg.entry <- NULL
   }
   else if (!is.null(di$generators))
        gen <- sapply(strsplit(di$generators,"="),
        function(obj) if (substr(obj[2],1,1)=="-") -1*which(names(Yates)==substring(obj[2],2))
                      else which(names(Yates) == obj[2]))
   if (is.null(gen) & !is.null(di$base.design)){ 
   if (substring(di$base.design,1,18)=="generator columns:")
      gen <- eval(parse(text=paste("c(",gsub("generator columns:", "",di$base.design),")")))
   else if (di$base.design %in% names(catlg)) gen <- catlg[di$base.design][[1]]$gen
   }
  ## now gen is a vector of signed columns, if it has been present before or NULL otherwise
  gen
}

generators.from.design.old <- function(design, ...){
## function to find generators from a design 
## that need not necessarily refer to the standard base factors
    if (!"design" %in% class(design))
        stop("generators.from.design works on class design objects only")
    di <- design.info(design)
    gen <- find.gen(design)
    
    ## relies on the fact that the first k factors are base factors
    ### must take care of map in estimable designs!!!
    ### try with FrF2(32,9, estimable=c("AC","BC","AB")) for reshuffled,
    ###          FrF2(32,9, estimable="CJ") for basic
    if (is.null(gen)){
           if (di$nfactors > 50) stop("generators.from.design does not work for more than 50 factors (and will presumably break down much earlier)")
           if (!is.null(di$ncube)) k <- round(log2(di$ncube)) else k <- round(log2(di$nruns))
           g <- di$nfactors - k
           if (is.null(di$ncube))
              al <- aliases(lm(formula(paste("I(1:",di$nruns,")~(.)^",k+1,sep="")), design[,names(di$factor.names)]))
           else al <- aliases(lm(formula(paste("I(1:",di$ncube,")~(.)^",k+1,sep="")), design[iscube(design),names(di$factor.names)]))
           sel <- al[[2]][1:di$nfactors]   ## aliases of base and generated factors
           let <- Letters[1:di$nfactors]
           if (length(grep("estimable",di$type))>0){
              let <- let[invperm(di$map[[1]])]  ## first k for base, next g for generated
              sel <- sel[invperm(di$map[[1]])][(k+1):di$nfactors]  ## generated
              }
           ## replace letters for base factors with blanks
           hilf <- lapply(sel, function(obj)
                    chartr(paste(c(let[1:k],":"),collapse=""),paste(rep(" ",k+1),collapse=""), obj))
           ## make blanks empty
           hilf <- lapply(hilf, function(obj) gsub(" ","",obj))
           hilf <- sapply(hilf, function(obj) which(obj==""))  ## the one that contains base factors only
           
           gen <- sapply(sel, function(obj) paste(obj[1],"~",sep="")) ## beginning of generator equation
           for (i in 1:length(gen)) gen[i] <- paste(gen[i],gsub(":","*",sel[[i]][hilf[i]]),sep="")
           
#           hilf <- sapply(sel, function(obj) obj[-grep(c(let[(k+1):di$nfactors],":"),obj)])
 #          gen <- sapply(hilf, function(obj) which(names(Yates)==chartr(let[1:k],Letters[1:k],obj)))
    }
    else {
           if (!is.null(di$ncube)) k <- round(log2(di$ncube)) else k <- round(log2(di$nruns))
           gen <- paste(Letters[(k+1):di$nfactors],"~",sapply(Yates[gen], function(obj) paste(Letters[obj],collapse="*")),sep="")
    }
    
  #
    gen <- lapply(gen, function(obj){ 
        for (i in 1:length(di$factor.names)) obj <- gsub("Letters[i]", names(di$factor.names)[i], obj)
        obj
        })
     for (i in 1:di$nfactors) 
        for (j in 1:length(gen))
           gen[[j]] <- gsub(Letters[i], paste("x",i,sep=""), gen[[j]])
     gen <- lapply(gen, function(obj) as.formula(obj))
      gen
}

generators.from.design <- function(design, ...){
## function to find generators from a design 
## that need not necessarily refer to the standard base factors
    if (!"design" %in% class(design))
        stop("generators.from.design works on class design objects only")
    di <- design.info(design)
    gen <- find.gen(design)
    
    ## relies on the fact that the first k factors are base factors
    ### must take care of map in estimable designs!!!
    ### try with FrF2(32,9, estimable=c("AC","BC","AB")) for reshuffled,
    ###          FrF2(32,9, estimable="CJ") for basic
    if (is.null(gen)){
           if (di$nfactors > 50) stop("generators.from.design does not work for more than 50 factors (and will presumably break down much earlier)")
           if (!is.null(di$ncube)) k <- round(log2(di$ncube)) else k <- round(log2(di$nruns))
           g <- di$nfactors - k
           if (is.null(di$ncube))
              al <- aliases(lm(formula(paste("I(1:",di$nruns,")~(.)^",k+1,sep="")), design[,names(di$factor.names)]))
           else al <- aliases(lm(formula(paste("I(1:",di$ncube,")~(.)^",k+1,sep="")), design[iscube(design),names(di$factor.names)]))
           sel <- al[[2]][1:di$nfactors]   ## aliases of base and generated factors
           let <- Letters[1:di$nfactors]
           sel <- lapply(sel, function(obj)
                    chartr(paste(c(let),collapse=""),paste(c(let[di$map[[1]]]),collapse=""), obj))

           ## replace letters for base factors with blanks
           hilf <- lapply(sel, function(obj)
                    chartr(paste(c(let[1:k],":"),collapse=""),paste(rep(" ",k+1),collapse=""), obj))
           ## make blanks empty
           hilf <- lapply(hilf, function(obj) gsub(" ","",obj))
           hilf <- sapply(hilf, function(obj) which(obj==""))  ## the one that contains base factors only
           
           gen <- sapply(sel, function(obj) paste(obj[1],"~",sep="")) ## beginning of generator equation
           for (i in 1:length(gen)) gen[i] <- paste(gen[i],gsub(":","*",sel[[i]][hilf[i]]),sep="")
           
#           hilf <- sapply(sel, function(obj) obj[-grep(c(let[(k+1):di$nfactors],":"),obj)])
 #          gen <- sapply(hilf, function(obj) which(names(Yates)==chartr(let[1:k],Letters[1:k],obj)))
    }
    else {
           if (!is.null(di$ncube)) k <- round(log2(di$ncube)) else k <- round(log2(di$nruns))
           gen <- paste(Letters[(k+1):di$nfactors],"~",sapply(Yates[gen], function(obj) paste(Letters[obj],collapse="*")),sep="")
    }
    
  #
    gen <- lapply(gen, function(obj){ 
        for (i in 1:length(di$factor.names)) obj <- gsub("Letters[i]", names(di$factor.names)[i], obj)
        obj
        })
     for (i in 1:di$nfactors) 
        for (j in 1:length(gen))
           gen[[j]] <- gsub(Letters[i], paste("x",i,sep=""), gen[[j]])
     gen <- lapply(gen, function(obj) as.formula(obj))
      gen
}