DanielPlot <- function(fit, ...){
    UseMethod("DanielPlot")
}
DanielPlot.design <- function(fit, ...){
    if (!"design" %in% class(fit)) 
        stop("DanielPlot.design works for obj from class design only.")
    di <- design.info(fit)
    if (is.null(di$response)) 
        stop("The design fit must have at least one response.")
    if (!(length(grep("FrF2",di$type))>0 | 
           length(grep("pb",di$type))>0)) 
        stop("The design fit must be of a type containing FrF2 or pb.")
    grad <- 1
    if (length(grep("pb",di$type)) > 0 & di$nfactors < di$nruns-1)
          warning("Effects plots for Plackett-Burman designs must be done with nruns-1 effects! The error effects are missing!")
    ## make sure there are as many effects as possible in the plots, redundant ones will not be shown
    if (length(grep("FrF2",di$type)) > 0 ) grad <- di$nfactors
    
    DanielPlot(lm(fit, degree=grad), ...)
}

DanielPlot.default <-
function (fit, code = FALSE, autolab = TRUE, alpha=0.05,
         faclab = NULL, 
         block = FALSE, datax = TRUE, 
         half = FALSE, pch = "*", 
         cex.fac = par("cex.lab"), cex.lab = par("cex.lab"), 
         cex.pch = par("cex"), cex.legend = par("cex.lab"), 
         main = NULL, ...) 
{
    ## transform into -1 and 1 coded model
    fit <- remodel(fit)$model
    ## check whether of appropriate type
    if (!check(fit))
     stop("This routine is applicable for 2-level factorial designs without partial aliasing only.")
   
    
    if (any(names(coef(fit)) == "(Intercept)")) {
        factor.effects <- 2 * coef(fit)[-1]
        }
    else {
        factor.effects <- 2 * coef(fit)
    }
    respnam <- colnames(fit$model)[attr(attr(fit$model,"terms"),"response")]
    names(factor.effects) <- attr(fit$terms, "term.labels")
    terms.ord <- attr(fit$terms, "order")[!is.na(factor.effects)] ##moved here
    factor.effects <- factor.effects[!is.na(factor.effects)]
    plotmain <- paste("Normal Plot for", respnam)
    if (autolab) {
        crit <- LenthPlot(factor.effects,alpha=alpha,plt=FALSE)["ME"]
        if (!code)
        faclab <- list(idx = which(crit<=abs(factor.effects)),
               lab = names(factor.effects)[which(crit<=abs(factor.effects))])
        plotmain <- paste(plotmain, ", ", "alpha=", alpha, sep="") 
        }
    if (half) {
        tn <- list(x = qnorm(0.5 * ((rank(abs(factor.effects)) - 
            0.5)/length(factor.effects) + 1)),y = abs(factor.effects))
        xlab <- "half-normal score"
        ylab <- "absolute effects"
        plotmain <- paste("Half", plotmain)
    }
    else {
        tn <- qqnorm(factor.effects, plot = FALSE)
        xlab <- "normal score"
        ylab <- "effects"
    }
    names(tn$x) <- names(factor.effects)  ## moved here
    names(tn$y) <- names(factor.effects)  ## added
    if (datax) {
        tmp <- tn$x
        tn$x <- tn$y
        tn$y <- tmp
        tmp <- xlab
        xlab <- ylab
        ylab <- tmp
    }
    labx <- names(factor.effects)
    laby <- 1:length(tn$y)
    points.labels <- names(factor.effects)
    if (is.null(main)) main <- plotmain
    plot.default(tn, xlim = c(min(tn$x), max(tn$x) + diff(range(tn$x))/5), 
        pch = pch, xlab = xlab, ylab = ylab, cex=cex.pch, cex.lab = cex.lab, 
        mgp=c(2,1,0), main = main, ...)
    if (is.null(faclab)) {
        if (!code) {
            effect.code <- labx
        }
        else {
            max.order <- max(terms.ord)
            no.factors <- length(terms.ord[terms.ord == 1])
            factor.label <- attr(fit$terms, "term.labels")[terms.ord == 1]
            faclet <- c(LETTERS[-9],letters[-9])
            factor.code <- faclet[1:no.factors]
            if (block) 
                factor.code <- c("BK", factor.code)
            texto <- paste(factor.code[1], "=", factor.label[1])
            for (i in 2:no.factors) {
                texto <- paste(texto, ", ", factor.code[i], "=", 
                  factor.label[i])
            }
            mtext(side = 1, line = 3.5, texto, cex = cex.legend)
            get.sep <- function(string, max.order) {
                k <- max.order - 1
                get.sep <- rep(0, k)
                j <- 1
                for (i in 2:(nchar(string)-1)) {
                  if (substring(string, i, i) == ":") {
                    get.sep[j] <- i
                    if (j == k) 
                      break
                    j <- j + 1
                  }
                }
                get.sep
            }
            labeling <- function(string, get.sep, max.order, 
                factor.code, factor.label) {
                labeling <- ""
                sep <- get.sep(string, max.order)
                sep <- sep[sep > 0]
                n <- length(sep) + 1
                if (n > 1) {
                  sep <- c(0, sep, nchar(string) + 1)
                  for (i in 1:n) {
                    labeling <- paste(labeling, sep = "", factor.code[factor.label == 
                      substring(string, sep[i] + 1, sep[i + 1] - 
                        1)][1])
                  }
                }
                else labeling <- paste(labeling, sep = "", factor.code[factor.label == 
                  string][1])
                labeling
            }
            effect.code <- rep("", length(terms.ord))
            for (i in 1:length(terms.ord)) {
                effect.code[i] <- labeling(names(tn$x)[i], get.sep, 
                  max.order, factor.code, factor.label)
            }
        }
        if (autolab){ 
               faclab <- list(idx = which(crit<=abs(factor.effects)),
                  lab = effect.code[which(crit<=abs(factor.effects))])
               if (length(faclab$idx) > 0)
               text(as.data.frame(tn)[faclab$idx,], paste(" ", faclab$lab), cex = cex.fac, adj = 0, 
                  xpd = NA)
           }
        else
        text(tn, paste("   ", effect.code), cex = cex.fac, adj = 0, 
            xpd = NA)
    }
    else {
        if (!is.list(faclab)) 
            stop("* Argument 'faclab' has to be NULL or a list with idx and lab objects")
        if (length(faclab$lab)>0) text(tn$x[faclab$idx], tn$y[faclab$idx], labels = faclab$lab, 
            cex = cex.fac, adj = 0)
    }
    invisible(cbind(as.data.frame(tn), no = 1:length(tn$x), effect=names(factor.effects)))
}

