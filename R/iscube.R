iscube <- function(design, ...){
    if (!"design" %in% class(design)) stop("this function is applicable for class design objects only")
    di <- design.info(design)
    if (!(substr(di$type,1,4)=="FrF2" | substr(di$type,1,2)=="pb" | (substr(di$type,1,14)=="full factorial" & all(di$nlevels==2))))
       stop("iscube only available for FrF2 and pb type designs")
    ## determine center point positions
    aus <- run.order(design)$run.no.in.std.order=="0"
    if (!sum(aus)==di$ncenter*di$replications){
       wrong <- TRUE
       if (!is.null(di$blocksize))
           if (sum(aus)==di$ncenter*di$bbreps*di$wbreps*di$nblocks) wrong <- FALSE
       if (wrong) stop("There is something wrong with the number of center points for this design.")
    }
    ## output cube point positions
    !aus
}