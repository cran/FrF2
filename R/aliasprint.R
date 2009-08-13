aliasprint <- function(design,...){
   ##function to print but not return a design's alias information
   if(!"design" %in% class(design)) stop("design must be of class design")
   print(design.info(design)$aliased)
}