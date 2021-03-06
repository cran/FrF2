library(FrF2)
### test programs for FrF2
### FrF2 functionality
### combination of everything with everything is not included, 
###    but presumably also not needed
FrF2(8,4,randomize=FALSE)
set.seed(98776)
test <- FrF2(8,4,replications=2, repeat.only=FALSE)
set.seed(98776)
test <- FrF2(8,4,replications=2, repeat.only=TRUE)
run.order(test)
test
test <- FrF2(8,4,replications=2, repeat.only=TRUE, randomize=FALSE)
test <- FrF2(8,4,replications=2, repeat.only=FALSE)
run.order(test)
test

strOptions(drop.deparse.attr = FALSE)
str.wover <- function(obj){
    di <- attr(obj, "design.info") 
    if (!is.null(di$orig.design.info)) di$orig.design.info <- di$orig.design.info[-which(names(di$orig.design.info)=="FrF2.version")]
    attr(obj, "design.info") <- di[-which(names(di)=="FrF2.version")]
    str(obj)
}

str.wover(FrF2(16,6,randomize=FALSE, factor.names=list(eins=c(1,2),zwei=c(43,87),drei=c("alt","neu"),vier="",fuenf="",sechs="")))
str.wover(FrF2(16,randomize=FALSE, factor.names=list(eins=c(1,2),zwei=c(43,87),drei=c("alt","neu"),vier="",fuenf="",sechs="")))
str.wover(FrF2(16,design="6-2.2",randomize=FALSE))
str.wover(FrF2(16,generators=catlg[["6-2.2"]]$gen,randomize=FALSE,factor.names=list(eins=c(1,2),zwei=c(43,87),drei=c("alt","neu"),vier="",fuenf="",sechs="")))
## alias structure for three generators that differ only by sign
design.info(FrF2(16,generators=c(7,13,15),randomize=FALSE))$aliased
design.info(FrF2(16,generators=c(7,-13,15),randomize=FALSE))$aliased
design.info(FrF2(16,generators=c(-7,-13,-15),randomize=FALSE))$aliased

str.wover(FrF2(16,design="6-2.2",randomize=FALSE,factor.names=list(eins=c(1,2),zwei=c(43,87),drei=c("alt","neu"),vier="",fuenf="",sechs="")))
str.wover(FrF2(16,design="6-2.2",nfactors=6,randomize=FALSE))
str.wover(FrF2(16,design="6-2.2",randomize=FALSE,factor.names=list(eins=c(1,2),zwei=c(43,87),drei=c("alt","neu"),vier="",fuenf="",sechs="")))

## estimable in examples
## replications in examples
## randomize in examples
## MaxC2 in examples

## blocks
  ## automatic selection
  FrF2(32,14,blocks=8,randomize=FALSE,alias.block.2fis=TRUE)
  ## automatic selection without aliasing of 2fis with blocks
  FrF2(32,6,blocks=4,randomize=FALSE)
  ## automatic selection with factor names
  FrF2(32,6,blocks=4,randomize=FALSE, factor.names=Letters[20:25])
  ## manual selection
  ##    new with version 2: checks for forbidden behavior
  FrF2(32,6,blocks="A",randomize=FALSE)
  FrF2(32,6,blocks=c("ABE","ACD"),randomize=FALSE, alias.block.2fis=TRUE)
  FrF2(32,8,blocks=list(c(1,2,3),c(4,5)),randomize=FALSE, alias.block.2fis=TRUE)
  FrF2(32,8,blocks=list(c(1,2,3,4),c(7,8)),randomize=FALSE, alias.block.2fis=TRUE)
  FrF2(32,8,blocks=list(2,5,7),randomize=FALSE, alias.block.2fis=TRUE)
  ## mixed single and generated
  str.wover(FrF2(32,8,blocks=list(2,5,c(6,7)),randomize=FALSE, alias.block.2fis=TRUE))
  ## with factor names
  str.wover(FrF2(32,blocks=c("ABE","ACD"),
       factor.names=list(first=c("-","+"),second=c("old","new"),
                         third=c(1000,2000),fourth="",fifth="",sixth=""),
       randomize=FALSE, alias.block.2fis=TRUE))
  str.wover(dat <- FrF2(32,blocks=c(7,20,25),
       factor.names=list(first=c("-","+"),second=c("old","new"),
                         third=c(1000,2000),fourth="",fifth="",sixth=""),
       randomize=FALSE,alias.info=3, alias.block.2fis=TRUE))
  str.wover(FrF2(32,blocks=list(1,2),
       factor.names=list(first=c("-","+"),second=c("old","new"),
                         third=c(1000,2000),fourth="",fifth="",sixth="",seven="",eight="",nine=""),
       randomize=FALSE, alias.block.2fis=TRUE))
  ## using blockpick.big
  str.wover(FrF2(64,7,blocks=32,alias.block.2fis=TRUE,randomize=FALSE, block.old=TRUE))
  ## using godolphin
  str.wover(FrF2(64,7,blocks=32,alias.block.2fis=TRUE,randomize=FALSE))

  ## godolphin with estimable
  str.wover(FrF2(64,9,blocks=16,factor.names=Letters[15:23], 
       estimable=compromise(9,3)$requirement,
       alias.block.2fis=TRUE,randomize=FALSE))
  
  ## godolphin with estimable and full factorial
  str.wover(FrF2(64,6,blocks=16,factor.names=Letters[15:20], 
                 estimable=compromise(6,3)$requirement,
                 alias.block.2fis=TRUE,randomize=FALSE, firsthit=FALSE))
  
  ## godolphin with estimable and the need that ignore.dom takes the value TRUE
  ## (which it should automatically do due to its default)
  str.wover(FrF2(64, 9, blocks=16, estimable=compromise(9,1:2)$requirement, 
       alias.block.2fis = TRUE, select.catlg = catlg[-1378]))
  summary(FrF2(256, 13, blocks=64, alias.block.2fis=TRUE, 
               factor.names = list(C1="",C2="",C3="",C4="",C5="",C6="",C7="",C8="",
                                   C9="",C10="",C11="",C12="",
                                   N1=c("low","high")), 
               default.levels = c("current","new"),
               estimable=compromise(13, 1)$requirement))

## split plot
  ## automatic selection
  FrF2(32,14,WPs=8,nfac.WP=4,randomize=FALSE)
  FrF2(32,14,WPs=8,nfac.WP=2,randomize=FALSE)
  test <- FrF2(32,14,WPs=8,nfac.WP=6,randomize=FALSE)
  str.wover(test[32:1,])
  str.wover(test[14:1,])
  str.wover(test[,14:1])
  str.wover(test[14:1,,drop.attr=FALSE])

## replications in combination with blocks
## (with randomization in examples)
run.order(FrF2(8,3,blocks=2,wbreps=2,randomize=FALSE))
run.order(FrF2(8,3,blocks=2,wbreps=2,repeat.only=TRUE,randomize=FALSE))
run.order(FrF2(8,3,blocks=2,bbreps=2,randomize=FALSE))
run.order(FrF2(8,3,blocks=2,bbreps=2,wbreps=2,randomize=FALSE))
FrF2(8,3,blocks=2,bbreps=2,wbreps=2,repeat.only=TRUE,randomize=FALSE)
set.seed(68702)
run.order(FrF2(8,3,blocks=2,bbreps=2,wbreps=2,randomize=TRUE))
set.seed(68702)
run.order(FrF2(8,3,blocks=2,bbreps=2,wbreps=2,repeat.only=TRUE,randomize=TRUE))

## replications in combination with split plot 
  FrF2(32,14,WPs=8,nfac.WP=2,randomize=FALSE, replications=3)
  FrF2(32,14,WPs=8,nfac.WP=6,randomize=FALSE, replications=3)
  set.seed(123451)
  FrF2(32,14,WPs=8,nfac.WP=6,replications=3)

## generators in combination!!!

## blocks with more than 50 factors
str.wover(FrF2(128, 55, blocks=64, alias.block.2fis = TRUE))