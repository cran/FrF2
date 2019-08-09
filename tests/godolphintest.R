require(FrF2)
### tests for Godolphin methodology

### colpick
## colpick for full factorial
colpick(6, 2)   ## default
## character estimability requirement
colpick(6, 3, estimable=compromise(6, 4)$requirement)
## matrix estimability requirement
colpick(6, 3, estimable=rbind(4, c(1:3,5:6)))
## character design specification
colpick("6-1.1", 3, estimable=compromise(6, 4)$requirement)
## catlg design specification
colpick(catlg["6-1.1"], 3, estimable=compromise(6, 4)$requirement)
## impossible request
colpick(catlg["8-2.1"], 1)

## phimax
phimax(8, 2)            ## maximum possible
phimax(8, 2, c(4,3,1))  ## fewer
phimax(9, 4)            ## all

## blockgencreate
blockgencreate(rbind(c(1,1,1,0,0,0,1),
                     c(1,1,0,1,1,1,0)), 2)

## Xcalc
## gen character
Xcalc(rbind(c(1,1,0,0),c(0,1,1,1)), 
       gen=c("AB","BCD"))
## gen list
Xcalc(rbind(c(1,1,0,0),c(0,1,1,1)), 
             gen=list(c(1,2),2:4))
## gen Yates columns
Xcalc(rbind(c(1,1,0,0),c(0,1,1,1)), 
             gen=c(3,14))
## gen catlg
Xcalc(rbind(c(1,1,0,0),c(0,1,1,1)), 
             gen=catlg[nruns(catlg)==16 & 
                         res(catlg)==4])

