CIG <- function(design, select.catlg=catlg, static=FALSE, layout=layout.auto, label="num", plot=TRUE, ...){
    ## function gained argument plot 10 Jul 2012
    
    ## create graph picture for design entry
    if ("catlg" %in% class(design)){
       if (length(design) > 1) stop("design must not contain more than one catlg entry")
       design <- design[[1]]
    }
    else{
    if (!"catlg" %in% class(select.catlg)) stop("select.catlg must be a catalogue")
    if (!(is.character(design) & length(design)==1))
       stop("design must be a design name")
    if (!(design %in% names(select.catlg)))
       stop("design must be a design name that occurs in select.catlg")
    design <- select.catlg[[design]]
    }
    if (!exists("vertex.label")){
        vertex.label <- 1:design$nfac
        if (!label=="num") vertex.label <- Letters[vertex.label]
     }
    go2 <- graph.empty(n=design$nfac,directed=FALSE)
    ## previous version subtracted 1 from design$clear.2fis for previous igraph node definition; changed 29/06/2012
    if (!length(design$clear.2fis)==0)
        go2 <- add.edges(go2,design$clear.2fis)
    if (design$res<4) warning("the design is of resolution less than IV")
    if (plot){
    if (!static){
    id <- tkplot(go2, vertex.label=vertex.label, ...)
    invisible(list(graph=go2, coords=tkplot.getcoords(id)))
    }
    else {
    invisible(go2)
    plot(go2, layout=layout, vertex.label=vertex.label, ...)
    }
    }
    else return(go2)
}

CIGstatic <- function(graph, id, label="num", xlim = c(-1,1), ylim=c(1, -1), ...){
    ## get coordinates for static graph from dynamic one
    if ("list" %in% class(graph)) if (names(graph)[1]=="graph") graph <- graph$graph
    if (!exists("vertex.label")){
        vertex.label <- 1:graph[[1]]
        if (!label=="num") vertex.label <- Letters[vertex.label]
     }
    coords <- tkplot.getcoords(id)
    plot(graph, layout=coords, vertex.label=vertex.label, xlim=xlim, ylim=ylim, ...)
}