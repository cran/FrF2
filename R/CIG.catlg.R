CIG.catlg <- function(design, select.catlg=catlg, static=FALSE, layout=layout.circle, label="num", ...){
    ## create graph picture for design entry
    if (!"catlg" %in% class(select.catlg)) stop("select.catlg must be a catalogue")
    if (!(is.character(design) & length(design)==1))
       stop("design must be a design name")
    if (!(design %in% names(select.catlg)))
       stop("design must be a design name that occurs in select.catlg")
    design <- select.catlg[[design]]
    if (!exists("vertex.label")){
        vertex.label <- 1:design$nfac
        if (!label=="num") vertex.label <- Letters[vertex.label]
     }
    go2 <- graph.empty(n=design$nfac,directed=FALSE)
    if (!length(design$clear.2fis)==0)
        go2 <- add.edges(go2,design$clear.2fis-1)
    if (design$res<4) warning("the design is of resolution less than IV")
    if (!static){
    id <- tkplot(go2, vertex.label=vertex.label, ...)
    invisible(list(graph=go2, coords=tkplot.getcoords(id)))
    }
    else {
    invisible(list(graph=go2))
    plot(go2, layout=layout, vertex.label=vertex.label, ...)
    }
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