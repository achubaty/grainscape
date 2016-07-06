## gsGraphDataFrame
gsGraphDataFrame <- function(gsObj) {
  if (!(class(gsObj) %in% c("gsMPG", "gsGOC", "igraph"))) {
    stop("grainscape: gsObj must be a gsMPG, gsGOC or igraph object", call.=FALSE)
  }

  if (class(gsObj) == "gsMPG") {
    theseGraphs <- vector("list", 1)
    theseGraphs[[1]] <- gsObj$mpg
  }
  else if (class(gsObj) == "igraph") {
    theseGraphs <- vector("list", 1)
    theseGraphs[[1]] <- gsObj
  }
  else {
    theseGraphs <- lapply(gsObj$th, function(x) x$goc)
  }

  results <- vector("list", length(theseGraphs))

  for (i in 1:length(theseGraphs)) {
    thisGraph <- theseGraphs[[i]]

    if (is.igraph(thisGraph))  {
      results[[i]] <- list()
      results[[i]]$v <- data.frame(sapply(list.vertex.attributes(thisGraph), function(x) get.vertex.attribute(thisGraph, x)), stringsAsFactors=FALSE)
      results[[i]]$e <- data.frame(get.edgelist(thisGraph), sapply(list.edge.attributes(thisGraph), function(x) get.edge.attribute(thisGraph, x)), stringsAsFactors=FALSE)
      edgeDfNames <- names(results[[i]]$e)
      names(results[[i]]$e) <- c("e1", "e2", edgeDfNames[3:length(edgeDfNames)])

      ## Clean-up storage mode structure of data.frames
      results[[i]]$e <- as.data.frame(sapply(results[[i]]$e, as.character), stringsAsFactors=FALSE)
      results[[i]]$v <- as.data.frame(sapply(results[[i]]$v, as.character), stringsAsFactors=FALSE)
      results[[i]]$e <- as.data.frame(lapply(results[[i]]$e, function(x) type.convert(x, as.is=TRUE)), stringsAsFactors=FALSE)
      results[[i]]$v <- as.data.frame(lapply(results[[i]]$v, function(x) type.convert(x, as.is=TRUE)), stringsAsFactors=FALSE)

    }
    else {
      results[[i]]$v <- NA
      results[[i]]$e <- NA
    }
  }

  return(results)

}
