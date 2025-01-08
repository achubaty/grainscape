## ----setup, include=FALSE-----------------------------------------------------
library(knitr)

opts_chunk$set(
  cache = TRUE,
  echo = TRUE,
  eval = TRUE,
  fig.align = "left",
  fig.height = 6,
  fig.width = 6,
  message = FALSE,
  out.extra = "",
  results = "hold",
  warning = FALSE
)

## ----call_graph_engine_initialize, echo = FALSE,  fig.cap = "Call diagram for Engine::initialize()"----
DiagrammeR::grViz("
  digraph initialize {
    graph [overlap = true, fontsize = 10]
    
    # nodes
    node [fillcolor = navy, fontcolor = white,
          fontname = Courier,
          shape = oval, style = filled]
    
    initialize
    
    node [fillcolor = grey50, fontcolor = white,
          fontname = Courier,
          shape = box, style = filled]
    
    cellIsZero; combinePatches; findPatches; getIndexFromList;
    outOfBounds; updateOutputMap; writeErrorMessage;
    
    # edges
    initialize->writeErrorMessage
    initialize->findPatches
    initialize->updateOutputMap
    initialize->cellIsZero
    cellIsZero->outOfBounds
    findPatches->outOfBounds
    findPatches->getIndexFromList
    findPatches->combinePatches
  }
")

## ----call_graph_engine_start, echo = FALSE, fig.cap = "Call diagram for Engine::start()"----
DiagrammeR::grViz("
  digraph start {
    graph [overlap = true, fontsize = 10]
    
    # nodes
    node [fillcolor = navy, fontcolor = white,
          fontname = Courier,
          shape = oval, style = filled]
    
    start
    
    node [fillcolor = grey50, fontcolor = white,
          fontname = Courier,
          shape = box, style = filled]
    
    activeCellSpreadChecker; calcDistance; cellsEqual; connectCell; createActiveCell;
    findPath; lookForIndirectPath;
    outOfBounds; parseMap; updateOutputMap; writeErrorMessage;
    
    # edges
    start->writeErrorMessage
    start->activeCellSpreadChecker
    start->createActiveCell
    start->updateOutputMap
    createActiveCell->outOfBounds
    createActiveCell->calcDistance
    createActiveCell->connectCell
    createActiveCell->findPath
    findPath->parseMap;
    findPath->lookForIndirectPath
    parseMap->cellsEqual
  }
")

