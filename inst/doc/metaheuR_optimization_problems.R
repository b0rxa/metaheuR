## ----message=FALSE-------------------------------------------------------
library("metaheuR")
n <- 50
rnd.matrix <- matrix(runif(n^2), ncol=n)
rnd.tsp <- tspProblem(cmatrix=rnd.matrix)
names(rnd.tsp)

## ------------------------------------------------------------------------
rnd.solution <- randomPermutation(length = n)
rnd.tsp$evaluate(rnd.solution)

## ------------------------------------------------------------------------

cost.matrix <- tsplibParser(file="http://www.iwr.uni-heidelberg.de/groups/comopt/software/TSPLIB95/XML-TSPLIB/instances/brazil58.xml.zip")
tsp.brazil <- tspProblem(cost.matrix)

## ------------------------------------------------------------------------
n <- dim(cost.matrix)[1]
csol <- identityPermutation(n)
tsp.brazil$evaluate(csol)

## ------------------------------------------------------------------------
n <- 10
weight <- runif(n)
value <- weight + 0.2 * runif(n)
capacity <- sum(sample(weight, size=0.25 * n, replace=FALSE))
ksp <- knapsackProblem(weight=weight, value=value, limit=capacity)
names(ksp)

## ------------------------------------------------------------------------
all.in.solution <- rep(TRUE, n)
ksp$valid(all.in.solution)
corrected.solution <- ksp$correct(all.in.solution)
ksp$valid(corrected.solution)

## ------------------------------------------------------------------------
set.seed(1)
library(igraph)
n <- 100
rnd.graph <- random.graph.game (n=n, p.or.m=0.05)
misp <- misProblem(graph=rnd.graph, penalization=0)
misp.penalized <- misProblem(graph=rnd.graph, penalization=0.5)

## ------------------------------------------------------------------------
rnd.solution <- runif(n) > 0.4
misp$valid(rnd.solution)
misp$evaluate(rnd.solution)
misp.penalized$evaluate(rnd.solution)

## ------------------------------------------------------------------------
misp$evaluate

## ------------------------------------------------------------------------
valid.solution <- misp$correct(rnd.solution)
misp$evaluate(valid.solution)
misp.penalized$evaluate(valid.solution)

## ------------------------------------------------------------------------
misp$plot(rnd.solution, node.size=5)
misp$plot(valid.solution)

## ------------------------------------------------------------------------
myProblem <- function(arg1, arg2, arg3) {
  
  evaluate <- function(solution) {
    return (-1)
  }
  
  valid <- function(solution) {
    return(TRUE)
  }
  
  correct <- function(solution) {
    return(solution)
  }
  
  return(list(evaluate=evaluate, valid=valid, correct=correct))
}

## ----problem_example, echo=FALSE-----------------------------------------
maximumCliqueProblem <- function(graph, penalization=0) {
  
  if (is.directed(graph)) {
      stop ("Only undirected graphs can be used for this problem")
  }
  graph <- simplify(graph)
  
  check <- function(solution) {
    if (class(solution)!="logical") {
      stop ("The solutions for the Maximum Clique Problem have to be logical vectors")
    }
    if (length(solution)!=vcount(graph)) {
      stop ("The solution has to have as many positions as nodes in the graph (", length(V(graph)), ")")
    }
  }
  
  evaluate <- function(solution) {
    check(solution)
    return(-1 * sum(solution))
  }
  
  valid <- function(solution) {
    check(solution)
    # Valid iff the induced subgraphs is complete
    sg <- induced.subgraph(graph, V(graph)[solution])
    num.nodes <- vcount(sg)
    num.edges <- ecount(sg)
    return(num.edges == num.nodes*(num.nodes-1)/2)
  }
  
  correct <- function(solution) {
    check(solution)
    while(!valid(solution)) {
      # Take the node in the current solution that has the least links and remove it from the solution
      sg <- induced.subgraph(graph, V(graph)[solution])
      degrees <- degree(sg, V(sg))
      id <- which.min(degrees)
      solution[which(solution)[id]] <- FALSE
      sum(solution)
    }
    return(solution)
  }
  
  plotSolution <- function(solution,  node.size=5, ...) {
    nodes <- V(graph)[solution]
    edgeInClique <- function (i) {
      return(all(get.edges(graph, E(graph)[i]) %in% nodes))
    }
    clique.edges <- sapply(1:ecount(graph), FUN=edgeInClique)
    
    width <- rep(1, ecount(graph))
    width[clique.edges] <- 3
    E(graph)$width <- width
    
    col.edge <- rep("lightgray", ecount(graph))
    col.edge[clique.edges] <- "black"
    E(graph)$color <- col.edge
    
    col.vertex <- rep("white", length(solution))
    col.vertex[solution] <- "black"
    V(graph)$color <- col.vertex
    plot.igraph(graph, vertex.size=node.size, 
                vertex.label=NA, edge.arrow.mode="-")
  }
  
  return(list(evaluate=evaluate, valid=valid, correct=correct, plotSolution=plotSolution))
}

## ----problem_example, echo=1, eval=FALSE---------------------------------
#  maximumCliqueProblem <- function(graph, penalization=0) {
#  
#    if (is.directed(graph)) {
#        stop ("Only undirected graphs can be used for this problem")
#    }
#    graph <- simplify(graph)
#  
#    check <- function(solution) {
#      if (class(solution)!="logical") {
#        stop ("The solutions for the Maximum Clique Problem have to be logical vectors")
#      }
#      if (length(solution)!=vcount(graph)) {
#        stop ("The solution has to have as many positions as nodes in the graph (", length(V(graph)), ")")
#      }
#    }
#  
#    evaluate <- function(solution) {
#      check(solution)
#      return(-1 * sum(solution))
#    }
#  
#    valid <- function(solution) {
#      check(solution)
#      # Valid iff the induced subgraphs is complete
#      sg <- induced.subgraph(graph, V(graph)[solution])
#      num.nodes <- vcount(sg)
#      num.edges <- ecount(sg)
#      return(num.edges == num.nodes*(num.nodes-1)/2)
#    }
#  
#    correct <- function(solution) {
#      check(solution)
#      while(!valid(solution)) {
#        # Take the node in the current solution that has the least links and remove it from the solution
#        sg <- induced.subgraph(graph, V(graph)[solution])
#        degrees <- degree(sg, V(sg))
#        id <- which.min(degrees)
#        solution[which(solution)[id]] <- FALSE
#        sum(solution)
#      }
#      return(solution)
#    }
#  
#    plotSolution <- function(solution,  node.size=5, ...) {
#      nodes <- V(graph)[solution]
#      edgeInClique <- function (i) {
#        return(all(get.edges(graph, E(graph)[i]) %in% nodes))
#      }
#      clique.edges <- sapply(1:ecount(graph), FUN=edgeInClique)
#  
#      width <- rep(1, ecount(graph))
#      width[clique.edges] <- 3
#      E(graph)$width <- width
#  
#      col.edge <- rep("lightgray", ecount(graph))
#      col.edge[clique.edges] <- "black"
#      E(graph)$color <- col.edge
#  
#      col.vertex <- rep("white", length(solution))
#      col.vertex[solution] <- "black"
#      V(graph)$color <- col.vertex
#      plot.igraph(graph, vertex.size=node.size,
#                  vertex.label=NA, edge.arrow.mode="-")
#    }
#  
#    return(list(evaluate=evaluate, valid=valid, correct=correct, plotSolution=plotSolution))
#  }

## ----problem_example, echo=3:6, eval=FALSE-------------------------------
#  maximumCliqueProblem <- function(graph, penalization=0) {
#  
#    if (is.directed(graph)) {
#        stop ("Only undirected graphs can be used for this problem")
#    }
#    graph <- simplify(graph)
#  
#    check <- function(solution) {
#      if (class(solution)!="logical") {
#        stop ("The solutions for the Maximum Clique Problem have to be logical vectors")
#      }
#      if (length(solution)!=vcount(graph)) {
#        stop ("The solution has to have as many positions as nodes in the graph (", length(V(graph)), ")")
#      }
#    }
#  
#    evaluate <- function(solution) {
#      check(solution)
#      return(-1 * sum(solution))
#    }
#  
#    valid <- function(solution) {
#      check(solution)
#      # Valid iff the induced subgraphs is complete
#      sg <- induced.subgraph(graph, V(graph)[solution])
#      num.nodes <- vcount(sg)
#      num.edges <- ecount(sg)
#      return(num.edges == num.nodes*(num.nodes-1)/2)
#    }
#  
#    correct <- function(solution) {
#      check(solution)
#      while(!valid(solution)) {
#        # Take the node in the current solution that has the least links and remove it from the solution
#        sg <- induced.subgraph(graph, V(graph)[solution])
#        degrees <- degree(sg, V(sg))
#        id <- which.min(degrees)
#        solution[which(solution)[id]] <- FALSE
#        sum(solution)
#      }
#      return(solution)
#    }
#  
#    plotSolution <- function(solution,  node.size=5, ...) {
#      nodes <- V(graph)[solution]
#      edgeInClique <- function (i) {
#        return(all(get.edges(graph, E(graph)[i]) %in% nodes))
#      }
#      clique.edges <- sapply(1:ecount(graph), FUN=edgeInClique)
#  
#      width <- rep(1, ecount(graph))
#      width[clique.edges] <- 3
#      E(graph)$width <- width
#  
#      col.edge <- rep("lightgray", ecount(graph))
#      col.edge[clique.edges] <- "black"
#      E(graph)$color <- col.edge
#  
#      col.vertex <- rep("white", length(solution))
#      col.vertex[solution] <- "black"
#      V(graph)$color <- col.vertex
#      plot.igraph(graph, vertex.size=node.size,
#                  vertex.label=NA, edge.arrow.mode="-")
#    }
#  
#    return(list(evaluate=evaluate, valid=valid, correct=correct, plotSolution=plotSolution))
#  }

## ----problem_example, echo=7:15, eval=FALSE------------------------------
#  maximumCliqueProblem <- function(graph, penalization=0) {
#  
#    if (is.directed(graph)) {
#        stop ("Only undirected graphs can be used for this problem")
#    }
#    graph <- simplify(graph)
#  
#    check <- function(solution) {
#      if (class(solution)!="logical") {
#        stop ("The solutions for the Maximum Clique Problem have to be logical vectors")
#      }
#      if (length(solution)!=vcount(graph)) {
#        stop ("The solution has to have as many positions as nodes in the graph (", length(V(graph)), ")")
#      }
#    }
#  
#    evaluate <- function(solution) {
#      check(solution)
#      return(-1 * sum(solution))
#    }
#  
#    valid <- function(solution) {
#      check(solution)
#      # Valid iff the induced subgraphs is complete
#      sg <- induced.subgraph(graph, V(graph)[solution])
#      num.nodes <- vcount(sg)
#      num.edges <- ecount(sg)
#      return(num.edges == num.nodes*(num.nodes-1)/2)
#    }
#  
#    correct <- function(solution) {
#      check(solution)
#      while(!valid(solution)) {
#        # Take the node in the current solution that has the least links and remove it from the solution
#        sg <- induced.subgraph(graph, V(graph)[solution])
#        degrees <- degree(sg, V(sg))
#        id <- which.min(degrees)
#        solution[which(solution)[id]] <- FALSE
#        sum(solution)
#      }
#      return(solution)
#    }
#  
#    plotSolution <- function(solution,  node.size=5, ...) {
#      nodes <- V(graph)[solution]
#      edgeInClique <- function (i) {
#        return(all(get.edges(graph, E(graph)[i]) %in% nodes))
#      }
#      clique.edges <- sapply(1:ecount(graph), FUN=edgeInClique)
#  
#      width <- rep(1, ecount(graph))
#      width[clique.edges] <- 3
#      E(graph)$width <- width
#  
#      col.edge <- rep("lightgray", ecount(graph))
#      col.edge[clique.edges] <- "black"
#      E(graph)$color <- col.edge
#  
#      col.vertex <- rep("white", length(solution))
#      col.vertex[solution] <- "black"
#      V(graph)$color <- col.vertex
#      plot.igraph(graph, vertex.size=node.size,
#                  vertex.label=NA, edge.arrow.mode="-")
#    }
#  
#    return(list(evaluate=evaluate, valid=valid, correct=correct, plotSolution=plotSolution))
#  }

## ----problem_example, echo=17:20, eval=FALSE-----------------------------
#  maximumCliqueProblem <- function(graph, penalization=0) {
#  
#    if (is.directed(graph)) {
#        stop ("Only undirected graphs can be used for this problem")
#    }
#    graph <- simplify(graph)
#  
#    check <- function(solution) {
#      if (class(solution)!="logical") {
#        stop ("The solutions for the Maximum Clique Problem have to be logical vectors")
#      }
#      if (length(solution)!=vcount(graph)) {
#        stop ("The solution has to have as many positions as nodes in the graph (", length(V(graph)), ")")
#      }
#    }
#  
#    evaluate <- function(solution) {
#      check(solution)
#      return(-1 * sum(solution))
#    }
#  
#    valid <- function(solution) {
#      check(solution)
#      # Valid iff the induced subgraphs is complete
#      sg <- induced.subgraph(graph, V(graph)[solution])
#      num.nodes <- vcount(sg)
#      num.edges <- ecount(sg)
#      return(num.edges == num.nodes*(num.nodes-1)/2)
#    }
#  
#    correct <- function(solution) {
#      check(solution)
#      while(!valid(solution)) {
#        # Take the node in the current solution that has the least links and remove it from the solution
#        sg <- induced.subgraph(graph, V(graph)[solution])
#        degrees <- degree(sg, V(sg))
#        id <- which.min(degrees)
#        solution[which(solution)[id]] <- FALSE
#        sum(solution)
#      }
#      return(solution)
#    }
#  
#    plotSolution <- function(solution,  node.size=5, ...) {
#      nodes <- V(graph)[solution]
#      edgeInClique <- function (i) {
#        return(all(get.edges(graph, E(graph)[i]) %in% nodes))
#      }
#      clique.edges <- sapply(1:ecount(graph), FUN=edgeInClique)
#  
#      width <- rep(1, ecount(graph))
#      width[clique.edges] <- 3
#      E(graph)$width <- width
#  
#      col.edge <- rep("lightgray", ecount(graph))
#      col.edge[clique.edges] <- "black"
#      E(graph)$color <- col.edge
#  
#      col.vertex <- rep("white", length(solution))
#      col.vertex[solution] <- "black"
#      V(graph)$color <- col.vertex
#      plot.igraph(graph, vertex.size=node.size,
#                  vertex.label=NA, edge.arrow.mode="-")
#    }
#  
#    return(list(evaluate=evaluate, valid=valid, correct=correct, plotSolution=plotSolution))
#  }

## ----problem_example, echo=21:28, eval=FALSE-----------------------------
#  maximumCliqueProblem <- function(graph, penalization=0) {
#  
#    if (is.directed(graph)) {
#        stop ("Only undirected graphs can be used for this problem")
#    }
#    graph <- simplify(graph)
#  
#    check <- function(solution) {
#      if (class(solution)!="logical") {
#        stop ("The solutions for the Maximum Clique Problem have to be logical vectors")
#      }
#      if (length(solution)!=vcount(graph)) {
#        stop ("The solution has to have as many positions as nodes in the graph (", length(V(graph)), ")")
#      }
#    }
#  
#    evaluate <- function(solution) {
#      check(solution)
#      return(-1 * sum(solution))
#    }
#  
#    valid <- function(solution) {
#      check(solution)
#      # Valid iff the induced subgraphs is complete
#      sg <- induced.subgraph(graph, V(graph)[solution])
#      num.nodes <- vcount(sg)
#      num.edges <- ecount(sg)
#      return(num.edges == num.nodes*(num.nodes-1)/2)
#    }
#  
#    correct <- function(solution) {
#      check(solution)
#      while(!valid(solution)) {
#        # Take the node in the current solution that has the least links and remove it from the solution
#        sg <- induced.subgraph(graph, V(graph)[solution])
#        degrees <- degree(sg, V(sg))
#        id <- which.min(degrees)
#        solution[which(solution)[id]] <- FALSE
#        sum(solution)
#      }
#      return(solution)
#    }
#  
#    plotSolution <- function(solution,  node.size=5, ...) {
#      nodes <- V(graph)[solution]
#      edgeInClique <- function (i) {
#        return(all(get.edges(graph, E(graph)[i]) %in% nodes))
#      }
#      clique.edges <- sapply(1:ecount(graph), FUN=edgeInClique)
#  
#      width <- rep(1, ecount(graph))
#      width[clique.edges] <- 3
#      E(graph)$width <- width
#  
#      col.edge <- rep("lightgray", ecount(graph))
#      col.edge[clique.edges] <- "black"
#      E(graph)$color <- col.edge
#  
#      col.vertex <- rep("white", length(solution))
#      col.vertex[solution] <- "black"
#      V(graph)$color <- col.vertex
#      plot.igraph(graph, vertex.size=node.size,
#                  vertex.label=NA, edge.arrow.mode="-")
#    }
#  
#    return(list(evaluate=evaluate, valid=valid, correct=correct, plotSolution=plotSolution))
#  }

## ----problem_example, echo=30:42, eval=FALSE-----------------------------
#  maximumCliqueProblem <- function(graph, penalization=0) {
#  
#    if (is.directed(graph)) {
#        stop ("Only undirected graphs can be used for this problem")
#    }
#    graph <- simplify(graph)
#  
#    check <- function(solution) {
#      if (class(solution)!="logical") {
#        stop ("The solutions for the Maximum Clique Problem have to be logical vectors")
#      }
#      if (length(solution)!=vcount(graph)) {
#        stop ("The solution has to have as many positions as nodes in the graph (", length(V(graph)), ")")
#      }
#    }
#  
#    evaluate <- function(solution) {
#      check(solution)
#      return(-1 * sum(solution))
#    }
#  
#    valid <- function(solution) {
#      check(solution)
#      # Valid iff the induced subgraphs is complete
#      sg <- induced.subgraph(graph, V(graph)[solution])
#      num.nodes <- vcount(sg)
#      num.edges <- ecount(sg)
#      return(num.edges == num.nodes*(num.nodes-1)/2)
#    }
#  
#    correct <- function(solution) {
#      check(solution)
#      while(!valid(solution)) {
#        # Take the node in the current solution that has the least links and remove it from the solution
#        sg <- induced.subgraph(graph, V(graph)[solution])
#        degrees <- degree(sg, V(sg))
#        id <- which.min(degrees)
#        solution[which(solution)[id]] <- FALSE
#        sum(solution)
#      }
#      return(solution)
#    }
#  
#    plotSolution <- function(solution,  node.size=5, ...) {
#      nodes <- V(graph)[solution]
#      edgeInClique <- function (i) {
#        return(all(get.edges(graph, E(graph)[i]) %in% nodes))
#      }
#      clique.edges <- sapply(1:ecount(graph), FUN=edgeInClique)
#  
#      width <- rep(1, ecount(graph))
#      width[clique.edges] <- 3
#      E(graph)$width <- width
#  
#      col.edge <- rep("lightgray", ecount(graph))
#      col.edge[clique.edges] <- "black"
#      E(graph)$color <- col.edge
#  
#      col.vertex <- rep("white", length(solution))
#      col.vertex[solution] <- "black"
#      V(graph)$color <- col.vertex
#      plot.igraph(graph, vertex.size=node.size,
#                  vertex.label=NA, edge.arrow.mode="-")
#    }
#  
#    return(list(evaluate=evaluate, valid=valid, correct=correct, plotSolution=plotSolution))
#  }

## ----problem_example, echo=44:65, eval=FALSE-----------------------------
#  maximumCliqueProblem <- function(graph, penalization=0) {
#  
#    if (is.directed(graph)) {
#        stop ("Only undirected graphs can be used for this problem")
#    }
#    graph <- simplify(graph)
#  
#    check <- function(solution) {
#      if (class(solution)!="logical") {
#        stop ("The solutions for the Maximum Clique Problem have to be logical vectors")
#      }
#      if (length(solution)!=vcount(graph)) {
#        stop ("The solution has to have as many positions as nodes in the graph (", length(V(graph)), ")")
#      }
#    }
#  
#    evaluate <- function(solution) {
#      check(solution)
#      return(-1 * sum(solution))
#    }
#  
#    valid <- function(solution) {
#      check(solution)
#      # Valid iff the induced subgraphs is complete
#      sg <- induced.subgraph(graph, V(graph)[solution])
#      num.nodes <- vcount(sg)
#      num.edges <- ecount(sg)
#      return(num.edges == num.nodes*(num.nodes-1)/2)
#    }
#  
#    correct <- function(solution) {
#      check(solution)
#      while(!valid(solution)) {
#        # Take the node in the current solution that has the least links and remove it from the solution
#        sg <- induced.subgraph(graph, V(graph)[solution])
#        degrees <- degree(sg, V(sg))
#        id <- which.min(degrees)
#        solution[which(solution)[id]] <- FALSE
#        sum(solution)
#      }
#      return(solution)
#    }
#  
#    plotSolution <- function(solution,  node.size=5, ...) {
#      nodes <- V(graph)[solution]
#      edgeInClique <- function (i) {
#        return(all(get.edges(graph, E(graph)[i]) %in% nodes))
#      }
#      clique.edges <- sapply(1:ecount(graph), FUN=edgeInClique)
#  
#      width <- rep(1, ecount(graph))
#      width[clique.edges] <- 3
#      E(graph)$width <- width
#  
#      col.edge <- rep("lightgray", ecount(graph))
#      col.edge[clique.edges] <- "black"
#      E(graph)$color <- col.edge
#  
#      col.vertex <- rep("white", length(solution))
#      col.vertex[solution] <- "black"
#      V(graph)$color <- col.vertex
#      plot.igraph(graph, vertex.size=node.size,
#                  vertex.label=NA, edge.arrow.mode="-")
#    }
#  
#    return(list(evaluate=evaluate, valid=valid, correct=correct, plotSolution=plotSolution))
#  }

## ----problem_example, echo=66:67, eval=FALSE-----------------------------
#  maximumCliqueProblem <- function(graph, penalization=0) {
#  
#    if (is.directed(graph)) {
#        stop ("Only undirected graphs can be used for this problem")
#    }
#    graph <- simplify(graph)
#  
#    check <- function(solution) {
#      if (class(solution)!="logical") {
#        stop ("The solutions for the Maximum Clique Problem have to be logical vectors")
#      }
#      if (length(solution)!=vcount(graph)) {
#        stop ("The solution has to have as many positions as nodes in the graph (", length(V(graph)), ")")
#      }
#    }
#  
#    evaluate <- function(solution) {
#      check(solution)
#      return(-1 * sum(solution))
#    }
#  
#    valid <- function(solution) {
#      check(solution)
#      # Valid iff the induced subgraphs is complete
#      sg <- induced.subgraph(graph, V(graph)[solution])
#      num.nodes <- vcount(sg)
#      num.edges <- ecount(sg)
#      return(num.edges == num.nodes*(num.nodes-1)/2)
#    }
#  
#    correct <- function(solution) {
#      check(solution)
#      while(!valid(solution)) {
#        # Take the node in the current solution that has the least links and remove it from the solution
#        sg <- induced.subgraph(graph, V(graph)[solution])
#        degrees <- degree(sg, V(sg))
#        id <- which.min(degrees)
#        solution[which(solution)[id]] <- FALSE
#        sum(solution)
#      }
#      return(solution)
#    }
#  
#    plotSolution <- function(solution,  node.size=5, ...) {
#      nodes <- V(graph)[solution]
#      edgeInClique <- function (i) {
#        return(all(get.edges(graph, E(graph)[i]) %in% nodes))
#      }
#      clique.edges <- sapply(1:ecount(graph), FUN=edgeInClique)
#  
#      width <- rep(1, ecount(graph))
#      width[clique.edges] <- 3
#      E(graph)$width <- width
#  
#      col.edge <- rep("lightgray", ecount(graph))
#      col.edge[clique.edges] <- "black"
#      E(graph)$color <- col.edge
#  
#      col.vertex <- rep("white", length(solution))
#      col.vertex[solution] <- "black"
#      V(graph)$color <- col.vertex
#      plot.igraph(graph, vertex.size=node.size,
#                  vertex.label=NA, edge.arrow.mode="-")
#    }
#  
#    return(list(evaluate=evaluate, valid=valid, correct=correct, plotSolution=plotSolution))
#  }

## ----use_example, eval=TRUE----------------------------------------------
num.nodes <- 25
rnd.graph <- random.graph.game(n=num.nodes, p.or.m=0.5)
mcp <- maximumCliqueProblem(rnd.graph)
rnd.sol <- runif(num.nodes) > 0.5
mcp$valid(rnd.sol)
sol <- mcp$correct(rnd.sol)
mcp$valid(sol)
mcp$evaluate(sol)
mcp$plotSolution(sol)

## ----use_example_2, eval=TRUE--------------------------------------------

rnd.graph <- random.graph.game(n=num.nodes, p.or.m=0.75)
mcp <- maximumCliqueProblem(rnd.graph)
init.sol <- rep(FALSE, num.nodes)

args <- list()
args$evaluate         <- mcp$evaluate
args$initial.solution <- init.sol
args$neighborhood     <- flipNeighborhood(init.sol)
args$selector         <- greedySelector
args$non.valid        <- "correct"
args$valid            <- mcp$valid
args$correct          <- mcp$correct
args$resources        <- cResource(time=15)

res <- do.call(basicLocalSearch, args)


