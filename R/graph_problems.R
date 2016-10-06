#' Graph coloring problem
#' 
#' This function generates an evaluation, validity and correction functions associated  with a classical graph coloring problem.
#' @family Problems
#' @param graph Graph to color
#' @return A list of functions to be used to solve a graph coloring problem. This includes the functions \code{evaluate}, for the evaluation of a solution, \code{valid}, to check whetehr a solution is valid or not, \code{correct}, to correct a non-valid solution and \code{plot} to graphically show the solution; all the functions have a single argument,  \code{solution}, representing the solution considered. Note that, given that the goal in all the algorithms in the library is minimizing the objective function, the \code{evaluate}. The solutions have to be vectors of factors indicating the color of each node
#' @examples
#' 
#' library("igraph")
#' n <- 10
#' rnd.graph <- random.graph.game(n, p.or.m=0.5)
#' rnd.sol <- factor(paste("c",sample(1:5, size=n, replace=TRUE), sep=""), 
#'                    levels = paste("c", 1:n, sep=""))
#' gcol.problem <- graphColoringProblem(rnd.graph)
#' gcol.problem$valid(rnd.sol)
#' corrected.sol <- gcol.problem$correct(rnd.sol)
#' gcol.problem$valid(corrected.sol)
#' gcol.problem$plot(rnd.sol)
#' gcol.problem$plot(corrected.sol)
#' 
graphColoringProblem <- function(graph) {
  size  <- length(V(graph))
  edges <- get.edgelist(graph)
  
  evaluate <- function(solution) {
    if (length(solution) != size) {
      stop(paste("The solution has to be of the same length as the list of nodes: ", size))
    }
    return(length(unique(solution)))
  }
  
  valid <- function(solution) {
    if (length(solution) != size) {
      stop(paste("The solution has to be of the same length as the list of nodes: ", size))
    }
    return(all(!(solution[edges[, 1]] == solution[edges[, 2]])))
  }
  
  correct <- function(solution) {
    if (length(solution) != size) {
      stop(paste("The solution has to be of the same length as the list of nodes: ", size))
    }
    
    to.correct <- which(solution[edges[, 1]] == solution[edges[, 2]])
    for (link in to.correct) {
      node <- edges[link, 1]
      neighbors <- neighbors(graph, node)
      # Get the first level that is not used in a neighbor
      replace <- which(!(levels(solution) %in% solution[neighbors]))[1]
      # Replace the value associated to the node with that level
      solution[node] <- levels(solution)[replace]
    }
    return(solution)
  }
  
  plotSolution <- function (solution, node.size=5, label.cex=0.5) {
    require(colorspace)
    values <- unique(as.numeric(solution))
    num.colors <- length(values)
    palette <- rainbow_hcl(num.colors, c=50, l=70, start=0, 
                           end=360 * (num.colors - 1) / num.colors)
    colors <- as.numeric(solution)
    for (i in 1:num.colors) {
      colors[colors == values[i]] <- palette[i]
    }
    V(graph)$color <- colors
    V(graph)$label <- solution
    plot.igraph(graph, vertex.size=node.size, edge.arrow.mode="-", 
                vertex.label.color="white", vertex.label.family="sans", 
                vertex.label.cex=label.cex)
  }
  
  return(list(evaluate=evaluate, valid=valid, 
              correct=correct, plot=plotSolution))
}


#' Maximum independent set
#' 
#' This function generates an evaluation, validity and correction functions associated  with a classical maximum independet set problem
#' @param graph Graph where we have to find the maximum independent set (MIS)
#' @return A list of functions to be used to solve a MIS problem. This includes the functions \code{evaluate}, for the evaluation of a solution, \code{valid}, to check whetehr a solution is valid or not, \code{correct}, to correct a non-valid solution and \code{plot} to graphically show the solution; all the functions have a single argument,  \code{solution}. The solutions passed to these functions has to be a logical vector indicating with \code{TRUE} which nodes are in the independent set.
#' @family Problems
#' @details The evaluation function includes another parameter, \code{penalization}, which can be used to penalize non-valid solutions. The penalization terms is the number of nodes that are connected in the solution, and it is weighted with the factor passed in the \code{penalization} parameter By default its value is 0.
#' @examples
#' 
#' library("igraph")
#' 
misProblem <- function (graph, penalization=0) {
  size <- length(V(graph))
  edges <- get.edgelist(graph)
  countViolations <- function (solution) {
    # Create a subgraph with the passed nodes and check there are no links
    subg <- induced.subgraph(graph, V(graph)[solution])
    sum(degree(subg) > 0)
  }
  
  evaluate <- function (solution) {
    ev <- -1 * (sum(solution) - penalization * countViolations(solution))
    return(ev)
  }
  
  valid <- function (solution){
    return(countViolations(solution) == 0)
  }
  
  correct <- function (solution) {
    while(!valid(solution)) {
      # Create a subgraph with the passed nodes and check there are no links
      subg <- induced.subgraph(graph, V(graph)[solution])
      id <- which.max(degree(graph) * solution)
      solution[id] <- FALSE
    }
    return(solution)
  }
  
  plotSolution <- function (solution, node.size=5) {
    col <- rep("white", length(solution))
    col[solution] <- "black"
    V(graph)$color <- col 
    plot.igraph(graph, vertex.size=node.size, 
                vertex.label=NA, edge.arrow.mode="-")
  }
  
  return(list(evaluate=evaluate, valid=valid, correct=correct, 
              plot=plotSolution))
}



#' Minimum dominating set
#' 
#' This function generates an evaluation, validity and correction functions associated  with a classical minimum dominating problem
#' @param graph Graph where we have to find the minimum dominating set (MDS)
#' @return A list of functions to be used to solve a MDS problem. This includes the functions \code{evaluate}, for the evaluation of a solution, \code{valid}, to check whetehr a solution is valid or not, \code{correct}, to correct a non-valid solution and \code{plot} to graphically show the solution; all the functions have a single argument,  \code{solution}. The solutions passed to these functions has to be a logical vector indicating with \code{TRUE} which nodes are in the independent set.
#' @details The evaluation function includes another parameter, \code{penalization}, which can be used to penalize non-valid solutions. The penalization terms is the number of nodes that are not connected to the nodes in the solution, and it is weighted with the factor passed in the \code{penalization} parameter. By default its value is 0.
#' @family Problems
#' @examples
#' 
#' library("igraph")
#' 
mdsProblem <- function (graph, penalization=0) {
  size <- length(V(graph))
  edges <- get.edgelist(graph)
  
  countDisconnectedNodes <- function (solution) {
    sol.nodes <- V(graph)[solution]
    linked.nodes <- unlist(sapply(sol.nodes, 
                                  FUN=function(x) {
                                    return(unique(neighbors(graph,x)))
                                  }))
    included.nodes <- unique(c(sol.nodes, linked.nodes))
    return (V(graph)[!(V(graph) %in% included.nodes)])
  }
  
  evaluate <- function (solution) {
    disc <- countDisconnectedNodes(solution)
    return (sum(solution) - penalization * length(disc))
  }
  
  valid <- function (solution) {
    return(length(countDisconnectedNodes(solution)) == 0)
  }
  
  correct <- function (solution) {
    while(!valid(solution)) {
      disc <- countDisconnectedNodes(solution)
      # Add the first disconnected node and check the validity
      id <- which(V(graph) %in% disc)[1]
      solution[id] <- TRUE
    }
    solution
  }
  
  plotSolution <- function (solution, node.size=5) {
    col <- rep("white", length(solution))
    col[solution] <- "black"
    V(graph)$color <- col 
    plot.igraph(graph, vertex.size=node.size, 
                vertex.label=NA, edge.arrow.mode="-")
  }
  
  return(list(evaluate=evaluate, valid=valid, 
              correct=correct, plot=plotSolution))
}