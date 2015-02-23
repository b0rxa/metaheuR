#' Graph coloring problem
#' 
#' This function generates an evaluation, validity and correction functions associated  with a classical graph coloring problem
#' @param graph Graph to color
#' @return A list of functions to be used to solve a graph coloring problem. This includes the functions \code{evaluate}, for the evaluation of a solution, \code{is.valid}, to check whetehr a solution is valid or not and 'correct', to correct a non-valid solution; all the functions have a single argument,  \code{solution}, representing the solution considered. Note that, given that the goal in all the algorithms in the library is minimizing the objective function, the \code{evaluate}. The solutions have to be vectors of factors indicating the color of each node
#' @examples
#' 
#' library("igraph")
#' n <- 10
#' rnd.graph <- random.graph.game(n , p.or.m = 0.5)
#' rnd.sol <- factor (paste("c",sample(1:5 , size = n , replace = TRUE) , sep="") , 
#'                    levels = paste("c" , 1:n , sep=""))
#' gcol.problem <- graph.coloring.problem (rnd.graph)
#' gcol.problem$is.valid(rnd.sol)
#' corrected.sol <- gcol.problem$correct(rnd.sol)
#' gcol.problem$is.valid(corrected.sol)
#' gcol.problem$plot(rnd.sol)
#' gcol.problem$plot(corrected.sol)

graph.coloring.problem<-function(graph){
  size <- length(V(graph))
  edges <- get.edgelist(graph)
  
  evaluate <- function(solution){
    if (length(solution) != size) stop(paste("The solution has to be of the same length as the list of nodes: " , size))
    return(length(unique(solution)))
  }
  
  is.valid <- function(solution){
    if (length(solution) != size) stop(paste("The solution has to be of the same length as the list of nodes: " , size))
    return(all(!(solution[edges[,1]]==solution[edges[,2]])))
  }
  
  correct <- function(solution){
    if (length(solution) != size) stop(paste("The solution has to be of the same length as the list of nodes: " , size))
    
    to.correct <- which(solution[edges[,1]]==solution[edges[,2]])
    for (link in to.correct){
      node <- edges[link,1]
      neighbors <- neighbors(graph,node)
      ## Get the first level that is not used in a neighbor
      replace <- which(!(levels(solution) %in% solution[neighbors]))[1]
      ## Replace the value associated to the node with that level
      solution[node] <- levels(solution)[replace]
    }
    return(solution)
  }
  
  plot.solution <- function (solution , node.size = 5 , label.cex = 0.5){
    require(colorspace)
    values <- unique(as.numeric(solution))
    num.colors <- length(values)
    palette <- rainbow_hcl(num.colors, c = 50, l = 70, start = 0, end = 360*(num.colors-1)/num.colors)
    colors <- as.numeric(solution)
    for (i in 1:num.colors) colors[colors==values[i]] <- palette[i]
    V(graph)$color <- colors
    V(graph)$label <- solution
    plot.igraph(graph , vertex.size = node.size , edge.arrow.mode="-" , 
                vertex.label.color = "white" , vertex.label.family = "sans" , 
                vertex.label.cex = label.cex)
  }
  
  return(list(evaluate = evaluate, is.valid = is.valid, correct = correct , plot = plot.solution))
}