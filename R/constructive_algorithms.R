#' Constructive greedy algorithm for the TSP problem
#' 
#' @description This function implements a simple constructive greedy algorithm for the TSP problem
#' @param cmatrix Cost matrix associated to the TSP instance
#' @param cl.size Size of candidate list considered at each step. By default it is 1, thus implementing a sheer greedy algorithm. If this value is greater than 1, then random solutions are created considering at each step the best options, as in GRASP algorithms
#' @return A permutation containing a solution for the problem
#' @details The algorithm builds the solution iteratively, selecting, at each step, the closest city to the one added in the previous step
#' @family Constructive algorithms
#' @examples
#' n <- 10
#' cost.matrix <- matrix(runif(n^2), ncol=n)
#' tspGreedy(cost.matrix, 2)
#' 
tspGreedy <- function (cmatrix, cl.size=1){
  diag(cmatrix) <- NA
  # If the TSP problem is symmetric, we double the number of candidates to consider 
  # both possibilities
  if (cl.size == 1) {
    # Get the position with the minimum value
    best.pair <- which(cmatrix == min(cmatrix, na.rm=TRUE), arr.ind=TRUE)
    solution <- c(best.pair[1, 1], best.pair[1, 2])
    # Remove from the candidate list the two columns
    cmatrix[, best.pair[1, ]] <- NA
    # Also remove the row of the first city
    cmatrix[best.pair[1, 1], ] <- NA
    for (i in 3:nrow(cmatrix)) {
      # Get the closest city
      next.city <- which.min(cmatrix[solution[i - 1], ])
      # Add to the solution
      solution <- append(solution, next.city)
      ## Update the matrix
      cmatrix[solution[i - 1], ] <- NA
      cmatrix[, next.city] <- NA
    }
  } else {
    if (cl.size < 0) { 
      stop("candidate.list must be a positive number")
    }
    # Calculate maximum number of possible candidates
    if (isSymmetric(cmatrix)) {
      num.candidates <- min(cl.size * 2, sum(!is.na(cmatrix)))
    } else {
      num.candidates <- min(cl.size, sum(!is.na(cmatrix)))
    }
    # Order the values in cmatrix, select candidates and choose one randomly
    candidates <- sort(cmatrix)[1:num.candidates]
    best.pair <- which(cmatrix == candidates[sample(1:num.candidates, 1)], 
                       arr.ind=TRUE)
    solution <- c(best.pair[1, 1], best.pair[1, 2])
    # Remove from the candidate list the two columns
    cmatrix[, best.pair[1, ]] <- NA
    # Also remove the row of the first city
    cmatrix[best.pair[1, 1], ] <- NA
    
    for (i in 3:dim(cmatrix)[1]) {
      # Calculate maximum number of candidates
      num.candidates <- min(cl.size, sum(!is.na(cmatrix[solution[i - 1], ])))
      # Get the candidates and choose one randomly
      next.city <- order(cmatrix[solution[i - 1], ])[sample(1:num.candidates, 1)]
      # Add to the solution
      solution <- append(solution, next.city)
      # Update the matrix
      cmatrix[solution[i - 1], ] <- NA
      cmatrix[, next.city] <- NA
    }
  }
  names(solution) <- NULL
  return(permutation(vector=solution))
}


#' Constructive greedy algorithm for the Maximum Independet Set (MIS) problem
#' 
#' @description This function implements a simple constructive greedy algorithm for the MIS problem
#' @param graph Graph where the MIS has to be found
#' @param cl.size Number of nodes to consider at each step of the algorithm.  By default it is 1, thus implementing a sheer greedy algorithm. If this value is greater than 1, then random solutions are created considering at each step the best options, as in GRASP algorithms.
#' @return A logical vector indicating which nodes are in the independent set
#' @details The algorithm builds the solution iteratively selecting, uniformly at random, one of the nodes in the candidate list; this list contains the available nodes with the lowest degree. By default the candidate list is 1, which means that, in absence of ties, the algorithm returns always the same solution. The candidate list can be longer, in which case the algorithm generates random greedy solutions that can be used in GRASP-like algorithms.
#' @family Constructive algorithms
#' @examples
#' library(igraph)
#' rnd.graph <- random.graph.game(20, 0.2)
#' solution <- misGreedy(rnd.graph)
#' misp <- misProblem(rnd.graph)
#' misp$plot(solution)
#' ## Random good solutions
#' solution <- misGreedy(rnd.graph, cl.size=5)
#' misp$plot (solution)
#' solution <- misGreedy(rnd.graph, cl.size=5)
#' misp$plot(solution)
#' 
misGreedy <- function (graph, cl.size=1){
  node.degrees <- degree(graph)
  mis <- rep(FALSE, length(node.degrees))
  aux.degrees <- node.degrees
  # Get a node with the smallest degree, insert into the set and
  # remove it and its neighbors from the candidate list (setting their degree at NA)
  while(sum(!is.na(aux.degrees)) > 0) {
    # Randomly select one position in the candidate list
    id <- sample(1:min(cl.size, sum(!is.na(aux.degrees))), size=1)
    # Identify the node to include in the solution and include it
    to.include <- order(aux.degrees)[id] 
    mis[to.include] <- TRUE
    # Remove nodes connected to the included node
    nd <- V(graph)[to.include]
    rm.id <- c(nd, neighbors(graph, nd))
    aux.degrees[rm.id] <- NA
  }
  return(mis)
}