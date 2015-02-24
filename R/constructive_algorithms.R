#' Constructive greedy algorithm for the TSP problem
#' 
#' This function implements a simple constructive greedy algorithm for the TSP problem.
#' @param cmatrix Cost matrix associated to the TSP instance
#' @return A permutation contining a solution for the problem
#' @details The algorithm builds the solution iteratively, selecting, at each step, the closest city to the one added in the previous step.
#' @examples
#' n <- 10
#' cost.matrix <- matrix(runif(n^2) , ncol = n)
#' tsp.greedy (cost.matrix)


tsp.greedy <- function (cmatrix){
  diag(cmatrix) <- NA
  ## Get the position with the minimum value
  best.pair <- which(cmatrix == min(cmatrix,na.rm=T), arr.ind=TRUE)
  solution <- c(best.pair[1,1],best.pair[1,2])
  ## Remove from the candidate list the two columns
  cmatrix[ , best.pair[1,]]<-NA
  ## Also remove the row of the first city
  cmatrix[best.pair[1,1],]<-NA
  for (i in 3:dim(cmatrix)[1]){
    ## Get the closest city
    next.city <- which.min (cmatrix[solution[i-1],])
    ## Add to the solution
    solution <- append(solution , next.city)
    ## Update the matrix
    cmatrix[solution[i-1],] <- NA
    cmatrix[ , next.city] <- NA
  }
  names(solution)<-NULL
  permutation(vector = solution)
}


#' Constructive greedy algorithm for the Maximum Independet Set (MIS) problem
#' 
#' This function implements a simple constructive greedy algorithm for the MIS problem
#' @param graph Graph where the MIS has to be found
#' @param cl.size Number of nodes to consider at each step of the algorithm.
#' @return A logical vector indicating which nodes are in the independent set
#' @details The algorithm builds the solution iteratively selecting, uniformly at random, one of the nodes in the candidate list; this list contains the available nodes with the lowest degree. By default the candidate list is 1, which means that, in absence of ties, the algorithm returns always the same solution. The candidate list can be longer, in which case the algorithm generates random greedy solutions that can be used in GRASP-like algorithms.
#' @examples
#' library(igraph)
#' rnd.graph <- random.graph.game (20 , 0.2)
#' solution <- mis.greedy (rnd.graph)
#' misp <- mis.problem(rnd.graph)
#' misp$plot (solution)
#' ## Random good solutions
#' solution <- mis.greedy (rnd.graph , cl.size = 5)
#' misp$plot (solution)
#' 
#' solution <- mis.greedy (rnd.graph , cl.size = 5)
#' misp$plot (solution)

mis.greedy <- function (graph , cl.size = 1){
  node.degrees <- degree(graph)
  mis <- rep(FALSE , length(node.degrees))
  aux.degrees<-node.degrees
  ## Get a node with the smallest degree, insert into the set and
  ## remove it and its neighbors from the candidate list (setting their degree at NA)
  while(sum(!is.na(aux.degrees))>0)
  {
    ## Randomly select one position in the candidate list
    id <- sample(1:min(cl.size , sum(!is.na(aux.degrees))),size = 1)
    ## Identify the node to include in the solution and include it
    to.include <- order(aux.degrees)[id] 
    mis[to.include] <- T
    ## Remove nodes connected to the included node
    nd <- V(graph)[to.include]
    rm.id <- c(nd,neighbors(graph,nd))
    aux.degrees[rm.id] <- NA
  }
  mis
}
  