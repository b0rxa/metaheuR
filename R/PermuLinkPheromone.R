#' An S4 class to basic link-based pheromone model for permutations
#'
#' @slot trail A matrix with the pheromone values for each link. Only the upper triangle of the matrix is considered
#' @slot evaporation.factor A value between 0 and 1 (both excluded) that indicates the factor used in the evaporation phase
#' 
setClass(
  Class="PermuLinkPheromone", 
  representation=representation(trail="matrix",
                                evaporation.factor="numeric")
)

setValidity(
  Class="PermuLinkPheromone", 
  method=function(object) {
    m <- object@trail
    if (sign(max(m)) - sign(min(m)) == 2) {
      stop("The pheromone matrix should not contain both positive and negative ",
           "values")
    }
    
    if (object@evaporation.factor >= 1 | object@evaporation.factor <= 0) {
      stop ("The evaporation factor has to be a value between 0 and 1")
    }
    
    if (ncol(m) != nrow(m)) {
      stop("The trail matrix should be square")
    }
    
    return (TRUE)
  })


setMethod(
  f="updateTrail",
  signature="PermuLinkPheromone",
  definition=function(object, solution, value, ...) {
    if (class(solution) != "Permutation") {
      stop("This pheromone model is designed for objects of class Permutation")
    }
    sol <- as.numeric(solution)
    n <- length(sol)        
    # Update the matrix
    objectGlobalName <- deparse(substitute(object))
    # Create all the pairs representing the links in the permutation
    pairs <- cbind(sol[-n], sol[-1])
    object@trail[pairs] <- object@trail[pairs] + value      
    if (sign(max(object@trail)) - sign(min(object@trail)) == 2) {
      stop("Positive and negative values mixed in the pheromone model")
    }
    assign(objectGlobalName, object, envir=parent.frame())  
  })


setMethod(
  f="evaporate",
  signature="PermuLinkPheromone",
  definition=function(object, ...) {
    objectGlobalName <- deparse(substitute(object))
    object@trail <- object@trail * (1 - object@evaporation.factor)
    assign(objectGlobalName, object, envir=parent.frame())     
  })


setMethod(
  f="buildSolution", 
  signature="PermuLinkPheromone", 
  definition=function(object, n=1, seed=NULL, ...) {
    if(!is.null(seed)) {
      set.seed(seed)
    }
    
    getSample <- function (i) {
      sol <- vector ()  
      mat <- object@trail
      # In case the values are negative, transform them so as the smallest one 
      # has the smallest probability and the (inverted) ratios are kept
      if (min(mat) < 0) {
        mat <- -1 / mat
      }
      n <- ncol(mat)
      # Select one node at random
      from <- sample(1:n, 1)
      sol <- c(sol, from)
      
      # Select the next node, according to the probabilities extracted from the 
      # pheromone matrix
      # The 0 in the middle is icluded so as the indices in "prob" are the same 
      # as in the permutation; the 0 corresponds to the probability of going 
      # from "from" to "from"
      prob <- vector()
      if (from > 1) {
        prob <- c(prob, mat[1:(from - 1), from])
      }
      prob <- c(prob, 0)
      if (from < n) {
        prob <- c(prob, mat[from, (from + 1):n])
      }
      prob <- prob / sum(prob)
      to <- sample (1:n, 1, prob=prob)
      
      # Update the matrix to avoid returning to the selected position
      mat[, from] <- 0
      mat[from, ] <- 0
      # Repeat the same process until all the positions except the last one are fixed
      while (length(sol) < n - 1) {
        sol <- c(sol, to)
        from <- to
        # Get the probabilities
        prob <- vector()
        if (from > 1) {
          prob <- c(prob, mat[1:(from - 1), from])
        }
        prob <- c(prob, 0)
        if (from < n) {
          prob <- c(prob, mat[from, (from + 1):n])
        }
        
        prob <- prob / sum(prob)
        # Select the new node
        to <- sample (1:n, 1, prob=prob)
        # Update the matrix to avoid returning to the selected position (rows 
        # represent the "from" node and columns the "to" node)
        mat[, from] <- 0
        mat[from, ] <- 0
      }
      
      # Finally, add the last element
      last <- which(!(1:n %in% sol))
      sol <- c(sol, last)
      return(permutation(vector=sol))
    }
    
    # Create all the samples
    return(lapply(1:n, FUN=getSample))
  })


# CONSTRUCTOR ------------------------------------------------------------------

#' Bais consturctor of pheromone models based on the view of permutations as Hamiltonian cycles in complete graphs
#' 
#' This function creates an object of class \code{\linkS4class{PermuLinkPheromone}}
#' 
#' @family ACO
#' @param initial.trail An square matrix containing the intial pheromone value for each link in the complete graph
#' @param evaporation.factor A number between 0 and 1 (both excluded) that represents the factor used in the evaporation phase
#' @param ... Ignored
#' @return An object of class \code{\linkS4class{PermuLinkPheromone}}
#' 
permuLinkPheromone<-function(initial.trail, evaporation.factor, ...) {  
  obj <- new("PermuLinkPheromone", trail=initial.trail, 
             evaporation.factor=evaporation.factor)
  return(obj)
}
