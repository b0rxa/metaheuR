#' An S4 class to basic position-based pheromone model for permutations
#'
#' @slot trail A matrix with the pheromone values for each position. The columns represent the position and the rows the value.
#' @slot evaporation.factor A value between 0 and 1 (both excluded) that indicates the factor used in the evaporation phase
#' 
setClass(
  Class="PermuPosPheromone", 
  representation=representation(trail="matrix",
                                evaporation.factor="numeric"))

setValidity(
  Class="PermuPosPheromone",
  method=function(object) {
    m <- object@trail
    if (sign(max(m)) - sign(min(m)) == 2) {
      stop("The pheromone matrix should not contain both positive and negative values")
    }
    
    if (object@evaporation.factor >= 1 | object@evaporation.factor <= 0){
      stop ("The evaporation factor has to be a value between 0 and 1")
    }
    
    if (ncol(m) != nrow(m)){
      stop("The trail matrix should be square")
    }
    return (TRUE)
  })

# GENERIC METHODS --------------------------------------------------------------

setMethod(
  f="updateTrail",
  signature="PermuPosPheromone",
  definition=function(object, solution, value, ...) {
    if (class(solution) != "Permutation") {
      stop("This pheromone model is designed for objects of class Permutation")
    }
    sol <- as.numeric(solution)
    n <- length(sol)        
    # Update the matrix
    objectGlobalName <- deparse(substitute(object))
    # Create all the pairs value - position
    pairs <- cbind(sol, 1:n)
    object@trail[pairs] <- object@trail[pairs] + value
    if (sign(max(object@trail)) - sign(min(object@trail)) == 2) {
      stop("Positive and negative values mixed in the pheromone model")
    }
    assign(objectGlobalName,object,envir=parent.frame())  
  })

setMethod(
  f="evaporate",
  signature="PermuPosPheromone",
  definition=function(object, ...) {
    objectGlobalName <- deparse(substitute(object))
    object@trail <- object@trail * (1 - object@evaporation.factor)
    assign(objectGlobalName, object, envir=parent.frame())     
  })

setMethod(
  f="buildSolution", 
  signature="PermuPosPheromone", 
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

      # Sample the marginal distribution and update the auxiliar matrix to avoid 
      # selecting already included values
      for (pos in 1:n) {
        prob <- mat[, pos]
        prob <- prob / sum(prob)
        v <- sample (1:n, 1, prob=prob)
        sol <- c(sol, v)
        # Update the matrix to avoid returning to the selected position 
        mat[v, ] <- 0
      }
      return (permutation(vector=sol))
    }
    
    ## Create all the samples
    return(lapply(1:n, FUN=getSample))
  })


# CONSTRUCTOR ------------------------------------------------------------------

#' Bais consturctor of pheromone models for permutation based on position
#' 
#' This function creates an object of class \code{\linkS4class{PermuPosPheromone}}
#' 
#' @family ACO
#' @param initial.trail An square matrix containing the intial pheromone value for combination of position and value
#' @param evaporation.factor A number between 0 and 1 (both excluded) that represents the factor used in the evaporation phase
#' @param ... Ignored
#' @return An object of class \code{\linkS4class{PermuPosPheromone}}
#' 
permuPosPheromone <- function(initial.trail, evaporation.factor, ...) {  
  obj <- new("PermuPosPheromone", trail=initial.trail, 
              evaporation.factor=evaporation.factor)
  return(obj)
}
