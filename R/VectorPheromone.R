#' An S4 class to basic pheromone models for categorical vectors
#'
#' @slot trail A matrix with the pheromone values. Each column is a variable and each row a possible value.
#' @slot binary A logical value indicating whether the vector is binary or factor
#' @slot evaporation.factor A value between 0 and 1 (both excluded) that indicates the factor used in the evaporation phase

setClass(
  Class = "VectorPheromone", 
  representation = representation(trail = "matrix" , 
                                  binary = "logical" , 
                                  evaporation.factor = "numeric")
)

setValidity(
  Class = "VectorPheromone", 
  method = function(object){
    m = object@trail
    
    if (sign(max(m)) - sign(min(m)) == 2) stop("The pheromone matrix should not contain both positive and negative values")
    
    if (object@evaporation.factor >= 1 | object@evaporation.factor <= 0)
      stop ("The evaporation factor has to be a value between 0 and 1")
    
    if (object@binary){
      if (nrow(object@trail)!=2) stop("For models over logical variables the 'trail' parameter should have two rows, the first corresponding to the trail associated to the 'FALSE' value and the other corresponding to the 'TRUE' value" )
    }else{
      if (is.null(rownames(object@trail)))
        stop("For factor models the rows in the probability table have to be named with the level values")
      if (nrow(object@trail)<2) stop("For factor models there should be at least two possible values")
    }
    return (TRUE)
  }
)


# GENERIC METHODS ---------------------------------------------------------

setMethod(
  f = "update.trail",
  signature = "VectorPheromone",
  definition = function (object , solution , value , ...){
    n <- length(solution)
    if (object@binary){
      # Identify the row corresponding to each value in the solution
      row.id <- as.numeric(solution) + 1
    }else{
      levels <- rownames(object@trail)
      # Identify the row corresponding to each value in the solution
      row.id <- sapply(solution , FUN = function(x) which(levels %in% x))
    }
    
    # Now, update the matrix
    objectGlobalName <- deparse(substitute(object))
    pairs <- cbind(row.id , 1:n)
    object@trail [pairs] <- object@trail [pairs] + value
    if (sign(max(object@trail)) - sign(min(object@trail)) == 2) stop("Positive and negative values mixed in the pheromone model")
    assign(objectGlobalName,object,envir=parent.frame())  
    
  })



setMethod(
  f = "evaporate",
  signature = "VectorPheromone",
  definition = function (object , ...){
    objectGlobalName <- deparse(substitute(object))
    object@trail <- object@trail * (1-object@evaporation.factor)
    assign(objectGlobalName,object,envir=parent.frame())  
    
  })


setMethod(
  f="build.solution", 
  signature = "VectorPheromone", 
  definition = function(object , n = 1, seed = NULL , ...) {
    
    if(!is.null(seed)) set.seed(seed)
    
    ## Build a probability table
    aux <- object@trail
    ## In case the values are negative, transform them so as the smallest one has the smallest probability and the (inverted) ratios are kept
    if (min(aux)<0) aux <- -1/aux
    # Normalize
    probs <- scale(aux, center=FALSE, scale=colSums(aux))
    c <- ncol(probs)
    
    if (object@binary) { ## The second column is the one corresponding with TRUE
      get.sample <- function(i) runif(c)<probs[2,]
    }else{
      get.sample <- function(i){
        levels <- rownames(probs)
        ## Get a level according to the probabilities
        sample <- sapply(1:c , FUN = function(j) sample(x = levels , 
                                                        size = 1 , 
                                                        prob = probs[,j]))
        ## Return a factor
        factor(sample , levels)
      }
    }
    
    ## Create all the samples
    lapply(1:n , FUN = get.sample)
  })


# CONSTRUCTOR -------------------------------------------------------------

#' Bais consturctor of pheromone models for vectors
#' 
#' This function creates an object of class \code{\linkS4class{VectorPheromone}}
#' 
#' @family ACO
#' @param initial.trail An square matrix containing the intial pheromone value for each link in the complete graph
#' @param evaporation.factor A number between 0 and 1 (both excluded) that represents the factor used in the evaporation phase
#' @param ... Ignored
#' @return An object of class \code{\linkS4class{VectorPheromone}} that includes the marginal probability distributions

vectorPheromone<-function(binary = FALSE , initial.trail , evaporation.factor , ...){
  if (binary & nrow(initial.trail)!=2) stop ("For binary pheromone models the initial matrix has to have two rows")
  if (nrow(initial.trail)<2) stop ("The initial matrix has to have, at least, two rows")
  if (nrow(initial.trail)>2 & is.null(rownames(initial.trail))) stop("For matrices with more than two rows the rows have to be named")
  
  if (nrow(initial.trail)>2) binary = FALSE
  
  new("VectorPheromone",trail = initial.trail , binary = binary , evaporation.factor = evaporation.factor)
}
