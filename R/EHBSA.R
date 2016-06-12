#' An S4 class to represent distributions based on Edge Histrogam Based Sampling Algorithm.
#'
#' @slot adjacencyMatrix A matrix containing the marginal second order probabilities from the data.
#' 
setClass(
	Class="EHBSA", 
	representation=representation(adjacencyMatrix="matrix")
)

# GENERIC METHODS ---------------------------------------------------------
setMethod(
  f="simulate", 
  signature="EHBSA", 
  definition=function(object, nsim=1, seed=NULL, ...) {
    # we create a roullette wheel
    new.population <- matrix(, nrow=0, ncol = length(object@adjacencyMatrix[1,]))
    for(q in 1:nsim){
      l.matrix <- object@adjacencyMatrix
      l <- c(sample(0, length(l.matrix[1,]), replace=TRUE)) #new list with numbers
      l[1] <- round(runif(1, 1, length(l.matrix[1,])), 0)
      l.matrix[, l[1]] <- c(0)
      for(i in 2:length(l.matrix[1,])){ # iterate positions of the new individual
        val <- runif(1, 0, sum(l.matrix[l[i-1],]))
        j <- 1
        acc <- l.matrix[l[i-1], j]
        #acc <- 0
        while(val > acc){
          j <- j + 1
          acc <- acc + l.matrix[l[i-1],j]
        }
        l[i] <- j
        l.matrix[, j] <- c(0)
      }
      l <- permutation(l)
      new.population <- c(new.population, l)
    }
    return(new.population)
})

# CONSTRUCTOR -------------------------------------------------------------
#' Constructor of EHBSA model
#' 
#' This function creates an object of class \code{\linkS4class{EHBSA}}
#' 
#' @family EDA
#' @param data The dataframe containing the initial-population to construct the EHBSA.
#' @param bias ratio to control the preassure of the learned probabilities. By default it is set to 0.5
#' @param ... Ignored
#' @return An object of class \code{\linkS4class{EHBSA}} that includes the adjacency matrix of the given data
#' 
ehbsa <- function(data, bratio, ...) {
	if(class(data) == "list"){
	  if(missing(bratio)){
	    bratio <- 0.5
	  }
		# We can create the object
	  # Learn matrix definition
	  adjacencyMatrix <- matrix(c(0) , nrow=length(data[[1]]), ncol=length(data[[1]])) #empty matrix
		for(i in 1:length(data)){
		  for(j in 2:length(data[[i]]@permutation)){
		    adjacencyMatrix[ data[[i]]@permutation[j-1], data[[i]]@permutation[j] ] <- adjacencyMatrix[ data[[i]]@permutation[j-1], data[[i]]@permutation[j] ] + 1
		  }
		}
	  colnames(adjacencyMatrix) <- paste("X", 1:length(adjacencyMatrix[1,]), sep="")
	  rownames(adjacencyMatrix) <- paste("X", 1:length(adjacencyMatrix[1,]), sep="")
	  adjacencyMatrix <- adjacencyMatrix + bratio
	  diag(adjacencyMatrix) <- 0
	  obj <- new("EHBSA", adjacencyMatrix = adjacencyMatrix)
	}else{
		stop("The data must be a list")
	}
}
