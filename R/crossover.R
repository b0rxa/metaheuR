#' @title k point crossover
#'
#' @description This function implements a k point crossover for vectors
#' @param sol.1 First vector to be crossed
#' @param sol.2 Second vector to be crossed
#' @param k Number of cut points. It has to be a number between 1 and the size of the vectors minus 1
#' @param ... Ignored
#' @details The vectors are splited into k+1 fragments and new solutions are constructed alternating elements from each vector
#' @examples
#' 
#' A.sol <- rep("A" , 10)
#' B.sol <- rep("B" , 10)
#' k.point.crossover (sol.1 = A.sol , sol.2 = B.sol , k = 1)
#' k.point.crossover (sol.1 = A.sol , sol.2 = B.sol , k = 2)
#' k.point.crossover (sol.1 = A.sol , sol.2 = B.sol , k = 9) 

k.point.crossover <- function (sol.1 , sol.2 , k=1 , ...){

  ## Parameter control
  if(!is.vector(sol.1) | !is.vector(sol.2)) stop("This crossover operator is for verctors") 
  if (length(sol.1)!=length(sol.2)) stop("The two vectos should be of the same size") 
  
  n <- length(sol.1)
  if(n<2) stop("The minimum length to use this crossover operator is 2")
  if (n<k+1) {
    warning(paste("The length of the vectors is ",n, " so at most there can be ", n-1, " cut points. The parameter will be updated to this limit",sep=""))
    k <- n-1
  }
  if (class(sol.1[1]) != class(sol.1[2])) stop("The elements in the two vectors should be of the same class")
  
  ## Take the k=n-1 as a special case
  if(k==n-1){
    groups <- 1:n    
  }else{
    ## Get the cut points, that cannot be neither 1 nor n
    cuts <- sort(sample(2:(n-1) , k , replace = FALSE))
  
    ## Calculate the size of each chunk. The first chunk will be positions between 1 and the first cut point,     and so on.
    chunk.sizes <- c(cuts[1] , diff(c(cuts , n)))
  
    ## Create a vector of groups that splits the positions into k+1 chunks
    groups <- rep(1:(k+1) , chunk.sizes)
  }
  
  ## Get the position for even chunks (2, 4, ...)
  id <- which(groups %% 2==0) 
  
  ## To create new solutions, copy each of the two and replace with the other those positions in id
  new.sol.1 <- sol.1
  new.sol.1[id] <- sol.2[id]
  new.sol.2 <- sol.2
  new.sol.2[id] <- sol.1[id]
  
  list(new.sol.1 , new.sol.2)
}