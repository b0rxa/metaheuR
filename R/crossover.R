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


#' @title Order crossover for permutations
#'
#' @description This function implements the order crossover operator for permutations
#' @param sol.1 First vector to be crossed
#' @param sol.2 Second vector to be crossed
#' @param ... Ignored
#' @details Two random points are selected and the values between this points are directly copied from one of the parents. The rest of the positions are filled using the order in the second parent
#' @examples
#' 
#' A.sol <- identity.permutation (10)
#' B.sol <- random.permutation (10)
#' order.crossover (sol.1 = A.sol , sol.2 = B.sol)

order.crossover <- function (sol.1 , sol.2 , ...){
  ## Parameter control
  if(class(sol.1)!='Permutation' | class(sol.2)!='Permutation') stop("This crossover operator is for permutation") 
  if (length(sol.1)!=length(sol.2)) stop("The two permutations should be of the same size") 
  
  n <- length(sol.1)
  if(n<3) stop("The minimum length to use this crossover operator is 3")
  
  ## Get the cut points, that cannot be neither 1 nor n
  cuts <- sort(sample(1:n , 2 , replace = FALSE))
  id.replace <- c(1:cuts[1] , cuts[2]:n)
  
  v1 <- as.numeric(sol.1)
  v2 <- as.numeric(sol.2)
  
  ## For the new solutions, we copy the values in the parents
  nv1 <- v1
  nv2 <- v2
  
  ## Now, get the positions to replace and take then in the order defined in the other solution
  nv1[id.replace] <- v2[v2 %in% v1[id.replace]]
  nv2[id.replace] <- v1[v1 %in% v2[id.replace]]
  
  new.sol.1 <- permutation(vector = nv1)
  new.sol.2 <- permutation(vector = nv2)
  
  list(new.sol.1 , new.sol.2)
}