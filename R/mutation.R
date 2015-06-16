#' @title Binary mutation
#'
#' @description This function takes random positions of a logical vector and changes their value
#' @param solution A logical vector to be mutated.
#' @param ratio Ratio of the positions to be mutated. It has to be a value greater than 0.
#' @examples
#' 
#' F.sol <- rep(FALSE, 10)
#' binaryMutation(F.sol, ratio=0.1)
#' binaryMutation(F.sol, ratio=0.1)
#' binaryMutation(F.sol, ratio=0.5)
#' 
binaryMutation <- function(solution, ratio, ...) {
  if (ratio <= 0){
    stop("The ratio has to be a strictly positive value")
  }
  n <- length(solution)
  for (i in 1:(n*ratio)) {
    id <- sample(n, 1)
    solution[id] <- !solution[id]
  }
  return(solution)
}


#' @title Factor mutation
#'
#' @description This function takes random positions of a factor vector and replaces them with a random value
#' @param solution A factor vector to be mutated.
#' @param ratio Ratio of the positions to be mutated. It has to be a value greater than 0.
#' @examples
#' 
#' A.sol <- factor(rep("A", 10), c("A", "B", "C"))
#' factorMutation(A.sol, ratio=0.1)
#' factorMutation(A.sol, ratio=0.1)
#' factorMutation(A.sol, ratio=0.5)

factorMutation <- function (solution, ratio, ...) {
  if (ratio<=0) {
    stop("The ratio has to be a strictly positive value")
  }
  n <- length(solution)
  for (i in 1:(n*ratio)) {
    id <- sample(n, 1)
    l <- levels(solution[id])
    solution[id] <- sample(subset(l, l != solution[id]), 1)
  }
  return(solution)
}


#' @title Swap mutation
#'
#' @description This function takes random pairs of positions in a permutation and swaps them
#' @param solution A permutation to be mutated.
#' @param ratio Ratio of the positions to be mutated. It has to be a value greater than 0.
#' @examples
#' 
#' id.perm <- identityPermutation(10)
#' swapMutation(id.perm, ratio=0.1)
#' swapMutation(id.perm, ratio=0.1)
#' swapMutation(id.perm, ratio  0.5)

swapMutation <- function (solution, ratio, ...){
  if (ratio <= 0) {
    stop("The ratio has to be a strictly positive value")
  }
  n <- length(solution)
  for (i in 1:(n*ratio)) {
    id <- sample(n, 2, replace=FALSE)
    solution <- swap(solution, id[1], id[2])
  }
  return(solution)
}