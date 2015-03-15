#' @title Binary mutation
#'
#' @description This function takes random positions of a logical vector and changes their value
#' @param solution A logical vector to be mutated.
#' @param ratio Ratio of the positions to be mutated. It has to be a value greater than 0.
#' @examples
#' 
#' F.sol <- rep(F , 10)
#' binary.mutation (F.sol , ratio = 0.1)
#' binary.mutation (F.sol , ratio = 0.1)
#' binary.mutation (F.sol , ratio = 0.5)

binary.mutation <- function (solution , ratio , ...){
  if (ratio<=0) stop("The ratio has to be a strictly positive value.")
  n <- length(solution)
  for (i in 1:n*ratio){
    id <- sample(n , 1)
    solution[id] <- !solution[id]
  }
  solution
}
