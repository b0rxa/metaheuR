#' Rosenbrock function to evaluate numeric algorithms
#' 
#' This function implements Rosenbrock's function
#' @param size Number of dimensions
#' @return A list with two elements, the evaluation function (\code{evaluate}) and the number of dimensions (\code{size})
#' @family Continuous problems
#' @examples
#' rs <- rosenbrockProblem(3)
#' rs$evaluate(c(2, -1, 1))
#' rs$evaluate(c(2, 1, 1))
#' # The only optimum for three dimensions is at 1,1,1
#' rs$evaluate(c(1, 1, 1))
#' 
rosenbrockProblem <- function(size){
  evaluate <- function(solution) {
    if (length(solution) != size) {
      stop("The solution should be a numeric vector of size ", size)
    }    
    id <- 1:(size - 1)
    f <- sum((1 - solution[id])^2 + 100 * (solution[id + 1] - solution[id]^2)^2)
    return(f)
  }
  return(list(evaluate=evaluate, size=size))
}