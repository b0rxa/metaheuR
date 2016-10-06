#' Knapsac problem evaluator
#' 
#' This function generates an evaluation, validity and correction functions associated  with a classical knapsack problem
#' @param weight Vector with the weights associated to each item
#' @param value Vector with the value associated to each item
#' @param limit Maximum weight allowed
#' @return A list of functions to be used to solve a knapsack problem. This includes the functions \code{evaluate}, for the evaluation of a solution, \code{valid}, to check whetehr a solution is valid or not and 'correct', to correct a non-valid solution; all the functions have a single argument,  \code{solution}, representing the solution considered. Note that, given that the goal in all the algorithms in the library is minimizing the objective function, the \code{evaluate}. The solutions have to be vectors of logic values, indicating which items are included in the solution.
#' @family Problems
#' @examples
#' ## Problem of size 100 with random vectors and limit
#' n <- 100
#' w <- runif(n)
#' v <- runif(n)
#' l <- sum(w[runif(n) > 0.5])
#' knp <- knapsackProblem(w, v, l)
#' # Use of the functioins
#' rnd.solution <- runif(n) > 0.5
#' knp$evaluate(rnd.solution)
#' knp$valid(rnd.solution)
#' corrected.solution <- knp$correct (rnd.solution)
#' knp$valid(corrected.solution)

knapsackProblem <- function(weight, value, limit) {
  if (length(weight) != length(value)) {
    stop ("The two vectors, 'weight' and 'value', should be of the same length")
  }
  
  if (min(weight,value) < 0) {
    stop ("The two vectors, 'weight' and 'value', should not contain negative values")
  }
  
  if (limit <= 0) {
    stop ("The limit should be a strictly positive value")
  }
  
  checkSolution <- function(solution) {
    if (!is.logical(solution) | length(solution) != length(weight)) {
      stop(paste("The solution should be a logical vector of size" , length(weight)))
    }
  }
  
  evaluate <- function(solution) {
    checkSolution(solution)
    return(-1 * sum(value[solution]))
  }
  
  valid <- function(solution) {
    checkSolution(solution)
    return(sum(weight[solution]) <= limit)
  }
  
  correct <- function(solution){
    checkSolution(solution)
    while(!valid(solution)) {
        ratio <- weight / value
        max.in <- max(ratio[solution])
        id <- which(ratio == max.in & solution)[1]
        solution[id] <- FALSE
    }
    return(solution)
  }
  return(list(evaluate=evaluate, valid=valid, correct=correct))
}