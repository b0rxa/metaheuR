#' Knapsac problem evaluator
#' 
#' This function generates an evaluation, validity and correction functions associated  with a classical knapsack problem
#' @param weight Vector with the weights associated to each item
#' @param value Vector with the value associated to each item
#' @param limit Maximum weight allowed
#' @return A list of functions to be used to solve a knapsack problem. This includes the functions \code{evaluate}, for the evaluation of a solution, \code{is.valid}, to check whetehr a solution is valid or not and 'correct', to correct a non-valid solution; all the functions have a single argument,  \code{solution}, representing the solution considered. Note that, given that the goal in all the algorithms in the library is minimizing the objective function, the \code{evaluate}. The solutions have to be vectors of logic values, indicating which items are included in the solution
#' @examples
#' ## Problem of size 100 with random vectors and limit
#' n<-100
#' w<-runif(n)
#' v<-runif(n)
#' l<-sum(w[runif(n)>0.5])
#' knp <- knapsack.problem(w,v,l)
#' ## Use of the functioins
#' rnd.solution <- runif(n)>0.5
#' knp$evaluate(rnd.solution)
#' knp$is.valid(rnd.solution)
#' corrected.solution <- knp$correct (rnd.solution)
#' knp$is.valid(corrected.solution)

knapsack.problem<-function(weight , value , limit){
  if (length(weight) != length(value)) stop ("The two vectors, 'weight' and 'value', should be of the same length")
  if (min(weight,value) < 0) stop ("The two vectors, 'weight' and 'value', should not contain negative values")
  if (limit <= 0) stop ("The limit should be a strictly positive value")
  
  evaluate <- function(solution){
    if (!is.logical(solution) | length(solution) != length(weight)) stop(paste("The solution should be a logical vector of size" , length(weight)))
    return(-1 * sum(value[solution]))
  }
  
  is.valid <- function(solution){
    if (!is.logical(solution) | length(solution) != length(weight)) stop(paste("The solution should be a logical vector of size" , length(weight)))
    return(sum(weight[solution]) <= limit)
  }
  
  correct <- function(solution){
    if (!is.logical(solution) | length(solution) != length(weight)) stop(paste("The solution should be a logical vector of size" , length(weight)))
    while(!is.valid(solution)){
        max.in <- max(weight[solution])
        id <- which(weight==max.in & solution)[1]
        solution[id] <- FALSE
    }
    return(solution)
  }
  
  return(list(evaluate = evaluate, is.valid = is.valid, correct = correct))
}