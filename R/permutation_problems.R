#' TSP problem evaluator
#' 
#' This function generates an evaluation function associated
#' with a TSP problem
#' @param cmatrix Cost matrix for the TSP problem
#' @return A function that can be used to evaluate solutions for a TSP problem
#' @examples
#' cmatrix<-matrix(runif(100),ncol=10)
#' tsp<-tsp.problem(cmatrix)
#' tsp$evaluate<-random.permutation(10)
#' eval(sol)
tsp.problem<-function(cmatrix){
  if (diff(dim(cmatrix))!=0) stop ("The cost matrix should be square")
  evaluate<-function(solution){
    if (!isClass(solution,"Permutation")) 
      stop("This function only evaluates objects of class permutation")
    if (length(solution)!=dim(cmatrix)[1]) 
      stop("The solution is not of the correct length. It should have ",
           dim(cmatrix)[1]," positions")
    ## Generate the pairs for the positions in the matrix
    ids<-cbind(as.numeric(solution), 
               as.numeric(insert(solution, 1, length(solution))))
    ## Sum the values in the generated positions
    cost<-sum(cmatrix[ids])
    return(cost)
  }
  list(evaluate = evaluate)
}