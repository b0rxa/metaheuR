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




#' QAP problem evaluator
#' 
#' This function generates an evaluation function associated
#' with a QAP problem
#' @param fmatrix Flow matrix for the QAP problem
#' @param dmatrix Distance matrix for the QAP problem
#' @return A function that can be used to evaluate solutions for a QAP problem
#' @examples
#' fmatrix<-matrix(runif(100),ncol=10)
#' dmatrix<-matrix(runif(100),ncol=10)
#' qap<-qap.problem(cmatrix)
#' qap$evaluate<-random.permutation(10)
#' eval(sol)
qap.problem<-function(fmatrix, dmatrix){
  if (diff(dim(fmatrix))!=0) stop ("The flow matrix should be square")
  if (diff(dim(dmatrix))!=0) stop ("The distance matrix should be square")
  if (all(dim(fmatrix)==dim(dmatrix))) 
    stop ("The flow matrix and the distance matrix should have the same dimension")
  evaluate<-function(solution){
    if (!isClass(solution,"Permutation")) 
      stop("This function only evaluates objects of class permutation")
    if (length(solution)!=dim(fmatrix)[1]) 
      stop("The solution is not of the correct length. It should have ",
           dim(dmatrix)[1]," positions")
    ## Calculate the objective function
    cost<-sum(fmatrix*dmatrix[as.numeric(solution),as.numeric(solution)])
    return(cost)
  }
  list(evaluate = evaluate)
}