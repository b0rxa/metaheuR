#' Constructive greedy algorithm for the TSP problem
#' 
#' This function implements a simple constructive greedy algorithm for the TSP problem.
#' @param cmatrix Cost matrix associated to the TSP instance
#' @param cl.size Size of candidate list. 
#' @return A permutation containing a solution for the problem
#' @details The algorithm builds the solution iteratively, selecting, at each step, the closest city to the one added in the previous step.
#' @examples
#' n <- 10
#' cost.matrix <- matrix(runif(n^2) , ncol = n)
#' tsp.greedy (cost.matrix)


tsp.greedy <- function (cmatrix, cl.size){
  diag(cmatrix) <- NA
  # If the TSP problem is symmetric, we double the number of candidates to consider both possibilities
  if (cl.size==1){
    ## Get the position with the minimum value
    best.pair <- which(cmatrix == min(cmatrix,na.rm=T), arr.ind=TRUE)
    solution <- c(best.pair[1,1],best.pair[1,2])
    ## Remove from the candidate list the two columns
    cmatrix[ , best.pair[1,]]<-NA
    ## Also remove the row of the first city
    cmatrix[best.pair[1,1],]<-NA
    for (i in 3:dim(cmatrix)[1]){
      ## Get the closest city
      next.city <- which.min (cmatrix[solution[i-1],])
      ## Add to the solution
      solution <- append(solution , next.city)
      ## Update the matrix
      cmatrix[solution[i-1],] <- NA
      cmatrix[ , next.city] <- NA
  }
  }else{
    if (cl.size<0){stop("candidate.list must be a positive number.")}
    ## Calculate maximum number of possible candidates
    if (isSymmetric(cmatrix)){
      num.candidates <- min(cl.size*2, sum(!is.na(cmatrix)))
    }else{
      num.candidates <- min(cl.size, sum(!is.na(cmatrix)))
    }
      ## Order the values in cmatrix, select candidates and choose one randomly
      candidates <- sort(cmatrix)[1:num.candidates]
      best.pair <- which(cmatrix == candidates[sample(1:num.candidates,1)], arr.ind=TRUE)
      solution <- c(best.pair[1,1],best.pair[1,2])
      ## Remove from the candidate list the two columns
      cmatrix[ , best.pair[1,]]<-NA
      ## Also remove the row of the first city
      cmatrix[best.pair[1,1],]<-NA
    
      for (i in 3:dim(cmatrix)[1]){
        ## Calculate maximum number of candidates
        num.candidates <- min(cl.size, sum(!is.na(cmatrix[solution[i-1],])))
        ## Get the candidates and choose one randomly
        next.city <- order(cmatrix[solution[i-1],])[sample(1:num.candidates,1)]
        ## Add to the solution
        solution <- append(solution , next.city)
        ## Update the matrix
        cmatrix[solution[i-1],] <- NA
        cmatrix[ , next.city] <- NA
      }
    
  }
  names(solution)<-NULL
  permutation(vector = solution)
}