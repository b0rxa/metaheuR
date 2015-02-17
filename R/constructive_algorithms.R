tsp.greedy <- function (cmatrix){
  diag(cmatrix) <- NA
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
  names(solution)<-NULL
  permutation(vector = solution)
}