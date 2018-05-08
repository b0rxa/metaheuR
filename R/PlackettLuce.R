#' An S4 class to represent distributions based on Plackett Luce distribution model.
#'
#' @slot parameters The weight parameters of the learned population.
#' 

setClass(
  Class="PlackettLuce", 
  representation=representation(parameters="numeric")
)
# GENERIC METHODS ---------------------------------------------------------
setMethod(
  f="simulate", 
  signature="PlackettLuce", 
  definition=function(object, nsim=1, seed=NULL, ...) {
    new.population <- c()
    parameters <- object@parameters # create a copy to manipulate it
    # Iterate through nsim
    for (k in 1:nsim){
      # Create a random number for roulette wheel
      ac <- 0
      rnd <- 0
      l <- sample(x=0, size=length(parameters), replace = T)
      for(i in 1:length(parameters)){
        rnd <- runif(1, 0, sum(parameters))
        ac <- parameters[1] # get the first position
        j <- 1 # position of the parameters vector
        while(ac <= rnd){
          j <- j + 1
          ac <- ac + parameters[j]
        }
        l[i] <- j
        parameters[j] <- 0
      }
      l <- permutation(l)
      new.population <- c(new.population, l)
    }
    return(new.population)
  })

# CONSTRUCTOR -------------------------------------------------------------

#' This function creates an object of class \code{\linkS4class{PlackettLuce}}
#' 
#' @family EDA
#' @param data The dataframe containing the initial-population to contruct the Plackett-Luce model.
#' @param maxIter Optional. Maximum iterations for the MM algorithm.
#' @param ... Ignored
#' @return An object of class \code{\linkS4class{PlackettLuce}} that includes the weight parameter vector of the given data
#' 
plackettLuce <- function(data, maxIter, ...) {

  
  # This code is based on Hunter's script of Matlab
  if(missing(maxIter)){
    maxIter <- 10000
  }
  if(class(data) == "list"){
    data <- c(data, permutation(rev(data[[1]]@permutation)))
     # We can create the object
    # Transform data to a matrix
    P <- length(data[[1]]@permutation) # Problem size
    N <- length(data) # Pop size
    M <- P # Problem size
    f <- matrix(data = 0, nrow=P, ncol=N) # f(i,j) = one who placed i in contenst j
    r <- matrix(data = 0, nrow=M, ncol=N) # r(i,j) = place of i in contest j, modified so that
    
    for(i in 1:N){
      ind <- data[[i]]@permutation
      for(j in 1:M){
        f[j,i] <- ind[j]
        r[ind[j], i] <- j + P*(i-1)
      }
    }
    r2 <- r
    
    w <- matrix(data = 0, nrow = M, ncol = 1) # w(i) = # times i placed higher than last
    pp <- rep(x=M, N)
    for(i in 1:N){
      tmp <- f[1:(pp[i]-1), i]
      w[tmp] <- 1 + w[tmp]
    }
    pp<- pp + seq(0,((N-1)*P),by = P)
    gamma <- matrix(1,M,1)
    dgamma <- matrix(1,M,1)
    iterations <- 0
    while((norm(x=dgamma ,type="2") > 1e-09) && iterations <= maxIter){
      iterations <- iterations + 1
      g <- f
      g[f>0] <- gamma[f[f>0]]
      aux <- g
      for(i in (P-1):1){
        for(j in 1:N){
          aux[i,j] <- g[i,j]+aux[i+1,j]
        }
      }
      g <- aux
      
      g[P,] <- 0
      
      g[g>0] <- 1./g[g>0]
      # At this point, g(i,j) should be the reciprocal of the sum of gamma's 
      # for places i and higher in the  jth contest exceppt for i=lastplace.

      g <- getColCumsum(g)
      # Now g(i,j) should be the sum of all the denominators for ith place in the jth contest
      
      r2[r>0] <- g[r[r>0]]
      sr2 <- c(unlist(lapply(1:length(r2[,1]), FUN=function(i){
                return(sum(r2[i,]))
      })))
      newgamma <- w/sr2
      dgamma <- newgamma - gamma
      gamma <- newgamma
    }
    params <- gamma/sum(gamma)
    obj <- new("PlackettLuce", parameters = c(params))
  }else{
    stop("The data must be a list")
  }
  return(obj)
}

#' Acumulate the matrix values by columns
#' @param m matrix
#' @return acumulated matrix
getColCumsum <- function(m){
  for(i in 2:(length(m[,1]))){
    m[i,] <- m[(i-1),] + m[i,]
  }
  return(m) 
}


#' Calculates the probabilities of the given model for the population especified.
#' @param model object of the class PlackettLuce.
#' @param data population whoose probabilities are desired to know.
calc.prob <- function(model, data){
  if(class(model) != "PlackettLuce"){
    stop("The model must be of class PlackettLuce")
  }else{
    result <- list()
    for(i in 1:length(data)){
      individual <- data[[i]]@permutation
      prob <- 1
      for(j in 1:(length(individual)-1)){
        if(j > 1){
          prob <- prob * (model@parameters[individual[j]] / sum(model@parameters[-appeared]))
          appeared <- c(appeared, individual[j])
        }else{
          prob <- prob * (model@parameters[individual[j]] / sum(model@parameters))
          appeared <- individual[j]
        }
      }
      appeared <- 0
      result <- c(result, prob)
    }
  }
  return(unlist(result))
}