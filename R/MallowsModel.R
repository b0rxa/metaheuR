#' An S4 class to represent distributions based on univariate marginals
#'
#' @slot prob.table A matrix with the marginal probabilities. Each column is a variable and each row a possible value.
#' @slot levels A list of levels in case the model is constructed over factors. If it is to be used with logical values, then this slot will be \code{NULL}.
#' 
setClass(
  Class="MallowsModel", 
  representation=representation(mode="Permutation", 
                                theta="numeric", 
                                distance="character")
)

setValidity(
  Class="MallowsModel", 
  method=function(object) {
    if (!object@distance %in% c("Kendall", "Cayley", "Hamming", "Ulam")) {
      stop ("Non valid distance. Valid options are 'Kendall', 'Cayley', ",
            "'Hamming' and 'Ulam'")
    }
    return (TRUE)
  })


# GENERIC METHODS --------------------------------------------------------------

setMethod(
  f="simulate", 
  signature="MallowsModel", 
  definition=function(object, nsim=1, seed=NULL, ...) {
    if(!is.null(seed)) {
      set.seed(seed)
    }
    
    s0 <- as.numeric(object@mode)
    
    f <- function(i) {
      permu <- rmm(n=1, sigma0=s0, theta=object@theta, 
                   dist.name=object@distance)[1, ]
      return(permutation(permu))
    }
    
    return(lapply(1:nsim, f))
  })


# CONSTRUCTOR ------------------------------------------------------------------

#' Bais consturctor of flip neighborhoods
#' 
#' This function creates an object of class \code{\linkS4class{FlipNeighborhood}}
#' 
#' @family EDA
#' @param data A matrix or data frame with the values to estimate the marginal probabilities. The columns represent the variables and the rwos the samples. Values can be logical, factors or characters 
#' @param ... Ignored
#' @return An object of class \code{\linkS4class{UnivariateMarginals}} that includes the marginal probability distributions
#' 
mallowsModel <- function(data, distance="Kendall", ...) {
  if (class(data)!="list") {
    stop ("The data has to be a list")
  }
  
  if (class(data[[1]]) != "Permutation") {
    stop ("The elements in the list have to be objects of class 'Permutation'")
  }
  
  if (!distance %in% c("Kendall", "Cayley", "Hamming", "Ulam")) {
    stop ("Non valid distance. Valid options are 'Kendall', 'Cayley', 'Hamming' ",
          "and 'Ulam'")
  }
  
  # Convert the list into a matrix
  mat <- do.call(rbind, lapply(data, as.numeric))
  model <- lmm(mat, dist.name=distance, ...)
  
  obj <- new("MallowsModel", mode=permutation(model$mode), theta=model$theta, 
             distance=distance)
  return(obj)
}
