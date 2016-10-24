#' An S4 class to represent distributions based the Mallows Model
#'
#' @slot mode The central permutation of the model.
#' @slot theta Theta parameter of the model
#' @slot distance Type of distance to be used in the model
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
    
    samp <- rmm(n=nsim, sigma0=s0, theta=object@theta, 
                 dist.name=object@distance)
    
    # Convert the sample matrix to a list of objects of class Permutation
    f <- function(vec) {
      return(permutation(vec))
    }
    
    return(apply(samp, MARGIN=1, FUN=f))
  })


# CONSTRUCTOR ------------------------------------------------------------------

#' Baisic consturctor of Mallows models
#' 
#' This function creates an object of class \code{\linkS4class{MallowsModel}}
#' 
#' @family EDA
#' @param data A list with the permutations from where the model will be learned
#' @param distance Type of distance to be used in the model. By default, it is set to \code{Kendall}
#' @param tmin Minimum value for the theta parameter. If \code{NULL} (default value) no bound is set
#' @param tmax Maximum value for the theta parameter. If \code{NULL} (default value) no bound is set
#' @param ... Ignored
#' @return An object of class \code{\linkS4class{MallowsModel}} that represents the learned model
#' 
mallowsModel <- function(data, distance="Kendall", tmin=NULL, tmax=NULL, ...) {
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
  
  if (!is.null(tmin) && !is.null(tmax) && tmin >= tmax) {
    stop("The maximum value of theta has to be greater than the minimum value of theta")
  }
  
  # Convert the list into a matrix
  mat <- do.call(rbind, lapply(data, as.numeric))
  model <- lmm(mat, dist.name=distance, ...)
  
  # Check the bounds
  model$theta <- min(max(model$theta, tmin), tmax)
  
  obj <- new("MallowsModel", mode=permutation(model$mode), theta=model$theta, 
             distance=distance)
  return(obj)
}
