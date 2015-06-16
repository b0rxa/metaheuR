#' An S4 class to represent distributions based on univariate marginals
#'
#' @slot prob.table A matrix with the marginal probabilities. Each column is a variable and each row a possible value.
#' @slot levels A list of levels in case the model is constructed over factors. If it is to be used with logical values, then this slot will be \code{NULL}.
#' 
setClass(
  Class="UnivariateMarginals", 
  representation=representation(prob.table="matrix", 
                                binary="logical")
)

setValidity(
  Class="UnivariateMarginals", 
  method=function(object) {
    m <- object@prob.table
    if (!all(object@prob.table <= 1 & object@prob.table >= 0)) {
      stop("All the values in the prob table have to be between 0 and 1")
    }
    
    if (object@binary) {
      if (nrow(object@prob.table) != 1) {
        stop("For models over logical variables the 'prob.table' should only ",
             "contain one row, representing the probability of the variable being 'TRUE'")
      }
    } else {
      if (is.null(rownames(object@prob.table))) {
        stop("For factor models the rows in the probability table have to be named ",
             "with the level values")
      }
      if (nrow(object@prob.table) < 2) {
        stop("For factor models there should be at least two possible values")
      }
      # Note that, due to rounding problems, the summ may not be exactly 1
      if (max(colSums(object@prob.table) - 1) > 1 / (100 * nrow(object@prob.table))) {
        stop("The column-wise sums in 'prob.table' should be 1")
      }
    }
    return (TRUE)
  })


# GENERIC METHODS ---------------------------------------------------------
setMethod(
  f="simulate", 
  signature="UnivariateMarginals", 
  definition=function(object, nsim=1, seed=NULL, ...) {
    
    if(!is.null(seed)) {
      set.seed(seed)
    }
    
    n <- ncol(object@prob.table)
    
    if (object@binary) {  # There is only one value per column, the probability of being TRUE
      getSample <- function(i) {
        return(runif(n)<object@prob.table[1,])
      }
    }else{
      getSample <- function(i) {
        levels <- rownames(object@prob.table)
        # Get a level according to the probabilities
        sample <- sapply(1:n, 
                         FUN=function(j) {
                           return(sample(x=levels,
                                         size=1,
                                         prob=object@prob.table[,j]))
                         })
        # Return a factor
        return(factor(sample, levels))
      }
    }
    
    # Create all the samples
    return(lapply(1:nsim, FUN=getSample))
  })


# CONSTRUCTOR -------------------------------------------------------------

#' Bais consturctor of flip neighborhoods
#' 
#' This function creates an object of class \code{\linkS4class{FlipNeighborhood}}
#' 
#' @family EDA
#' @param data A matrix or data frame with the values to estimate the marginal probabilities. The columns represent the variables and the rwos the samples. Values can be logical, factors or characters 
#' @param ... Ignored
#' @return An object of class \code{\linkS4class{UnivariateMarginals}} that includes the marginal probability distributions
#' 
univariateMarginals <- function(data, ...) {
  if (class(data) != "list") {
    stop ("The data has to be a list")
  }
  # In case we have a list, convert it to a data frame to manipulate the data column-wise
  cl <- class(data[[1]])
  df <-data.frame(data)
  
  if (cl == 'factor') {
    levels <- levels(data[[1]])
    probs <- apply(df, MARGIN=1, 
                   FUN = function(x) {
                     return(table(factor(x, levels)))
                   })
    probs <- probs / length(data)
  } else if (cl == 'logical') {
    probs <- matrix(rowSums(df) / length(data), nrow=1)
  } else {
    stop ("Only logical and factor vectors can be used to build the model")
  }
  
  obj <- new("UnivariateMarginals", prob.table=probs, binary=(cl == "logical"))
  return(obj)
}
