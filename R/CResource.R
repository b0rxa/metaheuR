# CLASS DEFINITION --------------------------------------------------------

setClassUnion("NumORNull", c("numeric", "NULL"))

#' An S4 class to represent computational resources
#'
#' @slot time.total A positive real number representing the total time available. It can be null, indicating that there is no time limit
#' @slot evaluations.total A positive integer number representing the total number of evaluations available. It can be null, indicating that there is no evaluations limit
#' @slot iterations.total A positive integer number representing the total number of iterations available. It can be null, indicating that there is no iterations limit 
#' @slot time.consumed A positive real number representing the time consumed.
#' @slot evaluations.consumed A positive integer number representing the number of evaluations consumed.  
#' @slot iterations.consumed A positive integer number representing the number of iterations consumed.  
setClass(
  Class="CResource", 
  representation=representation(time.total="NumORNull",
                                evaluations.total="NumORNull",
                                iterations.total="NumORNull",
                                time.consumed="numeric",
                                evaluations.consumed="numeric",
                                iterations.consumed="numeric")
)

notValid <- function (x){
  # Auxiliar function to check the validity of the values
  # Args:
  #   x: Value to check
  # Return:
  #   Check that x is either null or an integer
  #
  !is.null(x) && x-floor(x)!=0
}

setValidity(
  Class="CResource", 
  method=function(object) {
    if (min(object@time.total, object@time.consumed) < 0) {
      stop ("The time has to be a positive value")
    }
    if (min(object@evaluations.total, object@evaluations.consumed) < 0) {
      stop ("The number of evaluations has to be a positive value")
    }
    if (min(object@iterations.total, object@iterations.consumed) < 0) {
      stop ("The number of iterations has to be a positive value")
    }
    if (notValid(object@iterations.total) | notValid(object@iterations.consumed)) {
      stop("The number of iterations has to be an integer value")
    }
    if (notValid(object@evaluations.total) | notValid(object@evaluations.consumed)) {
      stop("The number of evaluations has to be an integer value")
    }
    return(TRUE)
  })

# GENERIC METHODS ---------------------------------------------------------
#' @title Function to increase the consumed resources
#'
#' @description This function increases the consumed resources
#' @param resource Object of class \code{\linkS4class{CResource}} to which the consumption will be added
#' @param t Time to add
#' @param ev Number of evaluations to add
#' @param it Number of iterations to add
#' @return This method does not return any output. For efficiecy reasons the object passed is modified. This can lead to problems if not handled properly, so use with caution!
#' 
setGeneric(name="addConsumed", 
           def=function(resource, t=0, ev=0, it=0){
             standardGeneric("addConsumed")
           })

setMethod(
  f="addConsumed", 
  signature="CResource", 
  definition=function(resource,t=0,ev=0,it=0) {
    aux<-c(t, ev, it)
    if (length(aux) != 3 || !all(!is.na(aux)) || min(aux) < 0) {
      stop("Only positive values are accepted for all the parameters")
    }
    # Replace the object passed as parameter
    objectGlobalName <- deparse(substitute(resource))
    resource@time.consumed <- resource@time.consumed + t
    resource@evaluations.consumed <- resource@evaluations.consumed + round(ev)
    resource@iterations.consumed <- resource@iterations.consumed + round(it)
    assign(objectGlobalName, resource, envir=parent.frame())  
  })

#' @title Function check what computational resources are finished (if any)
#'
#' @description This function determines what (time, evaluations and/or iterations) is finished
#' @param resource Object of class \code{\linkS4class{CResource}} to which the values will be substracted
#' @return A vector of characters that may include \code{'time'}, \code{'evaluations'} and/or \code{'iterations'}. In case it is empty, there are still resources available
#' 
setGeneric(name="whatFinished", 
           def=function(resource) {
             standardGeneric("whatFinished")
           })

setMethod(
  f="whatFinished", 
  signature="CResource", 
  definition=function(resource) {
    res <- vector()
    if (!is.null(resource@time.total) && 
          resource@time.total <= resource@time.consumed) {
      res <- c(res, "time")
    } 
    if (!is.null(resource@evaluations.total) && 
          resource@evaluations.total <= resource@evaluations.consumed) { 
      res <- c(res, "evaluations")
    }
    if (!is.null(resource@iterations.total) && 
          resource@iterations.total <= resource@iterations.consumed) { 
      res <- c(res,"iterations")
    }
    return(res)
  })


#' @title Function check whether the computational resources are finished
#'
#' @description This function determines what (time, evaluations and/or iterations) is finished
#' @param resource Object of class \code{\linkS4class{CResource}} to which the values will be substracted
#' @return A logical value, \code{TRUE} if any of the resources is finished
#' 
setGeneric(name="isFinished", 
           def=function(resource) {
             standardGeneric("isFinished")
           })

setMethod(
  f="isFinished", 
  signature="CResource", 
  definition=function(resource) {
    return(length(whatFinished(resource)) != 0)
  })


#' @title Function to get the remaining time available
#'
#' @description This function returns the remaining time available
#' @param resource Object of class \code{\linkS4class{CResource}} whose information we are quering
#' @return Remaining time
#' 
setGeneric(name = "getRemainingTime", 
           def=function(resource) {
             standardGeneric("getRemainingTime")
           })

setMethod(
  f="getRemainingTime", 
  signature="CResource", 
  definition=function(resource) {
    if(is.null(resource@time.total)) {
      t <- NULL
    } else {
      t <- max(0, resource@time.total - resource@time.consumed)
    }
    return(t)
  })

#' @title Function to get the remaining evaluations available
#'
#' @description This function returns the remaining evaluations available
#' @param resource Object of class \code{\linkS4class{CResource}} whose information we are quering
#' @return Remaining number of evaluations
#' 
setGeneric(name="getRemainingEvaluations", 
           def=function(resource) {
             standardGeneric("getRemainingEvaluations")
           })

setMethod(
  f="getRemainingEvaluations", 
  signature="CResource", 
  definition=function(resource) {
    if(is.null(resource@evaluations.total)) {
      t <- NULL
    } else {
      t <- max(0, resource@evaluations.total - resource@evaluations.consumed)
    }
    return(t)
  })

#' @title Function to get the remaining iterations available
#'
#' @description This function returns the remaining iterations available
#' @param resource Object of class \code{\linkS4class{CResource}} whose information we are quering
#' @return Remaining time
#' 
setGeneric(name="getRemainingIterations", 
           def=function(resource) {
             standardGeneric("getRemainingIterations")
           })

setMethod(
  f="getRemainingIterations", 
  signature="CResource", 
  definition=function(resource) {
    if(is.null(resource@iterations.total)) {
      t <- NULL
    } else {
      t <- max(0, resource@iterations.total - resource@iterations.consumed)
    }
    return(t)
  }
)

#' @title Function to get the consumed time
#'
#' @description This function returns the time consumed so far
#' @param resource Object of class \code{\linkS4class{CResource}} whose information we are quering
#' @return Consumed time
#' 
setGeneric(name="getConsumedTime", 
           def=function(resource) {
             standardGeneric("getConsumedTime")
           })

setMethod(
  f="getConsumedTime", 
  signature="CResource", 
  definition=function(resource) {
    return(resource@time.consumed)
  }
)


#' @title Function to get the number of evaluations consumed
#'
#' @description This function returns the number of evaluations consumed so far
#' @param resource Object of class \code{\linkS4class{CResource}} whose information we are quering
#' @return Number of evaluations consumed
#' 
setGeneric(name="getConsumedEvaluations", 
           def=function(resource) {
             standardGeneric("getConsumedEvaluations")
           })

setMethod(
  f="getConsumedEvaluations", 
  signature="CResource", 
  definition=function(resource) {
    return(resource@evaluations.consumed)
  }
)


#' @title Function to get the number of iterations consumed
#'
#' @description This function returns the number of iterations consumed so far
#' @param resource Object of class \code{\linkS4class{CResource}} whose information we are quering
#' @return Number of iterations consumed
#' 
setGeneric(name="getConsumedIterations", 
           def=function(resource) {
             standardGeneric("getConsumedIterations")
           })

setMethod(
  f="getConsumedIterations", 
  signature="CResource", 
  definition=function(resource) {
    return(resource@iterations.consumed)
  }
)

# CONSTRUCTORS ------------------------------------------------------------

cResource <- function (time=NULL, evaluations=NULL, iterations=NULL){
  cr <- new("CResource",
            time.total=time, time.consumed=0 ,
            evaluations.total=evaluations, evaluations.consumed=0,
            iterations.total=iterations, iterations.consumed=0)
  return(cr)
}
