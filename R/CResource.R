# CLASS DEFINITION --------------------------------------------------------

setClassUnion("NumORNull", c("numeric", "NULL"))

#' An S4 class to represent computational resources
#'
#' @slot time_total A positive real number representing the total time available. It can be null, indicating that there is no time limit
#' @slot evaluations_total A positive integer number representing the total number of evaluations available. It can be null, indicating that there is no evaluations limit
#' @slot iterations_total A positive integer number representing the total number of iterations available. It can be null, indicating that there is no iterations limit 
#' @slot time_consumed A positive real number representing the time consumed.
#' @slot evaluations_consumed A positive integer number representing the number of evaluations consumed.  
#' @slot iterations_consumed A positive integer number representing the number of iterations consumed.  
setClass(
  Class = "CResource", 
  representation = representation(time_total = "NumORNull",
                                  evaluations_total = "NumORNull",
                                  iterations_total = "NumORNull",
                                  time_consumed = "numeric",
                                  evaluations_consumed = "numeric",
                                  iterations_consumed = "numeric")
)

not.valid<-function (x){
  !is.null(x) && x-floor(x)!=0
}

setValidity(
  Class = "CResource", 
  method = function(object){
    if (min(object@time_total , object@time_consumed) < 0) stop ("The time has to be a positive value")
    if (min(object@evaluations_total , object@evaluations_consumed) < 0) stop ("The number of evaluations has to be a positive value")
    if (min(object@iterations_total , object@iterations_consumed) < 0) stop ("The number of iterations has to be a positive value")
    if (not.valid(object@iterations_total) | not.valid(object@iterations_consumed)) stop("The number of iterations has to be an integer value")
    if (not.valid(object@evaluations_total) | not.valid(object@evaluations_consumed)) stop("The number of evaluations has to be an integer value")
    return (TRUE)
  }
)

# GENERIC METHODS ---------------------------------------------------------
#' @title Function to increase the consumed resources
#'
#' @description This function increases the consumed resources
#' @param resource Object of class \code{\link{CResource}} to which the consumption will be added
#' @param t Time to add
#' @param ev Number of evaluations to add
#' @param it Number of iterations to add
#' @return A new object of class \code{\link{CResource}} with the updated consumed resources

setGeneric(name = "add.consumed", def = function(resource,t=0,ev=0,it=0){standardGeneric("add.consumed")})

setMethod(
  f="add.consumed", 
  signature = "CResource", 
  definition = function(resource,t=0,ev=0,it=0) {
    aux<-c(t,ev,it)
    if (length(aux)!=3 || !all(!is.na(aux)) || min(aux)<0) stop("Only positive values are accepted for all the parameters")
    
    tc <- resource@time_consumed + t
    ec <- resource@evaluations_consumed + round(ev)
    ic <- resource@iterations_consumed + round(it)
    
    new("CResource" , 
        time_total = resource@time_total , time_consumed = tc ,
        evaluations_total = resource@evaluations_total , evaluations_consumed = ec , 
        iterations_total = resource@iterations_total , iterations_consumed = ic)
  }
)

#' @title Function check what computational resources are finished (if any)
#'
#' @description This function determines what (time, evaluations and/or iterations) is finished
#' @param resource Object of class \code{\link{CResource}} to which the values will be substracted
#' @return A vector of characters that may include \code{'time'}, \code{'evaluations'} and/or \code{'iterations'}. In case it is empty, there are still resources available

setGeneric(name = "what.finished", def = function(resource){standardGeneric("what.finished")})

setMethod(
  f="what.finished", 
  signature = "CResource", 
  definition = function(resource) {
    res<-vector()
    if (!is.null(resource@time_total) && 
          resource@time_total<=resource@time_consumed){
      res<-c(res,"time")
    } 
    if (!is.null(resource@evaluations_total) && 
          resource@evaluations_total <= resource@evaluations_consumed){ 
      res<-c(res,"evaluations")
    }
    if (!is.null(resource@iterations_total) && 
          resource@iterations_total<=resource@iterations_consumed){ 
      res<-c(res,"iterations")
    }
    res    
  }
)


#' @title Function check whether the computational resources are finished
#'
#' @description This function determines what (time, evaluations and/or iterations) is finished
#' @param resource Object of class \code{\link{CResource}} to which the values will be substracted
#' @return A logical value, \code{TRUE} if any of the resources is finished

setGeneric(name = "is.finished", def = function(resource){standardGeneric("is.finished")})

setMethod(
  f="is.finished", 
  signature = "CResource", 
  definition = function(resource) {
    length(what.finished(resource))!=0
  } 
)


#' @title Function to get the remaining time available
#'
#' @description This function returns the remaining time available
#' @param resource Object of class \code{\link{CResource}} whose information we are quering
#' @return Remaining time

setGeneric(name = "remaining.time", def = function(resource){standardGeneric("remaining.time")})

setMethod(
  f="remaining.time", 
  signature = "CResource", 
  definition = function(resource) {
    ifelse(is.null(resource@time_total) , 
           NULL , 
           max(0 , resource@time_total - resource@time_consumed))
  }
)

#' @title Function to get the remaining evaluations available
#'
#' @description This function returns the remaining evaluations available
#' @param resource Object of class \code{\link{CResource}} whose information we are quering
#' @return Remaining number of evaluations

setGeneric(name = "remaining.evaluations", def = function(resource){standardGeneric("remaining.evaluations")})

setMethod(
  f="remaining.evaluations", 
  signature = "CResource", 
  definition = function(resource) {
    ifelse(is.null(resource@evaluations_total) , 
           NULL , 
           max(0 , resource@evaluations_total - resource@evaluations_consumed))
  }
)

#' @title Function to get the remaining iterations available
#'
#' @description This function returns the remaining iterations available
#' @param resource Object of class \code{\link{CResource}} whose information we are quering
#' @return Remaining time

setGeneric(name = "remaining.iterations", def = function(resource){standardGeneric("remaining.iterations")})

setMethod(
  f="remaining.iterations", 
  signature = "CResource", 
  definition = function(resource) {
    ifelse(is.null(resource@iterations_total) , 
           NULL , 
           max(0 , resource@iterations_total - resource@iterations_consumed))
  }
)

#' @title Function to get the consumed time
#'
#' @description This function returns the time consumed so far
#' @param resource Object of class \code{\link{CResource}} whose information we are quering
#' @return Consumed time

setGeneric(name = "consumed.time", def = function(resource){standardGeneric("consumed.time")})

setMethod(
  f="consumed.time", 
  signature = "CResource", 
  definition = function(resource) {
    resource@time_consumed
  }
)


#' @title Function to get the number of evaluations consumed
#'
#' @description This function returns the number of evaluations consumed so far
#' @param resource Object of class \code{\link{CResource}} whose information we are quering
#' @return Number of evaluations consumed

setGeneric(name = "consumed.evaluations", def = function(resource){standardGeneric("consumed.evaluations")})

setMethod(
  f="consumed.evaluations", 
  signature = "CResource", 
  definition = function(resource) {
    resource@evaluations_consumed
  }
)


#' @title Function to get the number of iterations consumed
#'
#' @description This function returns the number of iterations consumed so far
#' @param resource Object of class \code{\link{CResource}} whose information we are quering
#' @return Number of iterations consumed

setGeneric(name = "consumed.iterations", def = function(resource){standardGeneric("consumed.iterations")})

setMethod(
  f="consumed.iterations", 
  signature = "CResource", 
  definition = function(resource) {
    resource@iterations_consumed
  }
)

# CONSTRUCTORS ------------------------------------------------------------

cresource <- function (time = NULL, evaluations = NULL, iterations = NULL){
  new("CResource" , 
      time_total = time , time_consumed = 0 ,
      evaluations_total = evaluations , evaluations_consumed = 0 , 
      iterations_total = iterations , iterations_consumed = 0)
}
