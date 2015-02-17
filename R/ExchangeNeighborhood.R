
#' An S4 class to represent exchange neighborhood for permutations
#'
#' @slot time_total A positive real number representing the total time available. It can be null, indicating that there is no time limit
setClass(
  Class = "ExchangeNeighborhood", 
  representation = representation(base = "Permutation" , 
                                  position.list = "matrix" , 
                                  random = "logical" ,
                                  id = "numeric")
)

setValidity(
  Class = "ExchangeNeighborhood", 
  method = function(object){
    exchanges <- do.call(rbind,sapply(1:(length(object@base)-1), FUN=function(x) cbind((x),(x+1):length(object@base))))
    if (!all(object@position.list %in% exchanges)) stop("Some of the defined exchanges are not adequate")
    if (!all(exchanges %in% object@position.list)) stop("Not all possible exchanges are considered")
    if (object@id!=1) stop ("The first id to be used has to be 1")
    return (TRUE)
  }
)


# GENERIC METHODS ---------------------------------------------------------

setMethod(
  f="next.neighbor", 
  signature = "ExchangeNeighborhood", 
  definition = function(neighborhood) {
    if (has.more.neighbors(neighborhood)){
      ## We do not use directly the id, but the position in the postion list!!
      nxt <- swap(neighborhood@base,neighborhood@position.list[neighborhood@id,1], neighborhood@position.list[neighborhood@id,2])
      
      ## Update the object
      ## obtain the global name of the variable to modify
      objectGlobalName <- deparse(substitute(neighborhood))
      neighborhood@id <- neighborhood@id + 1
      ## assign the local variable to the global variable 
      assign(objectGlobalName,neighborhood,envir=parent.frame())  
    }else{
      nxt <- NULL
    }
    nxt
  })


setMethod(
  f="has.more.neighbors", 
  signature = "ExchangeNeighborhood", 
  definition = function(neighborhood) {
    n <- length(neighborhood@base)
    neighborhood@id <= n*(n-1)/2
  })


setMethod(
  f="reset.neighborhood", 
  signature = "ExchangeNeighborhood", 
  definition = function(neighborhood , solution) {
    if(length(solution)!=length(neighborhood@base)) stop ("The new solution is not of the correct size")
    ## obtain the global name of the variable to modify
    objectGlobalName <- deparse(substitute(neighborhood))
    neighborhood@id <- 1
    neighborhood@base <- solution
    ## If the search is random, shuffle the position list
    if (neighborhood@random) neighborhood@position.list <- neighborhood@position.list[sample(nrow(neighborhood@position.list)),]
    ## assign the local variable to the global variable
    assign(objectGlobalName,neighborhood,envir=parent.frame())  
  })



# CONSTRUCTOR -------------------------------------------------------------

exchangeNeighborhood<-function(base,random = FALSE){
  n <- length(base)
  ## Create all possible exchanges
  exchanges <- do.call(rbind,sapply(1:(n-1), FUN=function(x) cbind((x),(x+1):n)))
  if (random){
    positions <- exchanges[sample(nrow(exchanges)),]
  }else{
    positions <- exchanges
  }
  new("ExchangeNeighborhood",base=base, position.list=positions, random=random, id=1)
}


