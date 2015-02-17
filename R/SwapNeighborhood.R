
#' An S4 class to represent swap neighborhood for permutations
#'
#' @slot time_total A positive real number representing the total time available. It can be null, indicating that there is no time limit
setClass(
  Class = "SwapNeighborhood", 
  representation = representation(base = "Permutation" , 
                                  position.list = "Permutation" , 
                                  random = "logical" ,
                                  id = "numeric")
)

setValidity(
  Class = "SwapNeighborhood", 
  method = function(object){
    if (length(object@base) - length(object@position.list) != 1) stop ("The positions list should have the length of the base permutation minus 1")
    if (object@id!=1) stop ("The first id to be used has to be 1")
    return (TRUE)
  }
)


# GENERIC METHODS ---------------------------------------------------------

setMethod(
  f="next.neighbor", 
  signature = "SwapNeighborhood", 
  definition = function(neighborhood) {
    if (has.more.neighbors(neighborhood)){
      ## We do not use directly the id, but the position in the postion list!!
      nxt <- swap(neighborhood@base,neighborhood@position.list[neighborhood@id], neighborhood@position.list[neighborhood@id]+1)
      
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
  signature = "SwapNeighborhood", 
  definition = function(neighborhood) {
    neighborhood@id <= length(neighborhood@position.list)
  })


setMethod(
  f="reset.neighborhood", 
  signature = "SwapNeighborhood", 
  definition = function(neighborhood , solution) {
    if(length(solution)!=length(neighborhood@base)) stop ("The new solution is not of the correct size")
    ## obtain the global name of the variable to modify
    objectGlobalName <- deparse(substitute(neighborhood))
    neighborhood@id <- 1
    neighborhood@base <- solution
    ## If the search is random, shuffle the position list
    if (neighborhood@random) neighborhood@position.list <- random.permutation(length(neighborhood@position.list))
    ## assign the local variable to the global variable
    assign(objectGlobalName,neighborhood,envir=parent.frame())  
  })



# CONSTRUCTOR -------------------------------------------------------------

swapNeighborhood<-function(base,random = FALSE){
  n <- length(base)
  if (random){
    positions <- random.permutation(n-1)
  }else{
    positions <- identity.permutation(n-1)
  }
  new("SwapNeighborhood",base=base, position.list=positions, random=random, id=1)
}
