
setClass(
  Class = "FlipNeighborhood", 
  representation = representation(base = "logical" , 
                                  position.list = "Permutation" , 
                                  random = "logical" ,
                                  id = "numeric")
)

setValidity(
  Class = "FlipNeighborhood", 
  method = function(object){
    if (length(object@base) - length(object@position.list) != 0) stop ("The positions list and the base vector should have the same length")
    if (object@id!=1) stop ("The first id to be used has to be 1")
    return (TRUE)
  }
)


# GENERIC METHODS ---------------------------------------------------------

setMethod(
  f="next.neighbor", 
  signature = "FlipNeighborhood", 
  definition = function(neighborhood) {
    if (has.more.neighbors(neighborhood)){
      ## We do not use directly the id, but the position in the postion list!!
      nxt <- neighborhood@base
      
      nxt[neighborhood@id] <- !nxt[neighborhood@id]
      
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
  signature = "FlipNeighborhood", 
  definition = function(neighborhood) {
    neighborhood@id <= length(neighborhood@position.list)
  })


setMethod(
  f="reset.neighborhood", 
  signature = "FlipNeighborhood", 
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

flipNeighborhood<-function(base,random = FALSE){
  n <- length(base)
  if (random){
    positions <- random.permutation(n)
  }else{
    positions <- identity.permutation(n)
  }
  new("FlipNeighborhood",base=base, position.list=positions, random=random, id=1)
}
