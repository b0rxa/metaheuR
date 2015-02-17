
#' An S4 class to represent swap neighborhood for permutations
#'
#' @slot time_total A positive real number representing the total time available. It can be null, indicating that there is no time limit
setClass(
  Class = "HammingNeighborhood", 
  representation = representation(base = "factor" , 
                                  position.list = "Permutation" ,
                                  level.list = "Permutation" ,
                                  random = "logical" ,
                                  id.pos = "numeric" ,
                                  id.level = "numeric")
)

setValidity(
  Class = "HammingNeighborhood", 
  method = function(object){
    if (length(object@base) != length(object@position.list)) stop ("The positions list and the ase vector should have the length")
    if (length(object@level.list) - length(levels(object@base)) != -1) stop ("The levels list should have a size equal to the number of levels - 1")
    if (object@id.pos!=1) stop ("The first id to be used has to be 1")
    if (object@id.level!=1) stop ("The first id to be used has to be 1")
    return (TRUE)
  }
)


# GENERIC METHODS ---------------------------------------------------------

setMethod(
  f="next.neighbor", 
  signature = "HammingNeighborhood", 
  definition = function(neighborhood) {
    if (has.more.neighbors(neighborhood)){
      ## We do not use directly the id, but the position in the postion list!!
      
      levels <- levels(neighborhood@base)
      nxt <- neighborhood@base
      nxt[neighborhood@position.list[neighborhood@id.pos]] <- 
        levels[neighborhood@levels.list[neighborhood@id.levels]]
      neighborhood@id.levels <- neighborhood@id.levels + 1
      ## If we have explored all the values associated to the current position, now we reset the list of levels 
      
      
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
  signature = "HammingNeighborhood", 
  definition = function(neighborhood) {
    neighborhood@id <= length(neighborhood@position.list)
  })


setMethod(
  f="reset.neighborhood", 
  signature = "HammingNeighborhood", 
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

HammingNeighborhood<-function(base,random = FALSE){
  n <- length(base)
  if (random){
    positions <- random.permutation(n-1)
  }else{
    positions <- identity.permutation(n-1)
  }
  new("HammingNeighborhood",base=base, position.list=positions, random=random, id=1)
}
