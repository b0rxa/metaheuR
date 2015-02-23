
#' An S4 class to represent flip neighborhoods for binary vectors
#'
#' @slot base A logical vector representing the base solution whose neighborhood we will explore
#' @slot position.list \code{\linkS4class{Permutation}} indicating the oreder in which the exploration is carried out
#' @slot random A logical value indicating whether the exploration is at random or not
#' @slot id Numeric value indicating the current position (in the \code{position.list}) that will be used to generate a new neighbor
#' @details The new neighbors are generated modifying each position of the vector individually, replacing it with the negation of the current value. The modifications are carried out using the \code{pair.list}, which is iterated using the order defined by \code{index.list}

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

#' Bais consturctor of flip neighborhoods
#' 
#' This function creates an object of class \code{\linkS4class{FlipNeighborhood}}
#' 
#' @family neighborhoods
#' @param base Base solution for the neighborhood. It has to be a logical vector
#' @param random A logical value indicating whether the exploration should be done at random
#' @return An object of class \code{\linkS4class{FlipNeighobrhood}}
#' @seealso \code{\link{hasMoreNeighbors}} \code{\link{resetNeighborhood}} \code{\link{nextNeighbor}}


flipNeighborhood<-function(base,random = FALSE){
  n <- length(base)
  if (random){
    positions <- random.permutation(n)
  }else{
    positions <- identity.permutation(n)
  }
  new("FlipNeighborhood",base=base, position.list=positions, random=random, id=1)
}
