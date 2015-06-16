
#' An S4 class to represent swap neighborhood for permutations
#'
#' @slot base An object of class \code{\linkS4class{Permutation}}, which will be the solution whose neighborhood we will explore
#' @slot position.list A permutation indicating the order in which the exploration will be carried out. Each time a neighbohr is created, this list is used to get the first position of the swap movement (the other position will be the one inmediately after that in the base permutation)
#' @slot random A logical value indicating whether the exploration is at random or not
#' @slot id Numeric value indicating the current position (in the \code{position.list}) that will be used to generate a new neighbor
#' 
setClass(
  Class="SwapNeighborhood", 
  representation=representation(base="Permutation", 
                                position.list="Permutation",
                                random="logical",
                                id="numeric"))

setValidity(
  Class="SwapNeighborhood", 
  method=function(object) {
    if (length(object@base) - length(object@position.list) != 1) {
      stop ("The positions list should have the length of the base permutation minus 1")
    }
    if (object@id != 1) {
      stop ("The first id to be used has to be 1")
    }
    return (TRUE)
  })

# GENERIC METHODS ---------------------------------------------------------

setMethod(
  f="nextNeighbor", 
  signature="SwapNeighborhood", 
  definition=function(neighborhood) {
    if (hasMoreNeighbors(neighborhood)) {
      # We do not use directly the id, but the position in the postion list!!
      nxt <- swap(neighborhood@base, 
                  neighborhood@position.list[neighborhood@id], 
                  neighborhood@position.list[neighborhood@id]+1)
      
      # Update the object
      # obtain the global name of the variable to modify
      objectGlobalName <- deparse(substitute(neighborhood))
      neighborhood@id <- neighborhood@id + 1
      # assign the local variable to the global variable 
      assign(objectGlobalName, neighborhood, envir=parent.frame())  
    } else {
      nxt <- NULL
    }
    return(nxt)
  })


setMethod(
  f="hasMoreNeighbors", 
  signature="SwapNeighborhood", 
  definition=function(neighborhood) {
    return(neighborhood@id <= length(neighborhood@position.list))
  })


setMethod(
  f="resetNeighborhood", 
  signature="SwapNeighborhood", 
  definition=function(neighborhood, solution) {
    if(length(solution)!=length(neighborhood@base)) {
      stop ("The new solution is not of the correct size")
    }
    # obtain the global name of the variable to modify
    objectGlobalName <- deparse(substitute(neighborhood))
    neighborhood@id <- 1
    neighborhood@base <- solution
    # If the search is random, shuffle the position list
    if (neighborhood@random) {
      neighborhood@position.list <- randomPermutation(length(neighborhood@position.list))
    }
    # assign the local variable to the global variable
    assign(objectGlobalName, neighborhood, envir=parent.frame())  
  })

# CONSTRUCTOR -------------------------------------------------------------

#' Bais consturctor of swap neighborhoods
#' 
#' This function creates an object of class \code{\linkS4class{SwapNeighborhood}}
#' 
#' @family neighborhoods
#' @param base Base solution for the neighborhood. It has to be an object of class \code{\linkS4class{Permutation}}
#' @param random A logical value indicating whether the exploration should be done at random
#' @return An object of class \code{\linkS4class{SwapNeighborhood}}
#' @seealso \code{\link{hasMoreNeighbors}} \code{\link{resetNeighborhood}} \code{\link{nextNeighbor}}
#' 
swapNeighborhood <- function(base, random=FALSE) {
  n <- length(base)
  if (random) {
    positions <- randomPermutation(n - 1)
  } else {
    positions <- identityPermutation(n - 1)
  }
  obj <- new("SwapNeighborhood", base=base, position.list=positions, random=random, id=1)
  return(obj)
}
