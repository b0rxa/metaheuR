#' An S4 class to represent Hamming neighborhoods
#'
#' @slot base A factor vector which will be the solution whose neighborhood we will explore
#' @slot pair.list A data.frame with pairs position, new value 
#' @slot index.list A vector indicating the order in which pair.list is used to generate new neighbors
#' @slot random A logical value indicating whether the exploration is at random or not
#' @slot id.pos Numeric value indicating the current position (in the \code{position.list}) that will be used to generate a new neighbor
#' @details The new neighbors are generated modifying each position of the vector individually, replacing it with each of the possible values (levels of the factor). The modifications are carried out using the \code{pair.list}, which is iterated using the order defined by \code{index.list}
#' 
setClass(
  Class="HammingNeighborhood", 
  representation=representation(base="factor",
                                pair.list="data.frame",
                                index.list="Permutation", 
                                random="logical",
                                id.pos="numeric"))

setValidity(
  Class="HammingNeighborhood", 
  method=function(object) {
    if (!all(names(object@pair.list) %in% c("Position","Value"))) {
      stop ("The list of positions has to be a data frame with two columns named 'Position' and 'Value'")
    }
    
    if (!is.numeric(object@pair.list$Position)) {
      stop ("The column 'Position' in the data frame pair.list has to be numeric")
    }
    n <- length(object@base)
    l <- levels(object@base)
    aux <- lapply (1:n, 
                   FUN=function(i) {
                     r <- data.frame(Position=i,
                                     Value=subset(l, l!=object@base[i]))
                     return(r)
                    })
    pair.list <- do.call(rbind,aux)
    index.list <- 1:dim(pair.list)[1]
    if(!all(sort(object@pair.list$Position) == pair.list$Position)) {
      stop ("The provided list of positions is not valid")
    }
    if(!all(sort(object@pair.list$Value) == sort(pair.list$Value))) {
      stop ("The provided list of positions is not valid")
    }
    if (object@id.pos != 1) {
      stop ("The first id to be used has to be 1")
    }
    return (TRUE)
  })


# GENERIC METHODS ---------------------------------------------------------

setMethod(
  f="nextNeighbor", 
  signature="HammingNeighborhood", 
  definition=function(neighborhood) {
    if (hasMoreNeighbors(neighborhood)) {
      # We do not use directly the id, but the position in the postion list!!
      pos <- neighborhood@index.list[neighborhood@id.pos]
      nxt <- neighborhood@base
      nxt[neighborhood@pair.list[pos, 1]] <- neighborhood@pair.list[pos, 2]
      # Update the object
      # obtain the global name of the variable to modify
      objectGlobalName <- deparse(substitute(neighborhood))
      neighborhood@id.pos <- neighborhood@id.pos + 1
      # assign the local variable to the global variable 
      assign(objectGlobalName, neighborhood, envir=parent.frame())  
    } else {
      nxt <- NULL
    }
    return(nxt)
  })


setMethod(
  f="hasMoreNeighbors", 
  signature="HammingNeighborhood", 
  definition=function(neighborhood) {
    return(neighborhood@id.pos <= length(neighborhood@index.list))
  })


setMethod(
  f="resetNeighborhood", 
  signature="HammingNeighborhood", 
  definition= function(neighborhood, solution) {
    if(length(solution)!=length(neighborhood@base)) {
      stop ("The new solution is not of the correct size")
    }
    n <- length(solution)
    l <- levels(solution)
    aux <- lapply (1:n, 
                   FUN=function(i) {
                     r <- data.frame(Position=i,Value=subset(l,l!=solution[i]))
                     return(r)
                   })
    pair.list <- do.call(rbind, aux)
    if (neighborhood@random) {
      index.list <- randomPermutation(dim(pair.list)[1])
    } else {
      index.list <- identityPermutation(dim(pair.list)[1])
    }
    
    # obtain the global name of the variable to modify
    objectGlobalName <- deparse(substitute(neighborhood))
    neighborhood@base <- solution
    neighborhood@pair.list <- pair.list
    neighborhood@index.list <- index.list
    neighborhood@id.pos <- 1
    ## assign the local variable to the global variable
    assign(objectGlobalName, neighborhood, envir=parent.frame())  
  })



# CONSTRUCTOR -------------------------------------------------------------

#' Basic consturctor of Hamming neighborhoods
#' 
#' This function creates an object of class \code{\linkS4class{HammingNeighborhood}}
#' 
#' @family neighborhoods
#' @param base Base solution for the neighborhood. It has to be a vector of factors (all with the same levels)
#' @param random A logical value indicating whether the exploration should be done at random
#' @return An object of class \code{\linkS4class{HammingNeighobrhood}}
#' @seealso \code{\link{hasMoreNeighbors}} \code{\link{resetNeighborhood}} \code{\link{nextNeighbor}}

hammingNeighborhood<-function(base,random = FALSE){
  n <- length(base)
  l <- levels(base)
  aux <- lapply (1:n, 
                 FUN=function(i) {
                   return(data.frame(Position=i,Value=subset(l,l!=base[i])))
                 })
  pair.list <- do.call(rbind,aux)
  if (random) {
    index.list <- randomPermutation(dim(pair.list)[1])
  } else {
    index.list <- identityPermutation(dim(pair.list)[1])
  }
  obj <- new("HammingNeighborhood", base=base, pair.list=pair.list, 
             index.list=index.list, random=random, id.pos=1)
  return(obj)
}