
#' An S4 class to represent exchange (2-opt) neighborhoods
#'
#' @slot base A factor vector which will be the solution whose neighborhood we will explore
#' @slot position.list A mtrix with pairs of positions in the permutation 
#' @slot random A logical value indicating whether the exploration is at random or not
#' @slot id Numeric value indicating the current position (in the \code{position.list}) that will be used to generate a new neighbor
#' @details The new neighbors are generated swapping positions indicated in \code{position.list}.

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

#' Basic consturctor of exchange (2-opt) neighborhoods
#' 
#' This function creates an object of class \code{\linkS4class{ExchangeNeighborhood}}
#' 
#' @family neighborhoods
#' @param base Base solution for the neighborhood. It has to be an object of class \code{\linkS4class{Permutation}}
#' @param random A logical value indicating whether the exploration should be done at random
#' @return An object of class \code{\linkS4class{ExchangeNeighobrhood}}
#' @seealso \code{\link{hasMoreNeighbors}} \code{\link{resetNeighborhood}} \code{\link{nextNeighbor}} \code{\link{swapNeighborhood}}

hammingNeighborhood<-function(base,random = FALSE){
  n <- length(base)
  l <- levels(base)
  aux <- lapply (1:n, FUN=function(i){data.frame(Position=i,Value=subset(l,l!=l[i]))})
  pair.list <- do.call(rbind,aux)
  if (random){
    index.list <- random.permutation(dim(pair.list)[1])
  }else{
    index.list <- identity.permutation(dim(pair.list)[1])
  }
  new("HammingNeighborhood",base=base, pair.list = pair.list, index.list = index.list , random = random, id.pos = 1)
}


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


