#' An S4 class to represent exchange neighborhood for permutations
#'
#' @slot time_total A positive real number representing the total time available. It can be null, indicating that there is no time limit
#' 
setClass(
  Class="InsertNeighborhood", 
  representation=representation(base="Permutation", 
                                position.list="matrix", 
                                random="logical",
                                id="numeric"))

setValidity(
  Class="InsertNeighborhood", 
  method=function(object) {
    n <- length(object@base)
    # Create all possible fordward inserts
    ford.inserts <- do.call(rbind, sapply(1:(n - 1), 
                                          FUN=function(x) {
                                            return(cbind((x),(x+1):n))
                                          }))
    # Create all possible backward inserts, without taking repetitions into account
    back.inserts <- do.call(rbind, sapply(n:3, 
                                          FUN=function(x) {
                                            return(cbind(x, (x - 2):1))
                                          }))
    # Matrix with all possible inserts and no repetitions
    inserts <- rbind(ford.inserts, back.inserts)
    if (!all(object@position.list %in% inserts)) {
      stop("Some of the defined inserts are not adequate")
    }
    if (!all(inserts %in% object@position.list)) {
      stop("Not all possible inserts are considered")
    }
    if (object@id!=1) {
      stop("The first id to be used has to be 1")
    }
    return (TRUE)
  }
)


# GENERIC METHODS ---------------------------------------------------------

setMethod(
  f="nextNeighbor", 
  signature="InsertNeighborhood", 
  definition=function(neighborhood) {
    if (hasMoreNeighbors(neighborhood)) {
      # We do not use directly the id, but the position in the postion list!!
      nxt <- insert(neighborhood@base, 
                    neighborhood@position.list[neighborhood@id, 1], 
                    neighborhood@position.list[neighborhood@id, 2])
      
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
  signature="InsertNeighborhood", 
  definition=function(neighborhood) {
    return(neighborhood@id <= nrow(neighborhood@position.list))
  })


setMethod(
  f="resetNeighborhood", 
  signature="InsertNeighborhood", 
  definition=function(neighborhood, solution) {
    if(length(solution) != length(neighborhood@base)) {
      stop ("The new solution is not of the correct size")
    }
    # obtain the global name of the variable to modify
    objectGlobalName <- deparse(substitute(neighborhood))
    neighborhood@id <- 1
    neighborhood@base <- solution
    # If the search is random, shuffle the position list
    if (neighborhood@random) {
      neighborhood@position.list <- neighborhood@position.list[sample(nrow(neighborhood@position.list)), ]
    }
    # assign the local variable to the global variable
    assign(objectGlobalName, neighborhood, envir=parent.frame())  
  })

# CONSTRUCTOR -------------------------------------------------------------

insertNeighborhood <- function(base, random=FALSE) {
  n <- length(base)
  ## Create all possible fordward inserts
  ford.inserts <- do.call(rbind, sapply(1:(n-1), 
                                        FUN=function(x) {
                                          cbind((x),(x+1):n)
                                        }))
  # Create all possible backward inserts, without taking repetitions into account
  back.inserts <- do.call(rbind, sapply(n:3, 
                                        FUN=function(x) {
                                          return(cbind((x),(x-2):1))
                                        }))
  ## Matrix with all possible inserts and no repetitions
  inserts <- rbind(ford.inserts, back.inserts)
  
  if (random){
    positions <- inserts[sample(nrow(inserts)), ]
  }else{
    positions <- inserts
  }
  obj <- new("InsertNeighborhood", base=base, position.list=positions, 
             random=random, id=1)
  return(obj)
}

