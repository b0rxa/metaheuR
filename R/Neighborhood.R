# GENERIC METHODS ---------------------------------------------------------

#' Function to obtain a new neighbor in a neighborhood
#' 
#' This is one of the functions that has to be implemented by any object to be used as a neighborhood
#' 
#' @family neighborhoods
#' @param neighborhood Object representing the neighborhood
#' @return A new solution in th neighborhood represented by \code{neighborhood}
#' @seealso \code{\link{hasMoreNeighbors}} \code{\link{resetNeighborhood}}
#' 
setGeneric(name="nextNeighbor", 
           def=function(neighborhood) {
             standardGeneric("nextNeighbor")
           })


#' Function to check whether we have accessed to all the solutions in the neighborhood or not
#' 
#' This is one of the functions that has to be implemented by any object to be used as a neighborhood
#' 
#' @family neighborhoods
#' @param neighborhood Object representing the neighborhood
#' @return \code{TRUE} if there are more solutions to explore, \code{FALSE} in other case
#' @seealso \code{\link{nextNeighbor}} \code{\link{resetNeighborhood}}
#' 
setGeneric(name="hasMoreNeighbors", 
           def=function(neighborhood) {
             standardGeneric("hasMoreNeighbors")
           })

#' Function to reset a neighborhood to a given base solution
#' 
#' This is one of the functions that has to be implemented by any object to be used as a neighborhood
#' 
#' @family neighborhoods
#' @param neighborhood Object representing the neighborhood
#' @param solution Solution whose neighborhood we want to explore
#' @seealso \code{\link{nextNeighbor}} \code{\link{resetNeighborhood}}
#' 
setGeneric(name="resetNeighborhood", 
           def=function(neighborhood, solution) {
             standardGeneric("resetNeighborhood")
           })
