#' @title Function to update the trail of pheromones in the model
#'
#' @description This function updates the trail of pheromones according to a given solution
#' @param object Object of class \code{\linkS4class{VectorPheromone}} representing the pheromone model to be updated
#' @param solution A vector, either binary or factor, used to update the pheromone trail
#' @param value Number to be used in the update
#' 

setGeneric(name = "update.trail", def = function(object, solution , value , ...){standardGeneric("update.trail")})


#' @title Function to uniformly reduce the amount of pheromone
#'
#' @description This function updates the trail of pheromones through evaporation
#' @param object Object of class \code{\linkS4class{VectorPheromone}} representing the pheromone model to be updated
#' 

setGeneric(name = "evaporate", def = function(object, ...){standardGeneric("evaporate")})



#' @title Function to build solutions according to the pheromone trail
#'
#' @description This function creates new solutions using the pheromone trail
#' @param object Object of class \code{\linkS4class{VectorPheromone}} representing the pheromone model to be updated
#' @param n Numeric value indicating the amount of solutions to be created
#' @param seed Numeric value to indicate the seed used for the random number generator. If \code{NULL}, the seed is not set
#' 

setGeneric(name = "build.solution", def = function(object, n = 1, seed = NULL , ...){standardGeneric("build.solution")})
