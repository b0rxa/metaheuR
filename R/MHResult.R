# CLASS DEFINITION --------------------------------------------------------

setClassUnion("NumORNull", c("numeric", "NULL"))
setClassUnion("DFORNull", c("data.frame", "NULL"))

#' An S4 class to represent the output of a search heuristic
#'
#' @slot description A free filed to include comments
#' @slot algorithm Name of the algorithm used
#' @slot parameters This should be a named list with the parameters used by the algorithm
#' @slot optima List of optimal solutions
#' @slot evaluation Evaluation of the best solution(s)
#' @slot resources Object contining the information about the available and consumed computational resources
#' @slot log A numeric matrix containing the evolution of the search. The matrix should have four columns, named as \code{Time}, \code{Evaluations}, \code{Iterations}, \code{Current_sol}, \code{Current_sd} and \code{Best_sol}
setClass(
  Class = "MHResult", 
  representation = representation(description = "character",
                                  algorithm = "character",
                                  parameters = "list",
                                  optima = "list",
                                  evaluation = "numeric",
                                  resources = "CResource",
                                  log = "DFORNull")
)

setValidity(
  Class = "MHResult", 
  method = function(object){
    if (!all(names(object@log) %in% c("Time" , "Evaluations" , "Iterations" , "Current_sol" , "Current_sd" , "Best_sol"))) stop ("The definition of the log data.frame is not correct. It has to have six columns named 'Time', 'Evaluations', 'Iterations', 'Current_sol', 'Current_sd' and 'Best_sol'")
    return (TRUE)
  }
)

# GENERIC METHODS ---------------------------------------------------------

setMethod(
  f="print", 
  signature = "MHResult", 
  definition = function(x, ...) {
    consumed <- paste(what.finished(x@resources) , collapse=";")
    consumption.message <- switch (as.character(length(consumed)) , 
                                   "0" =  "None of the resources completely consumed" ,
                                   "1" =  paste(consumed[1] , "completely consumed") , 
                                   "2" =  paste(consumed[1] , "and" , consumed[2] , "completely consumed") , 
                                   "3" =  paste(consumed[1] , ", " , consumed[2] , " and " , consumed[3] , " completely consumed", sep=""))
    cat("RESULTS OF THE SEARCH\n")
    cat("Best solution's evaluation: " , x@evaluation , "\n")
    cat("Algorithm:" , x@algorithm , "\n")
    cat("Resource consumption:\n")
    cat("\tTime:" , consumed.time(x@resources) ,"\n")
    cat("\tEvaluations:" , consumed.evaluations(x@resources),"\n")
    cat("\tIterations:" , consumed.iterations(x@resources),"\n")
    cat(consumption.message,"\n\n")
    cat("You can use functions 'optima', 'parameters' and 'progress' to get the list of optimal solutions, the list of parameters of the search and the log of the process, respectively")
  }
)


setMethod(
  f="show", 
  signature = "MHResult", 
  definition = function(object) {
    print(object)
  }
)

#' @title Accession function for the \code{description} slot of the object
#'
#' @description Accession function for the \code{description} slot of the object
#' @param result Object of class \code{\link{MHResult}} whose information we are quering
#' 
#' @return Content of the \code{description} slot

setGeneric(name = "description", def = function(result){standardGeneric("description")})

setMethod(
  f="description", 
  signature = "MHResult", 
  definition = function(result) {
    result@description
  }
)

#' @title Accession function for the \code{algorithm} slot of the object
#'
#' @description Accession function for the \code{algorithm} slot of the object
#' @param result Object of class \code{\link{MHResult}} whose information we are quering
#' 
#' @return Content of the \code{algorithm} slot

setGeneric(name = "algorithm", def = function(result){standardGeneric("algorithm")})

setMethod(
  f="algorithm", 
  signature = "MHResult", 
  definition = function(result) {
    result@algorithm
  }
)

#' @title Accession function for the \code{parameters} slot of the object
#'
#' @description Accession function for the \code{parameters} slot of the object
#' @param result Object of class \code{\link{MHResult}} whose information we are quering
#' 
#' @return Content of the \code{parameters} slot

setGeneric(name = "parameters", def = function(result){standardGeneric("parameters")})

setMethod(
  f="parameters", 
  signature = "MHResult", 
  definition = function(result) {
    result@parameters
  }
)

#' @title Accession function for the \code{optima} slot of the object
#'
#' @description Accession function for the \code{optima} slot of the object
#' @param result Object of class \code{\link{MHResult}} whose information we are quering
#' 
#' @return Content of the \code{optima} slot

setGeneric(name = "optima", def = function(result){standardGeneric("optima")})

setMethod(
  f="optima", 
  signature = "MHResult", 
  definition = function(result) {
    result@optima
  }
)




#' @title Accession function for the \code{evaluation} slot of the object
#'
#' @description Accession function for the \code{evaluation} slot of the object
#' @param result Object of class \code{\link{MHResult}} whose information we are quering
#' 
#' @return Content of the \code{evaluation} slot

setGeneric(name = "evaluation", def = function(result){standardGeneric("evaluation")})

setMethod(
  f="evaluation", 
  signature = "MHResult", 
  definition = function(result) {
    result@evaluation
  }
)



#' @title Accession function for the \code{resources} slot of the object
#'
#' @description Accession function for the \code{resources} slot of the object
#' @param result Object of class \code{\link{MHResult}} whose information we are quering
#' 
#' @return Content of the \code{resources} slot

setGeneric(name = "resources", def = function(result){standardGeneric("resources")})

setMethod(
  f="resources", 
  signature = "MHResult", 
  definition = function(result) {
    result@resources
  }
)



#' @title Accession function for the \code{progress} slot of the object
#'
#' @description Accession function for the \code{progress} slot of the object
#' @param result Object of class \code{\link{MHResult}} whose information we are quering
#' 
#' @return Content of the \code{progress} slot

setGeneric(name = "progress", def = function(result){standardGeneric("progress")})

setMethod(
  f="progress", 
  signature = "MHResult", 
  definition = function(result) {
    result@log
  }
)

#' @title Function to plot the evolution of the evaluation function during the search
#'
#' @description This function produces an object of class \code{\link{ggplot}} with the evolution of the search
#' @param result Object of class \code{\link{MHResult}} whose log will be visualized
#' @param x Parameter indicating the value for the X axis. Valid options are 'time', 'evaluations' and 'iterations'
#' @param y Parameter indicating the value for the Y axis. Valid options are 'current' and 'best'
#' @param ... Additional parameters for the function \code{\link{geom_line}} used to draw the line
#' @return An object of class \code{\link{ggplot}} 

setGeneric(name = "plot.progress" , def = function(result , x = 'evaluations' , y = 'current', type = 'line' , ...){standardGeneric("plot.progress")})

setMethod(
  f="plot.progress", 
  signature = "MHResult", 
  definition = function(result , x = 'evaluations' , y = 'current' , type = 'line' , ...) {
    require("ggplot2")
    aes <- switch(x , 
                  "evaluations" = {
                    aes(x=Evaluations)  
                  }, 
                  "time" = {
                    aes(x=Time)  
                  }, 
                  "iterations" = {
                    aes(x=Iterations)  
                  },
                  stop("No-valid argument for the 'vs' parameter. Valid options are 'evaluations', 'time' and 'iterations'")
    )
    aes$y <- switch(y ,
                    "current" = {
                      aes(y=Current_sol)$y
                    },
                    "best" = {
                      aes(y=Best_sol)$y
                    },
                    stop("Non-valid argument for the y parameter. Valid options are 'current' and 'best'"))
    
    g <- ggplot(result@log , mapping = aes)
    switch(type,
           'line' = g <- g + geom_line(...) ,
           'point' = g <- g + geom_point(...) ,
           stop ("Type not known. Try 'line' or 'point'")
           )
    return(g)
  }
)

#' @title Function to plot the correlation among evaluations, iterations and time
#'
#' @description This function produces an object of class \code{\link{ggplot}} showing the correlation among evaluations, iterations and time
#' @param result Object of class \code{\link{MHResult}} whose log will be visualized
#' @param a Parameter indicating the value for the X axis. Valid options are 'time', 'evaluations' and 'iterations'
#' @param b Parameter indicating the value for the Y axis. Valid options are 'time', 'evaluations' and 'iterations'
#' @param ... Additional parameters for the function \code{\link{geom_point}} used to draw the plots
#' @return An object of class \code{\link{ggplot}} 

setGeneric(name = "plot.correlation", 
           def = function(result , a='time' , b='evaluations' , ...){standardGeneric("plot.correlation")})

setMethod(
  f="plot.correlation", 
  signature = "MHResult", 
  definition = function(result, a='time' , b='evaluations' , ...) {   
    require("ggplot2") 
    df <- data.frame(Time=diff(result@log$Time) , 
                     Evaluations=diff(result@log$Evaluations) , 
                     Iterations=diff(result@log$Iterations))
    aes <- switch(a,
                  'time' = aes(x=Time),
                  'evaluations' = aes(x=Evaluations),
                  'iterations' = aes(x=Iterations))
    aes$y <- switch(b,
                    'time' = aes(y=Time)$y,
                    'evaluations' = aes(y=Evaluations)$y,
                    'iterations' = aes(y=Iterations)$y)
    ggplot(df , mapping = aes) + geom_point(...)
  }
)

# CONSTRUCTORS ------------------------------------------------------------

#' @title Function to construct objects of type \code{MHResult}
#'
#' @description This function creates objects of class \code{\link{MHResult}}
#' @param algorithm String indicating the algorithm run
#' @param description Description of the algorithm run
#' @param parameters List of parameters used in the experiment
#' @param optima One or more optimal solutions
#' @param evaluation Optimal value of the objective function
#' @param resources Object of class \code{\link{CResource}} contining the information about the use of resources in the experiment
#' @param log Matrix with the evolution of the search
#' @return A new object of class \code{\link{MHResult}} with the updated remaining resources

mhresult <- function (algorithm, description = "", parameters, optima = NULL, evaluation, resources, log = matrix()){
  new("MHResult" , 
      algorithm = algorithm , description = description , parameters = parameters, 
      optima = optima , evaluation = evaluation , 
      resources = resources, log = log)
}
