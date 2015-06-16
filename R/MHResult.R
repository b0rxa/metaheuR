# CLASS DEFINITION --------------------------------------------------------

setClassUnion("NumORNull", c("numeric", "NULL"))
setClassUnion("DFORNull", c("data.frame", "NULL"))

#' An S4 class to represent the output of a search heuristic
#'
#' @slot description A free filed to include comments
#' @slot algorithm Name of the algorithm used
#' @slot parameters This should be a named list with the parameters used by the algorithm
#' @slot solution Best solution found
#' @slot evaluation Evaluation of the best solution(s)
#' @slot resources Object contining the information about the available and consumed computational resources
#' @slot log A numeric matrix containing the evolution of the search. The matrix should have four columns, named as \code{Time}, \code{Evaluations}, \code{Iterations}, \code{Current_sol}, \code{Current_sd} and \code{Best_sol}
#' 
setClass(
  Class="MHResult", 
  representation=representation(description="character",
                                algorithm="character",
                                parameters="list",
                                solution="ANY",
                                evaluation="numeric",
                                resources="CResource",
                                log="DFORNull")
)

setValidity(
  Class="MHResult", 
  method=function(object) {
    if (!all(names(object@log) %in% c("Time", "Evaluations", "Iterations", 
                                      "Current_sol", "Current_sd", "Best_sol"))) {
      stop ("The definition of the log data.frame is not correct. It has to have ",
            "six columns named 'Time', 'Evaluations', 'Iterations', 'Current_sol', ",
            "'Current_sd' and 'Best_sol'")
    }
    return (TRUE)
  })

# GENERIC METHODS --------------------------------------------------------------
setMethod(
  f="print", 
  signature="MHResult", 
  definition=function(x, ...) {
    consumed <- whatFinished(x@resources)
    consumption.message <- switch (as.character(length(consumed)), 
                                   "0"={
                                     "None of the resources completely consumed"
                                   },
                                   "1"={
                                     paste(consumed[1], "completely consumed")
                                   }, 
                                   "2"={
                                     paste(consumed[1], "and", 
                                           consumed[2], "completely consumed")
                                   }, 
                                   "3"={
                                     paste(consumed[1], ", ", consumed[2], 
                                           " and ", consumed[3], 
                                           " completely consumed", sep="")
                                   })
    message("RESULTS OF THE SEARCH\n")
    message("Best solution's evaluation: " , x@evaluation , "\n")
    message("Algorithm: " , x@algorithm , "\n")
    message("Resource consumption:\n")
    message("\tTime: " , getConsumedTime(x@resources) ,"\n")
    message("\tEvaluations: " , getConsumedEvaluations(x@resources),"\n")
    message("\tIterations: " , getConsumedIterations(x@resources),"\n")
    message(consumption.message,"\n\n")
    message("You can use functions 'getSolution', 'getParameters' and 'getProgress' to get the list of optimal solutions, the list of parameters of the search and the log of the process, respectively")
  }
)


setMethod(
  f="show", 
  signature="MHResult", 
  definition=function(object) {
    print(object)
  })

#' @title Accession function for the \code{description} slot of the object
#'
#' @description Accession function for the \code{description} slot of the object
#' @param result Object of class \code{\linkS4class{mHResult}} whose information we are quering
#' 
#' @return Content of the \code{description} slot
#' 

setGeneric(name="getDescription", 
           def=function(result) {
             standardGeneric("getDescription")
           })

setMethod(
  f="getDescription", 
  signature="MHResult", 
  definition=function(result) {
    return(result@description)
  })

#' @title Accession function for the \code{algorithm} slot of the object
#'
#' @description Accession function for the \code{algorithm} slot of the object
#' @param result Object of class \code{\linkS4class{mHResult}} whose information we are quering
#' 
#' @return Content of the \code{algorithm} slot
#' 
setGeneric(name="getAlgorithm", 
           def=function(result) {
             standardGeneric("getAlgorithm")
           })

setMethod(
  f="getAlgorithm", 
  signature="MHResult", 
  definition=function(result) {
    return(result@algorithm)
  })

#' @title Accession function for the \code{parameters} slot of the object
#'
#' @description Accession function for the \code{parameters} slot of the object
#' @param result Object of class \code{\linkS4class{mHResult}} whose information we are quering
#' 
#' @return Content of the \code{parameters} slot
#' 
setGeneric(name="getParameters", 
           def=function(result) {
             standardGeneric("getParameters")
           })

setMethod(
  f="getParameters", 
  signature="MHResult", 
  definition=function(result){
    return(result@parameters)
  })

#' @title Accession function for the \code{optima} slot of the object
#'
#' @description Accession function for the \code{optima} slot of the object
#' @param result Object of class \code{\linkS4class{mHResult}} whose information we are quering
#' 
#' @return Content of the \code{optima} slot
#' 
setGeneric(name="getSolution", 
           def=function(result) {
             standardGeneric("getSolution")
           })

setMethod(
  f="getSolution", 
  signature="MHResult", 
  definition=function(result) {
    return(result@solution)
  })

#' @title Accession function for the \code{evaluation} slot of the object
#'
#' @description Accession function for the \code{evaluation} slot of the object
#' @param result Object of class \code{\linkS4class{mHResult}} whose information we are quering
#' 
#' @return Content of the \code{evaluation} slot

setGeneric(name="getEvaluation", 
           def=function(result) {
             standardGeneric("getEvaluation")
           })

setMethod(
  f="getEvaluation", 
  signature="MHResult", 
  definition=function(result) {
    return(result@evaluation)
  })



#' @title Accession function for the \code{resources} slot of the object
#'
#' @description Accession function for the \code{resources} slot of the object
#' @param result Object of class \code{\linkS4class{mHResult}} whose information we are quering
#' 
#' @return Content of the \code{resources} slot
#' 
setGeneric(name="getResources", 
           def=function(result) {
             standardGeneric("getResources")
           })

setMethod(
  f="getResources", 
  signature="MHResult", 
  definition=function(result) {
    return(result@resources)
  })


#' @title Accession function for the \code{progress} slot of the object
#'
#' @description Accession function for the \code{progress} slot of the object
#' @param result Object of class \code{\linkS4class{mHResult}} whose information we are quering
#' 
#' @return Content of the \code{progress} slot
#' 
setGeneric(name="getProgress", 
           def=function(result) {
             standardGeneric("getProgress")
           })

setMethod(
  f="getProgress", 
  signature="MHResult", 
  definition=function(result) {
    return(result@log)
  })

#' @title Function to plot the evolution of the evaluation function during the search
#'
#' @description This function produces an object of class \code{\link{ggplot}} with the evolution of the search
#' @param result Object of class \code{\linkS4class{mHResult}} whose log will be visualized or a named list of objects
#' @param x Parameter indicating the value for the X axis. Valid options are 'time', 'evaluations' and 'iterations'
#' @param y Parameter indicating the value for the Y axis. Valid options are 'current' and 'best'
#' @param ... Additional parameters for the function \code{\link{geom_line}} used to draw the line
#' @return An object of class \code{\link{ggplot}} 
#' 
setGeneric(name="plotProgress", 
           def=function(result, x="evaluations", y="current", type="line", ...) {
             standardGeneric("plotProgress")
           })

setMethod(
  f="plotProgress", 
  signature="MHResult", 
  definition=function(result, x="evaluations", y="current", type="line", ...) {
    require("ggplot2")
    aes <- switch(x, 
                  "evaluations"={
                    aes(x=Evaluations)  
                  }, 
                  "time"={
                    aes(x=Time)  
                  }, 
                  "iterations"={
                    aes(x=Iterations)  
                  },
                  {
                    stop("No-valid argument for the 'vs' parameter. Valid options ",
                         "are 'evaluations', 'time' and 'iterations'")
                  })
    aes$y <- switch(y,
                    "current"={
                      aes(y=Current_sol)$y
                    },
                    "best"={
                      aes(y=Best_sol)$y
                    },
                    {
                      stop("Non-valid argument for the y parameter. Valid options ",
                           "are 'current' and 'best'")
                    })
    
    g <- ggplot(result@log, mapping=aes)
    switch(type,
           "line"={
             g <- g + geom_line(...) 
           },
           "point"={
             g <- g + geom_point(...)
           },
           {
             stop ("Type not known. Try 'line' or 'point'")
           })
    return(g)
  })


setMethod(
  f="plotProgress", 
  signature="list", 
  definition=function(result, x="evaluations", y="current", type="line", ...) {
    require("ggplot2")
    aux <- lapply(1:length(result), 
                  FUN=function(i) {
                    res <- cbind(getProgress(result[[i]]), 
                                 Group=names(result)[i])
                    return(res)
                  })
    df <- do.call(rbind, aux)
    aes <- switch(x, 
                  "evaluations"={
                    aes(x=Evaluations, color=Group)  
                  }, 
                  "time"={
                    aes(x=Time, color=Group)  
                  }, 
                  "iterations"={
                    aes(x=Iterations, color=Group)  
                  },
                  {
                    stop("No-valid argument for the 'vs' parameter. Valid options ",
                         "are 'evaluations', 'time' and 'iterations'")
                  })
    aes$y <- switch(y,
                    "current"={
                      aes(y=Current_sol)$y
                    },
                    "best"={
                      aes(y=Best_sol)$y
                    },
                    {
                      stop("Non-valid argument for the y parameter. Valid options ",
                           "are 'current' and 'best'")
                    })
    
    g <- ggplot(df, mapping=aes)
    switch(type,
           'line'={
             g <- g + geom_line(...)
           },
           'point'={
             g <- g + geom_point(...)
           },
           {
             stop ("Type not known. Try 'line' or 'point'")
           })
    return(g)
  })

#' @title Function to plot the correlation among evaluations, iterations and time
#'
#' @description This function produces an object of class \code{\link{ggplot}} showing the correlation among evaluations, iterations and time
#' @param result Object of class \code{\link{mHResult}} whose log will be visualized
#' @param a Parameter indicating the value for the X axis. Valid options are 'time', 'evaluations' and 'iterations'
#' @param b Parameter indicating the value for the Y axis. Valid options are 'time', 'evaluations' and 'iterations'
#' @param ... Additional parameters for the function \code{\link{geom_point}} used to draw the plots
#' @return An object of class \code{\link{ggplot}} 
#' 
setGeneric(name="plotCorrelation", 
           def=function(result, a="time", b="evaluations", ...) {
             standardGeneric("plotCorrelation")
           })

setMethod(
  f="plotCorrelation", 
  signature="MHResult", 
  definition=function(result, a="time", b="evaluations", ...) {   
    require("ggplot2") 
    df <- data.frame(Time=diff(result@log$Time), 
                     Evaluations=diff(result@log$Evaluations), 
                     Iterations=diff(result@log$Iterations))
    aes <- switch(a,
                  "time"=aes(x=Time),
                  "evaluations"=aes(x=Evaluations),
                  "iterations"=aes(x=Iterations))
    aes$y <- switch(b,
                    "time"=aes(y=Time)$y,
                    "evaluations"=aes(y=Evaluations)$y,
                    "iterations"=aes(y=Iterations)$y)
    g <- ggplot(df, mapping=aes) + geom_point(...)
    return(g)
  })

# CONSTRUCTORS ------------------------------------------------------------

#' @title Function to construct objects of type \code{mHResult}
#'
#' @description This function creates objects of class \code{\link{mHResult}}
#' @param algorithm String indicating the algorithm run
#' @param description Description of the algorithm run
#' @param parameters List of parameters used in the experiment
#' @param solution Best found solution
#' @param evaluation Optimal value of the objective function
#' @param resources Object of class \code{\link{CResource}} contining the information about the use of resources in the experiment
#' @param log Matrix with the evolution of the search
#' @return A new object of class \code{\link{mHResult}} with the updated remaining resources
#' 
mHResult <- function (algorithm, parameters, solution, evaluation, 
                      resources, description="", log=NULL){
  obj <- new("MHResult",
             algorithm=algorithm, description=description, parameters=parameters,
             solution=solution, evaluation=evaluation, resources=resources, log=log)
  return(obj)
}
