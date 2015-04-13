#' Greedy selection of neighbors
#' 
#' This function explores a neighborhood testing all the posible solutions and picks up the best one
#' 
#' @family negibhor selectors
#' @param neighborhood Object representing the type of neighborhood to be used
#' @param evaluate Function of a single parameter. Given a solution, the function returns its evaluation as a real number 
#' @param initial.solution Solution whose neighborhood will be explred
#' @param initial.evaluation Evaluation value of the initial solution
#' @param non.valid Action to be performed when a non valid solution is considered. The options are \code{'ignore'}, meaning that the solution is considered anyway, \code{'discard'}, meaning that the solution is not considered and \code{'correct'}, meaning that the solution has to be corrected. This parameter has to be set only when there can be non valid solutions
#' @param valid A function that, given a solution, determines whether it is valid or not
#' @param correct A function that, given a non valid solution, corrects it. This optional parameter has to be set only with the \code{'correct'} option
#' @param resources Object of class \code{\linkS4class{CResource}} representing the available computational resources for the search
#' @param ... Special argument to pass additional parameters to the functions used in the search
#' @return The funtion return a list with with three fields, \code{'solution'},  \code{'evaluation'}, contining the best found solution and its evaluation and \code{'resources'}, the remaining computational resources. If the initial solution corresponds to a local optimum, the function returns NULL in the \code{'solution'} field.
#' @details In case all the solutions are valid, set the \code{non.valid} parameter to \code{'ignore'} for efficiency. The \code{'neighborhood'} object can be of any type, as long as it has defined
#' the function \code{'reset.neighborhood'},  \code{'has.more.neighbors'} and  \code{'next.neighbor'}
#' @seealso  \code{\link{first.improvement.selector}}, \code{\link{basic.local.search}}, \code{\link{multistart.local.search}},  \code{\link{iterated.local.search}}


greedy.selector<-function (neighborhood, evaluate , initial.solution, initial.evaluation , 
                           non.valid='ignore', valid=function(solution){TRUE} , 
                           correct=function(solution){solution}, resources = cresource() , ....){
  
  best.sol<-initial.solution
  best.eval<-initial.evaluation
  improved<-FALSE
  ## Initialize neighborhood with the first solution and then explore the whole neighborhood
  t0<-Sys.time()
  reset.neighborhood(neighborhood, initial.solution)
  improved <- FALSE
  add.consumed(resources , t = as.numeric(Sys.time()-t0))
  while(has.more.neighbors(neighborhood) && !is.finished(resources)){
    ## Get each neighbor and evaluate it
    t0<-Sys.time()
    new.sol<-next.neighbor(neighborhood)
    if (!valid(new.sol) && non.valid!='ignore'){
      if (non.valid=='discard'){
        next
      }else if(non.valid=='correct'){
        new.sol<-correct(new.sol)
      }else{
        stop ("Unknown solution for the 'non.valid' parameter. Valid options are 'ignore' , 'discard' or 'correct'")
      }
    }
    new.eval<-evaluate(new.sol)
    ## If it improves the best so far, record it
    if (new.eval<best.eval){
      best.sol<-new.sol
      best.eval<-new.eval
      improved<-TRUE
    }
    add.consumed(resources , t = as.numeric(Sys.time()-t0), ev = 1)
  }
  ## If no improvement, return null. Otherwise, return the best solution and its evaluation
  
  res<-list('resources'=resources)
  if (improved)
  {
    res$solution<-best.sol
    res$evaluation<-best.eval
  }else{
    res$solution<-NULL
    res$evaluation<-NA
  }
  res
}


#' First improvement selection of neighbors
#' 
#' This function explores a neighborhood and returns the first solution that improves the current one
#' 
#' @family negibhor selectors
#' @param neighborhood Object representing the type of neighborhood to be used
#' @param evaluate Function of a single parameter. Given a solution, the function returns its evaluation as a real number 
#' @param initial.solution Solution whose neighborhood will be explred
#' @param initial.evaluation Evaluation value of the initial solution
#' @param non.valid Action to be performed when a non valid solution is considered. The options are \code{'ignore'}, meaning that the solution is considered anyway, \code{'discard'}, meaning that the solution is not considered and \code{'correct'}, meaning that the solution has to be corrected. This parameter has to be set only when there can be non valid solutions
#' @param valid A function that, given a solution, determines whether it is valid or not
#' @param correct A function that, given a non valid solution, corrects it. This optional parameter has to be set only with the \code{'correct'} option
#' @param resources Object of class \code{\linkS4class{CResource}} representing the available computational resources for the search
#' @param ... Special argument to pass additional parameters to the functions used in the search
#' @return The funtion return a list with with three fields, \code{'solution'},  \code{'evaluation'}, contining the best found solution and its evaluation and \code{'resources'}, the remaining computational resources. If the initial solution corresponds to a local optimum, the function returns NULL in the \code{'solution'} field.
#' @details In case all the solutions are valid, set the \code{non.valid} parameter to \code{'ignore'} for efficiency. The \code{'neighborhood'} object can be of any type, as long as it has defined
#' the function \code{'reset.neighborhood'},  \code{'has.more.neighbors'} and  \code{'next.neighbor'}
#' @seealso  \code{\link{greedy.selector}}, \code{\link{basic.local.search}}, \code{\link{multistart.local.search}},  \code{\link{iterated.local.search}}


first.improvement.selector<-function (neighborhood, evaluate , initial.solution, initial.evaluation ,
                                      non.valid='ignore', valid=function(solution){TRUE} ,
                                      correct=function(solution){solution}, resources = cresource() , ...){
  
  best.sol<-initial.solution
  best.eval<-initial.evaluation
  ## Initialize neighborhood with the first solution and then explore the whole neighborhood
  t0<-Sys.time()
  reset.neighborhood(neighborhood, initial.solution)
  improved <- FALSE
  add.consumed(resources , t = as.numeric(Sys.time()-t0))
  while(has.more.neighbors(neighborhood) && !is.finished(resources) && !improved){
    ## Get each neighbor and evaluate it
    ## Note that, 
    t0<-Sys.time()
    new.sol<-next.neighbor(neighborhood)
    if (!valid(new.sol) && non.valid!='ignore'){
      if (non.valid=='discard'){
        next
      }else if(non.valid=='correct'){
        new.sol<-correct(new.sol)
      }else{
        stop ("Unknown solution for the 'non.valid' parameter. Valid options are 'ignore' , 'discard' or 'correct'")
      }
    }
    new.eval<-evaluate(new.sol)
    ## If it improves the best so far, record it
    if (new.eval<best.eval){
      best.sol<-new.sol
      best.eval<-new.eval
      improved<-TRUE
    }
    add.consumed(resources , t = as.numeric(Sys.time()-t0), ev = 1)
  }
  ## If no improvement, return null. Otherwise, return the best solution and its evaluation
  
  res<-list('resources'=resources)
  if (improved)
  {
    res$solution<-best.sol
    res$evaluation<-best.eval
  }else{
    res$solution<-NULL
    res$evaluation<-NA
  }
  res
}