#' @title Basic local search
#'
#' @description \code{basic.local.search} performs a simple local search and returns the best solution
#' @param evaluate Function of a single parameter. Given a solution, the function returns its evaluation as a real number 
#' @param initial.solution Solution from where the search will start
#' @param neighborhood Object representing the type of neighborhood to be used
#' @param selector A function used to select a solution from the neighborhood. For an example of the parameters required and the result it should produce, see \code{\link{greedy.selector}}
#' @param do.log Logic value to indicate whether the progress shoul be tracked or not
#' @param verbose Logic value to indicate whether the function should print information about the search
#' @param non.valid Action to be performed when a non valid solution is considered. The options are \code{'ignore'}, meaning that the solution is considered anyway, \code{'discard'}, meaning that the solution is not considered and \code{'correct'}, meaning that the solution has to be corrected. This parameter has to be set only when there can be non valid solutions
#' @param valid A function that, given a solution, determines whether it is valid or not
#' @param correct A function that, given a non valid solution, corrects it. This optional parameter has to be set only with the \code{'correct'} option
#' @param resources Object of class \code{\linkS4class{CResource}} representing the available computational resources for the search
#' @param ... Special argument to pass additional parameters to the functions used in the search
#' @return The function returns an object of class \code{\linkS4class{MHResult}} with all the information about the search

basic.local.search<-function (evaluate, initial.solution, neighborhood, selector, do.log = TRUE , verbose = TRUE ,
                              non.valid='ignore', valid=function(solution){TRUE}, 
                              correct=function(solution){solution}, resources = cresource() , ...){
  if (!valid(initial.solution)) stop ("A valid solution has to be provided as the initial solution")
  ## Evaluate the initial solution and initialize the counters
  t0 <- Sys.time()
  current.solution <- initial.solution
  current.evaluation <- evaluate(current.solution)
  add.consumed(resources , t = as.numeric(Sys.time() - t0) , ev = 1)
  stop <- is.finished(resources)
  iteration <- 0
  log <- NULL
  if (do.log){
    log <- data.frame(Iterations = consumed.iterations(resources) ,
                      Evaluations = consumed.evaluations(resources) ,
                      Time = consumed.time (resources) , 
                      Current_sol = current.evaluation ,
                      Current_sd = NA ,
                      Best_sol = current.evaluation)
  }
  ## Main loop of the search, get each neighbor and evaluate it
  while(!stop){
    iteration <- iteration + 1
    if (verbose) cat("Running iteration" , iteration , ". Best solution:" , current.evaluation , "\n")
    
    ## Select a solution in the neighborhood
    selection <- selector(neighborhood = neighborhood , evaluate = evaluate, 
                          initial.solution = current.solution , initial.evaluation = current.evaluation ,  
                          non.valid = non.valid , valid = valid , correct = correct, resources = resources) 
    
    ## If it improves the best so far, record it
    resources <- selection$resources
    if (!is.null(selection$solution)){
      current.solution <- selection$solution
      current.evaluation <- selection$evaluation
      resources <- add.consumed(resources , it = 1)
      stop <- is.finished(resources)
      reset.neighborhood(neighborhood = neighborhood , solution = current.solution)
    }else{
      stop <- TRUE
    }
    
    ## Log the data from the iteration
    if (do.log){
      log <- rbind(log , data.frame(Iterations = consumed.iterations(resources) ,
                                    Evaluations = consumed.evaluations(resources) ,
                                    Time = consumed.time (resources) , 
                                    Current_sol = current.evaluation , 
                                    Current_sd = NA , 
                                    Best_sol = current.evaluation))
    }
  }
  
  ## Build the output
  mhresult(algorithm = "Basic Local Search" ,
           description = paste("Simple local search guided by " , deparse(substitute(evaluate))) ,
           parameters = list(selector = deparse(substitute(selector)) , 
                             neighborhood = deparse(substitute(neighborhood))),
           optima = list (current.solution) , 
           evaluation = current.evaluation , 
           resources = resources ,
           log = log)
}



#' @title Local search with multistart
#'
#' @description This function performs a local search with multistart. It can be used to implement random restarts or GRASP algorithms
#' @param evaluate Function of a single parameter. Given a solution, the function returns its evaluation as a real number 
#' @param generate.solution Function to generate initial solutions
#' @param neighborhood Object representing the type of neighborhood to be used
#' @param selector A function used to select a solution from the neighborhood. For an example of the parameters required and the result it should produce, see \code{\link{greedy.selector}}
#' @param num.restarts Number of restarts to perform. If NULL, then restarts will be performed unitl the resources are finished
#' @param do.log Logic value to indicate whether the progress shoul be tracked or not
#' @param verbose Logic value to indicate whether the function should print information about the search
#' @param non.valid Action to be performed when a non valid solution is considered. The options are \code{'ignore'}, meaning that the solution is considered anyway, \code{'discard'}, meaning that the solution is not considered and \code{'correct'}, meaning that the solution has to be corrected. This parameter has to be set only when there can be non valid solutions
#' @param valid A function that, given a solution, determines whether it is valid or not
#' @param correct A function that, given a non valid solution, corrects it. This optional parameter has to be set only with the \code{'correct'} option
#' @param resources Object of class \code{\link{CResource}} representing the available computational resources for the search
#' @param ... Special argument to pass additional parameters to the functions used in the search
#' @return The function returns an object of class \code{\link{MHResult}} with all the information about the search
#' @details If the function provided in the \code{generate.solution} parameter generates solutions completely at random, then the function performs a random multistart search; if the funciton generates random greedy solutions, then the function performs a GRASP-like search

multistart.local.search<-function (evaluate , generate.solution , neighborhood, selector , num.restarts = NULL ,  
                                   do.log = TRUE , verbose = TRUE , non.valid='ignore' , 
                                   valid=function(solution){TRUE} , correct=function(solution){solution}, resources = cresource() , ...){
  ## Perform the initial local search
  initial.solution <- generate.solution() 
  search.result <- basic.local.search(evaluate = evaluate , initial.solution = initial.solution , 
                                      neighborhood = neighborhood , selector = selector , 
                                      do.log = do.log , verbose = verbose ,
                                      non.valid = non.valid , valid = valid , 
                                      correct = correct , resources = resources)
  ## Extract the best solution and its evaluation, as well as the remaining resources
  current.solution <- optima(search.result)[[1]]
  current.evaluation <- evaluation(search.result)
  resources <- resources(search.result)
  log <- progress(search.result)
  restarts <- 0
  while(!is.finished(resources) & (is.null(num.restarts) || restarts < num.restarts)){
    restarts <- restarts + 1
    if (verbose) cat(paste("\n## Restart #" , restarts , " out of " , 
                           ifelse(is.null(num.restarts),"no limit",num.restarts) , 
                           "; Best solution so far: " , current.evaluation , "\n" , sep = ""))
    if (verbose) cat(paste("## -----------------------------------------------\n\n" , sep = ""))
    initial.solution <- generate.solution() 
    search.result <- basic.local.search(evaluate = evaluate , initial.solution = initial.solution , 
                                        neighborhood = neighborhood , selector = selector , 
                                        do.log = do.log , verbose = verbose ,
                                        non.valid = non.valid , valid = valid , 
                                        correct = correct , resources = resources)
    ## If we have a better solution, update the current best
    if (evaluation(search.result) < current.evaluation){
      current.solution <- optima(search.result)[[1]]
      current.evaluation <- evaluation(search.result)
    }
    ## Get the updated resources
    resources <- resources(search.result)
    
    ## Append the new search to the log
    log <- rbind (log , progress(search.result))
  }
  
  ## Build the output
  mhresult(algorithm = "Multistart Local Search" ,
           description = paste("Local search with multistarts guided by " , deparse(substitute(evaluate))) ,
           parameters = list(selector = deparse(substitute(selector)) , 
                             neighborhood = deparse(substitute(neighborhood)) , 
                             restarts = num.restarts) ,
           optima = list (current.solution) , 
           evaluation = current.evaluation , 
           resources = resources ,
           log = log)
}




#' @title Iterated local search (ILS)
#'
#' @description This function performs a basic ILS algorithm
#' @param evaluate Function of a single parameter. Given a solution, the function returns its evaluation as a real number
#' @param initial.solution Solution from where the search will start
#' @param neighborhood Object representing the type of neighborhood to be used
#' @param selector A function used to select a solution from the neighborhood. For an example of the parameters required and the result it should produce, see \code{\link{greedy.selector}}
#' @param perturb A perturbation function. This function has to contain at least one parameter, named \code{solution}, the solution to be perturbed
#' @param accept A function to determine when a new local optimum is accepted. This function has to have, at least, one parameter named \code{delta}, corresponding to the difference between the evaluation of the new solution and the existing one.
#' @param num.restarts Number of restarts to perform. If NULL, then restarts will be performed unitl the resources are finished
#' @param do.log Logic value to indicate whether the progress shoul be tracked or not
#' @param verbose Logic value to indicate whether the function should print information about the search
#' @param non.valid Action to be performed when a non valid solution is considered. The options are \code{'ignore'}, meaning that the solution is considered anyway, \code{'discard'}, meaning that the solution is not considered and \code{'correct'}, meaning that the solution has to be corrected. This parameter has to be set only when there can be non valid solutions
#' @param valid A function that, given a solution, determines whether it is valid or not
#' @param correct A function that, given a non valid solution, corrects it. This optional parameter has to be set only with the \code{'correct'} option
#' @param resources Object of class \code{\link{CResource}} representing the available computational resources for the search
#' @param ... Special argument to pass additional parameters to the functions used in the search
#' @return The function returns an object of class \code{\link{MHResult}} with all the information about the search
#' @details If the function provided in the \code{generate.solution} parameter generates solutions completely at random, then the function performs a random multistart search; if the funciton generates random greedy solutions, then the function performs a GRASP-like search

iterated.local.search<-function (evaluate , initial.solution , neighborhood, selector , perturb , accept = threshold.accept , 
                                 num.restarts = NULL , do.log = TRUE , verbose = TRUE , non.valid='ignore' , valid=function(solution){TRUE} , 
                                 correct=function(solution){solution}, resources = cresource() , ...){
  ## Perform the initial local search
  search.result <- basic.local.search(evaluate = evaluate , initial.solution = initial.solution , 
                                      neighborhood = neighborhood , selector = selector , 
                                      do.log = do.log , verbose = verbose ,
                                      non.valid = non.valid , valid = valid , 
                                      correct = correct , resources = resources)
  ## Extract the best solution and its evaluation, as well as the remaining resources
  current.solution <- optima(search.result)[[1]]
  current.evaluation <- evaluation(search.result)
  best.solution <- current.solution
  best.evaluation <- current.evaluation
  resources <- resources(search.result)
  log <- progress(search.result)
  restarts <- 0
  while(!is.finished(resources) & (is.null(num.restarts) || restarts < num.restarts)){
    restarts <- restarts + 1
    if (verbose) cat(paste("\n## Perturbation step #" , restarts , " out of " , 
                           ifelse(is.null(num.restarts),"no limit",num.restarts) , 
                           "; Best solution so far: " , current.evaluation , "\n" , sep = ""))
    if (verbose) cat(paste("## -----------------------------------------------\n\n" , sep = ""))
    initial.solution <- perturb(current.solution) 
    search.result <- basic.local.search(evaluate = evaluate , initial.solution = initial.solution , 
                                        neighborhood = neighborhood , selector = selector , 
                                        do.log = do.log , verbose = verbose ,
                                        non.valid = non.valid , valid = valid , 
                                        correct = correct , resources = resources)
    ## If we have a better solution, update the current best
    if (evaluation(search.result) < best.evaluation){
      best.solution <- optima(search.result)[[1]]
      best.evaluation <- evaluation(search.result)
    }
    ## Modify the current solution
    if (accept(delta = evaluation(search.result) - best.evaluation , ...)){
      current.solution <- optima(search.result)[[1]]
      current.evaluation <- evaluation(search.result)
    }
    
    ## Get the updated resources
    resources <- resources(search.result)
    
    ## Append the new search to the log
    log <- rbind (log , progress(search.result))
  }
  
  ## Build the output
  mhresult(algorithm = "Iterated Local Search" ,
           description = paste("ILS guided by " , deparse(substitute(evaluate))) ,
           parameters = list(selector = deparse(substitute(selector)) , 
                             neighborhood = deparse(substitute(neighborhood)) , 
                             restarts = num.restarts) ,
           optima = list (best.solution) , 
           evaluation = best.evaluation , 
           resources = resources ,
           log = log)
}
