#' @title Basic local search
#'
#' @description \code{basicLocalSearch} performs a simple local search and returns the best solution
#' @param evaluate Function of a single parameter. Given a solution, the function returns its evaluation as a real number 
#' @param initial.solution Solution from where the search will start
#' @param neighborhood Object representing the type of neighborhood to be used
#' @param selector A function used to select a solution from the neighborhood. For an example of the parameters required and the result it should produce, see \code{\link{greedySelector}}
#' @param do.log Logic value to indicate whether the progress shoul be tracked or not
#' @param save.sols Logic value to indicate whether the evaluated solutions should be stored
#' @param verbose Logic value to indicate whether the function should print information about the search
#' @param non.valid Action to be performed when a non valid solution is considered. The options are \code{'ignore'}, meaning that the solution is considered anyway, \code{'discard'}, meaning that the solution is not considered and \code{'correct'}, meaning that the solution has to be corrected. This parameter has to be set only when there can be non valid solutions
#' @param valid A function that, given a solution, determines whether it is valid or not
#' @param correct A function that, given a non valid solution, corrects it. This optional parameter has to be set only with the \code{'correct'} option
#' @param resources Object of class \code{\linkS4class{cResource}} representing the available computational resources for the search
#' @param ... Special argument to pass additional parameters to the functions used in the search
#' @return The function returns an object of class \code{\linkS4class{mHResult}} with all the information about the search
#' 
basicLocalSearch <- function (evaluate, initial.solution, neighborhood, selector, 
                              do.log=TRUE, save.sols=FALSE, verbose=TRUE, non.valid='ignore', 
                              valid=allValid, correct=doNothing, 
                              resources=cResource(), ...) {
  if (!valid(initial.solution)) {
    stop ("A valid solution has to be provided as the initial solution")
  }
  # Evaluate the initial solution and initialize the counters
  t0 <- Sys.time()
  current.solution <- initial.solution
  current.evaluation <- evaluate(current.solution)
  addConsumed(resources, t=as.numeric(Sys.time() - t0), ev=1)
  stopSearch <- isFinished(resources)
  iteration <- 0
  
  # Progress information
  log  <- NULL
  sols <- NULL
  if (do.log){
    log <- data.frame(Iterations=getConsumedIterations(resources),
                      Evaluations=getConsumedEvaluations(resources),
                      Time=getConsumedTime(resources), 
                      Current_sol=current.evaluation,
                      Current_sd=NA,
                      Best_sol=current.evaluation)
  }
  if (save.sols) {
    sols <- list()
    sols$iteration  <- iteration
    sols$evaluation <- current.evaluation
    sols$solution   <- list(current.solution)
  }
  
  # Main loop of the search, get each neighbor and evaluate it
  while(!stopSearch) {
    iteration <- iteration + 1
    if (verbose) {
      message("Running iteration ", iteration, 
              ". Best solution: ", current.evaluation)
    }
    # Select a solution in the neighborhood
    selection <- selector(neighborhood=neighborhood, evaluate=evaluate, 
                          initial.solution=current.solution, initial.evaluation=current.evaluation,  
                          non.valid=non.valid, valid=valid, correct=correct, resources=resources) 
    
    # If it improves the best so far, record it
    resources <- selection$resources
    if (!is.null(selection$solution)) {
      current.solution <- selection$solution
      current.evaluation <- selection$evaluation
      resources <- addConsumed(resources, it=1)
      stopSearch <- isFinished(resources)  
      t0 <- Sys.time()
      resetNeighborhood(neighborhood=neighborhood, solution=current.solution)
      addConsumed(resources, t=as.numeric(Sys.time() - t0))
    }else{
      stopSearch <- TRUE
    }
    
    # Log the data from the iteration
    if (do.log){
      log <- rbind(log, data.frame(Iterations=getConsumedIterations(resources),
                                   Evaluations=getConsumedEvaluations(resources),
                                   Time=getConsumedTime(resources), 
                                   Current_sol=current.evaluation, 
                                   Current_sd=NA , 
                                   Best_sol=current.evaluation))
    }
    
    # Save the solutions
    if (save.sols) {
      sols$iteration  <- c(sols$iteration, iteration)
      sols$evaluation <- c(sols$evaluation, current.evaluation)
      sols$solution   <- append(sols$solution, current.solution)
    }
  }
  
  # Build the output
  res <- mHResult(algorithm="Basic Local Search",
                  description=paste("Simple local search guided by ", 
                                    deparse(substitute(evaluate))),
                  parameters=list(selector=deparse(substitute(selector)), 
                                  neighborhood=deparse(substitute(neighborhood))),
                  solution=current.solution,
                  evaluation=current.evaluation, 
           resources=resources ,
           log=log,
           solutions=sols)
  return(res)
}


#' @title Local search with multistart
#'
#' @description This function performs a local search with multistart. It can be used to implement random restarts or GRASP algorithms
#' @param evaluate Function of a single parameter. Given a solution, the function returns its evaluation as a real number 
#' @param generateSolution Function to generate initial solutions
#' @param neighborhood Object representing the type of neighborhood to be used
#' @param selector A function used to select a solution from the neighborhood. For an example of the parameters required and the result it should produce, see \code{\link{greedySelector}}
#' @param num.restarts Number of restarts to perform. If NULL, then restarts will be performed unitl the resources are finished
#' @param do.log Logic value to indicate whether the progress shoul be tracked or not
#' @param save.sols Logic value to indicate whether the evaluated solutions should be stored
#' @param verbose Logic value to indicate whether the function should print information about the search
#' @param non.valid Action to be performed when a non valid solution is considered. The options are \code{'ignore'}, meaning that the solution is considered anyway, \code{'discard'}, meaning that the solution is not considered and \code{'correct'}, meaning that the solution has to be corrected. This parameter has to be set only when there can be non valid solutions
#' @param valid A function that, given a solution, determines whether it is valid or not
#' @param correct A function that, given a non valid solution, corrects it. This optional parameter has to be set only with the \code{'correct'} option
#' @param resources Object of class \code{\link{cResource}} representing the available computational resources for the search
#' @param ... Special argument to pass additional parameters to the functions used in the search
#' @return The function returns an object of class \code{\link{mHResult}} with all the information about the search
#' @details If the function provided in the \code{generateSolution} parameter generates solutions completely at random, then the function performs a random multistart search; if the funciton generates random greedy solutions, then the function performs a GRASP-like search
#' 
multistartLocalSearch <- function(evaluate, generateSolution, neighborhood, selector, 
                                  num.restarts=NULL, do.log=TRUE,  save.sols=FALSE, 
                                  verbose=TRUE, non.valid='ignore', valid=allValid, 
                                  correct=doNothing, resources=cResource(), ...) {
  # Perform the initial local search
  initial.solution <- generateSolution(...) 
  search.result <- basicLocalSearch(evaluate=evaluate, initial.solution=initial.solution, 
                                      neighborhood=neighborhood, selector=selector, 
                                      do.log=do.log, save.sols=save.sols, verbose=verbose,
                                      non.valid=non.valid, valid=valid, 
                                      correct=correct, resources=resources, ...)
  # Extract the best solution and its evaluation, as well as the remaining resources
  best.solution <- getSolution(search.result)
  best.evaluation <- getEvaluation(search.result)
    
  resources <- getResources(search.result)
  log <- getProgress(search.result)
  solutions <- NULL
  if (save.sols) {
    solutions <- getEvaluatedSolutions(search.results)
  }
  
  restarts <- 0
  if (is.null(num.restarts)) {
    num.restarts.txt <- "no limit"     
  } else {
    num.restarts.txt <- num.restarts
  }
  
  while(!isFinished(resources) & 
          (is.null(num.restarts) || restarts < num.restarts)) {
    restarts <- restarts + 1
    if (verbose) {
      message("\n## Restart #", restarts, " out of ", num.restarts.txt, 
              "; Best solution so far: ", best.evaluation)
      message("## -----------------------------------------------\n")
    }
    initial.solution <- generateSolution(...) 
    search.result <- basicLocalSearch(evaluate=evaluate, initial.solution=initial.solution, 
                                        neighborhood=neighborhood, selector=selector,
                                        do.log=do.log, save.sols=save.sols, verbose=verbose,
                                        non.valid=non.valid, valid=valid, 
                                        correct=correct, resources=resources, ...)
    # If we have a better solution, update the current best
    if (getEvaluation(search.result) < best.evaluation) {
      best.solution <- getSolution(search.result)
      best.evaluation <- getEvaluation(search.result)
    }
    # Get the updated resources
    resources <- getResources(search.result)
    
    # Append the new search to the log
    newlog <- getProgress(search.result)
    newlog$Best_sol <- best.evaluation
    log <- rbind (log, newlog)
    if (save.sols) {
      newsols <- getEvaluatedSolutions(search.result)
      solutions$evaluation <- c(solutions$evaluation, newsols$evaluation)
      solutions$solution   <- append(solutions$solution, newsols$solution)
      solutions$iteration  <- c(solutions$iteration, newsols$iteration + tail(solutions$iteration, 1))
    }
  }
  
  # Build the output
  res <- mHResult(algorithm="Multistart Local Search",
                  description=paste("Local search with multistarts guided by ", 
                                    deparse(substitute(evaluate))),
                  parameters=list(selector=deparse(substitute(selector)),
                                  neighborhood=deparse(substitute(neighborhood)),
                                  restarts=num.restarts),
                  solution=best.solution,
                  evaluation=best.evaluation, 
                  resources=resources,
                  log=log,
                  solutions=solutions)
  return(res)
}




#' @title Iterated local search (ILS)
#'
#' @description This function performs a basic ILS algorithm
#' @param evaluate Function of a single parameter. Given a solution, the function returns its evaluation as a real number
#' @param initial.solution Solution from where the search will start
#' @param neighborhood Object representing the type of neighborhood to be used
#' @param selector A function used to select a solution from the neighborhood. For an example of the parameters required and the result it should produce, see \code{\link{greedySelector}}
#' @param perturb A perturbation function. This function has to contain at least one parameter, named \code{solution}, the solution to be perturbed
#' @param accept A function to determine when a new local optimum is accepted. This function has to have, at least, one parameter named \code{delta}, corresponding to the difference between the evaluation of the new solution and the existing one.
#' @param num.restarts Number of restarts to perform. If NULL, then restarts will be performed unitl the resources are finished
#' @param do.log Logic value to indicate whether the progress shoul be tracked or not
#' @param save.sols Logic value to indicate whether the evaluated solutions should be stored
#' @param verbose Logic value to indicate whether the function should print information about the search
#' @param non.valid Action to be performed when a non valid solution is considered. The options are \code{'ignore'}, meaning that the solution is considered anyway, \code{'discard'}, meaning that the solution is not considered and \code{'correct'}, meaning that the solution has to be corrected. This parameter has to be set only when there can be non valid solutions
#' @param valid A function that, given a solution, determines whether it is valid or not
#' @param correct A function that, given a non valid solution, corrects it. This optional parameter has to be set only with the \code{'correct'} option
#' @param resources Object of class \code{\link{cResource}} representing the available computational resources for the search
#' @param ... Special argument to pass additional parameters to the functions used in the search
#' @return The function returns an object of class \code{\link{mHResult}} with all the information about the search
#' @details If the function provided in the \code{generateSolution} parameter generates solutions completely at random, then the function performs a random multistart search; if the funciton generates random greedy solutions, then the function performs a GRASP-like search
#' 
iteratedLocalSearch <- function (evaluate, initial.solution, neighborhood, selector, 
                                 perturb, accept=thresholdAccept, num.restarts=NULL, 
                                 do.log=TRUE, save.sols=FALSE, verbose=TRUE, 
                                 non.valid="ignore", valid=allValid, correct=doNothing, 
                                 resources=cResource(), ...) {
  # Perform the initial local search
  search.result <- basicLocalSearch(evaluate=evaluate, initial.solution=initial.solution,
                                    neighborhood=neighborhood, selector=selector,
                                    do.log=do.log, save.sols=save.sols, verbose=verbose,
                                    non.valid=non.valid, valid=valid,
                                    correct=correct, resources=resources, ...)
  # Extract the best solution and its evaluation, as well as the remaining resources
  current.solution <- getSolution(search.result)
  current.evaluation <- getEvaluation(search.result)
  best.solution <- current.solution
  best.evaluation <- current.evaluation
  resources <- getResources(search.result)
  log <- getProgress(search.result)
  solutions <- NULL
  if (save.sols) {
    solutions <- getEvaluatedSolutions(search.result)
  }
  restarts <- 0
  if(is.null(num.restarts)) {
    rst <- "no limit"
  } else {
    rst <- num.restarts
  }
  while(!isFinished(resources) & 
          (is.null(num.restarts) || restarts < num.restarts)) {
    restarts <- restarts + 1
    if (verbose) {
      message("\n## Perturbation step #", restarts, " out of ", rst, 
              "; Best solution so far: ", current.evaluation)
    }
      
    if (verbose) {
      message("## -----------------------------------------------\n")
    }
    initial.solution <- perturb(current.solution, ...) 
    search.result <- basicLocalSearch(evaluate=evaluate, initial.solution=initial.solution,
                                      neighborhood=neighborhood, selector=selector,
                                      do.log=do.log, save.sols=save.sols, verbose=verbose,
                                      non.valid=non.valid, valid=valid,
                                      correct=correct, resources=resources, ...)
    # If we have a better solution, update the current best
    if (getEvaluation(search.result) < best.evaluation) {
      best.solution <- getSolution(search.result)
      best.evaluation <- getEvaluation(search.result)
    }
    # Modify the current solution
    if (accept(delta=getEvaluation(search.result) - best.evaluation, ...)) {
      current.solution <- getSolution(search.result)
      current.evaluation <- getEvaluation(search.result)
    }
    
    # Get the updated resources
    resources <- getResources(search.result)
    
    # Append the new search to the log
    log <- rbind (log, getProgress(search.result))
    
    if (save.sols) {
      newsols <- getEvaluatedSolutions(search.result)
      solutions$evaluation <- c(solutions$evaluation, newsols$evaluation)
      solutions$solution   <- append(solutions$solution, newsols$solution)
      solutions$iteration  <- c(solutions$iteration, 
                                newsols$iteration + tail(solutions$iteration, 1) + 1)
    }
  }
  
  # The best solution recorded is the best found in each local search. To make it the
  # best in any local search we have to check that any solution is better than the previous
  
  log$Best_sol <- cummin(log$Best_sol)
  
  # Build the output
  res <- mHResult(algorithm="Iterated Local Search",
                  description=paste("ILS guided by ", deparse(substitute(evaluate))),
                  parameters=list(selector=deparse(substitute(selector)),
                                  neighborhood=deparse(substitute(neighborhood)),
                                  restarts=num.restarts),
                  solution=best.solution, 
                  evaluation=best.evaluation, 
                  resources=resources,
                  log=log,
                  solutions=solutions)
  return(res)
}
