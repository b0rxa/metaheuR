#' @title Simulated annealing
#'
#' @description This function performs a basic simulated annealing search
#' @param evaluate Function of a single parameter. Given a solution, the function returns its evaluation as a real number 
#' @param initial.solution Solution from where the search will start
#' @param neighborhood Object representing the type of neighborhood to be used
#' @param cooling.scheme A that, given the current tempreature, returns the new (smaller) temperature
#' @param initial.temperature Temperature to be used at the first iteration
#' @param eq.criterion Criterion to be used to determine when the equilibrium for the current temperature has been reached. There are two possible values, \code{evaluations} or \code{acceptances}
#' @param eq.value The value associated to the equilibrium criterion. If \code{eq.cirterion = 'evaluations'} this value represents the number of evaluations. If \code{eq.cirterion = 'accepts'} this value represents the number of accepted solutions#' 
#' @param do.log Logic value to indicate whether the progress shoul be tracked or not
#' @param log.frequency This value determines after how many evaluations a new log line is added. By default, its value is 100
#' @param verbose Logic value to indicate whether the function should print information about the search
#' @param non.valid Action to be performed when a non valid solution is considered. The options are \code{'ignore'}, meaning that the solution is considered anyway, \code{'discard'}, meaning that the solution is not considered and \code{'correct'}, meaning that the solution has to be corrected. This parameter has to be set only when there can be non valid solutions
#' @param valid A function that, given a solution, determines whether it is valid or not
#' @param correct A function that, given a non valid solution, corrects it. This optional parameter has to be set only with the \code{'correct'} option
#' @param resources Object of class \code{\linkS4class{cResource}} representing the available computational resources for the search. Bear in mind that there is no other stop criterion beyond a limited amount of resources. Therefore, you should set, at least, a limit to the total time, evaluations or iterations
#' @param ... Special argument to pass additional parameters to the functions used in the search
#' @return The function returns an object of class \code{\linkS4class{mHResult}} with all the information about the search
#' @details The \code{cooling.scheme} function has to have at least one parameter, \code{temperature}, and it should produce a value smaller than the one provided in that parameter. For an example, see \code{\link{linearCooling}}
#' 
simulatedAnnealing <- function (evaluate, initial.solution, neighborhood, 
                                cooling.scheme, initial.temperature, final.temperature, 
                                eq.criterion="evaluations", eq.value, do.log=TRUE, 
                                log.frequency=100, verbose=TRUE, non.valid="ignore",
                                valid=allValid, correct=doNothing, resources=cResource(), 
                                ...) {
  if (!valid(initial.solution)) {
    stop ("A valid solution has to be provided as the initial solution")
  }
  # Evaluate the initial solution and initialize the counters
  t0 <- Sys.time()
  current.solution <- initial.solution
  current.evaluation <- evaluate(current.solution)
  best.solution <- current.solution
  best.evaluation <- current.evaluation
  addConsumed(resources, t=as.numeric(Sys.time() - t0), ev=1)
  temperature <- initial.temperature
  iteration <- 0
  log <- NULL
  if (do.log){
    log <- data.frame(Iterations=getConsumedIterations(resources),
                      Evaluations=getConsumedEvaluations(resources),
                      Time=getConsumedTime (resources), 
                      Current_sol=current.evaluation,
                      Current_sd=NA, 
                      Best_sol=best.evaluation)
  }
  isInEq <- switch(eq.criterion,
                        "evaluations"={
                          function (eval , acc) {
                            return(eval > eq.value)
                          }
                        },
                        "acceptances"={
                          function (eval , acc) {
                            return(acc > eq.value)
                          }
                        })
  stop.criterion <- isFinished(resources)
  # Main loop of the search, get each neighbor and evaluate it
  t0 <- Sys.time()
  while(!isFinished(resources) & temperature > final.temperature) {
    t0 <- Sys.time()
    iteration <- iteration + 1
    if (verbose) {
      message("Running iteration ", iteration, " at temperature = ", temperature, 
              ". Current solution:", current.evaluation)
    }
    evaluations <- 0
    acceptances <- 0
    # Equlibrium loop
    while (!isInEq(evaluations, acceptances) & !isFinished(resources)) {
      if (!hasMoreNeighbors(neighborhood)) {
        resetNeighborhood(neighborhood=neighborhood, 
                          solution=current.solution)
      }
      rnd.solution <- nextNeighbor(neighborhood)
      evaluations <- evaluations + 1
      if (!valid(rnd.solution) && non.valid != "ignore") {
        if (non.valid == "discard"){
          next
        } else if(non.valid == "correct") {
          rnd.solution <- correct(rnd.solution)
        } else {
          stop ("Unknown solution for the 'non.valid' parameter. Valid options ",
                "are 'ignore' , 'discard' or 'correct'")
        }
      }
      rnd.evaluation <- evaluate(rnd.solution)
      accept.solution <- boltzmannAccept(delta=rnd.evaluation - current.evaluation, 
                                         temperature=temperature)
      if (accept.solution) {
        current.solution <- rnd.solution
        current.evaluation <- rnd.evaluation
        accptances <- acceptances + 1
        if (rnd.evaluation < best.evaluation) {
          best.solution <- rnd.solution
          best.evaluation <- rnd.evaluation
        }
      }
      
      addConsumed(resources, t=as.numeric(Sys.time() - t0), ev=1)
      
      if (do.log & evaluations %% log.frequency == 0) {
        log <- rbind(log, data.frame(Iterations=iteration,
                                     Evaluations=getConsumedEvaluations(resources),
                                     Time=getConsumedTime(resources), 
                                     Current_sol=current.evaluation,
                                     Current_sd=NA, 
                                     Best_sol=best.evaluation))
      }
    }
    addConsumed(resources, it=1)
    temperature <- cooling.scheme (temperature)
  }
  
  # Build the output
  res <- mHResult(algorithm="Simulated Annealing",
           description=paste("Basic simulated annealing guided by ", 
                             deparse(substitute(evaluate))),
           parameters=list(neighborhood=deparse(substitute(neighborhood)),
                           cooling.sheme=deparse(substitute(selector)),
                           initial.temperature=initial.temperature,
                           eq.criterion=eq.criterion,
                           eq.value=eq.value),
           solution=best.solution,
           evaluation=best.evaluation,
           resources=resources,
           log=log)
  return(res)
}