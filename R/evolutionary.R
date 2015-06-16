#' @title Basic Genetic Algorithm
#'
#' @export
#' @family Evoluationary algorithms
#' @description This function implements a basic version of the genetic algorithm
#' @param evaluate Function of a single parameter. Given a solution, the function returns its evaluation as a real number. 
#' @param initial.population A list of solutions representing the population to initialize the algorithm.
#' @param selectSubpopulation Function to select individuals to create the new generation. See details for more information about this function.
#' @param selection.ratio Ratio of the population that is selected and kept in the new generation.
#' @param selectCross Function to select individuals from the selected individuals to be crossed. See details for more information about this function.
#' @param cross Function to cross solutions. See details for more information.
#' @param mutate Function to mutate solutions. It's first argument has to be the solution to mutate and return the mutated solution. It can have other arguments and it should have, at the end, the special argument ...
#' @param mutation.rate Probability of mutating a solution. 
#' @param do.log Logic value to indicate whether the progress shoul be tracked or not
#' @param verbose Logic value to indicate whether the function should print information about the search
#' @param non.valid Action to be performed when a non valid solution is considered. The options are \code{'ignore'}, meaning that the solution is considered anyway, \code{'discard'}, meaning that the solution is not considered and \code{'correct'}, meaning that the solution has to be corrected. This parameter has to be set only when there can be non valid solutions
#' @param valid A function that, given a solution, determines whether it is valid or not
#' @param correct A function that, given a non valid solution, corrects it. This optional parameter has to be set only with the \code{'correct'} option
#' @param resources Object of class \code{\linkS4class{cResource}} representing the available computational resources for the search. Bear in mind that there is no other stop criterion beyond a limited amount of resources. Therefore, you should set, at least, a limit to the total time, evaluations or iterations
#' @param ... Special argument to pass additional parameters to the functions used in the search
#' @return The function returns an object of class \code{\linkS4class{mHResult}} with all the information about the search
#' @details The first three arguments in the selection functions have to be a list with the population, a vector with the evaluation of the individuals in the population and the size of the selection. Additionally the functio has to have the special argument ... The function should return a list with two elements named \code{population} and \code{evaluation}. The former has to be a list with the selected individuals and the second a vector with theri evaluation. For an example, please see function \code{\link{tournamentSelection}}. The crossover function has to have, as the first two arguments, two solutions to be crossed. It can have more arguments and has to have as last argument the ... This function should return a list with the newly created solutoins.
#' 
#' @seealso \code{\link{tournamentSelection}}
#' 
basicGeneticAlgorithm <- function (evaluate, initial.population, 
                                   selectSubpopulation,  selection.ratio, 
                                   selectCross, cross, mutate, mutation.rate, 
                                   do.log=TRUE, verbose=TRUE, non.valid="ignore", 
                                   valid=allValid, correct=doNothing, 
                                   resources=cResource(), ...) {
  
  if (selection.ratio <= 0 | selection.ratio >= 1) {
    stop("The selection ratio has to be >0 and <1")
  }
  
  if (mutation.rate < 0) {
    warning("The probability of mutation has been set to a negative value. No solution will be mutated")
  }
  
  if (mutation.rate > 1) {
    warning("The probability of mutation has been set to a value greater than 1. All the solutions will be mutated")
  }
  
  if (!non.valid %in% c("ignore", "discard", "correct")) {
    stop ("Unknown solution for the 'non.valid' parameter. Valid options are 'ignore' , 'discard' or 'correct'")
  }
  
  # Initialization
  t0 <- Sys.time()
  current.population <- initial.population
  pop.size <- length(initial.population)
  current.evaluation <- sapply(current.population, FUN = evaluate)
  evaluations <- length(initial.population)
  
  addConsumed(resources, t=as.numeric(Sys.time() - t0), ev=evaluations)
  
  best.evaluation <- min(current.evaluation)
  best.solution <- current.population[which.min(current.evaluation)]
  
  log <- NULL
  if (do.log)
    log <- data.frame(Iterations=getConsumedIterations(resources),
                      Evaluations=getConsumedEvaluations(resources),
                      Time=getConsumedTime (resources), 
                      Current_sol=mean(current.evaluation),
                      Current_sd=sd(current.evaluation) , 
                      Best_sol=best.evaluation)
  
  # Main Loop
  generation <- 0
  subpopulation.size <- round(selection.ratio * pop.size)
  while(!isFinished(resources)) {
    generation <- generation + 1
    if (verbose) {
      message("Running generation ", generation, ". Current population = ", 
              signif(mean(current.evaluation) , 3), " +- ", 
              signif(sd(current.evaluation) , 3), ". Best solution = ", 
              best.evaluation)
    }
    
    # Get the subpopulation
    t0 <- Sys.time()
    selected <- selectSubpopulation(current.population, current.evaluation,
                                      subpopulation.size, ...)
    
    ## Generate new solutions
    needed.solutions <- pop.size - length(selected$population)
    new.solutions <- list()
    while(length(new.solutions) < needed.solutions) {
      to.cross <- selectCross(population=selected$population , 
                               evaluation=selected$evaluation, 
                               size=2, ...)$population
      cross.results <- cross(to.cross[[1]], to.cross[[2]], ...)
      cross.results <- lapply (cross.results, 
                               FUN = function(x) {
                                 if (runif(1) < mutation.rate) {
                                   mutate(x , ...)
                                 } else {
                                   x
                                 }})
      new.solutions <- append(new.solutions, cross.results)
      
      # Handle any non valid solution
      new.solutions <- lapply(new.solutions, 
                              FUN = function(x) {
                                checkSolutions(x,non.valid,valid,correct)
                              })
    }
    new.solutions <- new.solutions [1:needed.solutions]
    # Evaluate the new solutions
    new.sol.evaluations <- sapply(new.solutions, evaluate)
    
    addConsumed(resources, t=as.numeric(Sys.time() - t0), 
                ev=needed.solutions, it=1)
    
    # Update the best solution so far.
    best <- min(new.sol.evaluations)
    if (best < best.evaluation) {
      best.solution <- new.solutions[which.min(new.sol.evaluations)]
      best.evaluation <- best
    }
    
    # Create the new populations based merging the selected and the new individuals
    current.population <- append(selected$population, new.solutions)
    current.evaluation <- append(selected$evaluation, new.sol.evaluations)
    
    # Logging
    if (do.log)
      log <- rbind(log, data.frame(Iterations=getConsumedIterations(resources),
                                   Evaluations=getConsumedEvaluations(resources),
                                   Time=getConsumedTime(resources), 
                                   Current_sol=mean(current.evaluation),
                                   Current_sd=sd(current.evaluation), 
                                   Best_sol=best.evaluation))
    
  }  
  # Build the output
  res <- mHResult(algorithm="Basic Genetic Algorithm",
                  description=paste("Basic Genetic Algorithm guided by ", 
                                    deparse(substitute(evaluate))),
                  parameters=list(crossover.operator=deparse(substitute(cross)),
                                  selector.subpopulation=deparse(substitute(selectSubpopulation)),
                                  selection.rate=selection.ratio,
                                  selector.cross=deparse(substitute(selectCross)),
                                  muatation=deparse(substitute(selectCross)),
                                  mutation.rate=mutation.rate),
                  solution=best.solution,
                  evaluation=best.evaluation,
                  resources=resources,
                  log=log)
  return(res)
}



#' @title Basic Genetic Algorithm
#'
#' @export
#' @family Evoluationary algorithms
#' @description This function implements a basic version of the genetic algorithm
#' @param evaluate Function of a single parameter. Given a solution, the function returns its evaluation as a real number. 
#' @param initial.population A list of solutions representing the population to initialize the algorithm.
#' @param selectSubpopulation Function to select individuals to create the new generation. See details for more information about this function.
#' @param selection.ratio Ratio of the population that is selected and kept in the new generation.
#' @param selectCross Function to select individuals from the selected individuals to be crossed. See details for more information about this function.
#' @param cross Function to cross solutions. See details for more information.
#' @param mutate Function to mutate solutions. It's first argument has to be the solution to mutate and return the mutated solution. It can have other arguments and it should have, at the end, the special argument ...
#' @param mutation.rate Probability of mutating a solution. 
#' @param do.log Logic value to indicate whether the progress shoul be tracked or not
#' @param verbose Logic value to indicate whether the function should print information about the search
#' @param non.valid Action to be performed when a non valid solution is considered. The options are \code{'ignore'}, meaning that the solution is considered anyway, \code{'discard'}, meaning that the solution is not considered and \code{'correct'}, meaning that the solution has to be corrected. This parameter has to be set only when there can be non valid solutions
#' @param valid A function that, given a solution, determines whether it is valid or not
#' @param correct A function that, given a non valid solution, corrects it. This optional parameter has to be set only with the \code{'correct'} option
#' @param resources Object of class \code{\linkS4class{cResource}} representing the available computational resources for the search. Bear in mind that there is no other stop criterion beyond a limited amount of resources. Therefore, you should set, at least, a limit to the total time, evaluations or iterations
#' @param ... Special argument to pass additional parameters to the functions used in the search
#' @return The function returns an object of class \code{\linkS4class{mHResult}} with all the information about the search
#' @details The first three arguments in the selection functions have to be a list with the population, a vector with the evaluation of the individuals in the population and the size of the selection. Additionally the functio has to have the special argument ... The function should return a list with two elements named \code{population} and \code{evaluation}. The former has to be a list with the selected individuals and the second a vector with theri evaluation. For an example, please see function \code{\link{tournamentSelection}}. The crossover function has to have, as the first two arguments, two solutions to be crossed. It can have more arguments and has to have as last argument the ... This function should return a list with the newly created solutoins.
#' 
#' @seealso \code{\link{tournamentSelection}}
#' 
basicEda <- function (evaluate, initial.population, selectSubpopulation,  
                      selection.ratio, learn, do.log=TRUE, verbose=TRUE, 
                      non.valid='ignore', valid=allValid, correct=doNothing, 
                      resources=cResource(), ...) {
  
  if (selection.ratio <= 0 | selection.ratio >= 1) {
    stop("The selection ratio has to be >0 and <1")
  }

  if (!non.valid %in% c("ignore", "discard", "correct")) {
    stop ("Unknown solution for the 'non.valid' parameter. Valid options are ",
          "'ignore', 'discard' or 'correct'")
  }
  
  # Initialization
  t0 <- Sys.time()
  current.population <- initial.population
  pop.size <- length(initial.population)
  current.evaluation <- sapply(current.population , FUN = evaluate)
  evaluations <- length(initial.population)
  
  addConsumed(resources, t=as.numeric(Sys.time() - t0), ev=evaluations)
  
  best.evaluation <- min(current.evaluation)
  best.solution <- current.population[which.min(current.evaluation)]
  
  log <- NULL
  if (do.log) {
    log <- data.frame(Iterations=getConsumedIterations(resources),
                      Evaluations=getConsumedEvaluations(resources),
                      Time=getConsumedTime (resources), 
                      Current_sol=mean(current.evaluation),
                      Current_sd=sd(current.evaluation), 
                      Best_sol=best.evaluation)
  }
  
  # Main Loop
  generation <- 0
  subpopulation.size <- round(selection.ratio * pop.size)
  while(!isFinished(resources)) {
    generation <- generation + 1
    if (verbose) {
      message("Running generation ", generation, ". Current population = ", 
              signif(mean(current.evaluation), 3), " +- ", 
              signif(sd(current.evaluation), 3), ". Best solution = ", best.evaluation)
    }
    # Get the subpopulation
    t0 <- Sys.time()
    selected <- selectSubpopulation (current.population,
                                      current.evaluation,
                                      subpopulation.size, ...)
    # Generate new solutions
    needed.solutions <- pop.size - length(selected$population)
    
    # Build the model and sample it
    model <- learn(selected$population)
    new.solutions <- simulate(model, needed.solutions, ...)
    
    # Evaluate the new solutions
    new.sol.evaluations <- sapply(new.solutions, evaluate)
    
    addConsumed(resources, 
                t=as.numeric(Sys.time() - t0), ev=needed.solutions, it=1)
    
    # Update the best solution so far.
    best <- min(new.sol.evaluations)
    if (best < best.evaluation) {
      best.solution <- new.solutions[which.min(new.sol.evaluations)]
      best.evaluation <- best
    }
    
    # Create the new populations based merging the selected and the new individuals
    current.population <- append(selected$population, new.solutions)
    current.evaluation <- append(selected$evaluation, new.sol.evaluations)
    
    # Logging
    if (do.log) {
      log <- rbind(log, data.frame(Iterations=getConsumedIterations(resources),
                                   Evaluations=getConsumedEvaluations(resources),
                                   Time=getConsumedTime (resources),
                                   Current_sol=mean(current.evaluation),
                                   Current_sd = sd(current.evaluation),
                                   Best_sol = best.evaluation))
    }
  }  
  # Build the output
  res <- mHResult(algorithm="Basic EDA",
                  description=paste("Basic Estimation of Distribution Algorithm guided by ", 
                                    deparse(substitute(evaluate))),
                  parameters=list(learn=deparse(substitute(learn)),
                                  selector.subpopulation=deparse(substitute(selectSubpopulation)),
                                  selection.rate=selection.ratio),
                  solution=best.solution,
                  evaluation=best.evaluation,
                  resources=resources,
                  log=log)
return(res)
}