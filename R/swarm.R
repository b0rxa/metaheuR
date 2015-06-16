# NON-EXPORTED FUNCTIONS -------------------------------------------------------

createNewSolutions <- function (pheromones, needed.solutions, non.valid, valid, 
                                correct, ...) {
  # Non-exported function to generate solutions correctly handling the non-valid solutions
  # Args:
  #   pheromones:       Phermone model
  #   needed.solutions: Number of solutions needed
  #   non.valid:        Action to take when non-valid solutions are generated
  #   valid:            Function to check the validity of a solution
  #   correct:          Function to correct solutions
  # Returns:
  #   A list of solutions
  new.solutions <- list()
  c <- 0
  while(length(new.solutions) < needed.solutions) {
    c <- c + 1
    new.sol <- buildSolution(pheromones, n=1, ...)[[1]]
    # Handle any non valid solution
    new.solutions <- append(new.solutions, 
                            list(checkSolutions(new.sol, non.valid, valid, correct)))
  }
  return(new.solutions)
}


generateRandomVelocity <- function (dimension , initial.velocity) {
  # Auxilir function to create random velocities 
  # Args:
  #   dimension:        Length of the resulting vector
  #   initial.velocity: Magnitude of the resulting velocity
  # Return:
  #   Random velocity of the given dimension whose magnitude is intial.velocity
  #
  speed <- runif(dimension)
  speed <- speed / sqrt(sum(speed^2))
  return(speed * initial.velocity)
}



# EXPORTED FUNCTIONS -----------------------------------------------------------

#' @title Basic ACO
#'
#' @export
#' @family Swarm Intelligence
#' @description This function implements a basic version of ACO algorithm
#' @param evaluate Function of a single parameter. Given a solution, the function returns its evaluation as a real number. 
#' @param initial.population A list of solutions representing the population to initialize the algorithm.
#' @param select.subpopulation Function to select individuals to create the new generation. See details for more information about this function.
#' @param selection.ratio Ratio of the population that is selected and kept in the new generation.
#' @param select.cross Function to select individuals from the selected individuals to be crossed. See details for more information about this function.
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
basicAco <- function (evaluate, nants, pheromones, update.sol="best.it", 
                      update.value=NULL, do.log=TRUE, verbose=TRUE, 
                      non.valid="ignore", valid=allValid, correct=doNothing, 
                      resources=cResource(), ...) {
  
  if (!is.null(update.value) && update.value <= 0) {
    stop("The value to update the solution has to be a positive value")
  }
  
  # Initialization
  t0 <- Sys.time()
  
  # Initialize the best solution
  best.solution <- createNewSolutions(pheromones, 1, non.valid=non.valid, 
                                      valid=valid, correct=correct, ...)[[1]]
  best.evaluation <- evaluate(best.solution)
  current.evaluation <- best.evaluation
  addConsumed(resources, t=as.numeric(Sys.time() - t0), ev=1)
  
  log <- NULL
  
  # Main Loop
  iteration <- 0
  while(!isFinished(resources)) {
    iteration <- iteration + 1
    if (verbose) {
      message("Running iteration ", iteration, ". Current average solution = ", 
              signif(mean(current.evaluation), 3), " +- " , 
              signif(sd(current.evaluation), 3), ". Best solution = ", best.evaluation)
    }
    
    t0 <- Sys.time()
    # First, we build the new solutions
    current.trail <- createNewSolutions(pheromones, nants, non.valid=non.valid, 
                                        valid=valid, correct=correct, ...)
    current.evaluation <- sapply(current.trail, FUN=evaluate)
    evaluations <- nants
    
    # Update the best solution so far.
    best <- min(current.evaluation)
    if (best < best.evaluation) {
      best.solution <- current.trail[[which.min(current.evaluation)]]
      best.evaluation <- best
    }
    
    
    # Now, evaporate the pheromones
    evaporate(pheromones, ...)
    
    # Finally, update the pheromone trail
    # In case we have to use the evaluation function, we use -f, as the goal is 
    # to minimize the function
    switch(update.sol, 
           "best.it"={
             # In case the value is null, use the evaluation function of the best solution in the iteration
             if(is.null(update.value)) {
               value <- 1 / best
             } else {
               value <- update.value
             }
             best.sol <- current.trail[[which.min(current.evaluation)]]
             updateTrail(pheromones, best.sol, value, ...)
           },
           "best.all"={
             # In case the value is null, use the evaluation function of the best solution in the iteration
             if(is.null(update.value)) {
               value <- 1 / best.solution
             } else {
               value <- update.value
             }
             updateTrail(pheromones, best.solution, value, ...)
           },
           "all"={
             sapply(1:nants, 
                    FUN=function(i) {
                      # In this case the only option is to update the trail with 
                      # the evaluation function
                      updateTrail(pheromones, current.trail [[i]], 
                                  1 / current.evaluation[i] , ...)
             })
           })
    
    addConsumed(resources, t=as.numeric(Sys.time() - t0), ev=nants, it=1)
    
    # Logging
    if (do.log) {
      log <- rbind(log, data.frame(Iterations=getConsumedIterations(resources),
                                    Evaluations=getConsumedEvaluations(resources),
                                    Time=getConsumedTime (resources) , 
                                    Current_sol=mean(current.evaluation),
                                    Current_sd=sd(current.evaluation), 
                                    Best_sol=best.evaluation))
    }    
  }  
  ## Build the output
  res <- mHResult(algorithm="Basic ACO",
                  description=paste("Basic ACO guided by ", 
                                    deparse(substitute(evaluate))),
                  parameters=list(nants=nants,
                                  pheromones=deparse(substitute(pheromones)),
                                  update.sol=update.sol,
                                  update.value=update.value),
                  solution=best.solution,
                  evaluation=best.evaluation,
                  resources=resources,
                  log = log)
  return(res)
}



#' @title Basic PSO
#'
#' @export
#' @family Swarm Intelligence
#' @description This function implements a basic version of the PSO algorithm
#' @param evaluate Function of a single parameter. Given a solution, the function returns its evaluation as a real number. 
#' @param initial.positions A list of solutions representing the initial positions of the particles
#' @param initial.velocity A list containing the intial velocities of the particles
#' @param max.velocity Maximum velocity allowed to a particle
#' @param c.personal Coefficient for the personal best solution (i.e., the cognitive component)
#' @param c.best Coefficient for the global best solution (i.e., the social component)
#' @param do.log Logic value to indicate whether the progress shoul be tracked or not
#' @param verbose Logic value to indicate whether the function should print information about the search
#' @param non.valid Action to be performed when a non valid solution is considered. The options are \code{'ignore'}, meaning that the solution is considered anyway, \code{'discard'}, meaning that the solution is not considered and \code{'correct'}, meaning that the solution has to be corrected. This parameter has to be set only when there can be non valid solutions
#' @param valid A function that, given a solution, determines whether it is valid or not
#' @param correct A function that, given a non valid solution, corrects it. This optional parameter has to be set only with the \code{'correct'} option
#' @param resources Object of class \code{\linkS4class{cResource}} representing the available computational resources for the search. Bear in mind that there is no other stop criterion beyond a limited amount of resources. Therefore, you should set, at least, a limit to the total time, evaluations or iterations
#' @param ... Special argument to pass additional parameters to the functions used in the search
#' @return The function returns an object of class \code{\linkS4class{mHResult}} with all the information about the search
#' @details This basic version of the PSO only works with numeric problems and, thus, the solutions have to be numeric vectors.
#' 
#' @seealso \code{\link{tournamentSelection}}
#' 
basicPso <- function(evaluate, initial.positions, initial.velocity, max.velocity, 
                     c.personal, c.best, do.log=TRUE, verbose=TRUE, 
                     non.valid="ignore", valid=allValid, correct=doNothing, 
                     resources=cResource(), ...) {
  
  if (initial.velocity < 0) {
    stop("The initial speed has to be a positive value")
  }
  if (max.velocity < 0) {
    stop("The max speed has to be a value greater than zero")
  }
  if (c.best < 0 | c.personal < 0) {
    stop("Both c.best and c.personal have to be positive values")
  }
  
  initial.velocity <- max(initial.velocity, max.velocity)
  
  # Initialization
  t0 <- Sys.time()
  current.positions <- initial.positions
  personal.best <- current.positions
  nparticles <- length(initial.positions)
  current.evaluation <- sapply(initial.positions, FUN=evaluate)
  personal.evaluation <- current.evaluation
  current.speeds <- lapply(1:nparticles, 
                           FUN=function(i) {
                             spd <- generateRandomVelocity(length(current.positions[[1]]), 
                                                 initial.velocity)
                             return(spd)
                           })
  
  addConsumed(resources, t=as.numeric(Sys.time() - t0), ev=nparticles)
  
  best.evaluation <- min(current.evaluation)
  best.solution <- current.positions[[which.min(current.evaluation)]]
  sol.length <- length(best.solution)
  
  log <- NULL
  if (do.log) {
    log <- data.frame(Iterations=getConsumedIterations(resources),
                      Evaluations=getConsumedEvaluations(resources),
                      Time=getConsumedTime(resources), 
                      Current_sol=mean(current.evaluation),
                      Current_sd=sd(current.evaluation), 
                      Best_sol=best.evaluation)
  }
  # Main Loop
  iteration <- 0
  while(!isFinished(resources)) {
    iteration <- iteration + 1
    if (verbose) {
      message("Running iteration ", iteration, ". Current average solution = ", 
              signif(mean(current.evaluation), 3), " +- ", 
              signif(sd(current.evaluation), 3),
              ". Best solution = ", best.evaluation)
    }
    
    t0 <- Sys.time()
    # First we have to update the speeds. For that, generate a random number for 
    # the personal best and another one for the global best
    rnd.personal <- runif(sol.length)
    rnd.best <- runif(sol.length)
    
    current.speeds <- lapply(1:nparticles, 
                             FUN=function (i) {
                               cogn <- rnd.personal * c.personal * (personal.best[[i]] - current.positions[[i]])
                               social <- rnd.best * c.best * (best.solution - current.positions[[i]])
                               new.speed <- current.speeds [[i]] +  cogn + social
                               if (sqrt(sum(new.speed^2)) > max.velocity) {
                                 new.speed <- new.speed / sqrt(sum(new.speed^2)) * max.velocity
                               }
                               return(new.speed)
                             })
    
  
    # Now, we update the position. 
    if (non.valid == 'correct') {
      current.positions <- lapply (1:nparticles, 
                                   FUN=function(i) {
                                     pos <- current.positions[[i]] + current.speeds[[i]]
                                     if (!valid(pos)) {
                                       pos<-correct(pos)
                                     }
                                     return (pos)
                                   })
    } else if (non.valid == "ignore") {
      current.positions <- lapply (1:nparticles,
                                   FUN=function(i) {
                                     pos <- current.positions[[i]] + current.speeds[[i]] 
                                     return(pos)
                                   })
    } else {
      stop("basicPso only accpets two ways of handling non valid solutions, ",
           "ignoring the fact that they are not valid or correcting them")
    }

    
    # Update the global and personal bests
    current.evaluation <- sapply(current.positions, 
                                 FUN=evaluate)
    
    if (min(current.evaluation) < best.evaluation) {
      best.evaluation <- min(current.evaluation)
      best.solution <- current.positions[[which.min(current.evaluation)]]
    }

    snk <- lapply (1:nparticles, 
                   FUN=function (i) {
                     if (current.evaluation[[i]] < personal.evaluation[[i]]) {
                       personal.best[[i]] <<- current.positions[[i]]
                       personal.evaluation[[i]] <<- current.evaluation[[i]]
                   }
                 })
        
    addConsumed(resources, t=as.numeric(Sys.time() - t0), ev=nparticles, it=1)
    
    # Logging
    if (do.log) {
      log <- rbind(log, data.frame(Iterations=getConsumedIterations(resources),
                                   Evaluations=getConsumedEvaluations(resources),
                                   Time=getConsumedTime (resources), 
                                   Current_sol=mean(current.evaluation),
                                   Current_sd=sd(current.evaluation), 
                                   Best_sol=best.evaluation))
    }
  }  
  # Build the output
  res <- mHResult(algorithm="Basic PSO",
                  description=paste("Basic PSO guided by ", 
                                    deparse(substitute(evaluate))),
                  parameters=list(nparticles=nparticles,
                                  initial.solutions=deparse(substitute(initial.positions)),
                                  initial.velocity=initial.velocity,
                                  max.velocity=max.velocity,
                                  c.personal=c.personal,
                                  c.best=c.best),
                  solution=best.solution,
                  evaluation=best.evaluation,
                  resources=resources,
                  log=log)
  return(res)
}
