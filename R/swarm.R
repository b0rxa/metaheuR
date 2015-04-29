#' @title Chek solutions
#'
#' @description This function checks the validity of a solution and does the required action if the soution is not valid
#' @param solution The solution to be cheked
#' @param non.valid Action to be performed if the solution is not valid. Options are \code{'ignore'},\code{'discard'} and \code{'correct'}. 
#' @param valid Function to check the validity of the solution
#' @param correct Function to correct the solution.
#' @keywords internal
check.solutions <- function(solution , non.valid , valid , correct){
  if (!valid(solution) && non.valid!='ignore'){
    if (non.valid=='discard'){
      solution <- NULL
    }else if(non.valid=='correct'){
      solution <- correct(solution)
    }
  }
  solution
}

## Auxiliar, non-exported function to generate solutions taking into account the handling of non-valid solutions
create.new.solutions <- function (pheromones , needed.solutions , non.valid , valid , correct , ...){
  new.solutions <- list()
  c <- 0
  while(length(new.solutions) < needed.solutions){
    c <- c+1
    new.sol <- build.solution (pheromones , n=1 , ...)[[1]]
    ## Handle any non valid solution
    new.solutions <- append(new.solutions , list(check.solutions(new.sol,non.valid,valid,correct)))
  }
  new.solutions
}



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
#' @param resources Object of class \code{\linkS4class{CResource}} representing the available computational resources for the search. Bear in mind that there is no other stop criterion beyond a limited amount of resources. Therefore, you should set, at least, a limit to the total time, evaluations or iterations
#' @param ... Special argument to pass additional parameters to the functions used in the search
#' @return The function returns an object of class \code{\linkS4class{MHResult}} with all the information about the search
#' @details The first three arguments in the selection functions have to be a list with the population, a vector with the evaluation of the individuals in the population and the size of the selection. Additionally the functio has to have the special argument ... The function should return a list with two elements named \code{population} and \code{evaluation}. The former has to be a list with the selected individuals and the second a vector with theri evaluation. For an example, please see function \code{\link{tournament.selection}}. The crossover function has to have, as the first two arguments, two solutions to be crossed. It can have more arguments and has to have as last argument the ... This function should return a list with the newly created solutoins.
#' 
#' @seealso \code{\link{tournament.selection}}

basic.aco<-function (evaluate , nants , pheromones , update.sol = 'best.it', update.value = NULL , do.log = TRUE , verbose = TRUE , non.valid='ignore', valid=function(solution){TRUE} , correct=function(solution){solution}, resources = cresource() , ...){
  
  if (!is.null(update.value) && update.value <= 0) stop("The value to update the solution has to be a positive value")
    
  ## Initialization
  t0 <- Sys.time()
  
  ## Initialize the best solution
  best.solution <- create.new.solutions(pheromones , 1 , non.valid = non.valid , valid = valid , correct = correct , ...)[[1]]
  best.evaluation <- evaluate(best.solution)
  current.evaluation <- best.evaluation
  add.consumed(resources, t = as.numeric(Sys.time() - t0) , ev = 1)
  
  log <- NULL
  
  ## Main Loop
  iteration <- 0
  while(!is.finished(resources)){
    iteration <- iteration + 1
    if (verbose) cat(paste("Running iteration " , iteration , 
                           ". Current average solution = " , signif(mean(current.evaluation) , 3), 
                           " +- " , signif(sd(current.evaluation) , 3) , 
                           ". Best solution = " , best.evaluation, "\n" , sep=""))
    
    t0 <- Sys.time()
    ## First, we build the new solutions
    current.trail <- create.new.solutions(pheromones , nants , non.valid = non.valid , valid = valid , correct = correct , ...)
    current.evaluation <- sapply(current.trail , FUN = evaluate)
    evaluations <- nants
    
    ## Update the best solution so far.
    best <- min(current.evaluation)
    if (best < best.evaluation){
      best.solution <- current.trail[[which.min(current.evaluation)]]
      best.evaluation <- best
    }
    
    
    ## Now, evaporate the pheromones
    evaporate(pheromones , ...)
    
    ## Finally, update the pheromone trail
    switch(update.sol , #In case we have to use the evaluation function, we use -f, as the goal is to minimize the function
           'best.it' = {
             ## In case the value is null, use the evaluation function of the best solution in the iteration
             value <- ifelse(is.null(update.value) , 1/best , update.value)
             best.sol <- current.trail[[which.min(current.evaluation)]]
             update.trail(pheromones , best.sol , value , ...)
           },
           'best.all' = {
             ## In case the value is null, use the evaluation function of the best solution in the iteration
             value <- ifelse(is.null(update.value) , 1/best.solution , update.value)
             update.trail(pheromones , best.solution , value , ...)
           },
           'all' = {
             sapply(1:nants , FUN = function(i){
               ## In this case the only option is to update the trail with the evaluation function
               update.trail(pheromones , current.trail [[i]] , 1/current.evaluation[i] , ...)
             })
           })
    

    add.consumed(resources, t = as.numeric(Sys.time() - t0) , ev = nants , it = 1)

    ## Logging
    if (do.log)
      log <- rbind(log , data.frame(Iterations = consumed.iterations(resources) ,
                                    Evaluations = consumed.evaluations(resources) ,
                                    Time = consumed.time (resources) , 
                                    Current_sol = mean(current.evaluation) ,
                                    Current_sd = sd(current.evaluation) , 
                                    Best_sol = best.evaluation))
    
  }  
  ## Build the output
  mhresult(algorithm = "Basic ACO" ,
           description = paste("Basic ACO guided by " , deparse(substitute(evaluate))) ,
           parameters = list(nants = nants,
                             pheromones = deparse(substitute(pheromones)),
                             update.sol = update.sol,
                             update.value = update.value),
           optima = list (best.solution) , 
           evaluation = best.evaluation , 
           resources = resources ,
           log = log)
}
