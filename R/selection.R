#' @title Tournament selection
#'
#' @description This function implements the tournament selection.
#' @param population  List of individuals where the selection will be applied.
#' @param evaluation Vector with the evaluation of the individuals.
#' @param size Number of individuals to be selected.
#' @param ... Ignored
#' @details This function takes random pairs of individuals and selects the best one. If the population contains no individual, \code{NULL} is returned. If there is only one individual, it is returned \code{size} times.
#' @examples
#' 
#' url <- system.file("bays29.xml.zip", package = "metaheuR")
#' cost.matrix <- tsplib.parser(url)
#' tsp <- tsp.problem(cost.matrix)
#' n <- ncol(cost.matrix)
#' rnd.pop <- lapply(1:20 , FUN = function(x) random.permutation(n))
#' eval <- unlist(lapply (rnd.pop , FUN = tsp$evaluate))
#' tournament.selection(population = rnd.pop , evaluation = eval , size = 5)
#' 
tournament.selection <- function (population , evaluation , size = 2 , ...){
  if (length(population) != length(evaluation)) stop("The arguments 'population' and 'evaluation' should have the same length")
  
  n <- length(population)
  
  if (n == 0){
    subpop <- NULL
    subpop.eval <- NULL
  }else if (n == 1){
    subpop <- lapply (1:size , FUN = function(i) population[[1]])
    subpop.eval <- lapply (1:size , FUN = function(i) evaluation)
  }else{
    tournament <- function (i){
      s <- sample(1:n , 2 , FALSE)
      ifelse(evaluation[s[1]]<evaluation[s[2]] , s[1] , s[2])
    }
    id <- sapply(1:size , FUN = tournament)  
    subpop <- population[id]
    subpop.eval <- evaluation[id]
  }
  list(population = subpop , evaluation = subpop.eval)
}

#' @title Elitist selection
#'
#' @description This function implements the elitist selection, i.e., selection of the best fitted individuals
#' @param population  List of individuals where the selection will be applied.
#' @param evaluation Vector with the evaluation of the individuals.
#' @param size Number of individuals to be selected.
#' @param ... Ignored
#' @details This function selects the individuals with the lowest evaluation function. If the population contains no individual, \code{NULL} is returned. If the number of individuals is smaller than size, then the whole population is returned.
#' @examples
#' 
#' url <- system.file("bays29.xml.zip", package = "metaheuR")
#' cost.matrix <- tsplib.parser(url)
#' tsp <- tsp.problem(cost.matrix)
#' n <- ncol(cost.matrix)
#' rnd.pop <- lapply(1:20 , FUN = function(x) random.permutation(n))
#' eval <- unlist(lapply (rnd.pop , FUN = tsp$evaluate))
#' elitist.selection(population = rnd.pop , evaluation = eval , size = 5)
#' 
elitist.selection <- function (population , evaluation , size = 2 , ...){
  if (length(population) != length(evaluation)) stop("The arguments 'population' and 'evaluation' should have the same length")
  
  n <- length(population)
  
  if (n == 0){ ## No individuals in the population
    subpop <- NULL
    subpop.eval <- NULL
  }else if (n < size){## Less individuals in the population than the asked
    subpop <- population
    subpop.eval <- evaluation
  }else{## Otherwise, take the best individuals
    id <- order(evaluation , decreasing = F) [1:size]
    subpop <- population[id]
    subpop.eval <- evaluation[id]
  }
  list(population = subpop , evaluation = subpop.eval)
}


#' @title Roulette selection
#'
#' @description This function implements the selection based on a biased roulette
#' @param population  List of individuals where the selection will be applied.
#' @param evaluation Vector with the evaluation of the individuals.
#' @param size Number of individuals to be selected.
#' @param ... Ignored
#' @details This function selects the individuals with the lowest evaluation function. If the population contains no individual, \code{NULL} is returned.
#' @examples
#' 
#' url <- system.file("bays29.xml.zip", package = "metaheuR")
#' cost.matrix <- tsplib.parser(url)
#' tsp <- tsp.problem(cost.matrix)
#' n <- ncol(cost.matrix)
#' rnd.pop <- lapply(1:20 , FUN = function(x) random.permutation(n))
#' eval <- unlist(lapply (rnd.pop , FUN = tsp$evaluate))
#' roulette.selection(population = rnd.pop , evaluation = eval , size = 5 , use.rankings = TRUE)
#' roulette.selection(population = rnd.pop , evaluation = eval , size = 9 , use.rankings = FALSE)
#' 
roulette.selection <- function (population , evaluation , size = 2 , use.rankings , ...){
  if (length(population) != length(evaluation)) stop("The arguments 'population' and 'evaluation' should have the same length")
  
  n <- length(population)
  
  if (n == 0){ ## No individuals in the population
    subpop <- NULL
    subpop.eval <- NULL
  }else{## Otherwise, use the roulette
    if (use.rankings){
      rank <- rank(evaluation) ## The first ranked, the one with the lowest value
      ## The most probable individual has to be the first one
      probabilities <- (1/rank) / sum(1/rank)
    }else{
      ## probability inversly proportional to the evaluation (the smaller the evaluation, the more probable the individual)
      probabilities <- (1/evaluation) / sum(1/evaluation)
    }
    
    ## Do the random sampling
    cs <- cumsum(probabilities)
    id <- sapply (1:size , FUN = function(i) min(which(cs>runif(1))))
    
    subpop <- population[id]
    subpop.eval <- evaluation[id]
  }
  list(population = subpop , evaluation = subpop.eval)
}