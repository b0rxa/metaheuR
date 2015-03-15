#' @title Tournament selection
#'
#' @description This function implements the tournament selection.
#' @param population  List of individuals where the selection will be applied.
#' @param evaluation Vector with the evaluation of the individuals.
#' @param size Number of individuals to be selected.
#' @param ... Ignored
#' @details This function takes random pairs of individuals and selects the best one
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