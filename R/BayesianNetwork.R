#' An S4 class to represent distributions based on Bayesian networks
#'
#' @slot network An object of the type bn.fit (see bnLearn pacakge for details)
#' @slot factor.population A logical value which determine if the learned population was factor or not.
#' 

setClass(
	Class="BayesianNetwork", 
	representation=representation(network="bn.fit", factor.population="logical")
)


setValidity(
   Class="BayesianNetwork", 
   method=function(object) {
     return (TRUE)
})


# GENERIC METHODS ---------------------------------------------------------
setMethod(
	f="simulate", 
	signature="BayesianNetwork", 
	definition=function(object, nsim=1, seed=NULL, ...) {
	  if (!require("bnlearn")) {
	    message("This function requires the bnlearn package. It can be installed running the command install.packages('bnlearn').")
	    ans <- readline(prompt="Do you want me to install it now (Y/N, default N)?")
	    if (ans=="y" | ans=="Y") {
	      install.packages("bnlearn")
	    }
	  }
	  
	  if(!is.null(seed)) {
	    set.seed(seed)
	  }
	  aux <- rbn(object@network, nsim, debug=F)
	  aux <- as.list(as.data.frame(t(aux)))
	  if(object@factor.population){
	    # We must return the same type of population which was given before
      for(i in 1:length(aux)){
        for(j in 1:length(aux[[i]])){
          if(is.na(aux[[i]][j])){
            aux[[i]][j] <- sample(levels(aux[[i]]), 1)
          }
        }
      }
	  }else{
	    aux <- lapply(aux, FUN=cat2bin)
	    for(i in 1:length(aux)){
	      for(j in 1:length(aux[[i]])){
	        if(is.na(aux[[i]][j])){
	          aux[[i]][j] <- runif(1) >= 0.5
	        }
	      }
	    }  
	  }
	  
	  return(aux)
	})

# CONSTRUCTOR -------------------------------------------------------------

#' Constructor of EBNA model
#' 
#' This function creates an object of class \code{\linkS4class{BayesianNetwork}}
#' 
#' @family EDA
#' @param data The dataframe containing the initial-population to construct the bayesian network
#' @param ... Ignored
#' @return An object of class \code{\linkS4class{BayesianNetwork}} that includes the Bayesian Network of the given data
#' 
bayesianNetwork <- function(data, ...) {
  
  if (!require("bnlearn")) {
    message("This function requires the bnlearn package. It can be installed running the command install.packages('bnlearn').")
    ans <- readline(prompt="Do you want me to install it now (Y/N, default N)?")
    if (ans=="y" | ans=="Y") {
      install.packages("bnlearn")
    }
  }
  
	if(class(data) == "list"){
	  if(class(data[[1]]) == "logical"){
	    # The model is going to learn from a logical population
	    factor.population <- F # is not a factor population
	    aux <- lapply(data, FUN=bin2cat)
	    pop.cat.df <- data.frame(t(matrix(unlist(aux), nrow = length(aux[[1]]), byrow = F)), stringsAsFactors = T)
	    names(pop.cat.df) <- paste("X", 1:length(pop.cat.df), sep = "")
	    for(i in 1:length(pop.cat.df)){
	      pop.cat.df[[i]] <- factor(x=pop.cat.df[[i]], levels=c("T","F"))
	    }
	  }else if(class(data[[1]]) == "factor"){
	    factor.population <- T
	    pop.cat.df <- data.frame(t(matrix(unlist(data), nrow = length(data[[1]]), byrow=F)), stringsAsFactors = T)
	  }
		network <- hc(x = pop.cat.df)
		network <- bn.fit(network, data=pop.cat.df)
		obj <- new("BayesianNetwork", network = network, factor.population = factor.population)
	}else{
		stop("The data must be a list")
	}
  return(obj)
}

#' Converts a vector from categorical to logical.
#' @param catVector categorical vector to be converted.
#' @return a logical vector
cat2bin <- function(catVector){
  return(catVector == "T")
}

#' Converts a vector from logical to categorical.
#' @param bin.v logical vector to be converted.
#' @return a categorical vector
bin2cat <- function(bin.v){
  chr.v <- rep("F", length(bin.v))
  chr.v[bin.v] <- "T"
  catVector <- factor(chr.v, levels=c("T", "F"))
  return(catVector)
}

#' Calculates the probabilities of the given model for the especified population.
#' @param object object of the class BayesianNetwork.
#' @param data population of individuals whoose probabilities are desired to know.
#' @return a vector containing the probabilities.
#' 
calc.prob <- function(object, data){
  if(class(object) == "BayesianNetwork"){
    aux <- lapply(data, FUN=bin2cat)
    pop.cat.df <- data.frame(t(matrix(unlist(aux), nrow = length(aux[[1]]), byrow = F)), stringsAsFactors = T)
    names(pop.cat.df) <- paste("X", 1:length(pop.cat.df), sep = "")
    for(i in 1:length(pop.cat.df)){
      pop.cat.df[[i]] <- factor(x=pop.cat.df[[i]], levels=c("T","F"))
    }
    logverosimilitud <- logLik(object@network, pop.cat.df, by.sample=TRUE)
    return(exp(logverosimilitud))
  }else{
    stop("The model must be of the class BayesianNetwork")
  }
}