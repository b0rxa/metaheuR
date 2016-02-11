#' An S4 class to represent distributions based on univariate marginals
#'
#' @slot population The population where the network is based on
#' @slot network An object of the type bn (see BNLearn pacakge for details)
#' @slot levels A list of levels in case the model is constructed over factors. If it is to be used with logical values, then this slot will be \code{NULL}.
#' 

# We import bnLearn Package
require(bnlearn)
require(gRain)
setClass(
	Class="BayesianNetwork", 
	representation=representation(network="ANY")
	# slots = c(network="CPTgrain", numColumns="integer")
	# representation=representation(prob.table="matrix", 
	#                               binary="logical")
)


#REPASAR LA VALIDACION
#setValidity(
#   Class="BayesianNetwork", 
#   method=function(object) {
#     m <- object@network
#     if (!all(object@network <= 1 & object@network >= 0)) {
#       stop("All the values in the prob table have to be between 0 and 1")
#     }
		
#     if (object@binary) {
#       if (nrow(object@prob.table) != 1) {
#         stop("For models over logical variables the 'prob.table' should only ",
#              "contain one row, representing the probability of the variable being 'TRUE'")
#       }
#     } else {
#       if (is.null(rownames(object@prob.table))) {
#         stop("For factor models the rows in the probability table have to be named ",
#              "with the level values")
#       }
#       if (nrow(object@prob.table) < 2) {
#         stop("For factor models there should be at least two possible values")
#       }
#       # Note that, due to rounding problems, the summ may not be exactly 1
#       if (max(colSums(object@prob.table) - 1) > 1 / (100 * nrow(object@prob.table))) {
#         stop("The colºumn-wise sums in 'prob.table' should be 1")
#       }
#     }
#     return (TRUE)
#})


# GENERIC METHODS ---------------------------------------------------------
setMethod(
	f="simulate", 
	signature="BayesianNetwork", 
	definition=function(object, nsim, ...) {      
		return(cat2bin(simulate(object@network, nsim = nsim)))
	})

# CONSTRUCTOR -------------------------------------------------------------

#' Bais consturctor of flip neighborhoods
#' 
#' This function creates an object of class \code{\linkS4class{FlipNeighborhood}}
#' 
#' @family EDA
#' @param data The dataframe containing the initial-population to contruct the bayesian network
#' @param ... Ignored
#' @return An object of class \code{\linkS4class{BayesianNetwork}} that includes the Bayesian Network of the given data
#' 
bayesianNetwork <- function(data) {
	if(class(data) == "list"){  
		# We can create the object
		data <- data.frame(data)
	  aux <- lapply(data, FUN=bin2cat)
		pop.cat.df <- data.frame(aux)
		names(pop.cat.df) <- paste("X", 1:length(pop.cat.df), sep = "")
		network <- hc(x = pop.cat.df)
		adjacencyMatrix <- amat(network)
		#n <- ncol(adjacencyMatrix)
		aux <- as(adjacencyMatrix, "graphNEL")
		network <- grain(x = aux, data = pop.cat.df, smooth = 1)
		obj <- new("BayesianNetwork", network = network)
		return(obj)
	}else{
		stop("The data must be a list")
	}
}

cat2bin <- function(catVector){
  return(catVector == "T")
}

bin2cat <- function(bin.v){
  chr.v <- rep("F", length(bin.v))
  chr.v[bin.v] <- "T"
  catVector <- factor(chr.v, levels=c("T", "F"))
  return(catVector)
}