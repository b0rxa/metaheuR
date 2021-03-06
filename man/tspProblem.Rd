% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/permutation_problems.R
\name{tspProblem}
\alias{tspProblem}
\title{TSP problem evaluator}
\usage{
tspProblem(cmatrix, coordinates = NULL)
}
\arguments{
\item{cmatrix}{Cost matrix for the TSP problem}

\item{coordinates}{Optinally, the coordinates of each city can be provided.}
}
\value{
A list with three elements,  \code{evaluate}, a function to evaluate
solutions, \code{size}, the number of cities in the problem and, \code{plotSolution}, 
a function to plot a solution for the problem. Note that if the coordinates are
not provided, the plotting function will rise an error.
}
\description{
This function generates an evaluation function associated
with a TSP problem
}
\examples{
cmatrix <- matrix(runif(100), ncol=10)
tsp <- tspProblem(cmatrix)
tsp$evaluate(randomPermutation(10))

coord <- matrix(runif(14), ncol=2)
rownames(coord) <- paste0("C", 1:7)
cmat <- as.matrix(dist(coord))
tsp <- tspProblem(cmat, coord)
tsp$plotSolution(randomPermutation(7), plot.names=TRUE)

}
\seealso{
Other Problems: \code{\link{graphColoringProblem}},
  \code{\link{knapsackProblem}}, \code{\link{lopProblem}},
  \code{\link{mdsProblem}}, \code{\link{misProblem}},
  \code{\link{pfspProblem}}, \code{\link{qapProblem}}
}

