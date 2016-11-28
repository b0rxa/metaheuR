#' TSP problem evaluator
#' 
#' This function generates an evaluation function associated
#' with a TSP problem
#' @param cmatrix Cost matrix for the TSP problem
#' @param coordinates Optinally, the coordinates of each city can be provided.
#' @return A list with three elements,  \code{evaluate}, a function to evaluate
#' solutions, \code{size}, the number of cities in the problem and, \code{plotSolution}, 
#' a function to plot a solution for the problem. Note that if the coordinates are
#' not provided, the plotting function will rise an error.
#' @family Problems
#' @examples
#' cmatrix <- matrix(runif(100), ncol=10)
#' tsp <- tspProblem(cmatrix)
#' tsp$evaluate(randomPermutation(10))
#' 
#' coord <- matrix(runif(14), ncol=2)
#' rownames(coord) <- paste0("C", 1:7)
#' cmat <- as.matrix(dist(coord))
#' tsp <- tspProblem(cmat, coord)
#' tsp$plotSolution(randomPermutation(7), plot.names=TRUE)
#' 
tspProblem <- function(cmatrix, coordinates=NULL) {
  if (diff(dim(cmatrix)) != 0) {
    stop ("The cost matrix should be square")
  }
  if (!is.null(coordinates) && nrow(coordinates)!=nrow(cmatrix)) {
    stop ("The coordinates and cmatrix arguments should have the same number of rows")
  }
  if (!is.null(coordinates) && ncol(coordinates)!=2) {
    stop("The coordinates matrix should have just two columns")
  }
  
  evaluate <- function(solution) {
    if (!isClass(solution, "Permutation")) {
      stop("This function only evaluates objects of class permutation")
    }
    if (length(solution) != dim(cmatrix)[1]) { 
      stop("The solution is not of the correct length. It should have ",
           dim(cmatrix)[1], " positions")
    }
    # Generate the pairs for the positions in the matrix
    ids <- cbind(as.numeric(solution), 
                 as.numeric(insert(solution, 1, length(solution))))
    # Sum the values in the generated positions
    cost <- sum(cmatrix[ids])
    return(cost)
  }
  
  plotSolution <- function(solution, path.color="red", path.width=1, point.size=7, 
                           point.color="gray", plot.names=FALSE, name.size=3, name.color="black"){
    
    if (is.null(coordinates)) {
      stop("The problem does not contain the coordinates of the cities and, thus, solutions cannot be plotted")
    }
    loadPackage("ggplot2")
    # Order the cities according to the solution
    coords <- coordinates[as.numeric(solution), ]
    # Add the first city at the end to close the circuit
    coords <- rbind(coords, coords[1, ])
    colnames(coords) <- c("X", "Y")
    
    # Now include the name information
    df <- data.frame(coords)
    if (!is.null(rownames(df)) & plot.names){
      df <- cbind(df, name=paste0(c(1:(nrow(df)-1), 1),": ",c(rownames(df)[c(1:(nrow(df)-1),1)])))
    }
    
    thm <- theme_bw() + theme(axis.text=element_blank(), axis.ticks=element_blank(), panel.grid=element_blank())
    g <- ggplot(df, aes(x=X, y=Y)) + geom_path(size=path.width, color=path.color) + 
      geom_point(data=df[-1,], size=point.size, color=point.color) + thm + labs(x="", y="")
    if (plot.names) {
      g <- g + geom_text(aes(label=name), size=name.size, color=name.color)
    }
    g
  }
  
  return(list(evaluate=evaluate, size=nrow(cmatrix), plotSolution=plotSolution))
}


#' QAP problem evaluator
#' 
#' This function generates an evaluation function associated
#' with a QAP problem
#' @param fmatrix Flow matrix for the QAP problem
#' @param dmatrix Distance matrix for the QAP problem
#' @return A function that can be used to evaluate solutions for a QAP problem
#' @family Problems
#' @examples
#' fmatrix <- matrix(runif(100), ncol=10)
#' dmatrix <- matrix(runif(100), ncol=10)
#' qap <- qapProblem(fmatrix, dmatrix)
#' qap$evaluate(randomPermutation(10))
#' 
qapProblem<-function(fmatrix, dmatrix) {
  if (diff(dim(fmatrix)) != 0) {
    stop ("The flow matrix should be square")
  }
  if (diff(dim(dmatrix)) != 0) {
    stop ("The distance matrix should be square")
  }
  if (!all(dim(fmatrix) == dim(dmatrix))) {
    stop ("The flow matrix and the distance matrix should have the same dimension")
  }
  evaluate <- function(solution) {
    if (!isClass(solution, "Permutation")) {
      stop("This function only evaluates objects of class permutation")
    }
    if (length(solution) != dim(fmatrix)[1]) {
      stop("The solution is not of the correct length. It should have ",
           dim(dmatrix)[1], " positions")
    }
    # Calculate the objective function
    cost <- sum(fmatrix * dmatrix[as.numeric(solution), as.numeric(solution)])
    return(cost)
  }
  return(list(evaluate=evaluate, size=nrow(dmatrix)))
}


#' LOP problem evaluator
#' 
#' This function generates an evaluation function associated
#' with a LOP problem
#' @param matrix matrix for the LOP problem
#' @return A function that can be used to evaluate solutions for a LOP problem
#' @family Problems
#' @examples
#' matrix <- matrix(runif(100), ncol=10)
#' lop <- lopProblem(matrix)
#' lop$evaluate(randomPermutation(10))
#' 
lopProblem <- function(matrix) {
  if (diff(dim(matrix)) != 0) {
    stop ("The matrix should be square")
  }
  evaluate <- function(solution) {
    if (!isClass(solution, "Permutation")) {
      stop("This function only evaluates objects of class permutation")
    }
    if (length(solution) != dim(matrix)[1]) {
      stop("The solution is not of the correct length. It should have ",
           dim(matrix)[1], " positions")
    }
    # Order the matrix by using the solution 
    matrix <- matrix[as.numeric(solution), as.numeric(solution)]
    # Sum the values in the lower triangle (without the diagonal)
    cost <- sum(matrix[lower.tri(matrix, diag=FALSE)])
    return(cost)
  }
  return(list(evaluate=evaluate, size=nrow(matrix)))
}


#' PFSP problem evaluator
#' 
#' This function generates an evaluation function associated with a PFSP problem
#' @param job.machine.matrix Matrix containing the time required by each job in each machine. 
#' Each row is a machine and each column a job
#' @return A list with three elements,  \code{evaluate}, a function to evaluate
#' solutions, \code{num.jobs}, the number of jobs, \code{num.machines}, the number of machines
#' and, \code{plotSolution}, a function to plot a solution for the problem. 
#' @family Problems
#' @examples
#' cmatrix <- matrix(runif(100), ncol=5)
#' tsp <- pfspProblem(cmatrix)
#' tsp$evaluate(randomPermutation(10))
#' 
#' coord <- matrix(runif(14), ncol=2)
#' rownames(coord) <- paste0("C", 1:7)
#' cmat <- as.matrix(dist(coord))
#' tsp <- tspProblem(cmat, coord)
#' tsp$plotSolution(randomPermutation(7), plot.names=TRUE)
#' 
pfspProblem <- function(job.machine.matrix) {
  
  getEndTimes <- function(job.length) {
    # This auxiliar function computes the total flow time of the jobs in the
    # 1 to n order.
    
    # We create a matix with all values equal to -1
    end.positions <- matrix(rep(-1, prod(dim(job.length))), ncol=ncol(job.length))
    
    # The first row and column are respectively the cumulative sums of the job lengths
    end.positions[1, ] = cumsum(job.length[1, ])
    end.positions[, 1] = cumsum(job.length[, 1])
    
    # Now we have the recursion. For each position i,j, the starting point will be 
    # the maximum value of the cell above (i-1, j) or to the left (i, j-1).
    for (i in 2:nrow(end.positions)) {
      for (j in 2:ncol(end.positions)) {
        end.positions[i, j] <- max(end.positions[i - 1, j], end.positions[i, j - 1]) + job.length[i, j]
      }
    }
    return(end.positions)
  }
  
  evaluate <- function(solution) {
    if (!isClass(solution, "Permutation")) {
      stop("This function only evaluates objects of class permutation")
    }
    if (length(solution) != ncol(job.machine.matrix)) { 
      stop("The solution is not of the correct length. It should have ",
           ncol(job.machine.matrix), " positions")
    }
    
    # Get the end timeswith the permutation
    end.times <- getEndTimes(job.machine.matrix[, as.numeric(solution)])
    return(sum(end.times[, ncol(end.times)]))
  }
  
  plotSolution <- function(solution){
    # TODO
    return(NULL)
  }
  
  return(list(evaluate=evaluate, num.jobs=ncol(job.machine.matrix), num.machines=nrow(job.machine.matrix), plotSolution=plotSolution))
}
