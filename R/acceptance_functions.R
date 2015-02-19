#' Dummy function to always accept changes
#' 
#' This function returns \code{TRUE}, regardless the increment in the evaluation function
#' @family Acceptance functions
#' @param delta Increment in the evaluation function computed as the evaluation of the new solution minus the evaluation of the existing solution
#' @return This function always accepts the new solution, so it returns always \code{TRUE}
#' @seealso \code{\link{threshold.accept}}, \code{\link{boltzmann.accept}}
#' @examples
#' always.accept (-1)
#' always.accept (1)

always.accept <- function (delta , ...) return(TRUE)

#' Acceptance based on a threshold
#' 
#' This function only accepts solutions (returns \code{TRUE}) when the increment in the objective function value is below a given threhold
#' @family Acceptance functions
#' @param delta Increment in the evaluation function computed as the evaluation of the new solution minus the evaluation of the existing solution
#' @param th Threshold for the acceptance. By default, the threshold is 0, meaning that the solution is accepted if it improves the evaluation function
#' @return \code{TRUE} when the increment is bellow the threshold
#' @seealso \code{\link{always.accept}}, \code{\link{boltzmann.accept}}
#' @examples
#' threshold.accept (delta = -1 , th = -2)
#' threshold.accept (delta = -3 , th = -2)
#' threshold.accept (delta = -1 , th = 0)

threshold.accept <- function (delta, th = 0 , ...){
  return(delta < th)
}

#' Probabilistic acceptance based on Boltzmann's distribution
#' 
#' This function accepts solutions (returns \code{TRUE}) with probability given by Boltzmann's distribution
#' @family Acceptance functions
#' @param delta Increment in the evaluation function computed as the evaluation of the new solution minus the evaluation of the existing solution
#' @param temperature Temperature parameter in Boltzmann's distribution. This temperature has to be a value strictly greater than 0. By default this parameter is equal to 1
#' @return \code{TRUE} with probability given by exp(-1*delta/tempreature)
#' @seealso \code{\link{always.accept}}, \code{\link{threshold.accept}}
#' @examples
#' boltzmann.accept (delta = -1 , temperature = 2)
#' boltzmann.accept (delta = -3 , th = -2)
#' boltzmann.accept (delta = -1 , th = 0)

boltzmann.accept <- function (delta, temperature = 1 , ...){
  if (temperature <= 0) stop("The temperature has to be strictly greter than 0")
  accept <- TRUE
  if (delta>0){
    accept <- runif(1) < exp(-1*delta/temperature)
  }
  accept
} 