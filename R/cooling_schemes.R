#' Linear decreasing of the temperature
#' 
#' This function updates the temperature decreasing it linearly
#' @family Cooling schemes
#' @param initial.temperature Starting temperature
#' @param final.temperature Final expected temperature
#' @param steps Number of desired steps to get from the initial temperature to the final one
#' @return A function that, given a temperature, returns the updated temperature according to the cooling scheme. The return function uses just a single parameter, the current temperature
#' @examples
#' boltzmann.accept (delta = -1 , temperature = 2)
#' boltzmann.accept (delta = -3 , th = -2)
#' boltzmann.accept (delta = -1 , th = 0)

linear.cooling <- function (initial.temperature, final.temperature, steps){
  delta <- (initial.temperature - final.temperature) / steps
  f <- function (temperature , ...){
    temperature - delta
  }
  f
}