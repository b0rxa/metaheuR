#' Linear decreasing of the temperature
#' 
#' @export
#' @description This function updates the temperature decreasing it linearly
#' @param initial.temperature Starting temperature
#' @param final.temperature Final expected temperature
#' @param steps Number of desired steps to get from the initial temperature to the final one
#' @return A function that, given a temperature, returns the updated temperature according to the cooling scheme. The return function uses just a single parameter, the current temperature
#' @family Cooling schemes
#' @examples
#' update <- linearCooling(initial.temperature=100, final.temperature=10, steps=9)
#' update(100)
#' update(update(100))
#' 
linearCooling <- function (initial.temperature, final.temperature, steps) {
  delta <- (initial.temperature - final.temperature) / steps
  f <- function (temperature, ...) {
    return(temperature - delta)
  }
  return(f)
}

#' Geometric decreasing of the temperature
#' 
#' @export
#' @description This function updates the temperature decreasing it geometrically
#' @param initial.temperature Starting temperature
#' @param final.temperature Final expected temperature
#' @param steps Number of desired steps to get from the initial temperature to the final one
#' @return A function that, given a temperature, returns the updated temperature according to the cooling scheme. The return function uses just a single parameter, the current temperature
#' @family Cooling schemes
#' @examples
#' update <- geometricCooling(initial.temperature=100, final.temperature=10, steps=9)
#' update(100)
#' update(update(100))
#' 
geometricCooling <- function (initial.temperature, final.temperature, steps) {
  alfa <- (final.temperature / initial.temperature)^(1 / round(steps))
  f <- function (temperature, ...) {
    return(temperature * alfa)
  }
  return(f)
}