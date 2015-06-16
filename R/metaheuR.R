# NON-EXPORTED FUNCTIONS -------------------------------------------------------

checkSolutions <- function(solution, non.valid, valid, correct){
  # Auxiliar function to process a solution according to its validity
  # Args:
  #   solution: Solution to process
  #   non.valid: String indicating how non-valid solutions have to be processed
  #   valid:     Function to check the validity of the solution
  #   correct:   Function to correct the solution
  # Return:
  #   The solution processed
  #
  if (!valid(solution) && non.valid != "ignore") {
    if (non.valid == "discard"){
      solution <- NULL
    } else if(non.valid == "correct") {
      solution <- correct(solution)
    }
  }
  return(solution)
}


# DUMMY FUNCTIONS TO BE USED AS DEFAULT PARAMETERS -----------------------------

allValid <- function (solution) {
  return(TRUE)
}

doNothing <- function(solution) {
  return(solution)
}

