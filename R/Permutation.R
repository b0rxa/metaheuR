
# CLASS DEFINITION --------------------------------------------------------
#' An S4 class to represent permutations.
#'
#' @slot permutation A numeric vector containing the permutation. This vector has to be a valid premutation
setClass(
  Class = "Permutation", 
  representation = representation(permutation = "numeric")
)

setValidity(
  Class = "Permutation", 
  method = function(object){
    n<-length(object@permutation)
    present<-rep(FALSE,n)
    present[object@permutation]<-TRUE
    if (!all(present)){
      stop("The vector passed is not a permutation")
    }else{
      return(TRUE)
    }
    }
)


# GENERIC METHODS ---------------------------------------------------------

setMethod(
  f="==", 
  signature = "Permutation", 
  definition = function(e1,e2) {
    all(e1@permutation==e2@permutation)
  }
)

setMethod(
  f="[", 
  signature = "Permutation", 
  definition = function(x,i,j,drop) {
    x@permutation[i]
  }
)

setMethod(f="length" , 
          signature = "Permutation" , 
          definition = function(x){
            length(x@permutation)
          } )


setMethod(f="as.numeric" , 
          signature = "Permutation" , 
          definition = function(x , ...){
            x@permutation
          } )


if (!isGeneric("swap")) setGeneric(name = "swap", def = function(permutation, i1, i2){standardGeneric("swap")})

##' Function to swap positions in a permutation
##' 
##' @description This function swaps two positions in a permutation
##' @param permutation Object of class \code{\linkS4class{Permutation}} whose positions will be swapped
##' @param i1 First position to be permuted
##' @param i2 Second position to be permuted
##' @return A new object of class \code{\linkS4class{Permutation}} equal to \code{permutation} but with positions \code{i1} and \code{i2} swapped

setMethod(
  f="swap", 
  signature = "Permutation", 
  definition = function(permutation,i1,i2) {
    if (min(i1,i2)<0 | max(i1,i2)>length(permutation@permutation))
      stop("The indices are out of range")
    
    newpermu<-as.numeric(permutation)
    aux<-newpermu[i1]
    newpermu[i1]<-newpermu[i2]
    newpermu[i2]<-aux
    permutation(newpermu)
  }
)



if (!isGeneric("shuffle")) setGeneric(name = "shuffle", def = function(permutation, ratio=1){standardGeneric("shuffle")})

##' Function to randomize permutations
##' 
##' @description This function reorders the elements of a permutation randomly
##' @param permutation Object of class \code{\linkS4class{Permutation}} to be randomized
##' @param ratio Ratio of positions to be randomly reordered. Note that this value can be grater than 1 if we want to perform more random movements than positions in the permutation
##' @return A new object of class \code{\linkS4class{Permutation}} with randomly reordered positions

setMethod(
  f="shuffle", 
  signature = "Permutation", 
  definition = function(permutation, ratio=1) {
    n<-length(permutation)
    for (i in 1:(n*ratio)) permutation<-swap(permutation,sample(1:n,1),sample(1:n,1))
    permutation
  }
)

if (!isGeneric("insert")) setGeneric(name = "insert", def = function(permutation,i1,i2){standardGeneric("insert")})


##' Function to insert one element of a permutation into another position
##' 
##' @description This function performs the input operation in permutations
##' @param permutation Object of class \code{\linkS4class{Permutation}} where the insert will be carried out
##' @param i1 Element to be inserted
##' @param i2 Position where the element will be inserted
##' @return A new object of class \code{\linkS4class{Permutation}} equal to \code{permutation} but with \code{i1}-th element inserted into \code{i2}

setMethod(
  f="insert", 
  signature = "Permutation", 
  definition = function(permutation , i1 , i2) {    
    if (min(i1,i2)<0 | max(i1,i2)>length(permutation@permutation))
      stop("The indices are out of range")
    
    n<-length(permutation@permutation)
    if (i1<i2){
      if (i1==1){
        newpermu<-2:i2  
      }else{
        newpermu<-c(1:(i1-1),(i1+1):i2)
      }
      if (i2==n){
        newpermu<-c(newpermu,i1)
      }else{
        newpermu<-c(newpermu,c(i1,(i2+1):n))
      }
    }else{
      if (i2==1){
        newpermu<-i1  
      }else{
        newpermu<-c(1:(i2-1),i1)
      }
      if (i1==n){
        newpermu<-c(newpermu,i2:(n-1))
      }else{
        newpermu<-c(newpermu,c(i2:(i1-1),(i1+1):n))
      }
    }
    permutation(permutation@permutation[newpermu])
  }
)


# CONSTRUCTORS ------------------------------------------------------------

permutation <- function(vector){
  new(Class = "Permutation" , permutation = vector)
}

identity.permutation <- function(length){
  new(Class = "Permutation" , permutation = 1:length)
}

random.permutation <- function(length){
  new(Class = "Permutation" , permutation = sample(1:length))
}
